# ===============================================
# Libraries
# ===============================================
library(data.table)
library(tidyverse)
library(lubridate)
library(mgcv)
library(DEoptim)
library(Metrics)
library(parallel)
library(nflreadr)
options(dplyr.summarise.inform = FALSE)
set.seed(123)

# ===============================================
# Load and Prepare Data
# ===============================================
computer <- "W"
path <- ifelse(computer == "W",
               "C:/Users/b.cashman/Documents/GitHub/Research/model_pred_qb_epa.RDS",
               "/Users/bryer/Documents/GitHub/Research/model_pred_qb_epa.RDS")
load(file = path)

convert_name <- function(name) {
  first_initial <- str_sub(name, 1, 1)
  last_name <- str_extract(name, "(?<= )[A-Za-z'\\-]+")
  suffix <- str_extract(name, "(?<= )[IVXLC]+$|Jr\\.?$")
  ifelse(!is.na(suffix),
         str_c(first_initial, ".", last_name, " ", suffix),
         str_c(first_initial, ".", last_name))
}

# faster version of EWM
ewm_irregular_lagged <- function(x, days_gap, base) {
  s <- w <- 0
  out <- numeric(length(x))
  for (i in seq_along(x)) {
    out[i] <- if (w > 0) s / w else NA_real_
    k <- base ^ days_gap[i]
    s <- s * k + x[i]
    w <- w * k + 1
  }
  out
}

# nflreadr -> data.table
data <- as.data.table(
  load_pbp(2019:2025) |>
    filter(season_type == "REG", (rush == 1 | pass == 1), !is.na(epa))
)
data[, game_date := as.Date(game_date)]

sched <- as.data.table(
  nflreadr::load_schedules() |>
    filter(season %in% 2019:2025) |>
    mutate(
      home_qb = convert_name(home_qb_name),
      away_qb = convert_name(away_qb_name),
      home_team = recode(home_team, OAK = "LV", SD = "LAC", STL = "LA"),
      away_team = recode(away_team, OAK = "LV", SD = "LAC", STL = "LA")
    )
)

master_id_list <- unique(c(sched$home_qb_id, sched$away_qb_id))
games <- sched[season %in% 2019:2025 & game_type == "REG" & !is.na(home_score),
               .(game_id, game_date = as.Date(gameday),
                 home_team, away_team,
                 point_diff = result,
                 starting_home_id = home_qb_id,
                 starting_away_id = away_qb_id)]

# ===============================================
# Helper: Efficient Rolling EWM by entity
# ===============================================
compute_ewm <- function(dt, idcol, valcol, base, datecol = "game_date") {
  setorder(dt, get(idcol), get(datecol))
  dt[, days_gap := pmax(0, as.numeric(get(datecol) - shift(get(datecol), fill = first(get(datecol))))),
     by = idcol]
  dt[, ewm := ewm_irregular_lagged(get(valcol), days_gap, base), by = idcol]
  dt
}

# ---- 3-season window builder (no `get()`) ----
build_windowed_features <- function(dt, entity, make_fn, base, keep_cols) {
  seasons <- sort(unique(dt$season))
  out_list <- vector("list", length(seasons))
  j <- 0L
  for (S in seasons) {
    win <- (S - 2):S
    sub <- dt[season %in% win]
    sub[, anchor_season := S]
    sub <- make_fn(sub, entity, base)          # add EWM cols grouped by anchor season + entity
    # snapshot first play of each game (what we knew at kickoff)
    snap <- sub[, .SD[1L], by = c("season", "game_id", "game_date", entity)]
    # keep rows for anchor season S only
    snap <- snap[season == S]
    # select just the columns we want
    snap <- snap[, ..keep_cols]
    j <- j + 1L
    out_list[[j]] <- snap
  }
  rbindlist(out_list[seq_len(j)], use.names = TRUE, fill = TRUE)
}

# ---- offense maker (no `get()`) ----
off_make <- function(dt, entity, base_off) {
  # order by entity + game
  setorderv(dt, c(entity, "game_date", "game_id", "play_id"))
  # days gap per entity
  dt[, days_gap := pmax(0, as.numeric(game_date - shift(game_date, type = "lag", fill = first(game_date)))),
     by = entity]
  # EWM reset per (anchor_season, entity)
  dt[, ewm_epa_play     := ewm_irregular_lagged(epa,     days_gap, base_off), by = c("anchor_season", entity)]
  dt[, ewm_success_rate := ewm_irregular_lagged(success, days_gap, base_off), by = c("anchor_season", entity)]
  dt[, ewm_proe         := ewm_irregular_lagged(pass_oe, days_gap, base_off), by = c("anchor_season", entity)]
  dt
}

# ---- defense maker (no `get()`) ----
def_make <- function(dt, entity, base_def) {
  setorderv(dt, c(entity, "game_date", "game_id", "play_id"))
  dt[, days_gap := pmax(0, as.numeric(game_date - shift(game_date, type = "lag", fill = first(game_date)))),
     by = entity]
  dt[, ewm_epa_play     := ewm_irregular_lagged(epa,     days_gap, base_def), by = c("anchor_season", entity)]
  dt[, ewm_success_rate := ewm_irregular_lagged(success, days_gap, base_def), by = c("anchor_season", entity)]
  dt
}

# ---- QB maker (no `get()`) ----
qb_make <- function(dt, entity, base_qb) {
  setorderv(dt, c(entity, "game_date", "game_id", "play_id"))
  dt[, days_gap := pmax(0, as.numeric(game_date - shift(game_date, type = "lag", fill = first(game_date)))),
     by = entity]
  dt[, ewm_qb_epa_play := ewm_irregular_lagged(qb_epa, days_gap, base_qb), by = c("anchor_season", entity)]
  dt[, dbs := shift(cumsum(!is.na(epa))), by = entity]
  # predict with the features we just built
  dt[, pred_qb_epa := predict(model_pred_qb_epa, .SD[, .(ewm_qb_epa_play, dbs)])]
  dt
}


# ===============================================
# Optimizer Function
# ===============================================
optimize_spread <- function(bases) {
  base_off <- bases[1]
  base_def <- bases[2]
  base_qb  <- bases[3]
  
  # ---- offense windowed (3-season) ----
  off_keep <- c("season","game_id","game_date","posteam",
                "ewm_epa_play","ewm_success_rate","ewm_proe")
  off_dt <- data[, .(season, posteam, game_id, game_date, play_id, epa, success, pass_oe)]
  offense_data <- build_windowed_features(off_dt, "posteam", off_make, base_off, off_keep)
  setnames(offense_data,
           c("ewm_epa_play","ewm_success_rate","ewm_proe"),
           c("epa_pp","sr","proe"))
  
  # ---- defense windowed (3-season) ----
  def_keep <- c("season","game_id","game_date","defteam",
                "ewm_epa_play","ewm_success_rate")
  def_dt <- data[, .(season, defteam, game_id, game_date, play_id, epa, success)]
  defense_data <- build_windowed_features(def_dt, "defteam", def_make, base_def, def_keep)
  setnames(defense_data,
           c("ewm_epa_play","ewm_success_rate"),
           c("epa_pp_allowed","sr_allowed"))
  
  # ---- QB windowed (3-season) ----
  qb_keep <- c("season","game_id","game_date","id",
               "ewm_qb_epa_play","dbs","pred_qb_epa")
  qb_dt <- data[id %in% master_id_list,
                .(season, id, game_id, game_date, play_id, qb_epa, epa)]
  qb_data <- build_windowed_features(qb_dt, "id", qb_make, base_qb, qb_keep)
  setnames(qb_data, "ewm_qb_epa_play", "qb_epa")
  
  
  
  # join datasets efficiently
  setkey(games, game_id, game_date)
  setkey(qb_data, game_id, game_date)
  setkey(offense_data, game_id, game_date)
  setkey(defense_data, game_id, game_date)
  
  df <- merge(games, qb_data, by.x = c("game_id", "game_date", "starting_home_id"), by.y = c("game_id", "game_date", "id"), all.x = TRUE)
  df <- merge(df, qb_data, by.x = c("game_id", "game_date", "starting_away_id"), by.y = c("game_id", "game_date", "id"), all.x = TRUE, suffixes = c("_home", "_away"))
  df <- merge(df, offense_data, by.x = c("game_id", "game_date", "home_team"), by.y = c("game_id", "game_date", "posteam"), all.x = TRUE)
  df <- merge(df, offense_data, by.x = c("game_id", "game_date", "away_team"), by.y = c("game_id", "game_date", "posteam"), all.x = TRUE, suffixes = c("_home", "_away"))
  df <- merge(df, defense_data, by.x = c("game_id", "game_date", "home_team"), by.y = c("game_id", "game_date", "defteam"), all.x = TRUE)
  df <- merge(df, defense_data, by.x = c("game_id", "game_date", "away_team"), by.y = c("game_id", "game_date", "defteam"), all.x = TRUE, suffixes = c("_home", "_away"))
  df <- na.omit(df)
  
  # train/test split (fixed)
  set.seed(42)
  idx <- sample(seq_len(nrow(df)), 0.75 * nrow(df))
  df_train <- df[idx]
  df_test  <- df[-idx]
  
  # faster GAM fit
  model <- bam(point_diff ~ home_epa_pp + away_epa_pp_allowed +
                 away_epa_pp + home_epa_pp_allowed +
                 s(pred_qb_epa_home, home_proe) + s(pred_qb_epa_away, away_proe),
               data = df_train, method = "fREML",
               discrete = TRUE, nthreads = max(1, detectCores() - 1))
  df_test[, proj_spread := predict(model, df_test)]
  rmse <- Metrics::rmse(df_test$point_diff, df_test$proj_spread)
  cat("Finished iteration at", Sys.time(), "RMSE:", rmse, "\n")
  rmse
}

# ===============================================
# Parallel DEoptim Setup
# ===============================================
cl <- makeCluster(detectCores() - 3, type = "PSOCK")
clusterEvalQ(cl, {
  library(data.table)
  library(mgcv)
  library(Metrics)
  library(tidyverse)
  NULL
})
clusterExport(
  cl,
  c(
    "optimize_spread",
    "data", "games", "master_id_list",
    "ewm_irregular_lagged", "model_pred_qb_epa","compute_ewm",
    # NEW helpers used on workers:
    "build_windowed_features", "off_make", "def_make", "qb_make"
  ),
  envir = environment()
)


ctrl <- DEoptim.control(
  steptol = 5, itermax = 20, trace = TRUE,
  parallelType = 2, cluster = cl
)

set.seed(1)
result <- DEoptim(
  fn = optimize_spread,
  lower = c(.9, .9, .9),
  upper = c(1, 1, 1),
  control = ctrl
)

stopCluster(cl)
result
