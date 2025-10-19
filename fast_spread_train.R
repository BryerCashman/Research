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
options(dplyr.summarise.inform = FALSE)
set.seed(123)

# ===============================================
# Load and Prepare Data
# ===============================================
computer <- "h"
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

# ===============================================
# Optimizer Function
# ===============================================
optimize_spread <- function(bases) {
  base_off <- bases[1]
  base_def <- bases[2]
  base_qb  <- bases[3]
  
  # offense EWM
  off_dt <- data[, .(season, posteam, game_id, game_date, epa, success, pass_oe)]
  off_dt <- compute_ewm(off_dt, "posteam", "epa", base_off)
  off_dt[, `:=`(
    ewm_epa_play = ewm_irregular_lagged(epa, days_gap, base_off),
    ewm_success_rate = ewm_irregular_lagged(success, days_gap, base_off),
    ewm_proe = ewm_irregular_lagged(pass_oe, days_gap, base_off)
  ), by = posteam]
  offense_data <- off_dt[, .(epa_pp = first(ewm_epa_play),
                             sr = first(ewm_success_rate),
                             proe = first(ewm_proe)), 
                         by = .(season, posteam, game_id, game_date)]
  
  # defense EWM
  def_dt <- data[, .(season, defteam, game_id, game_date, epa, success)]
  def_dt <- compute_ewm(def_dt, "defteam", "epa", base_def)
  def_dt[, `:=`(
    ewm_epa_play = ewm_irregular_lagged(epa, days_gap, base_def),
    ewm_success_rate = ewm_irregular_lagged(success, days_gap, base_def)
  ), by = defteam]
  defense_data <- def_dt[, .(epa_pp_allowed = first(ewm_epa_play),
                             sr_allowed = first(ewm_success_rate)),
                         by = .(season, defteam, game_id, game_date)]
  
  # qb EWM
  qb_dt <- data[id %in% master_id_list,
                .(season, name, id, game_id, game_date,
                  qb_epa, qb_dropback, play_type, epa)]
  qb_dt <- compute_ewm(qb_dt, "id", "qb_epa", base_qb)
  qb_dt[, dbs := shift(cumsum(!is.na(epa))), by = id]
  qb_dt[, pred_qb_epa := predict(model_pred_qb_epa, .SD[, .(ewm, dbs)])]
  qb_data <- qb_dt[, .(qb_epa = first(ewm), dbs = first(dbs),
                       pred_qb_epa = first(pred_qb_epa)),
                   by = .(season, id, game_id, game_date)]
  
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
                 s(pred_qb_epa_home) + s(pred_qb_epa_away),
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
cl <- makeCluster(detectCores() - 1, type = "PSOCK")
clusterEvalQ(cl, {
  library(data.table)
  library(mgcv)
  library(Metrics)
  NULL
})
clusterExport(cl, c("optimize_spread", "data", "games", "master_id_list",
                    "ewm_irregular_lagged", "model_pred_qb_epa"),
              envir = environment())

ctrl <- DEoptim.control(
  steptol = 5, itermax = 20, trace = TRUE,
  parallelType = 2, cluster = cl
)

set.seed(1)
result <- DEoptim(
  fn = optimize_spread,
  lower = c(.9, .9, .9),
  upper = c(.999, .999, .999),
  control = ctrl
)

stopCluster(cl)
result
