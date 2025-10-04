rmse_totals <- function(b){


base <- b

# Irregular-time exponential running *mean* (returns the mean known *before* each row)
ewm_irregular_lagged <- function(x, days_gap, base) {
  stopifnot(length(x) == length(days_gap))
  n <- length(x)
  s <- 0.0  # decayed numerator
  w <- 0.0  # decayed weight
  out <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    # mean *before* seeing row i
    out[i] <- if (w > 0) s / w else NA_real_
    # now update state with row i
    k <- base ^ days_gap[i]          # decay since previous row
    s <- s * k + x[i]                # add current value
    w <- w * k + 1.0                 # add unit weight
  }
  out
}

pbp <- pbp %>% mutate(game_date = as.Date(game_date))

qb_data2021_exp <- pbp %>%
  filter(season %in% c(2020, 2021),
         (pass == 1 | rush == 1),
         !is.na(epa),
         id %in% qbs) %>%
  group_by(name, id) %>%
  arrange(game_date, game_id, play_id, .by_group = TRUE) %>%
  # days since the *previous* row for this QB (0 on first row, 0 for same-day rows)
  mutate(
    days_gap = {
      dg <- as.numeric(game_date - lag(game_date, default = first(game_date)))
      pmax(0, replace(dg, is.na(dg), 0))
    },
    # EWM of qb_epa known *before* this play
    ewm_qb_epa_play = ewm_irregular_lagged(qb_epa, days_gap, base),
    dbs = lag(cumsum(!is.na(epa)))
  ) %>%
  ungroup() %>%
  # snapshot what we knew at kickoff: take the first play's value for each game
  group_by(season, name, id, game_id, game_date) %>%
  summarize(qb_epa = first(ewm_qb_epa_play), 
            dbs = first(dbs), 
            .groups = "drop")


qb_data2122_exp <- pbp %>%
  filter(season %in% c(2021, 2022),
         (pass == 1 | rush == 1),
         !is.na(epa),
         id %in% qbs) %>%
  group_by(name, id) %>%
  arrange(game_date, game_id, play_id, .by_group = TRUE) %>%
  # days since the *previous* row for this QB (0 on first row, 0 for same-day rows)
  mutate(
    days_gap = {
      dg <- as.numeric(game_date - lag(game_date, default = first(game_date)))
      pmax(0, replace(dg, is.na(dg), 0))
    },
    # EWM of qb_epa known *before* this play
    ewm_qb_epa_play = ewm_irregular_lagged(qb_epa, days_gap, base),
    dbs = lag(cumsum(!is.na(epa)))
  ) %>%
  ungroup() %>%
  # snapshot what we knew at kickoff: take the first play's value for each game
  group_by(season, name, id, game_id, game_date) %>%
  summarize(qb_epa = first(ewm_qb_epa_play), 
            dbs = first(dbs), 
            .groups = "drop") %>%
  filter(season == 2022)

qb_data2223_exp <- pbp %>%
  filter(season %in% c(2022, 2023),
         (pass == 1 | rush == 1),
         !is.na(epa),
         id %in% qbs) %>%
  group_by(name, id) %>%
  arrange(game_date, game_id, play_id, .by_group = TRUE) %>%
  # days since the *previous* row for this QB (0 on first row, 0 for same-day rows)
  mutate(
    days_gap = {
      dg <- as.numeric(game_date - lag(game_date, default = first(game_date)))
      pmax(0, replace(dg, is.na(dg), 0))
    },
    # EWM of qb_epa known *before* this play
    ewm_qb_epa_play = ewm_irregular_lagged(qb_epa, days_gap, base),
    dbs = lag(cumsum(!is.na(epa)))
  ) %>%
  ungroup() %>%
  # snapshot what we knew at kickoff: take the first play's value for each game
  group_by(season, name, id, game_id, game_date) %>%
  summarize(qb_epa = first(ewm_qb_epa_play),dbs = first(dbs), 
            .groups = "drop") %>%
  filter(season == 2023)

qb_data2324_exp <- pbp %>%
  filter(season %in% c(2023, 2024),
         (pass == 1 | rush == 1),
         !is.na(epa),
         id %in% qbs) %>%
  group_by(name, id) %>%
  arrange(game_date, game_id, play_id, .by_group = TRUE) %>%
  # days since the *previous* row for this QB (0 on first row, 0 for same-day rows)
  mutate(
    days_gap = {
      dg <- as.numeric(game_date - lag(game_date, default = first(game_date)))
      pmax(0, replace(dg, is.na(dg), 0))
    },
    # EWM of qb_epa known *before* this play
    ewm_qb_epa_play = ewm_irregular_lagged(qb_epa, days_gap, base),
    dbs = lag(cumsum(!is.na(epa)))
  ) %>%
  ungroup() %>%
  # snapshot what we knew at kickoff: take the first play's value for each game
  group_by(season, name, id, game_id, game_date) %>%
  summarize(qb_epa = first(ewm_qb_epa_play),
            dbs = first(dbs), 
            .groups = "drop") %>%
  filter(season == 2024)

qb_data_exp <- rbind(qb_data2021_exp, qb_data2122_exp, qb_data2223_exp, qb_data2324_exp)

offense_2021_exp <- pbp %>%
  filter(season %in% c(2020, 2021),
         (pass == 1 | rush == 1),
         !is.na(epa)) %>%
  group_by(posteam) %>%
  arrange(game_date, game_id, play_id, .by_group = TRUE) %>%
  mutate(
    days_gap = {
      dg <- as.numeric(game_date - lag(game_date, default = first(game_date)))
      pmax(0, replace(dg, is.na(dg), 0))
    },
    # EWM of success known *before* this play
    ewm_sr_play = ewm_irregular_lagged(success, days_gap, base)
  ) %>%
  ungroup() %>%
  group_by(season, posteam, game_id, game_date) %>%
  summarize(sr = first(ewm_sr_play), .groups = "drop") 

offense_2122_exp <- pbp %>%
  filter(season %in% c(2021, 2022),
         (pass == 1 | rush == 1),
         !is.na(epa)) %>%
  group_by(posteam) %>%
  arrange(game_date, game_id, play_id, .by_group = TRUE) %>%
  mutate(
    days_gap = {
      dg <- as.numeric(game_date - lag(game_date, default = first(game_date)))
      pmax(0, replace(dg, is.na(dg), 0))
    },
    # EWM of success known *before* this play
    ewm_sr_play = ewm_irregular_lagged(success, days_gap, base)
  ) %>%
  ungroup() %>%
  group_by(season, posteam, game_id, game_date) %>%
  summarize(sr = first(ewm_sr_play), .groups = "drop") %>%
  filter(season == 2022) 

offense_2223_exp <- pbp %>%
  filter(season %in% c(2022, 2023),
         (pass == 1 | rush == 1),
         !is.na(epa)) %>%
  group_by(posteam) %>%
  arrange(game_date, game_id, play_id, .by_group = TRUE) %>%
  mutate(
    days_gap = {
      dg <- as.numeric(game_date - lag(game_date, default = first(game_date)))
      pmax(0, replace(dg, is.na(dg), 0))
    },
    # EWM of success known *before* this play
    ewm_sr_play = ewm_irregular_lagged(success, days_gap, base)
  ) %>%
  ungroup() %>%
  group_by(season, posteam, game_id, game_date) %>%
  summarize(sr = first(ewm_sr_play), .groups = "drop") %>%
  filter(season == 2023) 

offense_2324_exp <- pbp %>%
  filter(season %in% c(2023, 2024),
         (pass == 1 | rush == 1),
         !is.na(epa)) %>%
  group_by(posteam) %>%
  arrange(game_date, game_id, play_id, .by_group = TRUE) %>%
  mutate(
    days_gap = {
      dg <- as.numeric(game_date - lag(game_date, default = first(game_date)))
      pmax(0, replace(dg, is.na(dg), 0))
    },
    # EWM of success known *before* this play
    ewm_sr_play = ewm_irregular_lagged(success, days_gap, base)
  ) %>%
  ungroup() %>%
  group_by(season, posteam, game_id, game_date) %>%
  summarize(sr = first(ewm_sr_play), .groups = "drop") %>%
  filter(season == 2024) 


offense_exp <- rbind(offense_2021_exp, offense_2122_exp, offense_2223_exp, offense_2324_exp)

game_totals <- pbp %>%
  group_by(game_id) %>%
  dplyr::summarize(total = first(home_score) + first(away_score),
                   home_team = first(home_team),
                   away_team = first(away_team))

df_train <- game_totals %>%
  left_join(offense_exp, by = c("game_id","home_team" = "posteam")) %>%
  rename(home_sr = sr) %>%
  left_join(offense_exp, by = c("game_id", "away_team" = "posteam","game_date","season")) %>%
  rename(away_sr = sr) %>%
  left_join(sched %>% select(game_id, home_qb_id, away_qb_id), by = "game_id") %>%
  left_join(qb_data_exp, by = c("game_id","game_date","home_qb_id" = "id")) %>%
  rename(home_qb = name, home_qb_epa = qb_epa, home_dbs = dbs) %>%
  left_join(qb_data_exp, by = c("game_id", "game_date","away_qb_id" = "id")) %>%
  rename(away_qb = name, away_qb_epa = qb_epa, away_dbs = dbs) %>%
  drop_na(home_sr, away_sr, home_qb_epa, away_qb_epa )

dt <- sample(nrow(df_train), .75 * nrow(df_train))
train <- df_train[dt,]
test <- df_train[-dt,]


model_totals <- mgcv::gam(total ~ s(home_qb_epa, home_sr) +  s(away_qb_epa, away_sr), data = train,  family = nb(), weights = log(home_dbs + away_dbs))

test$x_total <- predict(model_totals, test, type = "response")

rmse <- Metrics::rmse(test$total, test$x_total)

print(paste0("rmse function ending at ",Sys.time(), "with a value of ",rmse, ", Beta of ", base))

rm(qb_data_exp, qb_data2021_exp, qb_data2122_exp, qb_data2223_exp, qb_data2324_exp, 
   offense_2021_exp, offense_2122_exp, offense_2223_exp, offense_2324_exp, offense_exp, df_train, train, test, game_totals)

return(rmse)

}



