library(tidyverse)
library(nflreadr)
library(mgcv)

computer <- "W"

path <- ifelse(computer == "W", "C:/Users/b.cashman/Documents/GitHub/Research/proj_model.RDS","/Users/bryer/Documents/GitHub/Research/proj_model.RDS")

load(path)

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

base <- optimal_beta <- 0.9974176

data <- load_pbp(2021:2025) %>%
  filter((rush == 1 | pass == 1) ) %>%
  mutate(name = ifelse(name == "G.Minshew II","G.Minshew",name),
         name = ifelse(name == "Aa.Rodgers", "A.Rodgers",name),
         game_date = as.Date(game_date))


B <- optimal_beta <- 0.9974176

schedule <- load_schedules() %>% filter(season %in% c(2021:2025)) %>%
  mutate(home_qb = str_c(str_sub(home_qb_name, 1, 1), ".", str_extract(home_qb_name, "[^ ]+$")),
         away_qb = str_c(str_sub(away_qb_name, 1, 1), ".", str_extract(away_qb_name, "[^ ]+$")))

master_qb_list <- unique(rbind(schedule$home_qb_id, schedule$away_qb_id))

offense_2123 <- data %>%
  filter(season %in% c(2021, 2022, 2023),
         (pass == 1 | rush == 1),
         !is.na(epa)) %>%
  group_by(posteam) %>%
  arrange(game_date, game_id, play_id, .by_group = TRUE) %>%
  mutate(
    days_gap = {
      dg <- as.numeric(game_date - lag(game_date, default = first(game_date)))
      pmax(0, replace(dg, is.na(dg), 0))
    },

    ewm_epa_play = ewm_irregular_lagged(epa, days_gap, base)
  ) %>%
  ungroup() %>%
  group_by(season, posteam, game_id, game_date) %>%
  summarize(epa_pp = first(ewm_epa_play), .groups = "drop") 


offense_2224 <- data %>%
  filter(season %in% c(2022, 2023, 2024),
         (pass == 1 | rush == 1),
         !is.na(epa)) %>%
  group_by(posteam) %>%
  arrange(game_date, game_id, play_id, .by_group = TRUE) %>%
  mutate(
    days_gap = {
      dg <- as.numeric(game_date - lag(game_date, default = first(game_date)))
      pmax(0, replace(dg, is.na(dg), 0))
    },
    
    ewm_epa_play = ewm_irregular_lagged(epa, days_gap, base)
  ) %>%
  ungroup() %>%
  group_by(season, posteam, game_id, game_date) %>%
  summarize(epa_pp = first(ewm_epa_play), .groups = "drop") %>%
  filter(season == 2024)

offense_2325 <- data %>%
  filter(season %in% c(2023, 2024, 2025),
         (pass == 1 | rush == 1),
         !is.na(epa)) %>%
  group_by(posteam) %>%
  arrange(game_date, game_id, play_id, .by_group = TRUE) %>%
  mutate(
    days_gap = {
      dg <- as.numeric(game_date - lag(game_date, default = first(game_date)))
      pmax(0, replace(dg, is.na(dg), 0))
    },
    
    ewm_epa_play = ewm_irregular_lagged(epa, days_gap, base)
  ) %>%
  ungroup() %>%
  group_by(season, posteam, game_id, game_date) %>%
  summarize(epa_pp = first(ewm_epa_play), .groups = "drop") %>%
  filter(season == 2025)

offense_full <- rbind(offense_2123,offense_2224, offense_2325)


qb_data2123 <- data %>%
  filter(season %in% c(2021, 2022, 2023),
         (pass == 1 | rush == 1),
         !is.na(epa),
         id %in% master_qb_list) %>%
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

qb_data2224 <- data %>%
  filter(season %in% c(2022, 2023, 2024),
         (pass == 1 | rush == 1),
         !is.na(epa),
         id %in% master_qb_list) %>%
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

qb_data2325 <- data %>%
  filter(season %in% c(2023, 2024, 2025),
         (pass == 1 | rush == 1),
         !is.na(epa),
         id %in% master_qb_list) %>%
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
  filter(season == 2025)


qb_data <- rbind(qb_data2123, qb_data2224, qb_data2325)

defense_2123 <- data %>%
  filter(season %in% c(2021, 2022, 2023),
         (pass == 1 | rush == 1),
         !is.na(epa)) %>%
  group_by(defteam) %>%
  arrange(game_date, game_id, play_id, .by_group = TRUE) %>%
  mutate(
    days_gap = {
      dg <- as.numeric(game_date - lag(game_date, default = first(game_date)))
      pmax(0, replace(dg, is.na(dg), 0))
    },
    
    ewm_epa_play = ewm_irregular_lagged(epa, days_gap, base)
  ) %>%
  ungroup() %>%
  group_by(season, defteam, game_id, game_date) %>%
  summarize(epa_pp_allowed = first(ewm_epa_play), .groups = "drop") 

defense_2224 <- data %>%
  filter(season %in% c(2022, 2023, 2024),
         (pass == 1 | rush == 1),
         !is.na(epa)) %>%
  group_by(defteam) %>%
  arrange(game_date, game_id, play_id, .by_group = TRUE) %>%
  mutate(
    days_gap = {
      dg <- as.numeric(game_date - lag(game_date, default = first(game_date)))
      pmax(0, replace(dg, is.na(dg), 0))
    },
    
    ewm_epa_play = ewm_irregular_lagged(epa, days_gap, base)
  ) %>%
  ungroup() %>%
  group_by(season, defteam, game_id, game_date) %>%
  summarize(epa_pp_allowed = first(ewm_epa_play), .groups = "drop") %>%
  filter(season == 2024)

defense_2325 <- data %>%
  filter(season %in% c(2023, 2024, 2025),
         (pass == 1 | rush == 1),
         !is.na(epa)) %>%
  group_by(defteam) %>%
  arrange(game_date, game_id, play_id, .by_group = TRUE) %>%
  mutate(
    days_gap = {
      dg <- as.numeric(game_date - lag(game_date, default = first(game_date)))
      pmax(0, replace(dg, is.na(dg), 0))
    },
    
    ewm_epa_play = ewm_irregular_lagged(epa, days_gap, base)
  ) %>%
  ungroup() %>%
  group_by(season, defteam, game_id, game_date) %>%
  summarize(epa_pp_allowed = first(ewm_epa_play), .groups = "drop") %>%
  filter(season == 2025)

defense <- rbind(defense_2123, defense_2224, defense_2325)  

rm(qb_data2123, qb_data2224, qb_data2325, offense_2123, offense_2224, offense_2325, defense_2123, defense_2224, defense_2325)

df_spreads <- schedule %>%
  filter(result != 0) %>%
  select(game_id,home_team, away_team, home_qb, home_qb_id, away_qb, away_qb_id, result) %>%
  left_join(offense_full, by = c("game_id", "home_team" = "posteam")) %>%
  rename(home_epa_pp = epa_pp) %>%
  left_join(offense_full, by = c("game_id","game_date", "away_team" = "posteam")) %>%
  rename(away_epa_pp = epa_pp) %>%
  left_join(qb_data, by = c("game_id","game_date", "home_qb_id" = "id")) %>%
  rename(home_qb_epa_per_play = qb_epa, home_total_db = dbs ) %>%
  left_join(qb_data, by = c("game_id","game_date", "away_qb_id" = "id")) %>%
  rename(away_qb_epa_per_play = qb_epa, away_total_db = dbs ) %>%
  left_join(defense, by = c("game_id","game_date", "home_team" = "defteam")) %>%
  rename(home_epa_pp_allowed = "epa_pp_allowed") %>%
  left_join(defense, by = c("game_id","game_date", "away_team" = "defteam")) %>%
  rename(away_epa_pp_allowed = "epa_pp_allowed") %>%
  mutate(weight = ifelse(season.x == 2021,1,2),
         home_w = ifelse(result > 0, 1,0))

df_spreads$x_point_diff <- predict(proj_model, df_spreads, type = "response")

dt <- sample(nrow(df_spreads), .75 * nrow(df_spreads))
train <- df_spreads[dt,]
test <- df_spreads[-dt,]

model_wp <- glm(home_w ~ x_point_diff, data = df_spreads, weights = weight, family = "binomial")

summary(model_wp)

test$home_wp <- predict(model_wp, test, type = "response")
Metrics::logLoss(test$home_w[!is.na(test$home_wp)], test$home_wp[!is.na(test$home_wp)])

grid <- expand.grid(x_point_diff = c(-15:15))
grid$home_wp <- predict(model_wp, grid, type = "response")

ggplot(grid, aes(x_point_diff, home_wp)) +
  geom_line() +
  theme_bw()

model_home_wp <- model_wp

save(model_home_wp, file = "C:/Users/b.cashman/Documents/R scripts/NFL/model_nfl_total.RDS")


#### Comparison

