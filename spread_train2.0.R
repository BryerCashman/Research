library(tidyverse)
library(ggrepel)
library(nflreadr)
library(nflplotR)
library("scales")
library(lubridate)
library(mgcv)
library(DEoptim)
library(data.table)
library(profvis)
library(stringr)
options(dplyr.summarise.inform = FALSE)

addTaskCallback(function(...) {set.seed(123); TRUE}) 
load( file = "/Users/bryer/Documents/GitHub/model_pred_qba.R")

convert_name <- function(name) {
  # Extract first name initial
  first_initial <- str_sub(name, 1, 1)
  
  # Extract last name (allowing for apostrophes or hyphens)
  last_name <- str_extract(name, "(?<= )[A-Za-z'\\-]+")
  
  # Extract suffix (if present)
  suffix <- str_extract(name, "(?<= )[IVXLC]+$|Jr\\.?$")
  
  # Combine the first initial, last name, and suffix (if present)
  ifelse(!is.na(suffix), 
         str_c(first_initial, ".", last_name, " ", suffix),
         str_c(first_initial, ".", last_name))
}

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

base <- 1

data <- load_pbp(2019:2025) %>%
  filter(season_type == "REG",(rush == 1 | pass == 1) )

data <- data %>% mutate(game_date = as.Date(game_date),
                        name = ifelse(name == "G.Minshew II","G.Minshew",
                                      ifelse(name == "R.Griffin III","R.Griffin",ifelse(name == "Aa.Rodgers","A.Rodgers",name)))) %>% 
  filter(!is.na(epa))


sched <- nflreadr::load_schedules() %>% filter(season %in% c(2019:2025)) %>%
  mutate(home_qb = convert_name(home_qb_name),
         away_qb = convert_name(away_qb_name),
         home_team = ifelse(home_team == "OAK","LV",ifelse(home_team == "SD","LAC",ifelse(home_team == "STL","LA",home_team))),
         away_team = ifelse(away_team == "OAK","LV",ifelse(away_team == "SD","LAC",ifelse(away_team == "STL","LA",away_team))))

master_qb_list <- rbind(sched$home_qb_name,sched$away_qb_name) %>% unique()
master_id_list <- rbind(sched$home_qb_id,sched$away_qb_id) %>% unique()



games <- sched %>%
 select(game_id,game_date = gameday,home_team,away_team, home_score, away_score, point_diff = result)


games <- sched %>% 
  filter(season %in% c(2019:2025),game_type == "REG",!is.na(home_score)) %>%
  mutate(home_qb = ifelse(home_qb == "Gardner Minshew II","Gardner Minshew",home_qb),
         away_qb = ifelse(away_qb == "Gardner Minshew II","Gardner Minshew",away_qb)) %>%
  select(game_id,game_date = gameday,home_team,away_team,
         home_team_score = home_score,away_team_score = away_score,point_diff = result,
         starting_home_qb = home_qb,starting_home_id = home_qb_id,
         starting_away_qb = away_qb,starting_away_id = away_qb_id) %>%
  mutate(game_date = as.Date(game_date))

offense_1921 <- data %>%
  filter(season %in% c(2019, 2020, 2021),
         (pass == 1 | rush == 1),
         !is.na(epa)) %>%
  group_by(posteam) %>%
  arrange(game_date, game_id, play_id, .by_group = TRUE) %>%
  mutate(
    days_gap = {
      dg <- as.numeric(game_date - lag(game_date, default = first(game_date)))
      pmax(0, replace(dg, is.na(dg), 0))
    },
    
    ewm_epa_play = ewm_irregular_lagged(epa, days_gap, base),
    ewm_success_rate = ewm_irregular_lagged(success, days_gap, base),
  ) %>%
  ungroup() %>%
  group_by(season, posteam, game_id, game_date) %>%
  summarize(epa_pp = first(ewm_epa_play),
            sr = first(ewm_success_rate),
            .groups = "drop") %>%
  filter(season != 2019)

offense_2022 <- data %>%
  filter(season %in% c(2020, 2021, 2022),
         (pass == 1 | rush == 1),
         !is.na(epa)) %>%
  group_by(posteam) %>%
  arrange(game_date, game_id, play_id, .by_group = TRUE) %>%
  mutate(
    days_gap = {
      dg <- as.numeric(game_date - lag(game_date, default = first(game_date)))
      pmax(0, replace(dg, is.na(dg), 0))
    },
    
    ewm_epa_play = ewm_irregular_lagged(epa, days_gap, base),
    ewm_success_rate = ewm_irregular_lagged(success, days_gap, base),
  ) %>%
  ungroup() %>%
  group_by(season, posteam, game_id, game_date) %>%
  summarize(epa_pp = first(ewm_epa_play),
            sr = first(ewm_success_rate),
            .groups = "drop") %>%
  filter(season == 2022)

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
    
    ewm_epa_play = ewm_irregular_lagged(epa, days_gap, base),
    ewm_success_rate = ewm_irregular_lagged(success, days_gap, base),
  ) %>%
  ungroup() %>%
  group_by(season, posteam, game_id, game_date) %>%
  summarize(epa_pp = first(ewm_epa_play),
            sr = first(ewm_success_rate),
            .groups = "drop") %>%
  filter(season == 2023)

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
    
    ewm_epa_play = ewm_irregular_lagged(epa, days_gap, base),
    ewm_success_rate = ewm_irregular_lagged(success, days_gap, base),
  ) %>%
  ungroup() %>%
  group_by(season, posteam, game_id, game_date) %>%
  summarize(epa_pp = first(ewm_epa_play),
            sr = first(ewm_success_rate),
            .groups = "drop") %>%
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
    
    ewm_epa_play = ewm_irregular_lagged(epa, days_gap, base),
    ewm_success_rate = ewm_irregular_lagged(success, days_gap, base),
  ) %>%
  ungroup() %>%
  group_by(season, posteam, game_id, game_date) %>%
  summarize(epa_pp = first(ewm_epa_play),
            sr = first(ewm_success_rate),
            .groups = "drop") %>%
  filter(season == 2025)
   
offense_data <- rbind(offense_1921, offense_2022, offense_2123, offense_2224, offense_2325)
rm(offense_1921,offense_2022,offense_2123,offense_2224,offense_2325)

defense1921 <- data %>%
  filter(season %in% c(2019, 2020, 2021),
         (pass == 1 | rush == 1),
         !is.na(epa)) %>%
  group_by(defteam) %>%
  arrange(game_date, game_id, play_id, .by_group = TRUE) %>%
  mutate(
    days_gap = {
      dg <- as.numeric(game_date - lag(game_date, default = first(game_date)))
      pmax(0, replace(dg, is.na(dg), 0))
    },
    
    ewm_epa_play = ewm_irregular_lagged(epa, days_gap, base),
    ewm_success_rate = ewm_irregular_lagged(success, days_gap, base),
  ) %>%
  ungroup() %>%
  group_by(season, defteam, game_id, game_date) %>%
  summarize(epa_pp_allowed = first(ewm_epa_play),
            sr_allowed = first(ewm_success_rate),
            .groups = "drop") %>%
  filter(season != 2019)

defense2022 <- data %>%
  filter(season %in% c(2020, 2021, 2022),
         (pass == 1 | rush == 1),
         !is.na(epa)) %>%
  group_by(defteam) %>%
  arrange(game_date, game_id, play_id, .by_group = TRUE) %>%
  mutate(
    days_gap = {
      dg <- as.numeric(game_date - lag(game_date, default = first(game_date)))
      pmax(0, replace(dg, is.na(dg), 0))
    },
    
    ewm_epa_play = ewm_irregular_lagged(epa, days_gap, base),
    ewm_success_rate = ewm_irregular_lagged(success, days_gap, base),
  ) %>%
  ungroup() %>%
  group_by(season, defteam, game_id, game_date) %>%
  summarize(epa_pp_allowed = first(ewm_epa_play),
            sr_allowed = first(ewm_success_rate),
            .groups = "drop") %>%
  filter(season == 2022)

defense2123 <- data %>%
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
    
    ewm_epa_play = ewm_irregular_lagged(epa, days_gap, base),
    ewm_success_rate = ewm_irregular_lagged(success, days_gap, base),
  ) %>%
  ungroup() %>%
  group_by(season, defteam, game_id, game_date) %>%
  summarize(epa_pp_allowed = first(ewm_epa_play),
            sr_allowed = first(ewm_success_rate),
            .groups = "drop") %>%
  filter(season == 2023)

defense2224 <- data %>%
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
    
    ewm_epa_play = ewm_irregular_lagged(epa, days_gap, base),
    ewm_success_rate = ewm_irregular_lagged(success, days_gap, base),
  ) %>%
  ungroup() %>%
  group_by(season, defteam, game_id, game_date) %>%
  summarize(epa_pp_allowed = first(ewm_epa_play),
            sr_allowed = first(ewm_success_rate),
            .groups = "drop") %>%
  filter(season == 2024)
 
defense2325 <- data %>%
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
    
    ewm_epa_play = ewm_irregular_lagged(epa, days_gap, base),
    ewm_success_rate = ewm_irregular_lagged(success, days_gap, base),
  ) %>%
  ungroup() %>%
  group_by(season, defteam, game_id, game_date) %>%
  summarize(epa_pp_allowed = first(ewm_epa_play),
            sr_allowed = first(ewm_success_rate),
            .groups = "drop") %>%
  filter(season == 2025)

defense_data <- rbind(defense1921,defense2022,defense2123,defense2224,defense2325)
rm(defense1921,defense2022, defense2123, defense2224, defense2325)

qb_data1921 <- data %>%
  filter(season %in% c(2019, 2020, 2021),
         (pass == 1 | rush == 1),
         !is.na(epa),
         id %in% master_id_list) %>%
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
    dbs = lag(cumsum(!is.na(epa))),
    ewm_qb_sack_rate = ifelse(qb_dropback == 1, ewm_irregular_lagged(qb_epa[qb_dropback == 1], days_gap[qb_dropback == 1], base), NA),
    ewm_air_epa = ifelse(play_type == "pass", ewm_irregular_lagged(qb_epa[play_type == "pass"], days_gap[play_type == "pass"], base), NA),
    pred_qb_epa = predict(model_pred_qb_epa, pick(ewm_qb_epa_play, dbs))
  ) %>%
  ungroup() %>%
  # snapshot what we knew at kickoff: take the first play's value for each game
  group_by(season, name, id, game_id, game_date) %>%
  summarize(qb_epa = first(ewm_qb_epa_play), 
            dbs = first(dbs),
            qb_sack_rate = dplyr::first(ewm_qb_sack_rate, na_rm = T),
            qb_air_epa = dplyr::first(ewm_air_epa, na_rm = T),
            pred_qb_epa = first(pred_qb_epa),
            .groups = "drop") %>%
  filter(season != 2019)

qb_data2022 <- data %>%
  filter(season %in% c(2020, 2021, 2022),
         (pass == 1 | rush == 1),
         !is.na(epa),
         id %in% master_id_list) %>%
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
    dbs = lag(cumsum(!is.na(epa))),
    ewm_qb_sack_rate = ifelse(qb_dropback == 1, ewm_irregular_lagged(qb_epa[qb_dropback == 1], days_gap[qb_dropback == 1], base), NA),
    ewm_air_epa = ifelse(play_type == "pass", ewm_irregular_lagged(qb_epa[play_type == "pass"], days_gap[play_type == "pass"], base), NA),
    pred_qb_epa = predict(model_pred_qb_epa, pick(ewm_qb_epa_play, dbs))
  ) %>%
  ungroup() %>%
  # snapshot what we knew at kickoff: take the first play's value for each game
  group_by(season, name, id, game_id, game_date) %>%
  summarize(qb_epa = first(ewm_qb_epa_play), 
            dbs = first(dbs),
            qb_sack_rate = dplyr::first(ewm_qb_sack_rate, na_rm = T),
            qb_air_epa = dplyr::first(ewm_air_epa, na_rm = T),
            pred_qb_epa = first(pred_qb_epa),
            .groups = "drop") %>%
  filter(season == 2022)

qb_data2123 <- data %>%
  filter(season %in% c(2021, 2022, 2023),
         (pass == 1 | rush == 1),
         !is.na(epa),
         id %in% master_id_list) %>%
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
    dbs = lag(cumsum(!is.na(epa))),
    ewm_qb_sack_rate = ifelse(qb_dropback == 1, ewm_irregular_lagged(qb_epa[qb_dropback == 1], days_gap[qb_dropback == 1], base), NA),
    ewm_air_epa = ifelse(play_type == "pass", ewm_irregular_lagged(qb_epa[play_type == "pass"], days_gap[play_type == "pass"], base), NA),
    pred_qb_epa = predict(model_pred_qb_epa, pick(ewm_qb_epa_play, dbs))
  ) %>%
  ungroup() %>%
  # snapshot what we knew at kickoff: take the first play's value for each game
  group_by(season, name, id, game_id, game_date) %>%
  summarize(qb_epa = first(ewm_qb_epa_play), 
            dbs = first(dbs),
            qb_sack_rate = dplyr::first(ewm_qb_sack_rate, na_rm = T),
            qb_air_epa = dplyr::first(ewm_air_epa, na_rm = T),
            pred_qb_epa = first(pred_qb_epa),
            .groups = "drop") %>%
  filter(season == 2023)

qb_data2224 <- data %>%
  filter(season %in% c(2022, 2023, 2024),
         (pass == 1 | rush == 1),
         !is.na(epa),
         id %in% master_id_list) %>%
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
    dbs = lag(cumsum(!is.na(epa))),
    ewm_qb_sack_rate = ifelse(qb_dropback == 1, ewm_irregular_lagged(qb_epa[qb_dropback == 1], days_gap[qb_dropback == 1], base), NA),
    ewm_air_epa = ifelse(play_type == "pass", ewm_irregular_lagged(qb_epa[play_type == "pass"], days_gap[play_type == "pass"], base), NA),
    pred_qb_epa = predict(model_pred_qb_epa, pick(ewm_qb_epa_play, dbs))
  ) %>%
  ungroup() %>%
  # snapshot what we knew at kickoff: take the first play's value for each game
  group_by(season, name, id, game_id, game_date) %>%
  summarize(qb_epa = first(ewm_qb_epa_play), 
            dbs = first(dbs),
            qb_sack_rate = dplyr::first(ewm_qb_sack_rate, na_rm = T),
            qb_air_epa = dplyr::first(ewm_air_epa, na_rm = T),
            pred_qb_epa = first(pred_qb_epa),
            .groups = "drop") %>%
  filter(season == 2024)

qb_data2325 <- data %>%
  filter(season %in% c(2023, 2024, 2025),
         (pass == 1 | rush == 1),
         !is.na(epa),
         id %in% master_id_list) %>%
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
    dbs = lag(cumsum(!is.na(epa))),
    ewm_qb_sack_rate = ifelse(qb_dropback == 1, ewm_irregular_lagged(qb_epa[qb_dropback == 1], days_gap[qb_dropback == 1], base), NA),
    ewm_air_epa = ifelse(play_type == "pass", ewm_irregular_lagged(qb_epa[play_type == "pass"], days_gap[play_type == "pass"], base), NA),
    pred_qb_epa = predict(model_pred_qb_epa, pick(ewm_qb_epa_play, dbs))
  ) %>%
  ungroup() %>%
  # snapshot what we knew at kickoff: take the first play's value for each game
  group_by(season, name, id, game_id, game_date) %>%
  summarize(qb_epa = first(ewm_qb_epa_play), 
            dbs = first(dbs),
            qb_sack_rate = dplyr::first(ewm_qb_sack_rate, na_rm = T),
            qb_air_epa = dplyr::first(ewm_air_epa, na_rm = T),
            pred_qb_epa = first(pred_qb_epa),
            .groups = "drop") %>%
  filter(season == 2025)
    
qb_data <- rbind(qb_data1921, qb_data2022, qb_data2123, qb_data2224, qb_data2325)
rm(qb_data1921, qb_data2022, qb_data2123, qb_data2224, qb_data2325)
  
  

  
df <- inner_join(games,qb_data, by = c("game_date" ,"game_id","starting_home_id" = "id")) %>%
    rename(home_qb_id = starting_home_id,home_qb_epa_per_play = qb_epa, 
           home_total_db = dbs, home_sack_rate = qb_sack_rate, home_qb_air_epa = qb_air_epa) %>%
    inner_join(qb_data ,by = c("game_date","game_id","starting_away_id" = "id")) %>%
  rename(away_qb_id = starting_away_id,away_qb_epa_per_play = qb_epa, 
         away_total_db = dbs, away_sack_rate = qb_sack_rate, away_qb_air_epa = qb_air_epa) %>%
    inner_join(offense_data, by = c("game_date" ,"game_id","home_team" = "posteam")) %>%
    rename(home_epa_pp = epa_pp,  home_sr = sr
    ) %>%
    inner_join(offense_data, by = c("game_date","game_id","away_team" = "posteam")) %>%
    rename(away_epa_pp = epa_pp,  away_sr = sr
    ) %>%
    inner_join(defense_data, by = c("game_date","game_id","home_team" = "defteam")) %>%
    rename(home_epa_pp_allowed = epa_pp_allowed, home_sr_allowed = sr_allowed
    ) %>%
    inner_join(defense_data, by = c("game_date","game_id","away_team" = "defteam")) %>%
    rename(away_epa_pp_allowed = epa_pp_allowed,  away_sr_allowed = sr_allowed
    ) %>%
    arrange(desc(game_date)) %>%
    na.omit()
  
  rm(offense_data,defense_data,qb_data)
  
  print(paste0("Ending trial at ",Sys.time()))


# cl <- makeCluster(detectCores() - 1)  # Use all but one core
# registerDoParallel(cl)




  dt <- sample(nrow(df), 0.75*nrow(df))
  df_train <- df[dt,]
  df_test <- df[-dt,]
  
  model <- mgcv::gam(point_diff ~ s(home_epa_pp,away_epa_pp_allowed) + s(away_epa_pp,home_epa_pp_allowed) 
                     + s(home_qb_epa_per_play,home_total_db) + s(away_qb_epa_per_play,away_total_db),data = df_train)
  
  model_glm <- glm(point_diff ~ home_epa_pp + away_epa_pp_allowed + away_epa_pp + home_epa_pp_allowed + 
                   home_qb_epa_per_play + home_total_db + away_qb_epa_per_play + away_total_db, data = df_train)
  
  summary(model_glm)
  
  df_test$proj_spread <- predict(model, df_test)
  
  cor(df_test$proj_spread,df_test$point_diff) ^ 2

  Metrics::rmse(df_test$point_diff, df_test$proj_spread)
  
  print(paste0("rsq function ending at ",Sys.time(), "with a value of ",r2))


grid <- expand.grid(home_epa_pp =  c(-300:200)/1000, away_epa_pp_allowed = c(-250:150)/1000, away_epa_pp = c(0),
                                     home_epa_pp_allowed = c(0), home_qb_epa_per_play = c(0), home_total_db = c(500),
                                     away_qb_epa_per_play = c(0), away_total_db = c(500))

grid$proj_spread <- predict(model, grid)

ggplot(grid, aes(home_epa_pp, away_epa_pp_allowed, color = proj_spread)) +
  geom_point() +
  scale_color_gradient2(high = "red", low = "blue", mid = "white", midpoint = median(grid$proj_spread)) +
  theme_bw()

# lm_model_one <- lm(point_diff ~ home_epa_pp + away_epa_pp + home_epa_pp_allowed + away_epa_pp_allowed + home_qb_epa_per_play + away_qb_epa_per_play, data = df_train)
# 
# lm_model_two <- lm(point_diff ~ home_epa_pp + away_epa_pp + home_epa_pp_allowed + away_epa_pp_allowed + (home_qb_epa_per_play:home_qb_games_played) + (away_qb_epa_per_play:away_qb_games_played), data = df_train)
# lm_model_three <- lm(point_diff ~ home_epa_pp + away_epa_pp + home_epa_pp_allowed + away_epa_pp_allowed + (home_qb_epa_per_play:home_total_db) + (away_qb_epa_per_play:away_total_db), data = df_train)
# 
# gam_model_one <- mgcv::gam(point_diff ~ s(home_epa_pp,away_epa_pp_allowed) + s(away_epa_pp,home_epa_pp_allowed)
#                            + s(home_qb_epa_per_play,home_total_db) + s(away_qb_epa_per_play,away_total_db), data = df_train)
# 
# gam_model_two <- mgcv::gam(point_diff ~  s(home_success,away_success_allowed) + s(away_success,home_success_allowed)
#                            + s(home_qb_epa_per_play,home_total_db) + s(away_qb_epa_per_play,away_total_db), data = df_train)
# 
# gam_model_three <- mgcv::gam(point_diff ~ s(home_epa_pp,away_epa_pp_allowed) + s(away_epa_pp,home_epa_pp_allowed) 
#                              + s(home_run_epa_pp,away_run_epa_pp_allowed) + s(away_run_epa_pp,home_run_epa_pp_allowed) 
#                              + s(home_qb_epa_per_play,home_total_db) + s(away_qb_epa_per_play,away_total_db),data = df_train)
# 
# gam_model_four <- mgcv::gam(point_diff ~ s(home_pass_epa_pp,away_pass_epa_pp_allowed) + s(away_pass_epa_pp,home_pass_epa_pp_allowed) 
#                              + s(home_success,away_success_allowed) + s(away_success,home_success_allowed) 
#                              + s(home_qb_epa_per_play,home_total_db) + s(away_qb_epa_per_play,away_total_db),data = df_train)
# 
# gam_model_five <- mgcv::gam(point_diff ~ s(home_pass_epa_pp,home_qb_epa_per_play,away_epa_pp_allowed) + s(away_pass_epa_pp,away_qb_epa_per_play,home_epa_pp_allowed)
#                             ,
#                             data = df_train)
# 
# summary(lm_model_one)
# summary(lm_model_two)
# summary(lm_model_three)
# 
# summary(gam_model_one)
# summary(gam_model_two)
# summary(gam_model_three) ##
# summary(gam_model_four)
# summary(gam_model_five)
# 
# df_test$proj_spread <- predict(gam_model_three,df_test)
# df_test <- relocate(df_test,proj_spread,.before = starting_home_qb)
# 
# cor(df_test$point_diff,df_test$proj_spread) ^ 2
# 

lower_bound <- .850
upper_bound <- .999

result <- DEoptim(
  fn = maximize_r_squared, 
  lower = lower_bound, 
  upper = upper_bound,
  control = list(trace = TRUE, NP = 10, itermax = 5)
)



result



optimal_beta <- result$optim$bestmem


