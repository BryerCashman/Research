library(tidyverse)
library(nflreadr)
library(MASS)
library(mgcv)


def_epa_sd <- .0861



data <- load_pbp(2023:2025) %>%
  filter((rush == 1 | pass == 1) ) %>%
  mutate(name = ifelse(name == "G.Minshew II","G.Minshew",name))

schedule <- load_schedules(2025) %>%
  mutate(home_qb = str_c(str_sub(home_qb_name, 1, 1), ".", str_extract(home_qb_name, "[^ ]+$")),
         away_qb = str_c(str_sub(away_qb_name, 1, 1), ".", str_extract(away_qb_name, "[^ ]+$")))


computer <- "h"

path <- ifelse(computer == "W", "C:/Users/b.cashman/Documents/GitHub/Research/proj_model.RDS","/Users/bryer/Documents/GitHub/Research/proj_model.RDS")
load(path)
path <- ifelse(computer == "W", "C:/Users/b.cashman/Documents/GitHub/Research/model_nfl_home_wp.RDS","/Users/bryer/Documents/GitHub/Research/model_nfl_home_wp.RDS")
load(path)
path <- ifelse(computer == "W", "C:/Users/b.cashman/Documents/csv/NFL/cov_matrix.csv","/Users/bryer/Documents/cov_matrix.csv")
sigma <- read.csv(path)


current_week <- max(schedule$week[!is.na(schedule$home_qb_name)])

sunday <- schedule %>%
  filter(week == current_week) %>%
  group_by(date = gameday) %>%
  dplyr::summarize(count = n()) %>%
  ungroup() %>%
  slice_max(order_by = count,n = 1) %>%
  pull(date) %>%
  as.Date()

B <- optimal_beta <- 0.9974176

master_qb_list <- unique(rbind(schedule$home_qb_id, schedule$away_qb_id))


offense_data <- data %>%
  filter(!is.na(posteam)) %>%
  mutate(days_diff = as.numeric(difftime(sunday,game_date, "days"))) %>%
  group_by(posteam) %>%
  dplyr::summarize(epa_per_play = sum(epa * B^days_diff, na.rm = T)/sum(B^days_diff, na.rm = T),
                   #pass_epa_per_play = mean((epa[pass == 1] * B^days_diff)/B^days_diff,na.rm = T),
                   run_epa_per_play = sum((epa * B^days_diff)[rush==1], na.rm = T)/sum(B^days_diff[rush == 1], na.rm = T),
                   #success_rate = mean((success * B^days_diff)/B^days_diff,na.rm = T),
                   #plays = n()
  ) %>%
  ungroup() %>%
  mutate(date = sunday)

defense_data <- data %>%
  filter(!is.na(defteam)) %>%
  mutate(days_diff = as.numeric(difftime(sunday,game_date, "days"))) %>%
  group_by(defteam) %>%
  dplyr::summarize(epa_per_play_allowed =  sum(epa * B^days_diff, na.rm = T)/sum(B^days_diff, na.rm = T),
                   #pass_epa_per_play_allowed = mean((epa[pass == 1] * B^days_diff)/B^days_diff,na.rm = T),
                   run_epa_per_play_allowed = sum((epa * B^days_diff)[rush==1], na.rm = T)/sum(B^days_diff[rush == 1], na.rm = T),
                   #success_rate_allowed = mean((success * B^days_diff)/B^days_diff,na.rm = T),
                   #plays = n()
  ) %>%
  ungroup() %>%
  mutate(date = sunday)

qb_data <- data %>%
  filter(!is.na(qb_epa),id %in% master_qb_list) %>%
  mutate(days_diff = as.numeric(difftime(sunday,game_date, "days"))) %>%
  group_by(id,name) %>%
  mutate(qb_success = ifelse(qb_epa > 0,1,0)) %>%
  dplyr::summarize(qb_epa_per_play = sum((qb_epa * B^days_diff), na.rm = T)/sum(B^days_diff, na.rm = T),
                   #qb_total_epa_two_year = sum((qb_epa *B^days_diff)/B^days_diff ,na.rm = T),
                   games_played = length(unique(game_id)),
                   #qb_epa_per_game = qb_total_epa_two_year/games_played,
                   #qb_success_rate = mean((qb_success *B^days_diff)/B^days_diff,na.rm = T),
                   dropbacks_per_game = sum(qb_dropback,na.rm = T)/games_played
  ) %>%
  ungroup() %>%
  mutate(total_dbs = dropbacks_per_game * games_played) %>%
  filter(dropbacks_per_game > 3 )


current_qbs <- rbind(schedule %>% dplyr::select(team = home_team,qb = home_qb,week) %>% filter(week %in% c(current_week,current_week + 1,current_week - 1)),
                     schedule %>% dplyr::select(team = away_team,qb = away_qb,week) %>% filter(week %in% c(current_week,current_week + 1,current_week - 1))) %>%
  na.omit() %>%
  slice_max(order_by = week, n = 1, by = team) %>%
  dplyr::select(team,qb) %>%
  unique()

### Injured qbs
QB <- "L.Jackson"
Team <- "BAL"
week_return <- 7

if(!is.na(QB)){
games_after_return <- sum(schedule$home_team[schedule$week >= week_return] == Team) + sum(schedule$away_team[schedule$week >= week_return] == Team)
games_before_return <- sum(schedule$home_team[schedule$week < week_return] == Team) + sum(schedule$away_team[schedule$week < week_return] == Team) - sum(!is.na(schedule$result[schedule$home_team == Team | schedule$away_team == Team]) )
total_games <- games_after_return + games_before_return

adj_qb_epa <- qb_data$qb_epa_per_play[qb_data$name == QB] * (games_after_return/total_games) + qb_data$qb_epa_per_play[qb_data$name == current_qbs$qb[current_qbs$team == Team]] * (games_before_return/total_games)
adj_qb_dbs <- qb_data$total_dbs[qb_data$name == QB] * (games_after_return/total_games) + qb_data$total_dbs[qb_data$name == current_qbs$qb[current_qbs$team == Team]] * (games_before_return/total_games)
}

current_ratings <- current_qbs %>%
  inner_join(qb_data %>% dplyr::select(name, qb_epa_per_play, total_dbs), by = c("qb" = "name")) %>%
  inner_join(offense_data %>% dplyr::select(posteam, epa_per_play), by = c("team"="posteam")) %>%
  inner_join(defense_data %>% dplyr::select(defteam, epa_per_play_allowed), by = c("team" = "defteam"))

if(!is.na(QB)){
  current_rating$
}


new_cor <- mvrnorm(n = 32, mu = rep(0, 2), Sigma = sigma)

new_ratings <- current_ratings
new_ratings[, c("qb_epa_per_play", "epa_per_play")] <- current_ratings[, c("qb_epa_per_play", "epa_per_play")] + new_cor       

new_ratings[,c("epa_per_play_allowed")] <- current_ratings[,c("epa_per_play_allowed")] + rnorm(32, sd = def_epa_sd)
