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


computer <- "W"

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
  current_ratings$qb_epa_per_play[current_ratings$team == Team] <- adj_qb_epa
  current_ratings$total_dbs[current_ratings$team == Team] <- adj_qb_dbs
  
}

sim_ros <- function(runs = 100){

  
sim_wins <<- data.frame(team = rep(NA, runs * 32),wins = rep(NA, runs * 32), most_wins = rep(NA, runs * 32))  
  
for(i in 1:runs){

new_cor <- mvrnorm(n = 32, mu = rep(0, 2), Sigma = sigma)

new_ratings <- current_ratings
new_ratings[, c("qb_epa_per_play", "epa_per_play")] <- current_ratings[, c("qb_epa_per_play", "epa_per_play")] + new_cor       

new_ratings[,c("epa_per_play_allowed")] <- current_ratings[,c("epa_per_play_allowed")] + rnorm(32, sd = def_epa_sd)


df_wins <- schedule %>%
  dplyr::select(game_id, week, result, home_team, away_team) %>%
  inner_join(new_ratings, by = c("home_team" = "team")) %>%
  rename(home_epa_pp = epa_per_play, home_qb_epa_per_play = qb_epa_per_play, home_epa_pp_allowed = epa_per_play_allowed, home_total_db = total_dbs) %>%
  inner_join(new_ratings, by = c("away_team" = "team")) %>%
  rename(away_epa_pp = epa_per_play, away_qb_epa_per_play = qb_epa_per_play, away_epa_pp_allowed = epa_per_play_allowed, away_total_db = total_dbs)

df_wins$x_point_diff <- predict(proj_model, df_wins)

df_wins$home_wp <- predict(model_home_wp, df_wins, type = "response")

df_wins$rand <- runif(272)

df_wins$home_win <- ifelse(!is.na(df_wins$result),
                           ifelse(df_wins$result > 0, 1, 0),
                           ifelse(df_wins$rand < df_wins$home_wp, 1, 0))

df_wins$away_win <- ifelse(df_wins$result == 0 & !is.na(df_wins$result), 0, 1 - df_wins$home_win)


home_wins <- df_wins %>%
  group_by(home_team) %>%
  dplyr::summarize(home_wins = sum(home_win)) 

away_wins <- df_wins %>%
  group_by(away_team) %>%
  dplyr::summarize(away_wins = sum(away_win))


total_wins <- inner_join(home_wins, away_wins, by = c("home_team" = "away_team")) %>%
  mutate(total_wins = home_wins + away_wins,
         max_w = max(total_wins),
         num_tied = sum(total_wins == max_w),
         most = if_else(total_wins == max_w, 1 / num_tied, 0)) 

sim_wins$team[(i * 32 - 31):(i * 32)] <<- total_wins$home_team
sim_wins$wins[(i * 32 - 31):(i * 32)] <<- total_wins$total_wins
sim_wins$most_wins[(i * 32 - 31):(i * 32)] <<- total_wins$most

rm( new_ratings, df_wins, home_wins, away_wins, total_wins)

}
}

system.time(sim_ros(5000))


win_stats <- sim_wins %>%
  group_by(team) %>%
  dplyr::summarize(mean_wins = mean(wins),
                   median_wins = median(wins),
                   best_record_pct = mean(most_wins))

win_distribution <- sim_wins %>%
  group_by(team) %>%
  dplyr::summarize(wins_0 = sum(wins == 0)/(nrow(sim_wins)/32),
                   wins_1 = sum(wins == 1)/(nrow(sim_wins)/32),
                   wins_2 = sum(wins == 2)/(nrow(sim_wins)/32),
                   wins_3 = sum(wins == 3)/(nrow(sim_wins)/32),
                   wins_4 = sum(wins == 4)/(nrow(sim_wins)/32),
                   wins_5 = sum(wins == 5)/(nrow(sim_wins)/32),
                   wins_6 = sum(wins == 6)/(nrow(sim_wins)/32),
                   wins_7 = sum(wins == 7)/(nrow(sim_wins)/32),
                   wins_8 = sum(wins == 8)/(nrow(sim_wins)/32),
                   wins_9 = sum(wins == 9)/(nrow(sim_wins)/32),
                   wins_10 = sum(wins == 10)/(nrow(sim_wins)/32),
                   wins_11 = sum(wins == 11)/(nrow(sim_wins)/32),
                   wins_12 = sum(wins == 12)/(nrow(sim_wins)/32),
                   wins_13 = sum(wins == 13)/(nrow(sim_wins)/32),
                   wins_14 = sum(wins == 14)/(nrow(sim_wins)/32),
                   wins_15 = sum(wins == 15)/(nrow(sim_wins)/32),
                   wins_16 = sum(wins == 16)/(nrow(sim_wins)/32),
                   wins_17 = sum(wins == 17)/(nrow(sim_wins)/32)
                   )
