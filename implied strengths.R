library(tidyverse)
library(nflreadr)
library(nflplotR)
library(stringr)
library(mgcv)
library(gt)
library(scales)
library(purrr)
library(lubridate)

data <- load_pbp(2022:2024) %>%
  filter((rush == 1 | pass == 1) ) %>%
  mutate(name = ifelse(name == "G.Minshew II","G.Minshew",name))


B <- optimal_beta <- 0.9974176
current_week <- 12


schedule <- load_schedules() %>% filter(season == 2024) %>%
  mutate(home_qb = str_c(str_sub(home_qb_name, 1, 1), ".", str_extract(home_qb_name, "[^ ]+$")),
         away_qb = str_c(str_sub(away_qb_name, 1, 1), ".", str_extract(away_qb_name, "[^ ]+$")))


sunday <- schedule %>%
  filter(week == current_week) %>%
  group_by(date = gameday) %>%
  dplyr::summarize(count = n()) %>%
  ungroup() %>%
  slice_max(order_by = count,n = 1) %>%
  pull(date) %>%
  as.Date()

master_qb_list <- rbind(schedule %>% select(name = home_qb),schedule %>% select(name = away_qb)) %>% unique()

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
  filter(!is.na(qb_epa),name %in% master_qb_list$name) %>%
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
  filter(dropbacks_per_game > 3)



load("~/Documents/GitHub/Research/proj_model.RDS")

teams <- unique(schedule$home_team)

### Initial Ratings ####

matchups <- expand.grid(Home_Team = teams, Away_Team = teams)
matchups <- matchups[matchups$Home_Team != matchups$Away_Team, ]

current_qbs <- rbind(schedule %>% select(team = home_team,qb = home_qb,week) %>% filter(week %in% c(current_week,current_week -1)),
                     schedule %>% select(team = away_team,qb = away_qb,week) %>% filter(week %in% c(current_week,current_week - 1))) %>%
  na.omit() %>%
  slice_max(order_by = week,n = 1,by = team) %>%
  select(team,qb) %>%
  unique()

current_qbs <- current_qbs %>% mutate(qb = ifelse(team == "TEN","W.Levis",qb))

matchups <- left_join(matchups,current_qbs, by = c("Home_Team" = "team")) %>% rename(home_qb = qb)
matchups <- left_join(matchups,current_qbs, by = c("Away_Team" = "team")) %>% rename(away_qb = qb)

matchups <- left_join(matchups,qb_data, by = c("home_qb" = "name")) %>%
  rename(home_qb_epa_per_play = qb_epa_per_play, home_qb_games_played = games_played,
         home_qb_db_per_game = dropbacks_per_game,home_total_db = total_dbs) %>%
  left_join(qb_data,by = c("away_qb" = "name")) %>%
  rename(away_qb_epa_per_play = qb_epa_per_play, away_qb_games_played = games_played,
         away_qb_db_per_game = dropbacks_per_game, away_total_db = total_dbs) %>%
  left_join(offense_data, by = c("Home_Team" = "posteam")) %>%
  rename(home_epa_pp = epa_per_play,  home_run_epa_pp = run_epa_per_play,
  ) %>%
  left_join(offense_data, by = c("Away_Team" = "posteam")) %>%
  rename(away_epa_pp = epa_per_play,  away_run_epa_pp = run_epa_per_play, 
  ) %>%
  left_join(defense_data, by = c("Home_Team" = "defteam")) %>%
  rename(home_epa_pp_allowed = epa_per_play_allowed, home_run_epa_pp_allowed = run_epa_per_play_allowed,
  ) %>%
  left_join(defense_data, by = c("Away_Team" = "defteam")) %>%
  rename(away_epa_pp_allowed = epa_per_play_allowed,  away_run_epa_pp_allowed = run_epa_per_play_allowed
  ) 

matchups$proj_spread <- predict(proj_model,matchups)

ratings <- data.frame(team = teams,home_rating = c(NA),away_rating = c(NA))


for(i in 1:length(teams)){
  curteam <- teams[i]
  
  ratings$home_rating[ratings$team == curteam] <- mean(matchups$proj_spread[matchups$Home_Team == curteam])
  ratings$away_rating[ratings$team == curteam] <- -mean(matchups$proj_spread[matchups$Away_Team == curteam])
}

ratings$total <- round((ratings$home_rating + ratings$away_rating)/2,1)

avg_off <- data %>%
  filter(!is.na(posteam)) %>%
  mutate(days_diff = as.numeric(difftime(sunday,game_date, "days"))) %>%
  dplyr::summarize(epa_per_play = sum(epa * B^days_diff, na.rm = T)/sum(B^days_diff, na.rm = T),
                   #pass_epa_per_play = mean((epa[pass == 1] * B^days_diff)/B^days_diff,na.rm = T),
                   run_epa_per_play = sum((epa * B^days_diff)[rush==1], na.rm = T)/sum(B^days_diff[rush == 1], na.rm = T),
                   #success_rate = mean((success * B^days_diff)/B^days_diff,na.rm = T),
                   #plays = n()
  ) %>%
  ungroup() %>%
  mutate(date = sunday,
         posteam = "avg")

avg_def <- data %>%
  filter(!is.na(defteam)) %>%
  mutate(days_diff = as.numeric(difftime(sunday,game_date, "days"))) %>%
  dplyr::summarize(epa_per_play_allowed =  sum(epa * B^days_diff, na.rm = T)/sum(B^days_diff, na.rm = T),
                   #pass_epa_per_play_allowed = mean((epa[pass == 1] * B^days_diff)/B^days_diff,na.rm = T),
                   run_epa_per_play_allowed = sum((epa * B^days_diff)[rush==1], na.rm = T)/sum(B^days_diff[rush == 1], na.rm = T),
                   #success_rate_allowed = mean((success * B^days_diff)/B^days_diff,na.rm = T),
                   #plays = n()
  ) %>%
  ungroup() %>%
  mutate(date = sunday,
         defteam = "avg")

avg_from_plays <- data %>%
  filter(!is.na(qb_epa),name %in% master_qb_list$name) %>%
  mutate(days_diff = as.numeric(difftime(sunday,game_date, "days"))) %>%
  dplyr::summarize(qb_epa_per_play = sum((qb_epa * B^days_diff), na.rm = T)/sum(B^days_diff, na.rm = T),
                   #qb_total_epa_two_year = sum((qb_epa *B^days_diff)/B^days_diff ,na.rm = T),
                   games_played = length(unique(game_id)),
                   #qb_epa_per_game = qb_total_epa_two_year/games_played,
                   #qb_success_rate = mean((qb_success *B^days_diff)/B^days_diff,na.rm = T),
                   dropbacks_per_game = sum(qb_dropback,na.rm = T)/games_played,
                   num_qbs = length(unique(name)),
  ) %>%
  mutate(total_dbs = dropbacks_per_game * games_played,
         name = "average joe",
         total_dbs = round(total_dbs/num_qbs)) 

### Offense Ratings ###

ratings$home_rating_avg_off <- NA
ratings$away_rating_avg_off <- NA

new_matchups <- expand.grid(Home_Team = teams, Away_Team = teams)
new_matchups <- new_matchups[new_matchups$Home_Team != new_matchups$Away_Team, ]

new_matchups <- left_join(new_matchups,current_qbs, by = c("Home_Team" = "team")) %>% dplyr::rename(home_qb = qb)
new_matchups <- left_join(new_matchups,current_qbs, by = c("Away_Team" = "team")) %>% rename(away_qb = qb)

if(nrow(offense_data) == 32){offense_data <- rbind(offense_data,avg_off)}
if(nrow(qb_data %>% filter(name == "average_joe"))==0){qb_data <- bind_rows(qb_data,avg_from_plays %>% select(-num_qbs))}

for(i in 1:length(teams)){
  curteam <- teams[i]
  
  def_data <- defense_data %>%
    mutate(defteam = ifelse(defteam == curteam,"avg",defteam))
  
  w_avg_off <- new_matchups %>%
    mutate(Home_Team = ifelse(Home_Team == curteam,"avg",Home_Team),
           Away_Team = ifelse(Away_Team == curteam,"avg",Away_Team),
           home_qb = ifelse(Home_Team == "avg","average joe",home_qb),
           away_qb = ifelse(Away_Team == "avg","average joe",away_qb))
  
  w_avg_off <- left_join(w_avg_off,qb_data, by = c("home_qb" = "name")) %>%
    rename(home_qb_epa_per_play = qb_epa_per_play, home_qb_games_played = games_played,
           home_qb_db_per_game = dropbacks_per_game,home_total_db = total_dbs) %>%
    left_join(qb_data,by = c("away_qb" = "name")) %>%
    rename(away_qb_epa_per_play = qb_epa_per_play, away_qb_games_played = games_played,
           away_qb_db_per_game = dropbacks_per_game, away_total_db = total_dbs) %>%
    left_join(offense_data, by = c("Home_Team" = "posteam")) %>%
    rename(home_epa_pp = epa_per_play,  home_run_epa_pp = run_epa_per_play,
    ) %>%
    left_join(offense_data, by = c("Away_Team" = "posteam")) %>%
    rename(away_epa_pp = epa_per_play,  away_run_epa_pp = run_epa_per_play, 
    ) %>%
    left_join(def_data, by = c("Home_Team" = "defteam")) %>%
    rename(home_epa_pp_allowed = epa_per_play_allowed, home_run_epa_pp_allowed = run_epa_per_play_allowed,
    ) %>%
    left_join(def_data, by = c("Away_Team" = "defteam")) %>%
    rename(away_epa_pp_allowed = epa_per_play_allowed,  away_run_epa_pp_allowed = run_epa_per_play_allowed
    ) 
  
  w_avg_off$proj_spread <- predict(proj_model,w_avg_off)
  
  ratings$home_rating_avg_off[ratings$team == curteam] <- mean(w_avg_off$proj_spread[w_avg_off$Home_Team == "avg"])
  ratings$away_rating_avg_off[ratings$team == curteam] <- -mean(w_avg_off$proj_spread[w_avg_off$Away_Team == "avg"])
  
  rm(w_avg_off,def_data)
}

ratings$avg_off <- round((ratings$home_rating_avg_off + ratings$away_rating_avg_off)/2,1)
ratings$implied_offense_value <- ratings$total - ratings$avg_off

### Defense Ratings ###

ratings$home_rating_avg_def <- NA
ratings$away_rating_avg_def <- NA

new_matchups <- expand.grid(Home_Team = teams, Away_Team = teams)
new_matchups <- new_matchups[new_matchups$Home_Team != new_matchups$Away_Team, ]

new_matchups <- left_join(new_matchups,current_qbs, by = c("Home_Team" = "team")) %>% dplyr::rename(home_qb = qb)
new_matchups <- left_join(new_matchups,current_qbs, by = c("Away_Team" = "team")) %>% rename(away_qb = qb)

if(nrow(defense_data) == 32){defense_data <- rbind(defense_data,avg_def)}

offense_data <- offense_data %>% filter(posteam != "avg")

for(i in 1:length(teams)){
  curteam <- teams[i]
  
  off_data <- offense_data %>%
    mutate(posteam = ifelse(posteam == curteam,"avg",posteam))
  
  w_avg_def <- new_matchups %>%
    mutate(Home_Team = ifelse(Home_Team == curteam,"avg",Home_Team),
           Away_Team = ifelse(Away_Team == curteam,"avg",Away_Team))
  
  w_avg_def <- left_join(w_avg_def,qb_data, by = c("home_qb" = "name")) %>%
    rename(home_qb_epa_per_play = qb_epa_per_play, home_qb_games_played = games_played,
           home_qb_db_per_game = dropbacks_per_game,home_total_db = total_dbs) %>%
    left_join(qb_data,by = c("away_qb" = "name")) %>%
    rename(away_qb_epa_per_play = qb_epa_per_play, away_qb_games_played = games_played,
           away_qb_db_per_game = dropbacks_per_game, away_total_db = total_dbs) %>%
    left_join(off_data, by = c("Home_Team" = "posteam")) %>%
    rename(home_epa_pp = epa_per_play,  home_run_epa_pp = run_epa_per_play,
    ) %>%
    left_join(off_data, by = c("Away_Team" = "posteam")) %>%
    rename(away_epa_pp = epa_per_play,  away_run_epa_pp = run_epa_per_play, 
    ) %>%
    left_join(defense_data, by = c("Home_Team" = "defteam")) %>%
    rename(home_epa_pp_allowed = epa_per_play_allowed, home_run_epa_pp_allowed = run_epa_per_play_allowed,
    ) %>%
    left_join(defense_data, by = c("Away_Team" = "defteam")) %>%
    rename(away_epa_pp_allowed = epa_per_play_allowed,  away_run_epa_pp_allowed = run_epa_per_play_allowed
    ) 
  
  w_avg_def$proj_spread <- predict(proj_model,w_avg_def)
  
  ratings$home_rating_avg_def[ratings$team == curteam] <- mean(w_avg_def$proj_spread[w_avg_def$Home_Team == "avg"])
  ratings$away_rating_avg_def[ratings$team == curteam] <- -mean(w_avg_def$proj_spread[w_avg_def$Away_Team == "avg"])
  
  rm(w_avg_off,off_data)
}

ratings$avg_def <- round((ratings$home_rating_avg_def + ratings$away_rating_avg_def)/2,1)
ratings$implied_defense_value <- ratings$total - ratings$avg_def


unit_strengths <- ggplot(ratings, aes(implied_offense_value, implied_defense_value)) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "grey", linetype = "dashed") +
  geom_nfl_logos(aes(team_abbr = team), width = 0.075) +
  labs(
    title = "Implied Team Unit Strengths",
    subtitle = "By: @BryerCashman",
    x = "Implied Offensive Value",
    y = "Implied Defensive Value"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold", vjust = 0),
    plot.subtitle = element_text(size = 8, hjust = 1,vjust = 0,
                                 color = "dimgray"),
    plot.margin = margin(t = 8, b = 5, r = 5, l = 5)) 


unit_strengths

ggsave(paste0("/Users/bryer/Documents/NFL Projects/unit_strengths_plot",current_week,".png"), plot = unit_strengths, width = 8, height = 5, dpi = 300)

