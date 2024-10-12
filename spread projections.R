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




data <- load_pbp(2015:2024) %>%
  filter(season_type == "REG",(rush == 1 | pass == 1) )

data <- data %>% mutate(game_date = as.Date(game_date),
                        name = ifelse(name == "G.Minshew II","G.Minshew",
                                      ifelse(name == "R.Griffin III","R.Griffin",name))) %>% 
  filter(!is.na(epa))



  dates <- unique(data$game_date)
  
  sched <- nflreadr::load_schedules() %>% filter(season %in% c(2015:2024)) %>%
    mutate(home_qb_name = convert_name(home_qb_name),
           away_qb_name = convert_name(away_qb_name),
           home_team = ifelse(home_team == "OAK","LV",ifelse(home_team == "SD","LAC",ifelse(home_team == "STL","LA",home_team))),
           away_team = ifelse(away_team == "OAK","LV",ifelse(away_team == "SD","LAC",ifelse(away_team == "STL","LA",away_team))))
  
  master_qb_list <- rbind(sched$home_qb_name,sched$away_qb_name) %>% unique()
  master_id_list <- rbind(sched$home_qb_id,sched$away_qb_id) %>% unique()
  
  

games <- data %>%
  group_by(game_id,game_date,home_team,away_team) %>%
  dplyr::summarize(home_team_score = last(total_home_score) ,
                   away_team_score = last(total_away_score),
                   point_diff = home_team_score - away_team_score,
                   starting_home_qb = first(name[posteam == home_team & !is.na(qb_epa) & !is.na(name) & play_type == "pass" & (name %in% master_qb_list)]),
                   starting_home_id = first(id[posteam == home_team & !is.na(qb_epa) & !is.na(name) & play_type == "pass" & (id %in% master_id_list)]),
                   starting_away_qb = first(name[posteam == away_team & !is.na(qb_epa) & !is.na(name) & play_type == "pass" & (name %in% master_qb_list)]),
                   starting_away_id = first(id[posteam == away_team & !is.na(qb_epa) & !is.na(name) & play_type == "pass" & (id %in% master_id_list)]),) %>%
  ungroup()



games2 <- sched %>% 
  filter(season %in% c(2015:2024),game_type == "REG",!is.na(home_score)) %>%
  mutate(home_qb_name = ifelse(home_qb_name == "Gardner Minshew II","Gardner Minshew",home_qb_name),
         away_qb_name = ifelse(away_qb_name == "Gardner Minshew II","Gardner Minshew",away_qb_name)) %>%
  select(game_id,game_date = gameday,home_team,away_team,
         home_team_score = home_score,away_team_score = away_score,point_diff = result,
         starting_home_qb = home_qb_name,starting_home_id = home_qb_id,
         starting_away_qb = away_qb_name,starting_away_id = away_qb_id) %>%
  mutate(starting_home_qb = convert_name(starting_home_qb),
           starting_away_qb = convert_name(starting_away_qb),
         game_date = as.Date(game_date))

my_games <- games %>%
  select(game_id,starting_home_qb,starting_home_id,starting_away_qb,starting_away_id)

test_games <- games2 %>%
  select(game_id,game_date,home_team,away_team,home_team_score,away_team_score,point_diff) %>%
  left_join(my_games,by = "game_id")
  

games <- test_games


summary_data <- function(B){
  
  print(paste0("Starting new trial at ",Sys.time()))

  
offense_data <- data.frame()

defense_data <- data.frame()

qb_data <- data.frame()


for(i in 1:length(dates)){
  

  #print(paste0("Calculating for ", dates[i]))
  
  
  
  home_teams <- data %>%
    filter(game_date == dates[i]) %>%
    dplyr::select(team = home_team)%>%
    unique()
  
  away_teams <- data %>%
    filter(game_date == dates[i]) %>%
    dplyr::select(team = away_team) %>%
    unique()
  
  teams <- rbind(home_teams,away_teams)
  
  years_data <- data %>% filter(game_date < dates[i],game_date > (dates[i] - years(3)))
  
  offense_date <- years_data %>%
    filter(game_date < dates[i], posteam %in% teams$team,!is.na(posteam)) %>%
    mutate(days_diff = as.numeric(difftime(dates[i],game_date, "days"))) %>%
    group_by(posteam) %>%
    dplyr::summarize(epa_per_play = sum(epa * B^days_diff, na.rm = T)/sum(B^days_diff, na.rm = T),
                     #pass_epa_per_play = mean((epa[pass == 1] * B^days_diff)/B^days_diff,na.rm = T),
                     run_epa_per_play = sum((epa * B^days_diff)[rush==1], na.rm = T)/sum(B^days_diff[rush == 1], na.rm = T),
                     #success_rate = mean((success * B^days_diff)/B^days_diff,na.rm = T),
                     #plays = n()
                     ) %>%
    ungroup() %>%
    mutate(date = dates[i])
  
  defense_date <- years_data %>%
    filter(game_date < dates[i], defteam %in% teams$team,  !is.na(defteam)) %>%
    mutate(days_diff = as.numeric(difftime(dates[i],game_date, "days"))) %>%
    group_by(defteam) %>%
    dplyr::summarize(epa_per_play_allowed =  sum(epa * B^days_diff, na.rm = T)/sum(B^days_diff, na.rm = T),
                     #pass_epa_per_play_allowed = mean((epa[pass == 1] * B^days_diff)/B^days_diff,na.rm = T),
                     run_epa_per_play_allowed = sum((epa * B^days_diff)[rush==1], na.rm = T)/sum(B^days_diff[rush == 1], na.rm = T),
                     #success_rate_allowed = mean((success * B^days_diff)/B^days_diff,na.rm = T),
                     #plays = n()
                     ) %>%
    ungroup() %>%
    mutate(date = dates[i])
  
  qb_date <- years_data %>%
    filter(game_date < dates[i], !is.na(qb_epa),name %in% master_qb_list) %>%
    mutate(days_diff = as.numeric(difftime(dates[i],game_date, "days"))) %>%
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
    mutate(date = dates[i])
  
  offense_data <- rbind(offense_data,offense_date)
  defense_data <- rbind(defense_data,defense_date)
  qb_data <- rbind(qb_data, qb_date)
    
  rm(qb_date,defense_date,qb_date,home_teams,teams,away_teams)
}


qb_data <- qb_data %>%
  mutate(total_dbs = dropbacks_per_game * games_played)

df <- inner_join(games,qb_data %>% filter(dropbacks_per_game > 3), by = c("game_date" = "date","starting_home_qb" = "name","starting_home_id" = "id")) %>%
  rename(home_qb_id = starting_home_id,home_qb_epa_per_play = qb_epa_per_play, home_qb_games_played = games_played,
          home_qb_db_per_game = dropbacks_per_game,home_total_db = total_dbs) %>%
  inner_join(qb_data %>% filter(dropbacks_per_game > 3),by = c("game_date" = "date","starting_away_qb" = "name","starting_away_id" = "id")) %>%
  rename(away_qb_id = starting_away_id,away_qb_epa_per_play = qb_epa_per_play, away_qb_games_played = games_played,
          away_qb_db_per_game = dropbacks_per_game, away_total_db = total_dbs) %>%
  inner_join(offense_data, by = c("game_date" = "date","home_team" = "posteam")) %>%
  rename(home_epa_pp = epa_per_play,  home_run_epa_pp = run_epa_per_play,
        ) %>%
  inner_join(offense_data, by = c("game_date" = "date","away_team" = "posteam")) %>%
  rename(away_epa_pp = epa_per_play,  away_run_epa_pp = run_epa_per_play, 
         ) %>%
  inner_join(defense_data, by = c("game_date" = "date","home_team" = "defteam")) %>%
  rename(home_epa_pp_allowed = epa_per_play_allowed, home_run_epa_pp_allowed = run_epa_per_play_allowed,
         ) %>%
  inner_join(defense_data, by = c("game_date" = "date","away_team" = "defteam")) %>%
  rename(away_epa_pp_allowed = epa_per_play_allowed,  away_run_epa_pp_allowed = run_epa_per_play_allowed
         ) %>%
  arrange(desc(game_date)) %>%
  na.omit()

rm(offense_data,defense_data,qb_data)

print(paste0("Ending trial at ",Sys.time()))
return(df)

}

# cl <- makeCluster(detectCores() - 1)  # Use all but one core
# registerDoParallel(cl)


maximize_r_squared <- function(B) {
  
  df <- summary_data(B)
  
  dt <- sample(nrow(df), 0.75*nrow(df))
  df_train <- df[dt,]
  df_test <- df[-dt,]
  
  model <- mgcv::gam(point_diff ~ s(home_epa_pp,away_epa_pp_allowed) + s(away_epa_pp,home_epa_pp_allowed) 
                     + s(home_qb_epa_per_play,home_total_db) + s(away_qb_epa_per_play,away_total_db),data = df_train)
  
  df_test$proj_spread <- predict(model, df_test)
  
  r2 <- cor(df_test$proj_spread,df_test$point_diff) ^ 2
  
  print(paste0("rsq function ending at ",Sys.time(), "with a value of ",r2))
  
  return(-r2)
  
}




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


