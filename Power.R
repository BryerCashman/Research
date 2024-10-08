library(tidyverse)
library(nflreadr)
library(nflplotR)
library(stringr)
library(mgcv)
library(gt)
library(scales)
library(purrr)


data <- load_pbp(2022:2024) %>%
  filter((rush == 1 | pass == 1) )


B <- optimal_beta <- 0.9974176
current_week <- 6

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

master_qb_list <- rbind(schedule$home_qb,schedule$away_qb) %>%
  unique()

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
  filter(!is.na(qb_epa),name %in% master_qb_list) %>%
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

matchups <- expand.grid(Home_Team = teams, Away_Team = teams)
matchups <- matchups[matchups$Home_Team != matchups$Away_Team, ]
current_qbs <- rbind(schedule %>% select(team = home_team,qb = home_qb,week) %>% filter(week %in% c(current_week,current_week + 1)),
                     schedule %>% select(team = away_team,qb = away_qb,week) %>% filter(week %in% c(current_week,current_week + 1))) %>%
  select(team,qb) %>%
  unique()

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

ratings$total = round((ratings$home_rating + ratings$away_rating)/2,1)
ratings <- ratings %>% arrange(desc(total))

matchups <- mutate(matchups,matchup_name = paste0(Home_Team," - ",Away_Team))

weekly_games <- schedule %>%
  filter(week == current_week) %>%
  mutate(matchup_name = paste0(home_team," - ",away_team)) 

weekly_projections <- matchups %>%
  filter(matchup_name %in% weekly_games$matchup_name) %>%
  select(Home_Team, Away_Team, proj_spread,matchup_name) %>%
  mutate(week = current_week)

weekly_projections <- inner_join(weekly_projections,weekly_games,by = c("matchup_name","week")) %>%
  select(week,Home_Team,Away_Team,proj_spread,spread_line) %>%
  mutate(proj_spread = round(proj_spread,1),
         disagreement = proj_spread - spread_line)


top_teams <- ratings %>%
  select(team,total) %>%
  slice_max(order_by = total,n =16) %>%
  mutate(Rank = row_number()) %>%
  select(Rank,team,total)

bot_teams <- ratings %>%
  select(team2 = team,total2 = total) %>%
  slice_min(order_by = total2,n =16) %>%
  arrange(desc(total2)) %>%
  mutate(Rank2 = row_number() + 16) %>%
  select(Rank2,team2,total2)

df <- cbind(top_teams,bot_teams)


wordmark <- df %>%
  gt() %>%
  # Add NFL team logos for both columns
  gt_nfl_wordmarks(columns = c(team, team2),height = 50) %>%
  # Format the value columns with a blue to red gradient
  data_color(
    columns = c(total, total2),
    colors = scales::col_numeric(
      palette = c("dodgerblue", "white", "firebrick2"),
      domain = c(-10, 10)
    )
  ) %>%
  # Center the text in the body cells
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(columns = c(team, total, team2, total2,Rank,Rank2))
  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(columns = c(team, total, team2, total2))
  ) %>%
  tab_style(
    style = cell_text(size = px(25)),
    locations = cells_body(columns = c( total,  total2))
  ) %>%
  cols_label(
    team = "Team",
    team2 = "Team"
  ) %>%
  # Keep the original column names for totals
  cols_label(
    total = "Value",
    total2 = "Value"
  ) %>%
  cols_label(
    Rank = "Rank",
    Rank2 = "Rank"
  ) %>%
  # Set table width and add a title
  tab_options(table.width = pct(100)) %>%
  tab_header(title = paste0("Week ",current_week," NFL Power Ratings")) %>%
  cols_width(total ~ px(120),
             total2 ~ px(120),
             team ~ px(125),
             team2 ~ px(125),
             Rank ~ px(50),
             Rank2 ~ px(50)
             ) %>%
  tab_style(
    style = cell_text(size = px(30), font = "Arial", weight = "bold",align = "center"),
    locations = cells_body(columns = c(Rank,Rank2))
  ) %>%
  tab_style(
    style = cell_text(size = px(30), font = "Arial", weight = "bold",align = "center"),
    locations = cells_title(groups = "title")
  )

wordmark

wordmark %>% 
  gtsave("/Users/bryer/Documents/NFL Projects/Power Ratings/wordmark.png")








logos <- df %>%
  gt() %>%
  # Add NFL team logos for both columns
  gt_nfl_logos(columns = c(team, team2),height = 50) %>%
  # Format the value columns with a blue to red gradient
  data_color(
    columns = c(total, total2),
    colors = scales::col_numeric(
      palette = c("dodgerblue", "white", "firebrick2"),
      domain = c(-10, 10)
    )
  ) %>%
  # Center the text in the body cells
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels(columns = c(team, total, team2, total2))
  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(columns = c(team, total, team2, total2))
  ) %>%
  cols_label(
    team = "Team",
    team2 = "Team"
  ) %>%
  # Keep the original column names for totals
  cols_label(
    total = "Value",
    total2 = "Value"
  ) %>%
  cols_label(
    Rank = "Rank",
    Rank2 = "Rank"
  ) %>%
  # Set table width and add a title
  tab_options(table.width = pct(100)) %>%
  tab_header(title = paste0("Week ",current_week," NFL Power Ratings")) %>%
  cols_width(total ~ px(120),
             total2 ~ px(120),
             team ~ px(125),
             team2 ~ px(125),
             Rank ~ px(50),
             Rank2 ~ px(50)
  ) %>%
  tab_style(
    style = cell_text(size = px(30), font = "Arial", weight = "bold",align = "center"),
    locations = cells_body(columns = c(Rank,Rank2))
  )%>% 
  gtsave("/Users/bryer/Documents/NFL Projects/Power Ratings/logos.png")

