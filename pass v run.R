library(tidyverse)
library(ggrepel)
library(nflreadr)
library(nflplotR)
library("scales")
data <- load_pbp(2015:2024)


plays_by_year <- data %>%
  group_by(season) %>%
  dplyr::summarize(plays = n())


year_comparison <- data %>%
  filter((rush == 1 | pass == 1),week %in% c(1:6)) %>%
  group_by(season) %>%
  dplyr::summarize(pass_epa = mean(epa[pass == 1],na.rm = T),
                   run_epa = mean(epa[rush == 1],na.rm = T),
                   total_epa = mean(epa,na.rm = T),
                   diff = pass_epa - run_epa,
                   plays = n())

ggplot(year_comparison, aes(x = season)) +
  geom_line(aes(y = run_epa, color = "run_epa")) +
  geom_line(aes(y = pass_epa, color = "pass_epa")) +
  geom_line(aes(y = total_epa, color = "total_epa")) +
  scale_x_continuous(breaks = unique(year_comparison$season), 
                     labels = as.factor(unique(year_comparison$season))) +
  theme_bw() +
  labs(x = "Season")


game_scores <- data %>%
  filter(season_type == "REG",week %in% c(1:6)) %>%
  group_by(season,game_id) %>%
  dplyr::summarize(home_team_score = last(total_home_score),
                   away_team_score = last(total_away_score),
                   total_points = home_team_score + away_team_score) %>%
  ungroup() %>%
  group_by(season) %>%
  dplyr::summarize(ppg = mean(total_points))

ggplot(game_scores,aes(season,ppg)) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(breaks = unique(game_scores$season), 
                     labels = as.factor(unique(game_scores$season)))




