library(tidyverse)
library(mgcv)
library(nflreadr)
library(MASS)

data <- load_pbp(2019:2025) %>%
  mutate(name = ifelse(name == "G.Minshew II","G.Minshew",name),
         name = ifelse(name == "Aa.Rodgers", "A.Rodgers",name),
         game_date = as.Date(game_date))

qb_yearly <- data %>%
  filter(!is.na(qb_epa), pass == 1 | rush == 1, season_type == "REG") %>%
  group_by(name, id, season) %>%
  dplyr::summarize(mean_qb_epa = mean(qb_epa),
                   dbs = sum(qb_dropback),
                   total_epa = sum(qb_epa),
                   plays = n()) %>%
  filter(dbs > 200) %>%
  ungroup() %>%
  arrange(name, id, season) %>%
  group_by()






