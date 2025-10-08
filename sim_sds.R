library(tidyverse)
library(mgcv)
library(nflreadr)
library(MASS)

data <- load_pbp(2019:2025) %>%
  mutate(name = ifelse(name == "G.Minshew II","G.Minshew",name),
         name = ifelse(name == "Aa.Rodgers", "A.Rodgers",name),
         game_date = as.Date(game_date))


qb_deltas <- data %>%
  filter(!is.na(qb_epa), pass == 1 | rush == 1, season_type == "REG") %>%
  group_by(name, id,posteam, season) %>%
  dplyr::summarize(mean_qb_epa = mean(qb_epa),
                   dbs = sum(qb_dropback),
                   total_epa = sum(qb_epa),
                   plays = n()) %>%
  filter(dbs > 200) %>%
  ungroup() %>%
  arrange(name, id, season) %>%
  group_by(name, id) %>%
  mutate(delta_qb_epa = mean_qb_epa - lag(mean_qb_epa)) %>%
  ungroup()

offense_deltas <- data %>%
  filter(!is.na(epa), season_type == "REG", pass == 1 | rush == 1, season != 2025) %>%
  group_by(posteam, season) %>% 
  dplyr::summarize(epa_pp = mean(epa)) %>%
  ungroup() %>%
  arrange(posteam, season) %>%
  group_by(posteam) %>%
  mutate(delta_epa = epa_pp - lag(epa_pp)) %>%
  ungroup()

defense_deltas <- data %>%
  filter(!is.na(epa), season_type == "REG", pass == 1 | rush == 1, season != 2025) %>%
  group_by(defteam, season) %>% 
  dplyr::summarize(epa_pp_allowed = mean(epa)) %>%
  ungroup() %>%
  arrange(defteam, season) %>%
  group_by(defteam) %>%
  mutate(delta_epa_allowed = epa_pp_allowed - lag(epa_pp_allowed)) %>%
  ungroup()


sd_shock_qb <- qb_deltas %>%
  dplyr::summarize(sd_qb = sd(delta_qb_epa, na.rm = T))

sd_shock_off <- offense_deltas %>%
  dplyr::summarize(sd_off = sd(delta_epa, na.rm = T))

sd_shock_def <- defense_deltas %>%
  dplyr::summarize(sd_def = sd(delta_epa_allowed, na.rm = T))



cov_d <- inner_join(qb_deltas, offense_deltas, by = c("posteam", "season")) %>% na.omit() %>% dplyr::select(delta_qb_epa, delta_epa)


sigma <- cov(cov_d)


test_features <- data.frame(qb_epa = c(.01, .22, -.03), team_epa = c(.01,-.02, .28))

x_star <- test_features + mvrnorm(1, mu = c(0,0), Sigma = sigma)
x_star

test_features$adjusted <- test_features + mvrnorm(1, mu = c(0,0), Sigma = sigma)


p <- 2
e <- mvrnorm(n = 3, mu = rep(0,p), Sigma = sigma)
colnames(e) <- paste0(cols, "_shock")
xnew <- test_features + e


write.csv(sigma, file = "C:/Users/b.cashman/Documents/csv/NFL/cov_matrix.csv", row.names = F)
