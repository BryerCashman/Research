library(tidyverse)
library(mgcv)
library(nflreadr)
options(dplyr.summarise.inform = FALSE)
addTaskCallback(function(...) {set.seed(123); TRUE})

#optimal_beta <- 0.9905928 
other_b <- 0.9974176



df_with_optim <- summary_data(other_b)


where <- df_with_optim %>%
  filter(!game_id %in% df_with_optim_new_games$game_id)

where2 <- df_with_optim_new_games %>%
  filter(!game_id %in% df_with_optim$game_id)


dt <- sample(nrow(df_with_optim), 0.75*nrow(df_with_optim))
df_train <- df_with_optim[dt,]
df_test <- df_with_optim[-dt,]



#model <- mgcv::gam(point_diff ~ s(home_epa_pp,away_epa_pp_allowed) + s(away_epa_pp,home_epa_pp_allowed) 
 #                  + s(home_run_epa_pp,away_run_epa_pp_allowed) + s(away_run_epa_pp,home_run_epa_pp_allowed) 
  #                 + s(home_qb_epa_per_play,home_total_db) + s(away_qb_epa_per_play,away_total_db),data = df_train)

model2 <-  mgcv::gam(point_diff ~ s(home_epa_pp,away_epa_pp_allowed) + s(away_epa_pp,home_epa_pp_allowed) 
                     + s(home_qb_epa_per_play,home_total_db) + s(away_qb_epa_per_play,away_total_db),data = df_train)


summary(model2)

schedule <- nflreadr::load_schedules() %>% mutate(gameday = as.Date(gameday))

load("~/Documents/GitHub/Research/proj_model.RDS")


df_test$proj_spread <- predict(model2, df_test)

df_test <- df_test %>% relocate(proj_spread,.before = point_diff) %>% filter(game_date > as.Date("2016-03-10")) 




r2 <- cor(df_test$proj_spread,df_test$point_diff) ^ 2

grid <- expand.grid(home_epa_pp = c(-2:3)/10,away_epa_pp =  c(-2:3)/10,
                    home_qb_epa_per_play = c(-2:3)/10,away_qb_epa_per_play = c(-2:3)/10,
                    home_epa_pp_allowed = c(-2:2)/10, away_epa_pp_allowed = c(-2:2)/10,
                    home_total_db = seq(300,1500,by = 300),away_total_db = seq(300,1500,by = 300))

dt2 <- sample(nrow(grid), 0.5*nrow(grid))


grid_predict <- grid[dt2,]

rm(grid)

grid_predict$proj_spread = predict(model2,grid_predict)


grid <- expand.grid(home_epa_pp = c(-30:30)/100,away_epa_pp =  .05,
                    home_qb_epa_per_play = .1,away_qb_epa_per_play =.1,
                    home_epa_pp_allowed = 0, away_epa_pp_allowed = (-20:20)/100,
                    home_total_db = 500,away_total_db = 500)

grid$proj_spread <- predict(model2, grid)

ggplot(grid,aes(home_qb_epa_per_play,proj_spread)) +
  theme_bw() +
  geom_smooth()

ggplot(grid,aes(x = home_epa_pp,y = away_epa_pp_allowed,color = proj_spread)) +
  geom_point(size=2.5) +
  theme_bw() +
  scale_color_gradient2("Proj Spread",midpoint = 2.5, low = "blue", mid = "white", high = "red") 


mean(grid$proj_spread)  

proj_model <- model2

save(proj_model, file = "/Users/bryer/GitHub/Research/proj_model.RDS")


### Vs actual spread

df_with_optim$proj_spread <- predict(proj_model,df_with_optim)

check <- df_with_optim %>% filter(game_date > as.Date("2016-03-10"))

overall_r2 <- cor(check$proj_spread,check$point_diff) ^2
overall_rmse <- Metrics::rmse(check$proj_spread,check$point_diff)

spreads <- df_with_optim %>% relocate(proj_spread,.before = point_diff) %>% filter(game_date > as.Date("2016-03-10")) %>%
  left_join(schedule %>% select(season,week,home_team,away_team,spread_line,gameday),by = c("game_date" = "gameday","home_team","away_team")) %>%
  relocate(spread_line, .before = point_diff) %>%
  filter(!is.na(spread_line))

spreads_r2 <- cor(spreads$spread_line,spreads$point_diff) ^2
spreads_rmse <- Metrics::rmse(spreads$spread_line,spreads$point_diff)

print(paste0("My model r^2: ",round(overall_r2,2)," Vegas r^2: ",round(spreads_r2,2)," My model rmse: ",round(overall_rmse,2), " Vegas rmse: ",round(spreads_rmse,2)))

big_disagreements <- spreads %>%
  filter(abs(proj_spread - spread_line) > 6)

Metrics::rmse(big_disagreements$spread_line,big_disagreements$point_diff)
Metrics::rmse(big_disagreements$proj_spread,big_disagreements$point_diff)
