library(tidyverse)
library(mgcv)
options(dplyr.summarise.inform = FALSE)
addTaskCallback(function(...) {set.seed(123); TRUE})

#optimal_beta <- 0.9905928 
other_b <- 0.9974176



df_with_optim <- summary_data(1)


dt <- sample(nrow(df_with_optim), 0.75*nrow(df_with_optim))
df_train <- df_with_optim[dt,]
df_test <- df_with_optim[-dt,]



#model <- mgcv::gam(point_diff ~ s(home_epa_pp,away_epa_pp_allowed) + s(away_epa_pp,home_epa_pp_allowed) 
 #                  + s(home_run_epa_pp,away_run_epa_pp_allowed) + s(away_run_epa_pp,home_run_epa_pp_allowed) 
  #                 + s(home_qb_epa_per_play,home_total_db) + s(away_qb_epa_per_play,away_total_db),data = df_train)

model2 <-  mgcv::gam(point_diff ~ s(home_epa_pp,away_epa_pp_allowed) + s(away_epa_pp,home_epa_pp_allowed) 
                     + s(home_qb_epa_per_play,home_total_db) + s(away_qb_epa_per_play,away_total_db),data = df_train)

summary(model)
summary(model2)

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

