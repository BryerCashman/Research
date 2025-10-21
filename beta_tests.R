get_data <- function(base_def){

  defense1921 <- data %>%
    filter(season %in% c(2019, 2020, 2021),
           (pass == 1 | rush == 1),
           !is.na(epa)) %>%
    group_by(defteam) %>%
    arrange(game_date, game_id, play_id, .by_group = TRUE) %>%
    mutate(
      days_gap = {
        dg <- as.numeric(game_date - lag(game_date, default = first(game_date)))
        pmax(0, replace(dg, is.na(dg), 0))
      },
      
      ewm_epa_play = ewm_irregular_lagged(epa, days_gap, base_def),
      ewm_success_rate = ewm_irregular_lagged(success, days_gap, base_def),
    ) %>%
    ungroup() %>%
    group_by(season, defteam, game_id, game_date) %>%
    summarize(epa_pp_allowed = first(ewm_epa_play),
              sr_allowed = first(ewm_success_rate),
              .groups = "drop") %>%
    filter(season != 2019)
  
  defense2022 <- data %>%
    filter(season %in% c(2020, 2021, 2022),
           (pass == 1 | rush == 1),
           !is.na(epa)) %>%
    group_by(defteam) %>%
    arrange(game_date, game_id, play_id, .by_group = TRUE) %>%
    mutate(
      days_gap = {
        dg <- as.numeric(game_date - lag(game_date, default = first(game_date)))
        pmax(0, replace(dg, is.na(dg), 0))
      },
      
      ewm_epa_play = ewm_irregular_lagged(epa, days_gap, base_def),
      ewm_success_rate = ewm_irregular_lagged(success, days_gap, base_def),
    ) %>%
    ungroup() %>%
    group_by(season, defteam, game_id, game_date) %>%
    summarize(epa_pp_allowed = first(ewm_epa_play),
              sr_allowed = first(ewm_success_rate),
              .groups = "drop") %>%
    filter(season == 2022)
  
  defense2123 <- data %>%
    filter(season %in% c(2021, 2022, 2023),
           (pass == 1 | rush == 1),
           !is.na(epa)) %>%
    group_by(defteam) %>%
    arrange(game_date, game_id, play_id, .by_group = TRUE) %>%
    mutate(
      days_gap = {
        dg <- as.numeric(game_date - lag(game_date, default = first(game_date)))
        pmax(0, replace(dg, is.na(dg), 0))
      },
      
      ewm_epa_play = ewm_irregular_lagged(epa, days_gap, base_def),
      ewm_success_rate = ewm_irregular_lagged(success, days_gap, base_def),
    ) %>%
    ungroup() %>%
    group_by(season, defteam, game_id, game_date) %>%
    summarize(epa_pp_allowed = first(ewm_epa_play),
              sr_allowed = first(ewm_success_rate),
              .groups = "drop") %>%
    filter(season == 2023)
  
  defense2224 <- data %>%
    filter(season %in% c(2022, 2023, 2024),
           (pass == 1 | rush == 1),
           !is.na(epa)) %>%
    group_by(defteam) %>%
    arrange(game_date, game_id, play_id, .by_group = TRUE) %>%
    mutate(
      days_gap = {
        dg <- as.numeric(game_date - lag(game_date, default = first(game_date)))
        pmax(0, replace(dg, is.na(dg), 0))
      },
      
      ewm_epa_play = ewm_irregular_lagged(epa, days_gap, base_def),
      ewm_success_rate = ewm_irregular_lagged(success, days_gap, base_def),
    ) %>%
    ungroup() %>%
    group_by(season, defteam, game_id, game_date) %>%
    summarize(epa_pp_allowed = first(ewm_epa_play),
              sr_allowed = first(ewm_success_rate),
              .groups = "drop") %>%
    filter(season == 2024)
  
  defense2325 <- data %>%
    filter(season %in% c(2023, 2024, 2025),
           (pass == 1 | rush == 1),
           !is.na(epa)) %>%
    group_by(defteam) %>%
    arrange(game_date, game_id, play_id, .by_group = TRUE) %>%
    mutate(
      days_gap = {
        dg <- as.numeric(game_date - lag(game_date, default = first(game_date)))
        pmax(0, replace(dg, is.na(dg), 0))
      },
      
      ewm_epa_play = ewm_irregular_lagged(epa, days_gap, base_def),
      ewm_success_rate = ewm_irregular_lagged(success, days_gap, base_def),
    ) %>%
    ungroup() %>%
    group_by(season, defteam, game_id, game_date) %>%
    summarize(epa_pp_allowed = first(ewm_epa_play),
              sr_allowed = first(ewm_success_rate),
              .groups = "drop") %>%
    filter(season == 2025)
  
  defense_data <- rbind(defense1921,defense2022,defense2123,defense2224,defense2325)
  rm(defense1921,defense2022, defense2123, defense2224, defense2325)


df <- inner_join(games,defense_data, by = c("game_date","game_id","home_team" = "defteam")) %>%
  rename(home_epa_pp_allowed = epa_pp_allowed, home_sr_allowed = sr_allowed
  ) %>%
  inner_join(defense_data, by = c("game_date","game_id","away_team" = "defteam")) %>%
  rename(away_epa_pp_allowed = epa_pp_allowed,  away_sr_allowed = sr_allowed
  )

df
}


test <- get_data(1)

dt <- sample(nrow(test), .75 * nrow(test))


optimizer <- function(beta){
  d <- get_data(beta)
  train <- d[dt,]
  test <- d[-dt,] %>% drop_na(home_epa_pp_allowed, away_epa_pp_allowed)
  
  m <- lm(point_diff ~ home_epa_pp_allowed + away_epa_pp_allowed, data = train)
  
  
  test$guess <- predict(m, test)
  rmse <- Metrics::rmse(test$point_diff, test$guess)
  print(paste0("Trial with a beta of ", beta, " and resulting rmse of", round(rmse,3)))
  rmse
}

optimise(optimizer, c(.9,1))



qb_base <- 0.9992757
offense_base <- 0.9962453
defense_base <- 0.9958141


