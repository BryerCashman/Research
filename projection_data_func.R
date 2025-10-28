get_projection_data <- function(base_off, base_def, base_qb){
  sunday <- schedule %>%
    filter(week == current_week) %>%
    group_by(date = gameday) %>%
    dplyr::summarize(count = n()) %>%
    ungroup() %>%
    slice_max(order_by = count,n = 1) %>%
    pull(date) %>%
    as.Date()
  
  master_qb_list <- rbind(schedule %>% select(name = home_qb),schedule %>% select(name = away_qb))  %>% unique() %>% rbind(data.frame(name = c("A.Dalton")))
  
  offense_data <- data %>%
    filter(!is.na(posteam)) %>%
    mutate(days_diff = as.numeric(difftime(sunday,game_date, "days"))) %>%
    group_by(posteam) %>%
    dplyr::summarize(epa_per_play = sum(epa * base_off^days_diff, na.rm = T)/sum(base_off^days_diff, na.rm = T),
                     #pass_epa_per_play = mean((epa[pass == 1] * B^days_diff)/B^days_diff,na.rm = T),
                     run_epa_per_play = sum((epa * base_off^days_diff)[rush==1], na.rm = T)/sum(base_off^days_diff[rush == 1], na.rm = T),
                     #success_rate = mean((success * B^days_diff)/B^days_diff,na.rm = T),
                     #plays = n(), 
                     proe = sum(pass_oe * base_off^days_diff, na.rm = T)/sum(base_off^days_diff, na.rm = T)
    ) %>%
    ungroup() %>%
    mutate(date = sunday)
  
  defense_data <- data %>%
    filter(!is.na(defteam)) %>%
    mutate(days_diff = as.numeric(difftime(sunday,game_date, "days"))) %>%
    group_by(defteam) %>%
    dplyr::summarize(epa_per_play_allowed =  sum(epa * base_def^days_diff, na.rm = T)/sum(base_def^days_diff, na.rm = T),
                     #pass_epa_per_play_allowed = mean((epa[pass == 1] * B^days_diff)/B^days_diff,na.rm = T),
                     run_epa_per_play_allowed = sum((epa * base_def^days_diff)[rush==1], na.rm = T)/sum(base_def^days_diff[rush == 1], na.rm = T),
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
    dplyr::summarize(qb_epa_per_play = sum((qb_epa * base_qb^days_diff), na.rm = T)/sum(base_qb^days_diff, na.rm = T),
                     #qb_total_epa_two_year = sum((qb_epa *B^days_diff)/B^days_diff ,na.rm = T),
                     games_played = length(unique(game_id)),
                     #qb_epa_per_game = qb_total_epa_two_year/games_played,
                     #qb_success_rate = mean((qb_success *B^days_diff)/B^days_diff,na.rm = T),
                     dropbacks_per_game = sum(qb_dropback,na.rm = T)/games_played
    ) %>%
    ungroup() %>%
    mutate(total_dbs = dropbacks_per_game * games_played,
           ewm_qb_epa_play = qb_epa_per_play,
           dbs = total_dbs,
           qb_pred_epa = predict(model_pred_qb_epa, pick(ewm_qb_epa_play, dbs))) %>%
    filter(dropbacks_per_game > 3  ) %>%
    select(-ewm_qb_epa_play, -total_dbs)
  
  
  assign("offense_data", offense_data, envir = .GlobalEnv)
  assign("defense_data", defense_data, envir = .GlobalEnv)
  assign("qb_data", qb_data, envir = .GlobalEnv)
  
}