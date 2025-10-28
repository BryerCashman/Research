library(data.table)
library(furrr)
library(mgcv)
library(tidyverse)

sim_ros_fast <- function(runs = 100) {
  # --- Setup once ---
  setDTthreads(percent = 100)  # use all cores for data.table ops
  
  # Convert to data.table once
  ratings0 <- as.data.table(current_ratings)               # cols: team, qb_epa_per_play, epa_per_play, epa_per_play_allowed, total_dbs
  sched    <- as.data.table(schedule)[
    , .(game_id, week, result, home_team, away_team, divisional_game, conference_game, game_type)
  ]
  
  # Pre-index home/away team rows in ratings table (assumes unique team names)
  setkey(ratings0, team)
  sched[, home_idx := ratings0[home_team, which = TRUE]]
  sched[, away_idx := ratings0[away_team, which = TRUE]]
  
  # Pre-allocate result holder (list -> rbind once)
  out <- vector("list", runs)
  
  # Prepare models (optional but faster than predict() if applicable)
  # If these are GLM/lm, you can do:
  # beta_proj <- coef(proj_model)
  # beta_home <- coef(model_home_wp)
  # and build your X matrices each run. If not, keep predict(), but use data.table newdata.
  
  # Parallel plan (change to sequential if you prefer)
  plan(multisession, workers = max(1, parallel::detectCores() - 1))
  opts <- furrr_options(
    seed = TRUE,
    packages = c(
      "data.table", "MASS", "stats","mgcv"
    )
  )
  
  
  out <- future_map(1:runs, function(i) {
    # --- Per run (data.table copies are shallow; we copy only sched skeleton) ---
    new_ratings <- copy(ratings0)
    
    # Draw correlated shocks (mvrnorm gives 32x2)
    new_cor <- MASS::mvrnorm(n = 32, mu = rep(0, 2), Sigma = sigma)
    
    # Update ratings in place
    new_ratings[, c("qb_pred_epa", "epa_per_play") :=
                  .(qb_pred_epa + new_cor[,1],
                    epa_per_play + new_cor[,2])
    ]
    new_ratings[, epa_per_play_allowed := epa_per_play_allowed + rnorm(.N, sd = def_epa_sd)]
    new_ratings[, proe := proe + rnorm(.N, sd = 1.5)]
    
    
    # Build per-game data quickly via indices (no joins)
    df <- copy(sched)
    
    # Pull columns via vectorized indexing
    df[, `:=`(
      home_epa_pp          = new_ratings$epa_per_play[home_idx],
      home_qb_pred_epa = new_ratings$qb_pred_epa[home_idx],
      home_epa_pp_allowed  = new_ratings$epa_per_play_allowed[home_idx],
      home_proe  = new_ratings$proe[home_idx],
      away_epa_pp          = new_ratings$epa_per_play[away_idx],
      away_qb_pred_epa  = new_ratings$qb_pred_epa[away_idx],
      away_epa_pp_allowed  = new_ratings$epa_per_play_allowed[away_idx],
      away_proe  = new_ratings$proe[away_idx],
      sim                  = i
    )]
    
    # Predictions (keep predict if models are not plain GLM/lm)
    # Note: set stringsAsFactors/factor levels up-front to avoid coercion costs
    df[, x_point_diff := predict(model_proj_spread2, newdata = df)]
    df[, home_wp      := predict(model_home_wp, newdata = df, type = "response")]
    
    # Resolve winners only for undecided games
    df[, rand := runif(.N)]
    df[, home_win := fifelse(!is.na(result),
                             fifelse(result > 0, 1L, 0L),
                             fifelse(rand < home_wp, 1L, 0L))]
    
    # Feed only what standings actually needs
    df_feed <- df[, .(
      game_id, week,
      result = fifelse(is.na(result), ifelse(home_win == 0, -1, 1), result),
      home_team, away_team,
      divisional_game, conference_game, game_type,
      sim = 1L  # if nfl_standings requires this
    )]
    
    # The slow bit (can’t avoid; minimize its input size)
    standings <- nfl_standings(df_feed, verbosity = "NONE")
    
    # Return just what we need; no global <<-
    data.table(
      team      = standings$team,
      wins      = standings$true_wins,
      div_rank  = standings$div_rank,
      conf_rank = standings$conf_rank,
      run       = i
    )
  }, .options = opts)
  
  # Bind once at the end
  sim_wins <- rbindlist(out, use.names = TRUE)
  
  # return (or set in caller env if you must)
  sim_wins[]
}


