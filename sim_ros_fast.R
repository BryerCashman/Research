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


sim_ros_fast_postseason <- function(runs = 100) {
  # --- Setup once ---
  setDTthreads(percent = 100)
  
  ratings0 <- as.data.table(current_ratings)
  sched    <- as.data.table(schedule)[
    , .(game_id, week, result, home_team, away_team, divisional_game, conference_game, game_type)
  ]
  
  setkey(ratings0, team)
  sched[, home_idx := ratings0[home_team, which = TRUE]]
  sched[, away_idx := ratings0[away_team, which = TRUE]]
  
  plan(multisession, workers = max(1, parallel::detectCores() - 1))
  opts <- furrr::furrr_options(
    seed = TRUE,
    packages = c("data.table","MASS","stats","mgcv")
  )
  
  per_run <- furrr::future_map(1:runs, function(i) {
    new_ratings <- copy(ratings0)
    
    # Correlated weekly shocks
    new_cor <- MASS::mvrnorm(n = 32, mu = rep(0, 2), Sigma = sigma)
    new_ratings[, c("qb_pred_epa", "epa_per_play") :=
                  .(qb_pred_epa + new_cor[,1], epa_per_play + new_cor[,2])]
    new_ratings[, epa_per_play_allowed := epa_per_play_allowed + rnorm(.N, sd = def_epa_sd)]
    new_ratings[, proe := proe + rnorm(.N, sd = 1.5)]
    
    # Per-game features (REG season)
    df <- copy(sched)
    df[, `:=`(
      home_epa_pp          = new_ratings$epa_per_play[home_idx],
      home_qb_pred_epa     = new_ratings$qb_pred_epa[home_idx],
      home_epa_pp_allowed  = new_ratings$epa_per_play_allowed[home_idx],
      home_proe            = new_ratings$proe[home_idx],
      away_epa_pp          = new_ratings$epa_per_play[away_idx],
      away_qb_pred_epa     = new_ratings$qb_pred_epa[away_idx],
      away_epa_pp_allowed  = new_ratings$epa_per_play_allowed[away_idx],
      away_proe            = new_ratings$proe[away_idx],
      sim                  = i
    )]
    
    # Predictions
    df[, x_point_diff := predict(model_proj_spread2, newdata = df)]
    df[, home_wp      := predict(model_home_wp, newdata = df, type = "response")]
    
    # Resolve undecided games
    df[, rand := runif(.N)]
    df[, home_win := fifelse(!is.na(result),
                             fifelse(result > 0, 1L, 0L),
                             fifelse(rand < home_wp, 1L, 0L))]
    
    # Standings input
    df_feed <- df[, .(
      game_id, week,
      result = fifelse(is.na(result), ifelse(home_win == 0, -1, 1), result),
      home_team, away_team,
      divisional_game, conference_game, game_type,
      sim = 1L
    )]
    
    # Standings (tiebreakers via nflseedR)
    standings <- nfl_standings(df_feed, verbosity = "NONE")
    
    stand_dt <- as.data.table(standings)[, .(team, div_rank, conf_rank, true_wins)]
    # Build seeds with nflseedR::divisions (robust)
    afc_seeds <- .seed_conf(stand_dt, "AFC")
    nfc_seeds <- .seed_conf(stand_dt, "NFC")
    
    # Integrity check: no overlap between conferences
    if (length(intersect(afc_seeds$team, nfc_seeds$team)) > 0L) {
      stop("Conference overlap in seeds. Check team codes & mapping.")
    }
    
    afc <- .sim_conf_playoffs(afc_seeds, "AFC", new_ratings)
    nfc <- .sim_conf_playoffs(nfc_seeds, "NFC", new_ratings)
    sb  <- .sim_super_bowl(afc$conf_champ, nfc$conf_champ, new_ratings)
    
    po_games <- rbindlist(list(afc$games, nfc$games, sb), fill = TRUE)
    po_games[, run := i]
    
    # Regular-season return block (matches your existing output)
    reg_out <- data.table(
      team      = standings$team,
      wins      = standings$true_wins,
      div_rank  = standings$div_rank,
      conf_rank = standings$conf_rank,
      run       = i
    )
    
    # --- Postseason seeding (use conf-wide ranks to order seeds) ---
    stand_dt <- as.data.table(standings)
    stand_dt <- merge(stand_dt, as.data.table(teams), by.x = "team", by.y = "team_abbr", all.x = TRUE)
    
    afc_seeds <- .seed_conf(stand_dt, "AFC")
    nfc_seeds <- .seed_conf(stand_dt, "NFC")
    
    # --- Simulate playoffs with reseeding ---
    afc <- .sim_conf_playoffs(afc_seeds, "AFC", new_ratings)
    nfc <- .sim_conf_playoffs(nfc_seeds, "NFC", new_ratings)
    
    sb <- .sim_super_bowl(afc$conf_champ, nfc$conf_champ, new_ratings)
    
    # Compile playoff game log
    po_games <- rbindlist(list(afc$games, nfc$games, sb), use.names = TRUE, fill = TRUE)
    po_games[, run := i]
    
    list(regular = reg_out, playoffs = po_games, champ = data.table(run = i, champion = sb$winner))
  }, .options = opts)
  
  # Bind everything
  regular   <- rbindlist(lapply(per_run, `[[`, "regular"))
  playoffs  <- rbindlist(lapply(per_run, `[[`, "playoffs"))
  champions <- rbindlist(lapply(per_run, `[[`, "champ"))
  
  list(regular = regular, playoffs = playoffs, champions = champions)
}



