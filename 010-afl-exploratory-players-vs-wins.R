# This code loads the saved data extractions for player metrics and team results
# It is a first cut exploratory analysis at the match result level
# Player metrics will be grouped into indices:
#
#
#
#
# Hypothesis: player performance (metrics) explains team results

#### Prepare environment for analysis ####
  # clear environment, clean image
  rm(list=ls())
  # set working directory
  setwd("C:\\Users\\james.hooi\\Documents\\data analytics\\Springboard-FDS-Capstone")
  # load required libraries
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  
#### Load data files ####
  # TAKE 15 YEARS OF DATA - % Played is not available before 2003
  
  # Get saved match results data
  # Data frame: prep.team.res
  load(file = ".\\teamresults_1998to2017.Rda")
  team.results <- prep.team.res %>% filter(Year >= 2003) %>% ungroup
  rm("prep.team.res")
  
  # Get saved player metrics data
  # Data frame: cons.df
  # NOTE: player data in three files, will overwrite cons.df so save data each time
  load(file = ".\\playerstats_2012to2017.Rda")
  player.metrics <- cons.df %>% mutate(GF.1 = NA)
  
  load(file = ".\\playerstats_2006to2011.Rda")
  player.metrics <- rbind(player.metrics, cons.df)
  
  load(file = ".\\playerstats_1998to2005.Rda")
  cons.df <- cons.df %>% mutate(GF.1 = NA) %>% filter(Year >= 2003)
  player.metrics <- rbind(player.metrics, cons.df)
  
  rm("cons.df")
  

#### Clean and shape data for analysis ####
  # Remove "Tot" column, not required
  # Remove "Subs" metric, not required
  player.metrics <- player.metrics %>% select(-Tot) %>% filter(Metric != "Subs")
      # for Rmd
      # raw.player.metrics <- player.metrics
  
  # Turn data into long format
  # Clean non-ASCII characters
  # Note: conversion to numeric produces warning of coercion but before/after data reconciles 100%
  player.metrics <- player.metrics %>% 
    gather(key = round, value = round_stat, -Player, -Metric, -Team, -Year) %>%
    mutate(round_stat = iconv(trimws(round_stat, "both"), to = "ASCII", sub = "")
           , round_stat = if_else(!is.na(round_stat) & !(round_stat %in% c("","-")), as.numeric(round_stat), 0))
  
  # Bring out Played % to determine if player played the game
  # If either team didn't play (e.g. bye or final) or player didn't play, remove the records
  # These records would disturb means etc
  player.metrics <- player.metrics %>%
    mutate(Metric = trimws(Metric, "both")) %>% 
    spread(key = Metric, value = round_stat)
  
  
  
  ## SAVE POINT ##
  ## save(player.metrics, file=".\\tmp_010_players_01.Rda") ##
  
  
  
  # Rename columns
  player.metrics <- player.metrics %>% 
    rename(Played_pct = `% Played`
           , Brownlow_votes = `Brownlow Votes`
           , Contested_marks = `Contested Marks`
           , Contested_possessions = `Contested Possessions`
           , Frees_against = `Frees Against`
           , Goal_assists = `Goal Assists`
           , Hit_outs = `Hit Outs`
           , Inside_50s = `Inside 50s`
           , Marks_inside_50 = `Marks Inside 50`
           , One_pct = `One Percenters`
           , Uncontested_possessions = `Uncontested Possessions`)
  
  # Check players who played 0 game time did not participate
  # 147K records:   tmp.df <- player.metrics %>% filter(Played_pct==0)
  # Total sum of 5: sum(tmp.df[,5:27])
  # Explained by player taking the field less than 1% of game time then getting injured
  # Safe to remove all of these records
  # Also remove non-useful stats:
    # Brownlow votes - best & fairest voting not relevant
    # Frees - free kicks received - act of an opponent, not act of a player
    # Rebounds - not meaningful, no way of knowing why rebound occurred (defender skill or attacker mistake)
  
  player.metrics <- player.metrics %>% 
    filter(Played_pct > 0) %>% 
    select(-Brownlow_votes, -Rebounds)
  
  # Adjust round so it can be sorted alphabetically
  player.metrics$round <- sub("R([0-9])$", "R0\\1", player.metrics$round)
  player.metrics$round <- sub("EF", "R25-EF", player.metrics$round, fixed = TRUE)
  player.metrics$round <- sub("QF", "R25-QF", player.metrics$round, fixed = TRUE)
  player.metrics$round <- sub("SF", "R26-SF", player.metrics$round, fixed = TRUE)
  player.metrics$round <- sub("PF", "R27-PF", player.metrics$round, fixed = TRUE)
  player.metrics$round <- sub("GF", "R28-GF", player.metrics$round, fixed = TRUE)
  player.metrics$round <- sub("GF.1", "R29-GF2", player.metrics$round, fixed = TRUE)
  
  player.metrics <- player.metrics %>% arrange(desc(Year), Team, round, Player)
  
  
  ## SAVE POINT ##
  ## save(player.metrics, file=".\\tmp_010_players_02.Rda") ##
  
  
  
  # Shape team results for basic win/loss analysis
  team.results$round <- sub("R([0-9])$", "R0\\1", team.results$round)
  team.results$round <- sub("EF", "R25-EF", team.results$round, fixed = TRUE)
  team.results$round <- sub("QF", "R25-QF", team.results$round, fixed = TRUE)
  team.results$round <- sub("SF", "R26-SF", team.results$round, fixed = TRUE)
  team.results$round <- sub("PF", "R27-PF", team.results$round, fixed = TRUE)
  team.results$round <- sub("GF", "R28-GF", team.results$round, fixed = TRUE)
  team.results$round <- sub("GF.1", "R29-GF2", team.results$round, fixed = TRUE)
  # Check team names are consistent with player data source
  # current.teams.alias <- c("Adelaide","Brisbane","Carlton","Collingwood","Essendon","Fremantle",
  #                          "Geelong","Gold Coast","Greater Western Sydney","Hawthorn","Melbourne","North Melbourne",
  #                          "Port Adelaide","Richmond","St Kilda","Sydney","West Coast","Western Bulldogs")
  # unique(team.results[!team.results$Team %in% current.teams.alias, "Team"])
  team.results$Team <- sub("Brisbane Lions", "Brisbane", team.results$Team, fixed = TRUE)
  team.results$Team <- sub("Kangaroos", "North Melbourne", team.results$Team, fixed = TRUE)
  team.results$vs_opponent <- sub("Brisbane Lions", "Brisbane", team.results$vs_opponent, fixed = TRUE)
  team.results$vs_opponent <- sub("Kangaroos", "North Melbourne", team.results$vs_opponent, fixed = TRUE)
  
  # Remove draws - not meaningful to results
  team.results.y <- team.results %>% 
    filter(team_result != "draw") %>% 
    mutate(finals_game = if_else(grepl("F",round),1,0)) %>% 
    select(Team, Year, round, team_result, finals_game, match_id)
  
  
  
  
  
  
  
#### Feature engineering - combine data in meaningful ways for analysis ####
  # Get a table of means - each metric by year for the whole population
  mean.metrics.by.year <- player.metrics %>% 
    group_by(Year) %>% 
    select(-Player, -Team, -round, -Played_pct) %>% 
    summarise_all(funs(mean)) %>% 
    ungroup
  
  # Team metrics by year, by round
  team.metrics.by.round <- player.metrics %>% 
    group_by(Team, Year, round) %>% 
    select(-Player, -Played_pct) %>% 
    summarise_all(funs(mean)) %>% 
    ungroup
  
  
  
  
  
  
  
  # write.csv(team.metrics.by.round, file = ".\\tempexport_teams.csv")
  # write.csv(mean.metrics.by.year, file = ".\\tempexport_means.csv")
  
  
  
  
  
  
  
  
  
  # Calculate team indices
  team.indices.by.round <- team.metrics.by.round %>% 
    # totals across combined measures
    mutate(sum_close_skill = log10(Contested_possessions + Contested_marks + Clearances)
           , sum_general_skill = log10(Goals/(Goals+Behinds)*10 + Marks/Kicks*10 + Clearances/Hit_outs*10) - log10(Clangers)
           , sum_teamwork = log10(Goal_assists/Goals*10 + Marks_inside_50/Inside_50s*10 + Marks/Uncontested_possessions*10)
           , sum_free_running = log10(Bounces + Handballs/Kicks*10)
           , sum_discipline = log10(Tackles + One_pct)*2 - log10(Frees_against)) %>% 
    select(Team, Year, round, starts_with("sum_")) %>% 
    # bring in overall annual averages to create comparators
    inner_join(mean.metrics.by.year, by=c("Year")) %>% 
    mutate(comp_close_skill = log10(Contested_possessions + Contested_marks + Clearances)
           , comp_general_skill = log10(Goals/(Goals+Behinds)*10 + Marks/Kicks*10 + Clearances/Hit_outs*10) - log10(Clangers)
           , comp_teamwork = log10(Goal_assists/Goals*10 + Marks_inside_50/Inside_50s*10 + Marks/Uncontested_possessions*10)
           , comp_free_running = log10(Bounces + Handballs/Kicks*10)
           , comp_discipline = log10(Tackles + One_pct)*2 - log10(Frees_against)) %>%
    transmute(Team, Year, round
              , idx_close_skill = sum_close_skill/comp_close_skill
              , idx_general_skill = sum_general_skill/comp_general_skill
              , idx_teamwork = sum_teamwork/comp_teamwork
              , idx_free_running = sum_free_running/comp_free_running
              , idx_discipline = sum_discipline/comp_discipline) %>% 
    inner_join(team.results, by=c("Year","Team","round")) %>% 
    arrange(desc(Year), Team, round)
  
  

  
  
#### Shaped data for plotting ####
  
  
  team.indices.by.round.tidy <- team.indices.by.round %>% 
    select(-finals_game, -match_id) %>% 
    gather(key = index_type, value = team_index, -Team, -Year, -round, -team_result) %>% 
    arrange(desc(Year), Team, round, index_type)
  
  team.indices.by.round.tidy_finals <- team.indices.by.round %>% 
    filter(finals_game == 1) %>% 
    select(-finals_game, -match_id) %>% 
    gather(key = index_type, value = team_index, -Team, -Year, -round, -team_result) %>% 
    arrange(desc(Year), Team, round, index_type)
  
  
  team.indices.by.match <- team.indices.by.round %>%
    select(-finals_game, -round) %>%
    group_by(Year, match_id) %>%
    summarise(win_idx_close_skill = max(if_else(team_result == "win", idx_close_skill, -Inf)),
              lose_idx_close_skill = max(if_else(team_result == "loss", idx_close_skill, -Inf)),
              win_idx_discipline = max(if_else(team_result == "win", idx_discipline, -Inf)),
              lose_idx_discipline = max(if_else(team_result == "loss", idx_discipline, -Inf)),
              win_idx_free_running = max(if_else(team_result == "win", idx_free_running, -Inf)),
              lose_idx_free_running = max(if_else(team_result == "loss", idx_free_running, -Inf)),
              win_idx_general_skill = max(if_else(team_result == "win", idx_general_skill, -Inf)),
              lose_idx_general_skill = max(if_else(team_result == "loss", idx_general_skill, -Inf)),
              win_idx_teamwork = max(if_else(team_result == "win", idx_teamwork, -Inf)),
              lose_idx_teamwork = max(if_else(team_result == "loss", idx_teamwork, -Inf))
              ) %>%
    gather(key = index_type, value = team_index, -Year, -match_id) %>%
    extract(index_type, c("res_idx","index_type"), "^(.*)_(idx.*)") %>%
    spread(res_idx, team_index) %>%
    mutate(idx_diff = win - lose) %>% 
    arrange(desc(Year), match_id, index_type)
  
  
  # team.indices.by.match.tidy <- team.indices.by.round %>% 
  #   select(-finals_game, -round) %>%
  #   group_by(Year, match_id) %>%
  #   summarise(win_idx_close_skill = max(if_else(team_result == "win", idx_close_skill, -Inf)),
  #             lose_idx_close_skill = max(if_else(team_result == "loss", idx_close_skill, -Inf)),
  #             win_idx_discipline = max(if_else(team_result == "win", idx_discipline, -Inf)),
  #             lose_idx_discipline = max(if_else(team_result == "loss", idx_discipline, -Inf)),
  #             win_idx_free_running = max(if_else(team_result == "win", idx_free_running, -Inf)),
  #             lose_idx_free_running = max(if_else(team_result == "loss", idx_free_running, -Inf)),
  #             win_idx_general_skill = max(if_else(team_result == "win", idx_general_skill, -Inf)),
  #             lose_idx_general_skill = max(if_else(team_result == "loss", idx_general_skill, -Inf)),
  #             win_idx_teamwork = max(if_else(team_result == "win", idx_teamwork, -Inf)),
  #             lose_idx_teamwork = max(if_else(team_result == "loss", idx_teamwork, -Inf))
  #   ) %>%
  #   gather(key = index_type, value = team_index, -Year, -match_id) %>% 
  #   extract(index_type, c("team_result","index_type"), "^(.*)_(idx.*)") %>%  
  #   arrange(desc(Year), match_id, index_type)
  
  
  # player data
  save(player.metrics, file=".\\player_metrics.Rda")
  # season averages
  save(mean.metrics.by.year, file=".\\mean_metrics_by_year.Rda")
  # team results
  save(team.results, file=".\\team_results.Rda")
  # match-wise view of indices
  save(team.indices.by.match, file=".\\team_indices_by_match.Rda")
  # team-wise view of indices
  save(team.indices.by.round.tidy, file=".\\team_indices_by_round_tidy.Rda")
  # team-wise view of indices (wide format)
  save(team.metrics.by.round, file=".\\team_metrics_by_round.Rda")
  
  
  
  
  
  
#### Plots ####
  
  
  # Index names for plots
  idx_names <- c(
    idx_close_skill = "Close skill index",
    idx_discipline = "Discipline & effort index",
    idx_free_running = "Free movement index",
    idx_general_skill = "General skill index",
    idx_teamwork = "Teamwork index"
  )
  
  
  
  
  
  # Summary plot, all indices by year
    # All games
    ggplot(team.indices.by.round.tidy, aes(x = factor(Year), y = team_index, col=team_result)) +
      geom_point(alpha = 0.2, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8)) +
      facet_grid(. ~ index_type, labeller = as_labeller(idx_names)) +
      labs(title = "Summary: all indices by year"
           , x = "Year"
           , y = "Team index vs season avg"
           , col = "Result") +
      theme(axis.text.x = element_text(angle = 90))
    
    # Finals only
    ggplot(team.indices.by.round.tidy_finals, aes(x = factor(Year), y = team_index, col=team_result)) +
      geom_point(alpha = 0.2, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8)) +
      facet_grid(. ~ index_type, labeller = as_labeller(idx_names)) +
      labs(title = "Summary: all indices by year, Finals only"
           , x = "Year"
           , y = "Team index vs season avg"
           , col = "Result") +
      theme(axis.text.x = element_text(angle = 90))
    
    # winners vs losers
    ggplot(team.indices.by.match, aes(x = factor(Year), y = idx_diff, col = idx_diff>0)) +
      geom_jitter(width = 0.2, alpha = .2) +
      facet_grid(. ~ index_type, labeller = as_labeller(idx_names)) +
      labs(title = "Summary: differences in winning and losing indices"
           , x = "Year"
           , y = "Winning index vs losing index") +
      theme(axis.text.x = element_text(angle = 90),
            legend.position = "none")

    
  
  # Individual index plots by year
    # Close skill
    ggplot(team.indices.by.round, aes(x = factor(team_result), y = idx_close_skill, col=team_result)) +
      geom_point(alpha = 0.2, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8)) +
      facet_grid(. ~ factor(Year))+
      labs(title = "Index: Close skill by year"
           , y = "Team index vs season avg"
           , col = "Result") +
      theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.x = element_blank())
    
    # Discipline & Effort
    ggplot(team.indices.by.round, aes(x = factor(team_result), y = idx_discipline, col=team_result)) +
      geom_point(alpha = 0.2, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8)) +
      facet_grid(. ~ factor(Year))+
      labs(title = "Index: Discipline & effort by year"
           , y = "Team index vs season avg"
           , col = "Result") +
      theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.x = element_blank())
  
    # Free movement
    ggplot(team.indices.by.round, aes(x = factor(team_result), y = idx_free_running, col=team_result)) +
      geom_point(alpha = 0.2, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8)) +
      facet_grid(. ~ factor(Year))+
      labs(title = "Index: Free movement by year"
           , y = "Team index vs season avg"
           , col = "Result") +
      theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.x = element_blank())
    
    # General skill
    ggplot(team.indices.by.round, aes(x = factor(team_result), y = idx_general_skill, col=team_result)) +
      geom_point(alpha = 0.2, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8)) +
      facet_grid(. ~ factor(Year))+
      labs(title = "Index: General skill by year"
           , y = "Team index vs season avg"
           , col = "Result") +
      theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.x = element_blank())
    
    # Teamwork
    ggplot(team.indices.by.round, aes(x = factor(team_result), y = idx_teamwork, col=team_result)) +
      geom_point(alpha = 0.2, position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8)) +
      facet_grid(. ~ factor(Year))+
      labs(title = "Index: Teamwork by year"
           , y = "Team index vs season avg"
           , col = "Result") +
      theme(axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.x = element_blank())