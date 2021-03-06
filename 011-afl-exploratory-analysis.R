library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)



load(file=".\\player_metrics.Rda")
load(file=".\\team_results.Rda")


# Combine into one data frame
AFL.by.player <- merge(player.metrics, team.results, by.x = c("Year","round","Team"), by.y = c("Year","round","Team")) %>% 
  select(-score_str, -rownum, -(Q1score_cumul:ETscore)) %>% 
  mutate(team_result = as.factor(if_else(team_result == "win", 1, 0)),
         Q1_win = as.factor(Q1_win), Q2_win = as.factor(Q2_win), Q3_win = as.factor(Q3_win), Q4_win = as.factor(Q4_win),
         ET_win = as.factor(ET_win), match_score_idx = final_score/(final_score - final_margin)) %>% 
  select(-final_score, -final_margin)
# rm("player.metrics")
# rm("team.metrics")
save(AFL.by.player, file=".\\afl_by_player.Rda")


# Aggregate to team level
AFL.by.team <- AFL.by.player %>% 
  group_by(Year, round, Team, home_away, match_id, vs_opponent, team_result, Q1_win, Q2_win, Q3_win, Q4_win, ET_win, match_score_idx) %>% 
  select(-Player, -Played_pct) %>% 
  summarise_all(funs(sum)) %>% 
  ungroup

save(AFL.by.team, file=".\\afl_by_team.Rda")




# Statistical analysis of raw metrics: CI of difference of means (wins vs not wins)

# Behinds
diff_distr = AFL.by.team %>% group_by(team_result) %>% select(Behinds) %>% summarise_all(.funs = c("mean", "sd"))
diff_mean = as.numeric(diff_distr[2,"mean"]-diff_distr[1,"mean"])
diff_sd = as.numeric(sqrt(diff_distr[2,"sd"]^2 + diff_distr[1,"sd"]^2))
# one tail test: what is z-score
diff_mean/diff_sd #0.41 : 65.9%

# Bounces
diff_distr = AFL.by.team %>% group_by(team_result) %>% select(Bounces) %>% summarise_all(.funs = c("mean", "sd"))
diff_mean = as.numeric(diff_distr[2,"mean"]-diff_distr[1,"mean"])
diff_sd = as.numeric(sqrt(diff_distr[2,"sd"]^2 + diff_distr[1,"sd"]^2))
diff_mean/diff_sd #0.18 : 57.1%

# Clangers (REVERSED)
diff_distr = AFL.by.team %>% group_by(team_result) %>% select(Clangers) %>% summarise_all(.funs = c("mean", "sd"))
diff_mean = as.numeric(diff_distr[1,"mean"]-diff_distr[2,"mean"])
diff_sd = as.numeric(sqrt(diff_distr[1,"sd"]^2 + diff_distr[2,"sd"]^2))
diff_mean/diff_sd #0.22 : 58.7%

# Clearances
diff_distr = AFL.by.team %>% group_by(team_result) %>% select(Clearances) %>% summarise_all(.funs = c("mean", "sd"))
diff_mean = as.numeric(diff_distr[2,"mean"]-diff_distr[1,"mean"])
diff_sd = as.numeric(sqrt(diff_distr[2,"sd"]^2 + diff_distr[1,"sd"]^2))
diff_mean/diff_sd #0.25 : 59.9%

# Contested_marks
diff_distr = AFL.by.team %>% group_by(team_result) %>% select(Contested_marks) %>% summarise_all(.funs = c("mean", "sd"))
diff_mean = as.numeric(diff_distr[2,"mean"]-diff_distr[1,"mean"])
diff_sd = as.numeric(sqrt(diff_distr[2,"sd"]^2 + diff_distr[1,"sd"]^2))
diff_mean/diff_sd #0.31 : 62.2%

# Contested_possessions
diff_distr = AFL.by.team %>% group_by(team_result) %>% select(Contested_possessions) %>% summarise_all(.funs = c("mean", "sd"))
diff_mean = as.numeric(diff_distr[2,"mean"]-diff_distr[1,"mean"])
diff_sd = as.numeric(sqrt(diff_distr[2,"sd"]^2 + diff_distr[1,"sd"]^2))
diff_mean/diff_sd #0.33 : 62.9%

# Disposals
diff_distr = AFL.by.team %>% group_by(team_result) %>% select(Disposals) %>% summarise_all(.funs = c("mean", "sd"))
diff_mean = as.numeric(diff_distr[2,"mean"]-diff_distr[1,"mean"])
diff_sd = as.numeric(sqrt(diff_distr[2,"sd"]^2 + diff_distr[1,"sd"]^2))
diff_mean/diff_sd #0.52 : 69.9%

# Frees
diff_distr = AFL.by.team %>% group_by(team_result) %>% select(Frees) %>% summarise_all(.funs = c("mean", "sd"))
diff_mean = as.numeric(diff_distr[2,"mean"]-diff_distr[1,"mean"])
diff_sd = as.numeric(sqrt(diff_distr[2,"sd"]^2 + diff_distr[1,"sd"]^2))
diff_mean/diff_sd #0.06 : 52.4%

# Frees_against (REVERSED)
diff_distr = AFL.by.team %>% group_by(team_result) %>% select(Frees_against) %>% summarise_all(.funs = c("mean", "sd"))
diff_mean = as.numeric(diff_distr[1,"mean"]-diff_distr[2,"mean"])
diff_sd = as.numeric(sqrt(diff_distr[1,"sd"]^2 + diff_distr[2,"sd"]^2))
diff_mean/diff_sd #0.05 : 52.0%

# Goal_assists
diff_distr = AFL.by.team %>% group_by(team_result) %>% select(Goal_assists) %>% summarise_all(.funs = c("mean", "sd"))
diff_mean = as.numeric(diff_distr[2,"mean"]-diff_distr[1,"mean"])
diff_sd = as.numeric(sqrt(diff_distr[2,"sd"]^2 + diff_distr[1,"sd"]^2))
diff_mean/diff_sd #0.89 : 81.3%

# Goals
diff_distr = AFL.by.team %>% group_by(team_result) %>% select(Goals) %>% summarise_all(.funs = c("mean", "sd"))
diff_mean = as.numeric(diff_distr[2,"mean"]-diff_distr[1,"mean"])
diff_sd = as.numeric(sqrt(diff_distr[2,"sd"]^2 + diff_distr[1,"sd"]^2))
diff_mean/diff_sd #1.13 : 87.1%

# Handballs
diff_distr = AFL.by.team %>% group_by(team_result) %>% select(Handballs) %>% summarise_all(.funs = c("mean", "sd"))
diff_mean = as.numeric(diff_distr[2,"mean"]-diff_distr[1,"mean"])
diff_sd = as.numeric(sqrt(diff_distr[2,"sd"]^2 + diff_distr[1,"sd"]^2))
diff_mean/diff_sd #0.19 : 57.5%

# Hit_outs
diff_distr = AFL.by.team %>% group_by(team_result) %>% select(Hit_outs) %>% summarise_all(.funs = c("mean", "sd"))
diff_mean = as.numeric(diff_distr[2,"mean"]-diff_distr[1,"mean"])
diff_sd = as.numeric(sqrt(diff_distr[2,"sd"]^2 + diff_distr[1,"sd"]^2))
diff_mean/diff_sd #0.09 : 53.6%

# Inside_50s
diff_distr = AFL.by.team %>% group_by(team_result) %>% select(Inside_50s) %>% summarise_all(.funs = c("mean", "sd"))
diff_mean = as.numeric(diff_distr[2,"mean"]-diff_distr[1,"mean"])
diff_sd = as.numeric(sqrt(diff_distr[2,"sd"]^2 + diff_distr[1,"sd"]^2))
diff_mean/diff_sd #0.77 : 77.9%

# Kicks
diff_distr = AFL.by.team %>% group_by(team_result) %>% select(Kicks) %>% summarise_all(.funs = c("mean", "sd"))
diff_mean = as.numeric(diff_distr[2,"mean"]-diff_distr[1,"mean"])
diff_sd = as.numeric(sqrt(diff_distr[2,"sd"]^2 + diff_distr[1,"sd"]^2))
diff_mean/diff_sd #0.81 : 79.1%

# Marks
diff_distr = AFL.by.team %>% group_by(team_result) %>% select(Marks) %>% summarise_all(.funs = c("mean", "sd"))
diff_mean = as.numeric(diff_distr[2,"mean"]-diff_distr[1,"mean"])
diff_sd = as.numeric(sqrt(diff_distr[2,"sd"]^2 + diff_distr[1,"sd"]^2))
diff_mean/diff_sd #0.47 : 68.1%

# Marks_inside_50
diff_distr = AFL.by.team %>% group_by(team_result) %>% select(Marks_inside_50) %>% summarise_all(.funs = c("mean", "sd"))
diff_mean = as.numeric(diff_distr[2,"mean"]-diff_distr[1,"mean"])
diff_sd = as.numeric(sqrt(diff_distr[2,"sd"]^2 + diff_distr[1,"sd"]^2))
diff_mean/diff_sd #0.71 : 76.1%

# One_pct
diff_distr = AFL.by.team %>% group_by(team_result) %>% select(One_pct) %>% summarise_all(.funs = c("mean", "sd"))
diff_mean = as.numeric(diff_distr[2,"mean"]-diff_distr[1,"mean"])
diff_sd = as.numeric(sqrt(diff_distr[2,"sd"]^2 + diff_distr[1,"sd"]^2))
diff_mean/diff_sd #0.08 : 53.2%

# Tackles
diff_distr = AFL.by.team %>% group_by(team_result) %>% select(Tackles) %>% summarise_all(.funs = c("mean", "sd"))
diff_mean = as.numeric(diff_distr[2,"mean"]-diff_distr[1,"mean"])
diff_sd = as.numeric(sqrt(diff_distr[2,"sd"]^2 + diff_distr[1,"sd"]^2))
diff_mean/diff_sd #0.09 : 53.6%

# Uncontested_possessions
diff_distr = AFL.by.team %>% group_by(team_result) %>% select(Uncontested_possessions) %>% summarise_all(.funs = c("mean", "sd"))
diff_mean = as.numeric(diff_distr[2,"mean"]-diff_distr[1,"mean"])
diff_sd = as.numeric(sqrt(diff_distr[2,"sd"]^2 + diff_distr[1,"sd"]^2))
diff_mean/diff_sd #0.41 : 65.9%














# Match-wise (adversarial) view of results
AFL.by.team.sub <- AFL.by.team %>% select(-Disposals, -Frees, -Hit_outs)
AFL.by.match <- merge(AFL.by.team.sub, AFL.by.team.sub %>% rename (Team.opp = Team), 
                      by.x = c("Year","round","match_id","Team"), by.y = c("Year","round","match_id","vs_opponent"),
                      suffixes = c("",".opp")) %>% select(-vs_opponent)

save(AFL.by.match, file=".\\afl_by_match.Rda")


# Match-wise indices
AFL.by.match.idx <- AFL.by.match %>% filter(match_score_idx != 1) %>% 
  transmute(Year, round, match_id, Team, home_away, team_result, Q1_win, Q2_win, Q3_win, Q4_win, ET_win, match_score_idx, Team.opp, match_score_idx.opp,
            idx_win_ground_ball = Contested_possessions/Contested_possessions.opp, idx_win_aerial_ball = Contested_marks/Contested_marks.opp,
            idx_clear_ball = Clearances/Clearances.opp, idx_less_clangers = Clangers.opp/Clangers,
            idx_goal_assist = (Goal_assists/Goals)/(Goal_assists.opp/Goals.opp), idx_mark_kick = (Marks/Kicks)/(Marks.opp/Kicks.opp),
            idx_50m_entry = (Marks_inside_50/Inside_50s)/(Marks_inside_50.opp/Inside_50s.opp),
            idx_tackle = Tackles/Tackles.opp, idx_one_pct = One_pct/One_pct.opp, idx_less_frees = Frees_against.opp/Frees_against ) %>% 
  # produces three infinite values (div by 0)
  mutate(idx_goal_assist = if_else(idx_goal_assist == Inf, 1, if_else(idx_goal_assist == 0, 0.01, idx_goal_assist)),
         idx_win_aerial_ball = if_else(idx_win_aerial_ball == Inf, 1, if_else(idx_win_aerial_ball == 0, 0.01, idx_win_aerial_ball)),
  # take log10 to standardise the values
         idx_win_ground_ball = log10(idx_win_ground_ball), idx_win_aerial_ball = log10(idx_win_aerial_ball), idx_clear_ball = log10(idx_clear_ball),
         idx_less_clangers = log10(idx_less_clangers), idx_goal_assist = log10(idx_goal_assist), idx_mark_kick = log10(idx_mark_kick),
         idx_50m_entry = log10(idx_50m_entry), idx_tackle = log10(idx_tackle), idx_one_pct = log10(idx_one_pct), idx_less_frees = log10(idx_less_frees))
         
save(AFL.by.match.idx, file=".\\afl_by_match_idx.Rda")



AFL.by.match.idx.mosaic <- AFL.by.match.idx %>% 
  select(match_id, Team, team_result, starts_with("idx_")) %>% 
  mutate(team_result = factor(team_result, labels = c("Loss", "Win")))
  

library(reshape2)
ggplot_mosaic <- function(in_df, X, fillvar, in_cuts, plot_title){
  # Prepare table data from supplied parameters
  xvar <- cut(in_df[[X]], in_cuts)
  mos_df <- as.data.frame.matrix(table(xvar, in_df[[fillvar]]))
  # Calculate the x and y coordinates of the rectangle extremes of each segment
    # start with xmax and xmin    
    mos_df$groupSum <- rowSums(mos_df)
    mos_df$xmax <- cumsum(mos_df$groupSum)
    mos_df$xmin <- mos_df$xmax - mos_df$groupSum
    mos_df$groupSum <- NULL
    # Use default row names in variable X so they can be referenced, reshape into long format
    mos_df$xvar <- row.names(mos_df)
    mos_df_melted <- melt(mos_df, id = c("xvar", "xmin", "xmax"), variable.name = "fillvar")
    # Calculate ymax and ymin
    mos_df_melted <- mos_df_melted %>% group_by(xvar) %>% 
      mutate(ymax = cumsum(value/sum(value)), ymin = ymax - value/sum(value))
  # Perform Chi-squared test
    mos_df_chsq <- chisq.test(table(in_df[[fillvar]], xvar))
    # Reshape into long format and identify variables consistent with mos_df_melted
    mos_df_chsq_res <- melt(mos_df_chsq$residuals)
    names(mos_df_chsq_res) <- c("fillvar", "xvar", "residual")
  
  # Merge data
  mos_df_all <- merge(mos_df_melted, mos_df_chsq_res)
  # Positions for labels
  # x axis: halfway between xmax and xmin of each segment, y axis: one label per FILL group at the right (max)
  mos_df_all$xtext <- mos_df_all$xmin + (mos_df_all$xmax - mos_df_all$xmin)/2
  index <- mos_df_all$xmax == max(mos_df_all$xmax)
  mos_df_all$ytext <- mos_df_all$ymin[index] + (mos_df_all$ymax[index] - mos_df_all$ymin[index])/2
  
  # Create and return mosaic plot, mapped to x and y coordinates, fill = residuals
  gmos <- ggplot(mos_df_all, aes(ymin = ymin,  ymax = ymax, xmin = xmin,
                          xmax = xmax, fill = residual)) +
    # Add white border on rectangles to make them easier to distinguish
    geom_rect(col = "white") +
    # Add text to: x axis - effectively tick marks for each X; y axis - one for each FILL
    geom_text(aes(x = xtext, label = xvar), y = 1, size = 3, angle = 90, hjust = 1, show.legend = FALSE) +
    geom_text(aes(x = max(xmax),  y = ytext, label = fillvar), size = 3, hjust = 1, show.legend = FALSE) +
    # Fill gradient for residuals per aes mapping (with default 2-color scale), label legend as "Residuals"
    scale_fill_gradient2("Residuals", low = "red", high = "blue") +
    # Place data adjacent to axes (no gap), other appearance changes
    scale_x_continuous("Team results", expand = c(0,0)) +
    scale_y_continuous("Proportion", expand = c(0,0)) +
    labs(title = plot_title) +
    theme(legend.position = "bottom", legend.text = element_text(size = 6), legend.title = element_text(size = 7),
          plot.title = element_text(size = 9), axis.title = element_text(size = 7),
          axis.text = element_text(size = 6))
  
  print(gmos)
}

ggplot_mosaic(AFL.by.match.idx.mosaic, "idx_win_ground_ball", "team_result", 30, "Index: Win ground ball")



ggplot_hist <- function(in_df, xvar, in_width, plot_title){
  ggph <- ggplot(in_df, aes(x=in_df[[xvar]], fill=team_result)) +
            geom_histogram(binwidth = in_width, position = position_dodge(in_width), alpha=0.9, col="white") +
            scale_fill_manual(labels = c("Loss","Win"), values = c("red","blue")) +
            labs(title = plot_title, color = "Result") +
            theme(axis.title.x = element_blank(), axis.title.y = element_blank()
                  , axis.text.x = element_text(size = 6), axis.text.y = element_text(size = 6), legend.position = "none"
                  , plot.title = element_text(size = 9))
  print(ggph) }

ggplot_hist(AFL.by.match.idx, "idx_win_ground_ball", 0.02, "blah")





# margin by home/away
ggplot(AFL.by.match.idx, aes(x = factor(home_away), y = match_score_idx)) +
  geom_jitter() +   geom_hline(yintercept = 1, size = 0.8, col = "red")






# tidy
AFL.by.match.idx.tidy <- AFL.by.match.idx %>% select(-ends_with("_win"), -Team.opp, -match_score_idx.opp, -home_away) %>% 
  gather(key, value, -Year, -round, -match_id, -Team, -team_result, -match_score_idx)

# idx by score margin
ggplot(AFL.by.match.idx.tidy, aes(x = value, y = match_score_idx)) +
  geom_point(position = position_jitter(width = 0.01), alpha = 0.6, shape = 1) +
  geom_hline(yintercept = 1, size = 0.6, col = "blue") +
  geom_vline(xintercept = 0, size = 0.6, col = "blue") +
  geom_rug(sides = "r", alpha = 0.1, size = 0.05) +
  scale_x_continuous(limits = c(-1, 1)) +
  facet_wrap(~ key, nrow = 2) +
  theme(legend.position = "none")
  



# Match-wise indices 2: combined indices
# removed: idx_goal_assist
AFL.by.match.idx.2 <- AFL.by.match.idx %>% mutate(round = paste(Year, round, sep = "-")) %>% 
  select(-Year, -match_id, -home_away, -ET_win, -ends_with(".opp")) %>% 
  arrange(Team, round) %>%
  transmute(Team, round, Q1_win, Q2_win, Q3_win, Q4_win, team_result, match_score_idx,
            idx_win_the_ball = 1 + 100 * idx_win_ground_ball * idx_win_aerial_ball * idx_clear_ball, 
            idx_use_the_ball = 1 + 100 * idx_less_clangers * idx_mark_kick * idx_50m_entry,
            idx_discipline = 1 + 100 * idx_tackle * idx_one_pct * idx_less_frees)

cor( AFL.by.match.idx.2 %>% select(-Team, -round) %>%
    mutate(team_result = as.numeric(team_result),
           Q1_win = as.numeric(Q1_win), Q2_win = as.numeric(Q2_win), Q3_win = as.numeric(Q3_win), Q4_win = as.numeric(Q4_win) ) )

AFL.by.match.idx.2.tidy <- AFL.by.match.idx.2 %>%
  gather(key, value, -round, -Team, -team_result) %>% arrange(Team, round, key)

ggplot(AFL.by.match.idx.2.tidy %>% filter(Team %in% c("St Kilda","Sydney") & substr(round, 1, 4) %in% c("2016","2017") ), aes(x = round, y = value)) +
  geom_line(aes(group = key, col = key)) +
  facet_wrap(~ Team, nrow = 3)
  





  
transmute(round, match_id, home_away, team_result, Q1_win, Q2_win, Q3_win, Q4_win, ET_win, match_score_idx, Team.opp, match_score_idx.opp,
  mutate(idx_win_the_ball.3 = 100*rollmean(idx_win_the_ball, 3, align = "right", fill = NA),
         idx_use_the_ball.3 = 100*rollmean(idx_use_the_ball, 3, align = "right", fill = NA),
         idx_discipline.3 = 100*rollmean(idx_discipline, 3, align = "right", fill = NA))

AFL.by.match.idx.2.tidy <- AFL.by.match.idx.2 %>%
  gather(key, value, -Year, -round, -match_id, -Team, -team_result, -match_score_idx, -ends_with("_win"), -Team.opp, -match_score_idx.opp, -home_away)

# idx 2 by score margin
ggplot(AFL.by.match.idx.2.tidy, aes(x = value, y = match_score_idx)) +
  geom_point(col = "black", fill = "black", alpha = 0.2, shape = 19) +
  geom_hline(yintercept = 1, size = 0.6, col = "blue") +
  geom_vline(xintercept = 0, size = 0.6, col = "blue") +
  geom_rug(sides = "r", alpha = 0.1, size = 0.01) +
  scale_x_continuous(limits = c(-0.02, 0.02)) +
  scale_y_continuous(limits = c(0, 5)) +
  facet_grid(. ~ key) +
  theme(legend.position = "none")





pos.d <- position_dodge(width = 0.3)

# stat function to find range of plot
plot_range <- function(x){ data.frame(ymin = min(x), ymax = max(x)) }

# stat function to find median and interquartile range of plot
plot_med_IQR <- function(x){ data.frame(y = median(x), ymin = quantile(x, probs=0.25), ymax = quantile(x, probs=0.75)) }

# template line range plot, dummy variable for x, parameter for y, split out wins/losses and facet by year
# template line range plot, dummy variable for x, parameter for y, split out wins/losses and facet by year
plot_linerange_by_year <- function(in_df, yvar, plot_title, plot_caption){
  ggp <- ggplot(in_df, aes_string(x=1, y=yvar, col="team_result")) +
    stat_summary(geom = "linerange", fun.data = plot_med_IQR, position = pos.d, size = 1.2, alpha = 0.6) +
    stat_summary(geom = "linerange", fun.data = plot_range, position = pos.d, size = 1.2, alpha = 0.3) +
    scale_colour_manual(labels = c("Loss or draw","Win"), values = c("red","blue")) +
    stat_summary(geom = "point", fun.y = median, position = pos.d, size = 1.5, shape = 15) +
    facet_grid(. ~ Year) +
    labs(title = plot_title, color = "Result", caption = plot_caption) +
    theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()
          , axis.title.y = element_blank(), axis.text.y = element_text(size = 6), legend.position = "none"
          , strip.text.x = element_text(size = 7, angle = 90)
          , plot.title = element_text(size = 9), plot.caption = element_text(size = 7))
  print(ggp)
}

plot_linerange_by_year(AFL.by.player, "Played_pct","Player % game time by year, showing median, IQR and range")




ggplot(AFL.by.team, aes_string(x=1, y="Behinds", col="team_result")) +
  stat_summary(geom = "linerange", fun.data = plot_med_IQR, position = pos.d, size = 1.2, alpha = 0.6) +
  stat_summary(geom = "linerange", fun.data = plot_range, position = pos.d, size = 1.2, alpha = 0.3) +
  stat_summary_bin(geom = "point", fun.y = cor, position = pos.d, size=2.5) +
  scale_colour_manual(labels = c("Loss or draw","Win"), values = c("red","blue")) +
  stat_summary(geom = "point", fun.y = median, position = pos.d, size = 1.5, shape = 15) +
  facet_grid(. ~ Year) +
  labs(title = "Behinds", color = "Result") +
  theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), axis.text.x = element_blank()
        , axis.title.y = element_blank(), axis.text.y = element_text(size = 6), legend.position = "none"
        , strip.text.x = element_text(size = 7, angle = 90)
        , plot.title = element_text(size = 9), plot.caption = element_text(size = 7))
