library(dplyr)
library(tidyr)
library(ggplot2)
#library(zoo)
library(reshape2)
library(data.table)

AFL.by.match.idx.1 <- AFL.by.match.idx %>% select(-home_away, -ends_with("_win"), -ends_with(".opp"))
cormat <- round(cor(AFL.by.match.idx.1 %>% select(-Year, -match_id, -Team, -round, -match_score_idx) %>% mutate(team_result = as.numeric(team_result))), 2)
cormat[lower.tri(cormat)]<- NA
cormat_melted <- melt(cormat, na.rm = TRUE)
ggplot(data = cormat_melted, aes(Var2, Var1, fill = value)) + geom_tile(color = "white") +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8), axis.text.y = element_text(size = 8), axis.title = element_blank()) +
  coord_fixed()



AFL.by.match.idx.1.tidy <- AFL.by.match.idx.1 %>% arrange(match_id) %>% 
  select(-round, -Team, -match_score_idx) %>% filter(team_result == 1) %>% gather(key, value, -Year, -match_id, -team_result)


ggplot(AFL.by.match.idx.1.tidy %>% filter(!(key %in% c("idx_50m_entry", "idx_goal_assist", "idx_less_frees", "idx_win_aerial_ball"))), aes(x = match_id, y = value)) +
  geom_line(aes(group = key), alpha = 0.6) +
  facet_wrap(~ key, nrow = 3) +
  geom_hline(yintercept = 0, size = 1, col = "blue") 
  
ggplot(AFL.by.match.idx.1.tidy %>% filter((key %in% c("idx_50m_entry", "idx_goal_assist", "idx_less_frees", "idx_win_aerial_ball"))), aes(x = match_id, y = value)) +
  geom_line(aes(group = key), alpha = 0.6) +
  facet_wrap(~ key, nrow = 2) +
  geom_hline(yintercept = 0, size = 1, col = "blue") 






# deep-dive: player influence

# idx_win_ground_ball = Contested_possessions/Contested_possessions.opp
# idx_win_aerial_ball = Contested_marks/Contested_marks.opp
# idx_clear_ball = Clearances/Clearances.opp
# idx_less_clangers = Clangers.opp/Clangers
# idx_50m_entry = (Marks_inside_50/Inside_50s)/(Marks_inside_50.opp/Inside_50s.opp),
# idx_less_frees = Frees_against.opp/Frees_against

AFL.by.player.disagg <- semi_join(AFL.by.player, AFL.by.match.idx.1, by = c("Team", "Year", "round", "match_id", "team_result")) %>% 
  select(match_id, Year:Team, Clangers:Contested_possessions, Frees_against, Inside_50s, Marks_inside_50)

var.top.n <- function(matchid, team.nm, pvar, n = 10){
  DF <- AFL.by.player.disagg %>% select(match_id, Team, pvar) %>% filter(match_id == matchid) %>% top_n(n)
  prop.top.10 <- (DF %>% filter(Team == team.nm) %>% count)/(nrow(DF))
  return(prop.top.10)
}

# AFL.by.player.agg <- AFL.by.match.idx.1 %>% ungroup
# setDT(AFL.by.player.agg)[ , Clangers := var.top.n(match_id, Team, "Clangers", 10), by=c("match_id","Year","round","Team")]
# setDT(AFL.by.player.agg)[ , Clearances := var.top.n(match_id, Team, "Clearances", 10), by=c("match_id","Year","round","Team")]
# setDT(AFL.by.player.agg)[ , Contested_marks := var.top.n(match_id, Team, "Contested_marks", 10), by=c("match_id","Year","round","Team")]
# setDT(AFL.by.player.agg)[ , Contested_possessions := var.top.n(match_id, Team, "Contested_possessions", 10), by=c("match_id","Year","round","Team")]
# setDT(AFL.by.player.agg)[ , Frees_against := var.top.n(match_id, Team, "Frees_against", 10), by=c("match_id","Year","round","Team")]
# setDT(AFL.by.player.agg)[ , Inside_50s := var.top.n(match_id, Team, "Inside_50s", 10), by=c("match_id","Year","round","Team")]
# setDT(AFL.by.player.agg)[ , Marks_inside_50 := var.top.n(match_id, Team, "Marks_inside_50", 10), by=c("match_id","Year","round","Team")]

#save(AFL.by.player.agg, file = "AFL_by_player_agg.Rda")



# ML
library(rpart)
library(rpart.plot)
#library(caTools)
library(ROCR)
library(caret)
library(e1071)

# split out 2017 season data
train1 = subset(AFL.by.player.agg, AFL.by.player.agg$Year != 2017)
test1 = subset(AFL.by.player.agg, AFL.by.player.agg$Year == 2017)


# decision tree
  tree1 = rpart(team_result ~ idx_win_ground_ball + idx_win_aerial_ball + idx_clear_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Clangers + Clearances + Contested_marks + Contested_possessions + Frees_against + Inside_50s + Marks_inside_50,
                data = train1, method = "class", control = rpart.control(minbucket=25))
  
  # only keep variables shown on tree
  tree1_V2 = rpart(team_result ~ idx_win_ground_ball + idx_50m_entry + Inside_50s,
                   data = train1, method = "class", control = rpart.control(minbucket=25))
  prp(tree1_V2, extra = 1, fallen.leaves = TRUE, varlen = 0)
  pred1 = predict(tree1, newdata = test1, type = "class")
  table(test1$team_result, pred1)
  (159+154)/408 #0.77
  
  pred1ROC = predict(tree1, newdata = test1)
  pred1R = prediction(pred1ROC[ ,2], test1$team_result)
  perf1R = performance(pred1R, "tpr", "fpr")
  par(cex = 0.7)
  plot(perf1R, col="blue", main="ROC Curve for Decision Tree - Basic", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
  text(0.8, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred1R, "auc")@y.values),2),sep=""), adj=1, cex = 1.5)
  text(0.8, 0.3, labels = paste("Class accuracy = ", round((159+154)/408,2),sep=""), adj=1, cex = 1.5)
  # AUC
  as.numeric(performance(pred1R, "auc")@y.values) #0.81
  
  
# decision tree - cross-validated
  fitControl = trainControl(method = "cv", number = 10)
  cpGrid = expand.grid(.cp=(1:50)*0.0001)
  train(team_result ~ idx_win_ground_ball + idx_win_aerial_ball + idx_clear_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Clangers + Clearances + Contested_marks + Contested_possessions + Frees_against + Inside_50s + Marks_inside_50,
        data = train1, method = "rpart", trControl = fitControl, tuneGrid = cpGrid) #0.002
  # adjusted cp due to too many branches
  tree2 = rpart(team_result ~ idx_win_ground_ball + idx_win_aerial_ball + idx_clear_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Clangers + Clearances + Contested_marks + Contested_possessions + Frees_against + Inside_50s + Marks_inside_50,
                data = train1, method = "class", control = rpart.control(cp = 0.005))
    # only keep variables shown on tree
    tree2_V2 = rpart(team_result ~ idx_win_ground_ball + idx_less_clangers + idx_50m_entry + Inside_50s,
                  data = train1, method = "class", control = rpart.control(cp = 0.005))
    
    prp(tree2_V2, extra = 1, fallen.leaves = TRUE, varlen = 0)
    pred2 = predict(tree2_V2, newdata = test1, type = "class")
    table(test1$team_result, pred2)
    (168+148)/408 #0.77
    
    pred2ROC = predict(tree2_V2, newdata = test1)
    pred2R = prediction(pred2ROC[ ,2], test1$team_result)
    perf2R = performance(pred2R, "tpr", "fpr")
    plot(perf2R)
    # AUC
    as.numeric(performance(pred2R, "auc")@y.values) #0.83
    
    # plot(perf2R, print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
    # table(test1$team_result, pred2ROC[,2]>0.6)
    
  
# logistic regression
  # removed: Contested_marks, Clearances, Contested_possessions, Clangers, Marks_inside_50, Frees_against, idx_clear_ball
  # removed strong multicollinearity: 
  # now all variables are significant predictors
  
  # https://amunategui.github.io/variable-importance-shuffler/
  # bonus - great package for fast variable importance
  
  
  log3a = glm(team_result ~ Contested_marks + Clearances + Contested_possessions + Clangers + idx_win_aerial_ball + Marks_inside_50 + idx_less_frees + idx_win_ground_ball + idx_clear_ball + idx_less_clangers + idx_50m_entry + Frees_against + Inside_50s,
             data = train1, family = binomial)
  
  log3 = glm(team_result ~ idx_win_aerial_ball + idx_less_frees + idx_win_ground_ball + idx_less_clangers + idx_50m_entry + Inside_50s,
             data = train1, family = binomial)
  
  
  train1num <- train1 %>% transmute(team_result = as.numeric(team_result), idx_win_aerial_ball, idx_less_frees, idx_win_ground_ball, idx_less_clangers, idx_50m_entry, Inside_50s)
  cor(train1num)
  
  summary(log3)
    # training accuracy
    pred3tr = predict(log3, type = "response")
    table(train1$team_result, pred3tr >= 0.5)
    2145/(2145+542) #0.80
    pred3trR <- prediction(pred3tr, train1$team_result)
    as.numeric(performance(pred3trR, "auc")@y.values) #0.89
    # plot ROC curve to choose t
    perf3trR = performance(pred3trR, "tpr", "fpr")
    plot(perf3trR, colorize=TRUE, print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
  # test accuracy
  pred3 = predict(log3, type = "response", newdata = test1)
  table(test1$team_result, pred3 >= 0.5)
  158/(158+46) #0.77
  
  table(test1$team_result, pred3 >= 0.4) # note: changing threshold to 0.4 improved results
  (143+174)/404 #0.78
  
  pred3R <- prediction(pred3, test1$team_result)
  as.numeric(performance(pred3R, "auc")@y.values) #0.87
  # plot ROC curve
  perf3R = performance(pred3R, "tpr", "fpr")
  plot(perf3R, colorize=TRUE, print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
  

  # compare variable importance
  logx = glm(team_result ~ idx_win_aerial_ball + idx_less_frees + idx_win_ground_ball + idx_less_clangers + idx_50m_entry + Inside_50s,
             data = train1, family = binomial)
  predx = predict(logx, type = "response", newdata = test1)
  predxR <- prediction(predx, test1$team_result)
  as.numeric(performance(predxR, "auc")@y.values)
  # AUC excl
    # idx_win_aerial_ball: 0.88
    # idx_less_frees: 0.87
    # idx_win_ground_ball: 0.84 **
    # idx_less_clangers: 0.86 *
    # idx_50m_entry: 0.83 ***
    # Inside_50s: 0.86 *


  
  
  
  
  
  
  
  # idx_win_ground_ball = Contested_possessions/Contested_possessions.opp
  # idx_win_aerial_ball = Contested_marks/Contested_marks.opp
  # idx_less_clangers = Clangers.opp/Clangers
  # idx_50m_entry = (Marks_inside_50/Inside_50s)/(Marks_inside_50.opp/Inside_50s.opp),
  # idx_less_frees = Frees_against.opp/Frees_against
  
# How well do the models predict actual results
  # Take player data for 2015-2016 (two years), try to predict match results for 2017 based on averages
  AFL.by.player.2015.2016 = AFL.by.player %>% filter(Year %in% 2015:2016) %>% select(-(Year:Team), -Played_pct, -(home_away:match_score_idx)) %>% 
    group_by(Player) %>% summarise_all(.funs = mean) %>% select(Player, Contested_marks, Frees_against, Contested_possessions, Clangers, Marks_inside_50, Inside_50s)
  
  # note: we will not have stats for players without 2015-16 records which is realistic
  AFL.matches.2017.pred = AFL.by.player %>% filter(Year == 2017) %>% select(match_id, round, Team, vs_opponent, Player, team_result) %>% inner_join(AFL.by.player.2015.2016, by = "Player")
  # as before, take a team-wise view by summing up the player averages
  AFL.matches.2017.pred.sum = AFL.matches.2017.pred %>% group_by(match_id, round, Team, vs_opponent, team_result) %>% summarise_at(vars(Contested_marks:Inside_50s), .funs = sum)
  
  i50.top.n <- function(matchid, team.nm, n = 10){
    DF <- AFL.matches.2017.pred %>% select(match_id, Team, "Inside_50s") %>% filter(match_id == matchid) %>% top_n(n)
    prop.top.10 <- (DF %>% filter(Team == team.nm) %>% count)/(nrow(DF))
    return(prop.top.10)
  }
  setDT(AFL.matches.2017.pred.sum)[ , Inside_50s_prop := i50.top.n(match_id, Team, 10), by=c("match_id","round","Team")]

  AFL.matches.2017.pred.sum.1 = merge(AFL.matches.2017.pred.sum, AFL.matches.2017.pred.sum %>% rename (Team.opp = Team), by.x = c("match_id","round","Team"), by.y = c("match_id","round","vs_opponent"), suffix = c("",".opp")) %>% 
    mutate(idx_win_ground_ball = log10(Contested_possessions/Contested_possessions.opp), idx_win_aerial_ball = log10(Contested_marks/Contested_marks.opp), idx_less_clangers = log10(Clangers.opp/Clangers),
           idx_50m_entry = log10((Marks_inside_50/Inside_50s)/(Marks_inside_50.opp/Inside_50s.opp)), idx_less_frees = log10(Frees_against.opp/Frees_against)) %>% 
    select(match_id:team_result, starts_with("idx_"), Inside_50s = Inside_50s_prop)
  
  
  
  
  # Take player data for 2016 (one year), try to predict match results for 2017 based on averages
  AFL.by.player.2016 = AFL.by.player %>% filter(Year %in% 2016) %>% select(-(Year:Team), -Played_pct, -(home_away:match_score_idx)) %>% 
    group_by(Player) %>% summarise_all(.funs = mean) %>% select(Player, Contested_marks, Frees_against, Contested_possessions, Clangers, Marks_inside_50, Inside_50s)
  
  # note: we will not have stats for players without 2016 records which is realistic
  AFL.matches.2017.pred = AFL.by.player %>% filter(Year == 2017) %>% select(match_id, round, Team, vs_opponent, Player, team_result) %>% inner_join(AFL.by.player.2016, by = "Player")
  # as before, take a team-wise view by summing up the player averages
  AFL.matches.2017.pred.sum = AFL.matches.2017.pred %>% group_by(match_id, round, Team, vs_opponent, team_result) %>% summarise_at(vars(Contested_marks:Inside_50s), .funs = sum)
  
  i50.top.n <- function(matchid, team.nm, n = 10){
    DF <- AFL.matches.2017.pred %>% select(match_id, Team, "Inside_50s") %>% filter(match_id == matchid) %>% top_n(n)
    prop.top.10 <- (DF %>% filter(Team == team.nm) %>% count)/(nrow(DF))
    return(prop.top.10)
  }
  setDT(AFL.matches.2017.pred.sum)[ , Inside_50s_prop := i50.top.n(match_id, Team, 10), by=c("match_id","round","Team")]
  
  AFL.matches.2017.pred.sum.1 = merge(AFL.matches.2017.pred.sum, AFL.matches.2017.pred.sum %>% rename (Team.opp = Team), by.x = c("match_id","round","Team"), by.y = c("match_id","round","vs_opponent"), suffix = c("",".opp")) %>% 
    mutate(idx_win_ground_ball = log10(Contested_possessions/Contested_possessions.opp), idx_win_aerial_ball = log10(Contested_marks/Contested_marks.opp), idx_less_clangers = log10(Clangers.opp/Clangers),
           idx_50m_entry = log10((Marks_inside_50/Inside_50s)/(Marks_inside_50.opp/Inside_50s.opp)), idx_less_frees = log10(Frees_against.opp/Frees_against)) %>% 
    select(match_id:team_result, starts_with("idx_"), Inside_50s = Inside_50s_prop)
  
  
  
  
  # using basic CART decision tree
    prp(tree1_V2)
    pred2017t = predict(tree1_V2, newdata = AFL.matches.2017.pred.sum.1, type = "class")
    table(AFL.matches.2017.pred.sum.1$team_result, pred2017t)
    (112+139)/414 #0.61
    
    pred2017tROC = predict(tree1_V2, newdata = AFL.matches.2017.pred.sum.1)
    pred2017tR = prediction(pred2017tROC[ ,2], AFL.matches.2017.pred.sum.1$team_result)
    perf2017tR = performance(pred2017tR, "tpr", "fpr")
    plot(perf2017tR)
    # AUC
    as.numeric(performance(pred2017R, "auc")@y.values) #0.63
  
  # using tuned CART decision tree
    prp(tree2_V2)
    pred2017 = predict(tree2_V2, newdata = AFL.matches.2017.pred.sum.1, type = "class")
    table(AFL.matches.2017.pred.sum.1$team_result, pred2017)
    (136+131)/414 #0.64
    
    pred2017ROC = predict(tree2_V2, newdata = AFL.matches.2017.pred.sum.1)
    pred2017R = prediction(pred2017ROC[ ,2], AFL.matches.2017.pred.sum.1$team_result)
    perf2017R = performance(pred2017R, "tpr", "fpr")
    plot(perf2017R)
    # AUC
    as.numeric(performance(pred2017R, "auc")@y.values) #0.63
  
  # using tuned logistic regression model
    pred2017L = predict(log3, type = "response", newdata = AFL.matches.2017.pred.sum.1)
    table(AFL.matches.2017.pred.sum.1$team_result, pred2017L >= 0.5)
    (131+128)/414 #0.63
    
    plot(perf2017LR)
    # AUC
    pred2017LR <- prediction(pred2017L, AFL.matches.2017.pred.sum.1$team_result)
    as.numeric(performance(pred2017LR, "auc")@y.values) #0.65
  
# Conclusions
  # i) important factors to winning
  # ii) ability to predict match results
  # iii) high performing players who are not getting enough game time
  
  
  
  
  
  
    
    
    
    
  rm("AFL.by.player.win")
  rm("AFL.by.player.key")
  rm("AFL.by.player.win.key")
# Winningest players
  AFL.by.player.win <- AFL.by.player %>% transmute(Year, round, Team, Player, team_result = if_else(team_result==1, 1, 0), result_influence = team_result * Played_pct/100, game_time = Played_pct/100) %>% 
    select(-(Year:round)) %>% group_by(Team, Player) %>% summarise_all(.funs = sum) %>% mutate(win_rate = result_influence/game_time) #%>% filter(team_result >= 50) %>% top_n(n=100, wt=win_rating) %>% arrange(desc(win_rating))
  
  
  # Players who top the key measures: idx_50m_entry (Marks_inside_50), Inside_50s, idx_win_ground_ball (Contested_possessions), idx_less_clangers
  AFL.by.player.key <- AFL.by.player %>% transmute(Year, round, Team, Player, team_result = if_else(team_result==1, 1, 0), Played_pct = Played_pct/100,
                                                   Marks_inside_50_rate = Marks_inside_50/Played_pct, Inside_50s_rate = Inside_50s/Played_pct, Contested_possessions_rate = Contested_possessions/Played_pct,
                                                   Clangers_rate = -Clangers/Played_pct, Composite_rate = Marks_inside_50_rate + Inside_50s_rate + Contested_possessions_rate + Clangers_rate) %>%
    select(-(Year:round)) %>% group_by(Team, Player) %>% summarise_at(vars(ends_with("_rate")), .funs = mean)
  
  # Played at least 50 full games
  AFL.by.player.win.key <- merge(AFL.by.player.win, AFL.by.player.key, by = c("Team", "Player")) %>% filter(game_time >= 50) %>% arrange(desc(win_rate)) %>% 
    mutate_at(vars(ends_with("_rate")), .funs = funs(dense_rank(desc(.))))
  
  # Plot win rate ranking against other rankings
  
  