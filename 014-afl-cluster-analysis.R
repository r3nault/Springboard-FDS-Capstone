library(dplyr)
library(rpart)
library(rpart.plot)
library(ROCR)

# improving performance by clustering  
train1_Matrix = as.matrix(train1 %>% select(idx_win_ground_ball:Marks_inside_50))
test1_Matrix = as.matrix(test1 %>% select(idx_win_ground_ball:Marks_inside_50))

# assume 3x2=6 clusters (team: top, middle, bottom; strategy: fast, slow)
k=6
set.seed(1)

train1_kmc = kmeans(train1_Matrix, centers = k, iter.max = 10000)
train1_clusters = train1_kmc$cluster

library(flexclust)
train1_kcca = as.kcca(train1_kmc, train1_Matrix)
test1_clusters = predict(train1_kcca, newdata = test1_Matrix)

train1_clust1 = subset(train1, train1_clusters==1)  
train1_clust2 = subset(train1, train1_clusters==2)    
train1_clust3 = subset(train1, train1_clusters==3)  
train1_clust4 = subset(train1, train1_clusters==4)    
train1_clust5 = subset(train1, train1_clusters==5)  
train1_clust6 = subset(train1, train1_clusters==6)   

test1_clust1 = subset(test1, test1_clusters==1)  
test1_clust2 = subset(test1, test1_clusters==2)    
test1_clust3 = subset(test1, test1_clusters==3)  
test1_clust4 = subset(test1, test1_clusters==4)    
test1_clust5 = subset(test1, test1_clusters==5)  
test1_clust6 = subset(test1, test1_clusters==6)


# BUILD DECISION TREES

library(caret)
library(e1071)
  # cluster 1
  fitControl = trainControl(method = "cv", number = 10)
  cpGrid = expand.grid(.cp=(1:50)*0.001)
  train(team_result ~ idx_win_ground_ball + idx_win_aerial_ball + idx_clear_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Clangers + Clearances + Contested_marks + Contested_possessions + Frees_against + Inside_50s + Marks_inside_50,
        data = train1_clust1, method = "rpart", trControl = fitControl, tuneGrid = cpGrid) #0.02
  
  tree_clust1 = rpart(team_result ~ idx_win_ground_ball + idx_win_aerial_ball + idx_clear_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Clangers + Clearances + Contested_marks + Contested_possessions + Frees_against + Inside_50s + Marks_inside_50,
                      data = train1_clust1, method = "class", control = rpart.control(cp = 0.03)) #altered cp
  prp(tree_clust1, extra = 1, fallen.leaves = TRUE, varlen = 0)  
  
  table(test1_clust1$team_result, predict(tree_clust1, newdata = test1_clust1, type = "prob")[,2] > 0.5)
  45/66 #0.68
  pred_tree_clust1 = prediction(predict(tree_clust1, newdata = test1_clust1)[ ,2], test1_clust1$team_result) # note: changing threshold to 0.6 did not improve results
  perf_tree_clust1 = performance(pred_tree_clust1, "tpr", "fpr")
    par(cex = 0.6)
    plot(perf_tree_clust1, col="blue", main="ROC Curve for Decision Tree - Cluster 1", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
    text(0.9, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred_tree_clust1, "auc")@y.values),2),sep=""), adj=1, cex = 1.5) #0.65
    text(0.9, 0.22, labels = paste("Class accuracy = ", round(45/66,2)*100, "%", sep=""), adj=1, cex = 1.5)

  # cluster 2
  fitControl = trainControl(method = "cv", number = 10)
  cpGrid = expand.grid(.cp=(1:50)*0.001)
  train(team_result ~ idx_win_ground_ball + idx_win_aerial_ball + idx_clear_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Clangers + Clearances + Contested_marks + Contested_possessions + Frees_against + Inside_50s + Marks_inside_50,
        data = train1_clust2, method = "rpart", trControl = fitControl, tuneGrid = cpGrid) #0.02
  
  tree_clust2 = rpart(team_result ~ idx_win_ground_ball + idx_win_aerial_ball + idx_clear_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Clangers + Clearances + Contested_marks + Contested_possessions + Frees_against + Inside_50s + Marks_inside_50,
                      data = train1_clust2, method = "class", control = rpart.control(cp = 0.02))
  prp(tree_clust2, extra = 1, fallen.leaves = TRUE, varlen = 0)  
  
  table(test1_clust2$team_result, predict(tree_clust2, newdata = test1_clust2, type = "prob")[,2] > 0.5)
  65/93 #0.70
  pred_tree_clust2 = prediction(predict(tree_clust2, newdata = test1_clust2)[ ,2], test1_clust2$team_result) # note: changing threshold to 0.6 did not improve results
  perf_tree_clust2 = performance(pred_tree_clust2, "tpr", "fpr")
    par(cex = 0.6)
    plot(perf_tree_clust2, col="blue", main="ROC Curve for Decision Tree - Cluster 2", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
    text(0.9, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred_tree_clust2, "auc")@y.values),2),sep=""), adj=1, cex = 1.5) #0.62
    text(0.9, 0.22, labels = paste("Class accuracy = ", round(65/93,2)*100, "%", sep=""), adj=1, cex = 1.5)

  # cluster 3
  fitControl = trainControl(method = "cv", number = 10)
  cpGrid = expand.grid(.cp=(1:50)*0.0001)
  train(team_result ~ idx_win_ground_ball + idx_win_aerial_ball + idx_clear_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Clangers + Clearances + Contested_marks + Contested_possessions + Frees_against + Inside_50s + Marks_inside_50,
        data = train1_clust3, method = "rpart", trControl = fitControl, tuneGrid = cpGrid) #0.005
  
  tree_clust3 = rpart(team_result ~ idx_win_ground_ball + idx_win_aerial_ball + idx_clear_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Clangers + Clearances + Contested_marks + Contested_possessions + Frees_against + Inside_50s + Marks_inside_50,
                      data = train1_clust3, method = "class", control = rpart.control(cp = 0.016)) #altered cp
  prp(tree_clust3, extra = 1, fallen.leaves = TRUE, varlen = 0)  
  
  table(test1_clust3$team_result, predict(tree_clust3, newdata = test1_clust3, type = "prob")[,2] > 0.5)
  59/86 #0.69
  pred_tree_clust3 = prediction(predict(tree_clust3, newdata = test1_clust3)[ ,2], test1_clust3$team_result) # note: changing threshold to 0.7 did not improve results
  perf_tree_clust3 = performance(pred_tree_clust3, "tpr", "fpr")
    par(cex = 0.6)
    plot(perf_tree_clust3, col="blue", main="ROC Curve for Decision Tree - Cluster 3", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
    text(0.9, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred_tree_clust3, "auc")@y.values),2),sep=""), adj=1, cex = 1.5) #0.63
    text(0.9, 0.22, labels = paste("Class accuracy = ", round(59/86,2)*100, "%", sep=""), adj=1, cex = 1.5)

  # cluster 4
  fitControl = trainControl(method = "cv", number = 10)
  cpGrid = expand.grid(.cp=(1:50)*0.001)
  train(team_result ~ idx_win_ground_ball + idx_win_aerial_ball + idx_clear_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Clangers + Clearances + Contested_marks + Contested_possessions + Frees_against + Inside_50s + Marks_inside_50,
        data = train1_clust4, method = "rpart", trControl = fitControl, tuneGrid = cpGrid) #0.005
  
  tree_clust4 = rpart(team_result ~ idx_win_ground_ball + idx_win_aerial_ball + idx_clear_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Clangers + Clearances + Contested_marks + Contested_possessions + Frees_against + Inside_50s + Marks_inside_50,
                      data = train1_clust4, method = "class", control = rpart.control(cp = 0.02)) #altered cp
  prp(tree_clust4, extra = 1, fallen.leaves = TRUE, varlen = 0)  
  
  table(test1_clust4$team_result, predict(tree_clust4, newdata = test1_clust4, type = "prob")[,2] > 0.5)
  45/47 #0.96
  pred_tree_clust4 = prediction(predict(tree_clust4, newdata = test1_clust4)[ ,2], test1_clust4$team_result) # note: changing threshold to 0.9 did not improve results
  perf_tree_clust4 = performance(pred_tree_clust4, "tpr", "fpr")
    par(cex = 0.6)
    plot(perf_tree_clust4, col="blue", main="ROC Curve for Decision Tree - Cluster 4", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
    text(0.9, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred_tree_clust4, "auc")@y.values),2),sep=""), adj=1, cex = 1.5) #0.98
    text(0.9, 0.22, labels = paste("Class accuracy = ", round(45/47,2)*100, "%", sep=""), adj=1, cex = 1.5)

  # cluster 5
  fitControl = trainControl(method = "cv", number = 10)
  cpGrid = expand.grid(.cp=(1:50)*0.001)
  train(team_result ~ idx_win_ground_ball + idx_win_aerial_ball + idx_clear_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Clangers + Clearances + Contested_marks + Contested_possessions + Frees_against + Inside_50s + Marks_inside_50,
        data = train1_clust5, method = "rpart", trControl = fitControl, tuneGrid = cpGrid) #0.041
  
  tree_clust5 = rpart(team_result ~ idx_win_ground_ball + idx_win_aerial_ball + idx_clear_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Clangers + Clearances + Contested_marks + Contested_possessions + Frees_against + Inside_50s + Marks_inside_50,
                      data = train1_clust5, method = "class", control = rpart.control(cp = 0.04)) #altered cp
  prp(tree_clust5, extra = 1, fallen.leaves = TRUE, varlen = 0)  
  
  table(test1_clust5$team_result, predict(tree_clust5, newdata = test1_clust5, type = "prob")[,2] > 0.3)
  52/64 #0.81
  pred_tree_clust5 = prediction(predict(tree_clust5, newdata = test1_clust5)[ ,2], test1_clust5$team_result) # note: changing threshold to 0.3 makes sense here
  perf_tree_clust5 = performance(pred_tree_clust5, "tpr", "fpr")
    par(cex = 0.6)
    plot(perf_tree_clust5, col="blue", main="ROC Curve for Decision Tree - Cluster 5", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
    text(0.9, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred_tree_clust5, "auc")@y.values),2),sep=""), adj=1, cex = 1.5) #0.57
    text(0.9, 0.22, labels = paste("Class accuracy = ", round(52/64,2)*100, "%", sep=""), adj=1, cex = 1.5)
    
  # cluster 6
  fitControl = trainControl(method = "cv", number = 10)
  cpGrid = expand.grid(.cp=(1:50)*0.001)
  train(team_result ~ idx_win_ground_ball + idx_win_aerial_ball + idx_clear_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Clangers + Clearances + Contested_marks + Contested_possessions + Frees_against + Inside_50s + Marks_inside_50,
        data = train1_clust6, method = "rpart", trControl = fitControl, tuneGrid = cpGrid) #0.042
  
  tree_clust6 = rpart(team_result ~ idx_win_ground_ball + idx_win_aerial_ball + idx_clear_ball + idx_less_clangers + idx_50m_entry + idx_less_frees + Clangers + Clearances + Contested_marks + Contested_possessions + Frees_against + Inside_50s + Marks_inside_50,
                      data = train1_clust6, method = "class", control = rpart.control(cp = 0.02)) #altered cp
  prp(tree_clust6, extra = 1, fallen.leaves = TRUE, varlen = 0)  
  
  table(test1_clust6$team_result, predict(tree_clust6, newdata = test1_clust6, type = "prob")[,2] > 0.3)
  44/52 #0.85
  pred_tree_clust6 = prediction(predict(tree_clust6, newdata = test1_clust6)[ ,2], test1_clust6$team_result) # note: changing threshold to 0.3 makes sense here
  perf_tree_clust6 = performance(pred_tree_clust6, "tpr", "fpr")
    par(cex = 0.6)
    plot(perf_tree_clust6, col="blue", main="ROC Curve for Decision Tree - Cluster 6", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
    text(0.9, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred_tree_clust6, "auc")@y.values),2),sep=""), adj=1, cex = 1.5) #0.71
    text(0.9, 0.22, labels = paste("Class accuracy = ", round(44/52,2)*100, "%", sep=""), adj=1, cex = 1.5)  


# BUILD LOGISTIC REGRESSIONS
  # cluster 1
  log_clustx = glm(team_result ~ Contested_marks + Clearances + Contested_possessions + Clangers + idx_win_aerial_ball + Marks_inside_50 + idx_less_frees + idx_win_ground_ball + idx_clear_ball + idx_less_clangers + idx_50m_entry + Frees_against + Inside_50s,
                   data = train1_clust1, family = binomial)
  summary(log_clustx)
  # remove variables one by one until all significant, then check multicollinearity
  log_clust1 = glm(team_result ~ idx_less_frees + idx_win_ground_ball + idx_less_clangers + idx_50m_entry + Inside_50s,
                   data = train1_clust1, family = binomial)
  summary(log_clust1)
  cor(train1_clust1 %>% transmute(team_result = as.numeric(team_result), idx_less_frees, idx_win_ground_ball, idx_less_clangers, idx_50m_entry, Inside_50s))
    # plot training ROC curve to choose t
    pred_log_train1clust1 = predict(log_clust1, type = "response")
    pred_log_train1clust1R = prediction(pred_log_train1clust1, as.factor(train1_clust1$team_result))
    perf_log_train1clust1R = performance(pred_log_train1clust1R, "tpr", "fpr")
    plot(perf_log_train1clust1R, col="blue", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
    
    # test accuracy
    pred_log_clust1 = predict(log_clust1, type = "response", newdata = test1_clust1)
    table(test1_clust1$team_result, pred_log_clust1 >= 0.5)
    49/66 #0.74
    pred_log_clust1R = prediction(pred_log_clust1, as.factor(test1_clust1$team_result))
    # plot ROC curve
    perf_log_clust1R = performance(pred_log_clust1R, "tpr", "fpr")
      par(cex = 0.6)
      plot(perf_log_clust1R, col="blue", main="ROC Curve for Logistic Regression - Cluster 1", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
      text(0.9, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred_log_clust1R, "auc")@y.values),2),sep=""), adj=1, cex = 1.5) #0.81
      text(0.9, 0.22, labels = paste("Class accuracy = ", round(49/66,2)*100, "%", sep=""), adj=1, cex = 1.5)  

  # cluster 2
  log_clustx = glm(team_result ~ Contested_marks + Clearances + Contested_possessions + Clangers + idx_win_aerial_ball + Marks_inside_50 + idx_less_frees + idx_win_ground_ball + idx_clear_ball + idx_less_clangers + idx_50m_entry + Frees_against + Inside_50s,
                   data = train1_clust2, family = binomial)
  
  log_clust2 = glm(team_result ~ idx_less_frees + idx_win_ground_ball + idx_less_clangers + idx_50m_entry + Inside_50s,
                   data = train1_clust2, family = binomial)
  summary(log_clust2)
  cor(train1_clust2 %>% transmute(team_result = as.numeric(team_result), idx_less_frees, idx_win_ground_ball, idx_less_clangers, idx_50m_entry, Inside_50s))
    # plot training ROC curve to choose t
    pred_log_train1clust2 = predict(log_clust2, type = "response")
    pred_log_train1clust2R = prediction(pred_log_train1clust2, as.factor(train1_clust2$team_result))
    perf_log_train1clust2R = performance(pred_log_train1clust2R, "tpr", "fpr")
    plot(perf_log_train1clust2R, col="blue", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
    
    # test accuracy
    pred_log_clust2 = predict(log_clust2, type = "response", newdata = test1_clust2)
    table(test1_clust2$team_result, pred_log_clust2 >= 0.5)
    66/93 #0.71
    pred_log_clust2R = prediction(pred_log_clust2, as.factor(test1_clust2$team_result))
    # plot ROC curve
    perf_log_clust2R = performance(pred_log_clust2R, "tpr", "fpr")
      par(cex = 0.6)
      plot(perf_log_clust2R, col="blue", main="ROC Curve for Logistic Regression - Cluster 2", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
      text(0.9, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred_log_clust2R, "auc")@y.values),2),sep=""), adj=1, cex = 1.5) #0.75
      text(0.9, 0.22, labels = paste("Class accuracy = ", round(66/93,2)*100, "%", sep=""), adj=1, cex = 1.5)

  # cluster 3
  log_clustx = glm(team_result ~ Contested_marks + Clearances + Contested_possessions + Clangers + idx_win_aerial_ball + Marks_inside_50 + idx_less_frees + idx_win_ground_ball + idx_clear_ball + idx_less_clangers + idx_50m_entry + Frees_against + Inside_50s,
                   data = train1_clust3, family = binomial)
  
  log_clust3 = glm(team_result ~ idx_less_frees + idx_win_ground_ball + idx_less_clangers + idx_50m_entry + Inside_50s,
                   data = train1_clust3, family = binomial)
  summary(log_clust3)
  cor(train1_clust3 %>% transmute(team_result = as.numeric(team_result), idx_less_frees, idx_win_ground_ball, idx_less_clangers, idx_50m_entry, Inside_50s))
    # plot training ROC curve to choose t
    pred_log_train1clust3 = predict(log_clust3, type = "response")
    pred_log_train1clust3R = prediction(pred_log_train1clust3, as.factor(train1_clust3$team_result))
    perf_log_train1clust3R = performance(pred_log_train1clust3R, "tpr", "fpr")
    plot(perf_log_train1clust3R, col="blue", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
    
    # test accuracy
    pred_log_clust3 = predict(log_clust3, type = "response", newdata = test1_clust3)
    table(test1_clust3$team_result, pred_log_clust3 >= 0.5)
    60/86 #0.70
    pred_log_clust3R = prediction(pred_log_clust3, as.factor(test1_clust3$team_result))
    # plot ROC curve
    perf_log_clust3R = performance(pred_log_clust3R, "tpr", "fpr")
      par(cex = 0.6)
      plot(perf_log_clust3R, col="blue", main="ROC Curve for Logistic Regression - Cluster 3", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
      text(0.9, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred_log_clust3R, "auc")@y.values),2),sep=""), adj=1, cex = 1.5) #0.77
      text(0.9, 0.22, labels = paste("Class accuracy = ", round(60/86,2)*100, "%", sep=""), adj=1, cex = 1.5)      

  # cluster 4
  log_clustx = glm(team_result ~ Contested_marks + Clearances + Contested_possessions + Clangers + idx_win_aerial_ball + Marks_inside_50 + idx_less_frees + idx_win_ground_ball + idx_clear_ball + idx_less_clangers + idx_50m_entry + Frees_against + Inside_50s,
                   data = train1_clust4, family = binomial)
  
  log_clust4 = glm(team_result ~ idx_less_frees + idx_win_ground_ball + idx_less_clangers + idx_50m_entry + Inside_50s,
                   data = train1_clust4, family = binomial)
  summary(log_clust4)
  cor(train1_clust4 %>% transmute(team_result = as.numeric(team_result), idx_less_frees, idx_win_ground_ball, idx_less_clangers, idx_50m_entry, Inside_50s))
    # plot training ROC curve to choose t
    pred_log_train1clust4 = predict(log_clust4, type = "response")
    pred_log_train1clust4R = prediction(pred_log_train1clust4, as.factor(train1_clust4$team_result))
    perf_log_train1clust4R = performance(pred_log_train1clust4R, "tpr", "fpr")
    plot(perf_log_train1clust4R, col="blue", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
    
    # test accuracy
    pred_log_clust4 = predict(log_clust4, type = "response", newdata = test1_clust4)
    table(test1_clust4$team_result, pred_log_clust4 >= 0.7)
    44/47 #0.94
    pred_log_clust4R = prediction(pred_log_clust4, as.factor(test1_clust4$team_result))
    # plot ROC curve
    perf_log_clust4R = performance(pred_log_clust4R, "tpr", "fpr")
      par(cex = 0.6)
      plot(perf_log_clust4R, col="blue", main="ROC Curve for Logistic Regression - Cluster 4", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
      text(0.9, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred_log_clust4R, "auc")@y.values),2),sep=""), adj=1, cex = 1.5) #0.89
      text(0.9, 0.22, labels = paste("Class accuracy = ", round(44/47,2)*100, "%", sep=""), adj=1, cex = 1.5)      

  # cluster 5
  log_clustx = glm(team_result ~ Contested_marks + Clearances + Contested_possessions + Clangers + idx_win_aerial_ball + Marks_inside_50 + idx_less_frees + idx_win_ground_ball + idx_clear_ball + idx_less_clangers + idx_50m_entry + Frees_against + Inside_50s,
                   data = train1_clust5, family = binomial)
  
  log_clust5 = glm(team_result ~ Clearances + Clangers + idx_less_frees + idx_win_ground_ball + idx_less_clangers + idx_50m_entry + Inside_50s,
                   data = train1_clust5, family = binomial)
  summary(log_clust5)
  cor(train1_clust5 %>% transmute(team_result = as.numeric(team_result), idx_less_frees, idx_win_ground_ball, idx_less_clangers, idx_50m_entry, Inside_50s))
    # plot training ROC curve to choose t
    pred_log_train1clust5 = predict(log_clust5, type = "response")
    pred_log_train1clust5R = prediction(pred_log_train1clust5, as.factor(train1_clust5$team_result))
    perf_log_train1clust5R = performance(pred_log_train1clust5R, "tpr", "fpr")
    plot(perf_log_train1clust5R, col="blue", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
    
    # test accuracy
    pred_log_clust5 = predict(log_clust5, type = "response", newdata = test1_clust5)
    table(test1_clust5$team_result, pred_log_clust5 >= 0.3)
    55/64 #0.86
    pred_log_clust5R = prediction(pred_log_clust5, as.factor(test1_clust5$team_result))
    # plot ROC curve
    perf_log_clust5R = performance(pred_log_clust5R, "tpr", "fpr")
      par(cex = 0.6)
      plot(perf_log_clust5R, col="blue", main="ROC Curve for Logistic Regression - Cluster 5", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
      text(0.9, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred_log_clust5R, "auc")@y.values),2),sep=""), adj=1, cex = 1.5) #0.91
      text(0.9, 0.22, labels = paste("Class accuracy = ", round(55/64,2)*100, "%", sep=""), adj=1, cex = 1.5)      

  # cluster 6
  log_clustx = glm(team_result ~ Contested_marks + Clearances + Contested_possessions + Clangers + idx_win_aerial_ball + Marks_inside_50 + idx_less_frees + idx_win_ground_ball + idx_clear_ball + idx_less_clangers + idx_50m_entry + Frees_against + Inside_50s,
                   data = train1_clust6, family = binomial)
  
  log_clust6 = glm(team_result ~ idx_win_aerial_ball + idx_less_frees + idx_win_ground_ball + idx_less_clangers + idx_50m_entry + Inside_50s,
                   data = train1_clust6, family = binomial)
  summary(log_clust6)
  cor(train1_clust6 %>% transmute(team_result = as.numeric(team_result), idx_less_frees, idx_win_ground_ball, idx_less_clangers, idx_50m_entry, Inside_50s))
    # plot training ROC curve to choose t
    pred_log_train1clust6 = predict(log_clust6, type = "response")
    pred_log_train1clust6R = prediction(pred_log_train1clust6, as.factor(train1_clust6$team_result))
    perf_log_train1clust6R = performance(pred_log_train1clust6R, "tpr", "fpr")
    plot(perf_log_train1clust6R, col="blue", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
    
    # test accuracy
    pred_log_clust6 = predict(log_clust6, type = "response", newdata = test1_clust6)
    table(test1_clust6$team_result, pred_log_clust6 >= 0.4)
    44/52 #0.85
    pred_log_clust6R = prediction(pred_log_clust6, as.factor(test1_clust6$team_result))
    # plot ROC curve
    perf_log_clust6R = performance(pred_log_clust6R, "tpr", "fpr")
      par(cex = 0.6)
      plot(perf_log_clust6R, col="blue", main="ROC Curve for Logistic Regression - Cluster 6", print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
      text(0.9, 0.4, labels = paste("AUC = ", round(as.numeric(performance(pred_log_clust6R, "auc")@y.values),2),sep=""), adj=1, cex = 1.5) #0.85
      text(0.9, 0.22, labels = paste("Class accuracy = ", round(44/52,2)*100, "%", sep=""), adj=1, cex = 1.5)  