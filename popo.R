R Script 

#Data Preparation and Cleaning 
#Census dataset

census <- census[,-c(1,5:23)] popullation <- census[,-1] 
colnames(popullation)[2] <- "Popullation" 

#Referendum Results dataset

referendum <- EU.referendum.result.data[,-c(1:4,12,13,15:19,7)] 
#General elections results 2015 
results1 <- RESULTS.FOR.ANALYSIS[,-c(1,3,4,5,6,7)] results2 <- results1[,-c(4:20,22:40,42:46)] results3 <- results2[,-c(7:21,23,25:50)] results4 <- results3[,-c(9:41,43:50)] results <- results4[-c(10:17,2,3)] results <- results[,-3] 

#Treating the NAs 

colnames(results)[1] <- "Area" 
Results_NA_count <- apply(is.na(results), 2, sum) 
Results_NA_count 
Results_NA_perc <- Results_NA_count / dim(results)[1] * 100 
Results_NA_perc 
results[is.na(results$C), 'C'] = '1' results[is.na(results$Green), 'Green'] = '1' results[is.na(results$Lab), 'Lab'] = '1' results[is.na(results$LD), 'LD'] = '1' results[is.na(results$UKIP), 'UKIP'] = '1' colnames(results)[1] <- "Area" 

#Income and Tax dataset

Income_and_Tax <- NS_Table_3_15_1516[,-c(2:15,19,20,23,16)] colnames(Income_and_Tax)[1] <- "Area" colnames(Income_and_Tax)[2] <- "Mean_Income" colnames(Income_and_Tax)[3] <- "Median_Income" colnames(Income_and_Tax)[4] <- "Mean_Tax" colnames(Income_and_Tax)[5] <- "Median_Tax" 
Income_and_Tax <- na.omit(Income_and_Tax)  
Income_and_Tax$Area <- factor(Income_and_Tax$Area) 
Income_and_Tax$Median_Tax <- as.numeric(as.character(Income_and_Tax$Median_Tax)) 
Income_and_Tax$Median_Income <- as.numeric(as.character(Income_and_Tax$Median_Income)) 
Income_and_Tax$Mean_Tax <- as.numeric(as.character(Income_and_Tax$Mean_Tax)) 
Income_and_Tax$Mean_Income <- as.numeric(as.character(Income_and_Tax$Mean_Income)) 

#Library dplyr  - Join the datasets together

leave <- inner_join(referendum,results) leave <- inner_join(Income_and_Tax,leave) 
leave <- inner_join(popullation,leave) 

leave$Area <- factor(leave$Area) leave$C <- as.numeric(as.character(leave$C)) leave$Green <- as.numeric(as.character(leave$Green)) leave$UKIP <- as.numeric(as.character(leave$UKIP)) leave$Lab <- as.numeric(as.character(leave$Lab)) leave$LD <- as.numeric(as.character(leave$LD)) 

#Exploratory Data Analysis 
#Histograms
summary(leave) 
par(mfrow = c(2,2)) 
hist( 
  leave$Median_Income,   xlab = 'Median Income in $1000', 
  ylab = 'Frequency', 
  main = 'Median Income Per Consituency',   col = rgb(0, 1, 0),   border = 'white',   xaxt = 'n' 
) 
axis(1, at = seq(0, 65000, 5000), seq(0, 65, 5)) abline(v = median(leave$Median_Income,), lty = 2) legend('topright', 'median Income', lty = 2, bty = 'n') 
hist( 
  leave$Median_Tax,   xlab = 'Median Tax in $1000',   ylab = 'Frequency', 
  main = 'Media Tax Per Consituency', 
  col = rgb(0, 1, 0),   border = 'white',   xaxt = 'n' 
) 
axis(1, at = seq(0, 65000, 5000), seq(0, 65, 5)) 
abline(v = median(leave$Median_Tax,), lty = 2) 
legend('topright', 'median Tax', lty = 2, bty = 'n') 

hist( 
  leave$Mean_Income,   xlab = 'Mean Income in $1000',   ylab = 'Frequency', 
  main = 'Mean Income Per Consituency',   col = rgb(0, 1, 0),   border = 'white', 
  xaxt = 'n' 
) 
axis(1, at = seq(0, 65000, 5000), seq(0, 65, 5)) abline(v = median(leave$Mean_Income,), lty = 2) legend('topright', 'Mean Income', lty = 2, bty = 'n') 
hist( 
  leave$Mean_Tax,   xlab = 'Mean Tax in $1000',   ylab = 'Frequency',   main = 'Mean Tax Per Consituency', 
  col = rgb(0, 1, 0),   border = 'white', 
  xaxt = 'n' 
) 
axis(1, at = seq(0, 65000, 5000), seq(0, 65, 5)) abline(v = median(leave$Mean_Tax,), lty = 2) 
legend('topright', 'Mean Tax', lty = 2, bty = 'n') 
plot( 
  leave$Pct_Leave ~ leave$Median_Income, 
  ylab = 'Percentage of Leave',   xlab = 'Median Income',   main = 'Leave vs Income', pch = 0.15 
) 
abline(v = median(leave$Median_Income,), lty = 2) legend('topright', 'Median Income', lty = 2, bty = 'n') dev.off() 
plot( 
  leave$Pct_Leave ~ leave$Median_Tax,   ylab = 'Percentage of Leave',   xlab = 'Median Tax',   main = 'Leave vs Tax', pch = 0.15 
) 
abline(v = median(leave$Median_Tax,), lty = 2) 
legend('topright', 'Median Tax', lty = 2, bty = 'n') 

#Principal Component Analysis
par(mfrow = c(2,2)) 
plot(density(leave[, 3]), main = names(leave)[3], xlab = names(leave)[3]) plot(density(leave[, 4]), main = names(leave)[4], xlab = names(leave)[4]) plot(density(leave[, 5]), main = names(leave)[5], xlab = names(leave)[5]) plot(density(leave[, 6]), main = names(leave)[6], xlab = names(leave)[6]) pc_leave <- prcomp(leave[,-1], center = T, scale. = T) pc_leave_var <- pc_leave$sdev^2 pc_leave_var 
pc_leave_PEV <- pc_leave_var / sum(pc_leave_var) pc_leave_PEV png(file = "percent") plot(pc_leave) dev.off() 
plot(   cumsum(pc_leave_PEV), 
        ylim = c(0,1),   xlab = 'PC',   ylab = 'Cumulative PEV',   pch = 20,   col = 'orange' 
) 
abline(h = 0.8, col = 'red', lty = 'dashed') pc_leave_loadings <- pc_leave$rotation pc_leave_loadings 

colvector = c('red', 'orange', 'yellow', 'green', 'cyan', 'blue') labvector = c('PC1', 'PC2', 'PC3') barplot(   pc_leave_loadings[,c(1:3)], 
                                                                                                                beside = T, 
                                                                                                                yaxt = 'n',   names.arg = labvector,   col = colvector,   ylim = c(-1,1),   border = 'white', 
                                                                                                                ylab = 'loadings' 
) 
axis(2, seq(-1,1,0.1)) legend(   'bottomright',   bty = 'n',   col = colvector, 
                                 pch = 15, 
                                 row.names(pc_leave_loadings) 
) 
par(mfrow = c(3,1)) biplot(   pc_leave,   scale = 0, 
                              col = c('grey40','orange') 
) 
#Biplot
biplot(   pc_leave,   choices = c(1,3),   scale = 0,   col = c('grey40','orange') 
) 
biplot(   pc_leave,   choices = c(2,3),   scale = 0, 
          col = c('grey40','orange') 
) 
leave_clear <- leave[,-c(2,8,10,11,12,14,7)] summary(pc_leave_clear) 
pc_leave_clear <- prcomp(leave_clear[,-1], center = T, scale. = T) 
pc_leave_var <- pc_leave_clear$sdev^2 pc_leave_var 
pc_leave_PEV <- pc_leave_var / sum(pc_leave_var) pc_leave_PEV plot(pc_leave_clear) png(file = "percent") plot(   cumsum(pc_leave_PEV), 
                                                                                                                 ylim = c(0,1),   xlab = 'PC',   ylab = 'Cumulative PEV',   pch = 20, 
                                                                                                                 col = 'orange' 
) 
abline(h = 0.8, col = 'red', lty = 'dashed') pc_leave_loadings <- pc_leave_clear$rotation pc_leave_loadings opar <- par() 
colvector = c('red', 'orange', 'yellow', 'green', 'cyan', 'blue') labvector = c('PC1', 'PC2', 'PC3') barplot(   pc_leave_loadings[,c(1:3)], 
                                                                                                                beside = T, 
                                                                                                                yaxt = 'n',   names.arg = labvector,   col = colvector,   ylim = c(-1,1),   border = 'white',   ylab = 'loadings' 
) 
axis(2, seq(-1,1,0.1)) legend(   'bottomright',   bty = 'n',   col = colvector, 
                                 pch = 15, 
                                 row.names(pc_leave_loadings) 
) 
par(mfrow = c(2,2)) png(file = "percent") biplot(   pc_leave_clear,   scale = 0, 
                                                    col = c('grey40','orange') 
) 
biplot(   pc_leave_clear,   choices = c(1,3),   scale = 0, 
          col = c('grey40','orange') 
) 
biplot(   pc_leave_clear,   choices = c(2,3),   scale = 0, 
          col = c('grey40','orange') 
) 
pca3d::pca3d(pc_leave_clear, show.labels = T) 

summary(pc_leave) 
summary(pc_leave_clear) 

plot(pc_leave, type = "l") 
#Clustering 
#Hierarchical Clustering
dist_leave <- dist(leave_clear[,-1], method = 'euclidian') 
#   then apply complete linkage png(file = "percent") 
hc_leave <- hclust(dist_leave, method = 'ward.D') hc_leave 

dist_leave <- dist(leave_clear[,-1], method = 'euclidian') hc_leave_complete <- hclust(dist_leave, method = 'complete') 
#Plot the associated dendrogram 

plot(hc_leave, hang = -0.1, labels = leave_clear$Area)  png(file = "percent") 
plot(hc_leave_complete, hang = -0.1, labels = leave_clear$Area) 
Evaluation of cluster results 
#Silhouette Plot
sil_hc_leave <- cluster::silhouette(hc_cluster_id_leave, dist_leave) 
sil_hc_leave_complete <- cluster::silhouette(hc_cluster_id_leave_complete, dist_leave) 

opar <- par() par(mfrow = c(2,1)) png(file = "percent") plot(sil_hc_leave) png(file = "percent") plot(sil_hc_leave_complete) par(opar) 

leave_clear["Outcome"] <- 0 
leave_clear$Outcome[ leave_clear$Pct_Leave > 50] <- "1" leave_clear$Outcome[ leave_clear$Pct_Leave < 50] <- "0" 
leave_clear$Outcome <- as.numeric(leave_clear$Outcome) 

#Neural network 
set.seed(2018) MinMax <- function(x){   tx <- (x - min(x)) / (max(x) - min(x))   return(tx) 
} 
leave_minmax <- apply(leave_clear[,-1], 2, MinMax) 
leave_minmax <- as.data.frame(leave_minmax) n_rows <- nrow(leave_minmax) 
training_idx <- sample(n_rows, n_rows * 0.7) training_leave_minmax <- leave_minmax[training_idx,] test_leave <- leave_minmax[-training_idx,] 
leave_formula = Outcome ~ Mean_Income + Median_Income + Mean_Tax + Median_Tax + Pct_Turnout + C + Lab + G reen + UKIP + LD 
png(file = "p") 
#Library neural net
#Trying several nn architecture 
leave_nn_64 <- neuralnet(leave_formula, hidden = c(1,2), data = training_leave_minmax,linear.output = FALSE) leave_nn_65 <- neuralnet(leave_formula, hidden = c(2,1), data = training_leave_minmax,linear.output = FALSE) leave_nn_66 <- neuralnet(leave_formula, hidden = c(1,1), data = training_leave_minmax, linear.output = FALSE) leave_nn_67 <- neuralnet(leave_formula, hidden = c(2,2), data = training_leave_minmax,linear.output = FALSE) leave_nn_68 <- neuralnet(leave_formula, hidden = c(2,3), data = training_leave_minmax,linear.output = FALSE) leave_nn_69 <- neuralnet(leave_formula, hidden = c(4,4), data = training_leave_minmax, linear.output = FALSE) pred_leave_nn_64 <- compute(leave_nn_64, test_leave[,-13]) pred_leave_nn_65 <- compute(leave_nn_65, test_leave[,-13]) pred_leave_nn_66 <- compute(leave_nn_66, test_leave[,-13]) pred_leave_nn_67 <- compute(leave_nn_67, test_leave[,-13]) pred_leave_nn_68 <- compute(leave_nn_68, test_leave[,-13]) pred_leave_nn_69 <- compute(leave_nn_69, test_leave[,-13]) 
#Evaluating nn results
leave_results <- data.frame(   actual = test_leave$Outcome,   nn_64 = pred_leave_nn_64$net.result,   nn_65 = pred_leave_nn_65$net.result, 
                               nn_66 = pred_leave_nn_66$net.result 
) 
leave_results <- data.frame(   actual = test_leave$Outcome,   nn_67 = pred_leave_nn_67$net.result,   nn_68 = pred_leave_nn_68$net.result, 
                               nn_69 = pred_leave_nn_69$net.result 
) 
cor(leave_results[,'actual'], leave_results[,c("nn_64","nn_65", "nn_66")]) cor(leave_results[,'actual'], leave_results[,c("nn_67","nn_68", "nn_69")]) leave_results <- data.frame(   actual = test_leave$Outcome,   nn_65 = pred_leave_nn_65$net.result 
) 
leave_results <- data.frame(   actual = test_leave$Outcome,   nn_65 = pred_leave_nn_65$net.result 
) 

temptest <- subset(test_leave, select = c("Mean_Income", "Median_Income", "Mean_Tax", "Median_Tax", "Pct_Turn out", "C", "Lab", "Green", "UKIP", "LD")) 
head(temptest) 
nn_results <- compute(leave_nn_65,temptest) nn_results <- compute(leave_nn_69,temptest) nn_results 
results <- data.frame(actual = test_leave$Outcome, prediction = nn_results$net.result) results 
roundedresults <- sapply(results, round,digits=0) roundedresultsdf = data.frame(roundedresults) table(leave_results) 

predict_testNN = (pred_leave_nn_65$net.result * (max(test_leave$Outcome) - min(test_leave$Outcome))) + min(tes t_leave$Outcome) 
predict_testNN = (pred_leave_nn_64$net.result * (max(test_leave$Outcome) - min(test_leave$Outcome))) + min(tes t_leave$Outcome) 
predict_testNN = (pred_leave_nn_66$net.result * (max(test_leave$Outcome) - min(test_leave$Outcome))) + min(tes t_leave$Outcome) 
predict_testNN = (pred_leave_nn_67$net.result * (max(test_leave$Outcome) - min(test_leave$Outcome))) + min(tes t_leave$Outcome) 
predict_testNN = (pred_leave_nn_68$net.result * (max(test_leave$Outcome) - min(test_leave$Outcome))) + min(tes t_leave$Outcome) 
predict_testNN = (pred_leave_nn_69$net.result * (max(test_leave$Outcome) - min(test_leave$Outcome))) + min(tes t_leave$Outcome) 

plot(test_leave$Outcome, predict_testNN, col='blue', pch=16, ylab = "Predicted Outcome NN", xlab = "Real Outcome
") 

abline(0,1) 

#Calculating RMSE and MAE for nn performance evaluation
RMSE.NN = (sum((test_leave$Outcome - predict_testNN)^2) / nrow(test_leave)) ^ 0.5 RMSE.NN 

1-mae(pred_leave_nn_64$net.result * (max(test_leave$Outcome) - min(test_leave$Outcome))) + min(test_leave$Ou tcome) 
1-mae(pred_concrete_nn_65$net.result * (max(test_concrete_minmax$Outcome) - min(test_concrete_minmax$Out come))) + min(test_concrete_minmax$Outcome) 
1-mae(pred_concrete_nn_66$net.result * (max(test_concrete_minmax$Outcome) - min(test_concrete_minmax$Out come))) + min(test_concrete_minmax$Outcome) 
1-mae(pred_concrete_nn_67$net.result * (max(test_concrete_minmax$Outcome) - min(test_concrete_minmax$Out come))) + min(test_concrete_minmax$Outcome) 
1-mae(pred_concrete_nn_68$net.result * (max(test_concrete_minmax$Outcome) - min(test_concrete_minmax$Out come))) + min(test_concrete_minmax$Outcome) 
1-mae(pred_concrete_nn_69$net.result * (max(test_concrete_minmax$Outcome) - min(test_concrete_minmax$Out come))) + min(test_concrete_minmax$Outcome) 



#Create an extra column for classification  

leave_clear["Outcome"] <- 0 
leave_clear$Outcome[ leave_clear$Pct_Leave > 50] <- "Leave" leave_clear$Outcome[ leave_clear$Pct_Leave < 50] <- "Remain" leave_clear$Outcome <- as.factor(leave_clear$Outcome) 

#Decision Tree # Library tree 
set.seed(2018) 
n_rows <- nrow(leave_clear) training_idx <- sample(n_rows, n_rows * 0.7) training_leave <- leave_clear[training_idx,] test_leave <- leave_clear[-training_idx,] 
leave_formula = Outcome ~ Mean_Income + Median_Income + Mean_Tax + Median_Tax + Pct_Turnout + C + Lab + G reen + UKIP + LD 
tree_leave <- tree(leave_formula, data = training_leave) summary(tree_leave) 
plot(tree_leave) text(tree_leave, pretty = 0) 
cv_leave <- cv.tree(tree_leave, FUN=prune.misclass) cv_leave_table <- data.frame(   size = cv_leave$size, 
                                                                                    error = cv_leave$dev 
) 

plot(   cv_leave,   xaxt = 'n', 
        yaxt = 'n' 
) 
axis(1, seq(1,max(cv_leave_table$size))) axis(2, seq(50,150,5)) 
#Prune the decision tree
pruned_tree_size <- cv_leave_table[which.min(cv_leave_table$error), 'size'] pruned_tree_leave <- prune.misclass(tree_leave, best = pruned_tree_size) summary(pruned_tree_leave) 
tree_leave_pred <- predict(tree_leave, test_leave[,-13], type= "class") 
pruned_tree_leave_pred <- predict(pruned_tree_leave, test_leave[,-13], type= "class") leave_results <- data.frame(   actual = test_leave$Outcome,   unpruned = tree_leave_pred,   pruned = pruned_tree_leave_pred) 

unpruned_results_table <- table(leave_results[,c('actual', 'unpruned')]) unpruned_results_table 
pruned_results_table <- table(leave_results[,c('actual', 'pruned')]) pruned_results_table 

acc_unpruned <- sum(diag(unpruned_results_table)) / sum(unpruned_results_table) acc_unpruned 
acc_pruned <- sum(diag(pruned_results_table)) / sum(pruned_results_table) acc_pruned 


#Performance evaluation Decision Tree and Random Forest 

#Library caret 
set.seed(2018) 

leave_formula <- reformulate(names(training_leave[, -13]), response = 'Outcome') 

ctrl_parameters <- trainControl(method = 'CV', number = 10) ctrl_parameters <- trainControl(method = 'CV', number = 30) modelLookup('rpart') 
leave_tree_perf <- train(leave_formula, data = training_leave, method = "rpart", trControl = ctrl_parameters) leave_tree_perf png(file = "p") 
plot(leave_tree_perf) 

modelLookup('rf') 
leave_rf <- train(leave_formula, data = training_leave, method = "rf", trControl = ctrl_parameters) leave_rf plot(leave_rf) leave_tree_predict <-  cbind(   actual = test_leave$Outcome, 
                                                                                                                                                            predicted = predict(leave_tree_perf, test_leave[, -13], type = 'raw'),   predict(leave_tree_perf, test_leave[, -13], type = 'prob') 
) 
leave_rf_predict <-  cbind(   actual = test_leave$Outcome, 
                              predicted = predict(leave_rf, test_leave[, -13], type = 'raw'),   predict(leave_rf, test_leave[, -13], type = 'prob') 
) 
leave_rf_predict 
plot(leave_rf_predict) 

tree_confmat <- confusionMatrix(data = leave_tree_predict$predicted, reference = leave_tree_predict$actual, positi ve = "Leave") 
rf_confmat <- confusionMatrix(data = leave_rf_predict$predicted, reference = leave_rf_predict$actual, positive = "Le ave") 

tree_confmat 
rf_confmat 

leave_models_prob <- data.frame(   tree = leave_tree_predict$Leave, 
                                   rf = leave_rf_predict$Leave 
) 
leave_label <- data.frame(   tree = leave_tree_predict$actual, 
                             rf = leave_rf_predict$actual 
) 

opar <- par() par(pty = 's') 
png(file = "p") 

#Random Forest #Library Random Forest 

print(leave_rf) 

rf_leave <- randomForest(leave_formula, ntree = 500, importance = T, data = training_leave) png(file = "p") plot(rf_leave) 
rf_leave_pred <- predict(rf_leave, test_leave[,-1], type= "class") rf_leave_pred <- predict(rf_leave, training_leave[,-1], type= "class") rf_results_table <- table(rf = rf_leave_pred,  actual = test_leave$Outcome) rf_results_table <- table(rf = rf_leave_pred,  actual = training_leave$Outcome) rf_results_table 
acc_rf <- sum(diag(rf_results_table)) / sum(rf_results_table) acc_rf 

#Performance Evaluation Random Forest 

leave_rf_predict <-  cbind(   actual = test_leave$Outcome,   predicted = predict(rf_leave, test_leave[, -13], type = 'response'),   predict(rf_leave, test_leave[, -13], type = 'response') 
) 
plot(leave_rf_predict) 
#Support vector machines 
qplot(Percent.Leave, Median_Income, data = leave_clear, colour = Outcome) qplot(C, Lab, data = leave_clear, colour = Outcome) 

#SVMs
#Kernel linear  
#Library e1071 

mymodel <- svm(Outcome ~ Mean_Income + Median_Income + Mean_Tax + Median_Tax + Pct_Turnout + C + Lab + G reen + UKIP + LD, data = leave_clear, kernel = "linear") 
mymodel <- svm(Outcome ~ Mean_Income + Median_Income + Mean_Tax + Median_Tax + Pct_Turnout + C + Lab + G reen + UKIP + LD, data = leave_clear, kernel = "polynomial") 
mymodel <- svm(Outcome ~ Mean_Income + Median_Income + Mean_Tax + Median_Tax + Pct_Turnout + C + Lab + G reen + UKIP + LD, data = leave_clear, kernel = "radial") 

summary(mymodel) 
plot(mymodel , data = leave_clear, 
     Median_Income~LD, 
) 
pred <- predict(mymodel, leave_clear) 
tab <- table(Predicted = pred, Actual = leave_clear$Outcome) tab 
1-sum(diag(tab)/sum(tab)) 
sum(diag(tab)/sum(tab)) 

#Kernel Stigmoid 

mymodel <- svm(Outcome ~ Mean_Income + Median_Income + Mean_Tax + Median_Tax + Percent.Turnout + C + La b + Green + UKIP + LD, data = leave_clear, kernel = "sigmoid") summary(mymodel) 
plot(mymodel , data = leave_clear, 
     Median_Income~LD, 
) 
pred <- predict(mymodel, leave_clear) 
tab <- table(Predicted = pred, Actual = leave_clear$Outcome) tab 
sum(diag(tab)/sum(tab)) 1-sum(diag(tab)/sum(tab)) 

#Tuning 

set.seed(2018) 
tmodel <- tune(svm,Outcome ~ Mean_Income + Median_Income + Mean_Tax + Median_Tax + Pct_Turnout + C + Lab + Green + UKIP + LD, data = leave_clear, ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:5))) 

plot(tmodel) summary(tmodel) 
bestmodel <- tmodel$best.model summary(bestmodel) 
png(file = "p") 
plot(bestmodel , data = leave_clear, 
     C~Pct_Leave, 
) 
pred <- predict(mymodel, leave_clear) 
tab <- table(Predicted = pred, Actual = leave_clear$Outcome) tab 
1-sum(diag(tab)/sum(tab)) 
sum(diag(tab)/sum(tab)) 
p1 <- predict(rf_leave, data = training_leave) 
head(p1) 
head(training_leave$Outcome) confusionMatrix(p1, training_leave$Outcome) p2 <- predict(rf_leave, test_leave) confusionMatrix(p2, test_leave$Outcome) 
plot(rf_leave) 
legend('topright', colnames(rf_leave$err.rate), bty = 'n', lty = c(1,2,3), col = c(1:3)) png(file = "p") 
varImpPlot(rf_leave, type = 1) 

