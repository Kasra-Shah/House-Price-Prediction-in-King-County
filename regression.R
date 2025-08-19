#libraries:
library(dplyr)
library(ggplot2)
library(GGally)
library(tidyverse)
library(tibble)
library(caret)
library(rpart)
library(rpart.plot)
#------------------------
#importing data:
kc_data = read.csv("C:\\Users\\Salar_System\\Desktop\\kingcounty\\kingcounty\\kc_house_data.csv")
view(head(kc_data))
kc_data = kc_data[,3:21]
#--------------------
#splitting data:
set.seed(42)
train_index = sample(nrow(kc_data), size = 0.8*nrow(kc_data))
kc_train = kc_data[train_index,]
kc_test = kc_data[-train_index,]

est_index = sample(nrow(kc_train), size = 0.8*nrow(kc_train))
kc_est = kc_train[est_index,]
kc_valid = kc_train[-est_index,]
#--EDA------------------------------------
#EDA:
skimr :: skim(kc_est)
str(kc_est)

m = cor(kc_est)
corrplot::corrplot(m,method = 'ellipse', order = 'AOE', type = 'upper')
qplot(y = long, x = lat, data = kc_est, 
      alpha = price, main = "KC City Limits ")
#----------------
#RMSE calculator function:
calc_rmse = function(actual,predicted){
  sqrt(mean((actual-predicted)^2))
}
#----REGRESSION------------------------------------
#Part ONE - LINEAR REGRESSION:
#creating models:
#forward selection is being used to reach some good models:
lm_model1_est = lm(price ~ sqft_living ,data = kc_est)
predict_lm_model1_val= predict(lm_model1_est, kc_valid)
rmse_lm_model1_val = calc_rmse(kc_valid$price, predict_lm_model1_val)

lm_model2_est = lm(price ~ sqft_living + grade ,data = kc_est)
predict_lm_model2_val= predict(lm_model2_est, kc_valid)
rmse_lm_model2_val = calc_rmse(kc_valid$price, predict_lm_model2_val)

lm_model3_est = lm(price ~ sqft_living + grade + sqft_above ,data = kc_est)
predict_lm_model3_val= predict(lm_model3_est, kc_valid)
rmse_lm_model3_val = calc_rmse(kc_valid$price, predict_lm_model3_val)

lm_model4_est = lm(price ~ sqft_living + grade + sqft_above + sqft_living15 ,data = kc_est)
predict_lm_model4_val= predict(lm_model4_est, kc_valid)
rmse_lm_model4_val = calc_rmse(kc_valid$price, predict_lm_model4_val)

lm_model5_est = lm(price ~ sqft_living + grade + sqft_above + sqft_living15 + bathrooms ,data = kc_est)
predict_lm_model5_val= predict(lm_model5_est, kc_valid)
rmse_lm_model5_val = calc_rmse(kc_valid$price, predict_lm_model5_val)

lm_model5_est = lm(price ~ sqft_living + grade + sqft_above + sqft_living15 + bathrooms ,data = kc_est)
predict_lm_model5_val= predict(lm_model5_est, kc_valid)
rmse_lm_model5_val = calc_rmse(kc_valid$price, predict_lm_model5_val)

lm_model6_est = lm(price ~ sqft_living + grade + sqft_above + sqft_living15 + bathrooms + view,data = kc_est)
predict_lm_model6_val= predict(lm_model6_est, kc_valid)
rmse_lm_model6_val = calc_rmse(kc_valid$price, predict_lm_model6_val)

lm_model7_est = lm(price ~ . ,data = kc_est)
predict_lm_model7_val= predict(lm_model7_est, kc_valid)
rmse_lm_model7_val = calc_rmse(kc_valid$price, predict_lm_model7_val)

lm_model8_est = lm(price ~ . + poly(sqft_living, 2),data = kc_est)
predict_lm_model8_val= predict(lm_model8_est, kc_valid)
rmse_lm_model8_val = calc_rmse(kc_valid$price, predict_lm_model8_val)

lm_model9_est = lm(price ~ . + poly(sqft_living, 2) + poly(grade,2),data = kc_est)
predict_lm_model9_val= predict(lm_model9_est, kc_valid)
rmse_lm_model9_val = calc_rmse(kc_valid$price, predict_lm_model9_val)

lm_model10_est = lm(as.formula(
  paste('price ~.+poly(sqft_living, 4) +',paste('poly(',colnames(kc_est[-c(1,7)]),',3)',collapse = ' + ')))
  ,data = kc_est)
predict_lm_model10_val= predict(lm_model10_est, kc_valid)
rmse_lm_model10_val = calc_rmse(kc_valid$price, predict_lm_model10_val)

lm_model11_est = lm(as.formula(
  paste('price ~.+poly(sqft_living, 4) + (sqft_living*grade) + 
        (grade*sqft_above) +',
        paste('poly(',colnames(kc_est[-c(1,7)]),',3)',collapse = ' + ')))
  ,data = kc_est)
predict_lm_model11_val= predict(lm_model11_est, kc_valid)
rmse_lm_model11_val = calc_rmse(kc_valid$price, predict_lm_model11_val)

lm_model12_est = lm(as.formula(
  paste('price ~.+poly(sqft_living, 4) + (sqft_living*grade) + 
        (grade*sqft_above) + (sqft_above*bathrooms) +',
        paste('poly(',colnames(kc_est[-c(1,7)]),',3)',collapse = ' + ')))
  ,data = kc_est)
predict_lm_model12_val= predict(lm_model12_est, kc_valid)
rmse_lm_model12_val = calc_rmse(kc_valid$price, predict_lm_model12_val)

lm_model13_est = lm(as.formula(
  paste('price ~.+poly(sqft_living, 4) + (sqft_living*grade) + 
        (grade*sqft_above) + (sqft_above*bathrooms) + (grade*bathrooms) +',
        paste('poly(',colnames(kc_est[-c(1,7)]),',3)',collapse = ' + ')))
  ,data = kc_est)
predict_lm_model13_val= predict(lm_model13_est, kc_valid)
rmse_lm_model13_val = calc_rmse(kc_valid$price, predict_lm_model13_val)

lm_model14_est = lm(as.formula(
  paste('price ~.+poly(sqft_living, 4) + (sqft_living*grade) + 
        (grade*sqft_above) + (sqft_above*bathrooms) + (grade*bathrooms) + (view*bathrooms) +',
        paste('poly(',colnames(kc_est[-c(1,7)]),',3)',collapse = ' + ')))
  ,data = kc_est)
predict_lm_model14_val= predict(lm_model14_est, kc_valid)
rmse_lm_model14_val = calc_rmse(kc_valid$price, predict_lm_model14_val)

lm_model15_est = lm(as.formula(
  paste('price ~.+poly(sqft_living, 4) + (sqft_living*grade) + 
        (grade*sqft_above) + (sqft_above*bathrooms) + (grade*bathrooms) + (view*bathrooms) +
        (view*sqft_living) + (view*grade) + (view*sqft_living15) + (waterfront*sqft_living) +
        (lat*sqft_living) + (lat*grade) + (waterfront*sqft_above) +',
        paste('poly(',colnames(kc_est[-c(1,7)]),',3)',collapse = ' + ')))
  ,data = kc_est)
predict_lm_model15_val= predict(lm_model15_est, kc_valid)
rmse_lm_model15_val = calc_rmse(kc_valid$price, predict_lm_model15_val)

#creating a table of chosen models and their results:
#creating a list of models:
lm_models_list = list(
  lm_model_lin = lm(price ~ . ,data = kc_est),
  lm_model_quad = lm(as.formula(
    paste('price ~.+poly(sqft_living, 4) +',paste('poly(',colnames(kc_est[-c(1,7)]),',3)',collapse = ' + ')))
    ,data = kc_est),
  lm_model_intract = lm(as.formula(
    paste('price ~.+poly(sqft_living, 4) + (sqft_living*grade) + 
        (grade*sqft_above) + (sqft_above*bathrooms) + (grade*bathrooms) + (view*bathrooms) +
        (view*sqft_living) + (view*grade) + (view*sqft_living15) + (waterfront*sqft_living) +
        (lat*sqft_living) + (lat*grade) + (waterfront*sqft_above) +',
          paste('poly(',colnames(kc_est[-c(1,7)]),',3)',collapse = ' + ')))
    ,data = kc_est)
)
#fitting models and RMSE calculation:
predict_lm_validation = lapply(lm_models_list ,predict, kc_valid)
RMSE_lm_validarion = data.frame(lapply(predict_lm_validation, calc_rmse, kc_valid$price))
RMSE_lm_validarion = gather(RMSE_lm_validarion)
colnames(RMSE_lm_validarion) = c("model", "RMSE_validation")

predict_lm_estimation = lapply(lm_models_list ,predict, kc_est)
RMSE_lm_estimation = data.frame(lapply(predict_lm_estimation, calc_rmse, kc_valid$price))
RMSE_lm_estimation = gather(RMSE_lm_estimation)
colnames(RMSE_lm_estimation) = c("model", "RMSE_estimation")

#creating a table:
RMSE_lm = cbind(RMSE_lm_validarion,RMSE_lm_estimation$RMSE_estimation)
colnames(RMSE_lm) = c("model","RMSE_validation","RMSE_estimation")
RMSE_lm$predictors = ""

RMSE_lm[1,4] = "all varibales with first degree"
RMSE_lm[2,4] = "all first degree and second degree + 'sqft_living' 4th and 3rd degree"
RMSE_lm[3,4] = "same befor + interaction between variables "

#final result:
view(RMSE_lm)
final_lm_model = lm(as.formula(
  paste('price ~.+poly(sqft_living, 4) + (sqft_living*grade) + 
        (grade*sqft_above) + (sqft_above*bathrooms) + (grade*bathrooms) + (view*bathrooms) +
        (view*sqft_living) + (view*grade) + (view*sqft_living15) + (waterfront*sqft_living) +
        (lat*sqft_living) + (lat*grade) + (waterfront*sqft_above) +',
        paste('poly(',colnames(kc_est[-c(1,7)]),',3)',collapse = ' + ')))
  ,data = kc_train)
predict_final_lm= predict(final_lm_model, kc_test)
RMSE_final_lm = calc_rmse(kc_test$price, predict_final_lm)
max(kc_test$price)
sd(kc_test$price)

reg_final_plot_df = data.frame(actual = kc_test$price,predicted = predict_final_lm)

reg_final_plot_df %>%
  ggplot(aes(x = actual))+
  geom_point(aes(y = predicted),color = "darkgreen")+
  geom_abline(slope = 1)+
  labs(title = "acual price VS predicted price - Regression")
#best model: the one with interactions.
#----KNN---------------------------------------------------------------
#PART TWO - KNN:
#making normalized data frames:
#estimation data normalization:
kc_est_scaled = kc_est
kc_est_scale_center = data.frame(matrix(nrow = 2, ncol = length(colnames(kc_est_scaled))),
                                 row.names = c("center","scale"))
colnames(kc_est_scale_center) = colnames(kc_est_scaled)
for(i in 2:length(colnames(kc_est_scaled))){
  kc_est_scaled[,i] = scale(kc_est_scaled[,i])
  kc_est_scale_center[1,i]= attr(kc_est_scaled[,i], "scaled:center")
  kc_est_scale_center[2,i]= attr(kc_est_scaled[,i], "scaled:scale")
}
#validation data normalization:
kc_val_scaled = kc_valid
kc_val_scale_center = data.frame(matrix(nrow = 2, ncol = length(colnames(kc_val_scaled))),
                                 row.names = c("center","scale"))
colnames(kc_val_scale_center) = colnames(kc_val_scaled)
for(i in 2:length(colnames(kc_val_scaled))){
  kc_val_scaled[,i] = scale(kc_val_scaled[,i], center = kc_est_scale_center[1,i],scale = kc_est_scale_center[2,i])
}
#train data normalization:
kc_train_scaled = kc_train
kc_train_scale_center = data.frame(matrix(nrow = 2, ncol = length(colnames(kc_train_scaled))),
                                 row.names = c("center","scale"))
colnames(kc_train_scale_center) = colnames(kc_train_scaled)
for(i in 2:length(colnames(kc_train_scaled))){
  kc_train_scaled[,i] = scale(kc_train_scaled[,i])
  kc_train_scale_center[1,i]= attr(kc_train_scaled[,i], "scaled:center")
  kc_train_scale_center[2,i]= attr(kc_train_scaled[,i], "scaled:scale")
}
#test data normalization:
kc_test_scaled = kc_test
kc_test_scale_center = data.frame(matrix(nrow = 2, ncol = length(colnames(kc_test_scaled))),
                                   row.names = c("center","scale"))
colnames(kc_test_scale_center) = colnames(kc_test_scaled)
for(i in 2:length(colnames(kc_test_scaled))){
  kc_test_scaled[,i] = scale(kc_test_scaled[,i],center = kc_train_scale_center[1,i],scale = kc_train_scale_center[2,i])
}
#normalized learn:
names = c(paste("knn_model_norm_",1:100,sep = ""))

knn_model_norm_list = list()
for (i in 1:100) {
  knn_model_norm_list[[i]] = knnreg(price ~ . ,data =kc_est_scaled, k = i)
}

#normalized prediction:
knn_norm_val_predict = lapply(knn_model_norm_list, predict, kc_val_scaled)
knn_norm_rmse = sapply(knn_norm_val_predict, calc_rmse, kc_val_scaled$price)
knn_rmse_df = data.frame(k=c(1:100),normal_rmse=knn_norm_rmse)
view(knn_rmse_df)
#not normal learn:
knn_models_list_not = list()
for (i in 1:100) {
  knn_models_list_not[[i]] = knnreg(price ~ . ,data =kc_est, k = i)
}

#not normalized prediction:
knn_not_val_predict = lapply(knn_models_list_not, predict, kc_valid)
knn_not_rmse = sapply(knn_not_val_predict, calc_rmse, kc_valid$price)
knn_rmse_df$not_normal_rmse = knn_not_rmse
view(knn_rmse_df)

knn_rmse_df %>%
  ggplot(aes(x = k))+
  geom_line(aes(y = normal_rmse), size = 1)

#normalized is better!
paste("best k value in knn models is: ",which(knn_rmse_df$normal_rmse == min(knn_rmse_df$normal_rmse), arr.ind = TRUE))

final_knn_model = knnreg(price ~ . ,data =kc_train_scaled, k = 13) 
predict_final_knn = predict(final_knn_model, kc_test_scaled)
RMSE_final_knn = calc_rmse(kc_test_scaled$price , predict_final_knn)

knn_final_plot_df = data.frame(actual = kc_test_scaled$price , predicted = predict_final_knn)
knn_final_plot_df %>%
  ggplot(aes(x = actual))+
  geom_point(aes(y = predicted),color = "darkblue")+
  geom_abline(slope = 1)+
  labs(title = "acual price VS predicted price - KNN")

#----TREE-----------------------------------------------------------------
#PART THREE:
tree_model_list = list(
  model_1_5 = rpart(price ~ .,data = kc_est, cp = 0.001, minsplit = 5),
  model_2_5 = rpart(price ~ .,data = kc_est, cp = 0.01, minsplit = 5),
  model_3_5 = rpart(price ~ .,data = kc_est, cp = 0.02, minsplit = 5),
  model_4_5 = rpart(price ~ .,data = kc_est, cp = 0.1, minsplit = 5),
  model_5_5 = rpart(price ~ .,data = kc_est, cp = 0.2, minsplit = 5),
  model_1_20 = rpart(price ~ .,data = kc_est, cp = 0.001, minsplit = 20),
  model_2_20 = rpart(price ~ .,data = kc_est, cp = 0.01, minsplit = 20),
  model_3_20 = rpart(price ~ .,data = kc_est, cp = 0.02, minsplit = 20),
  model_4_20 = rpart(price ~ .,data = kc_est, cp = 0.1, minsplit = 20),
  model_5_20 = rpart(price ~ .,data = kc_est, cp = 0.2, minsplit = 20)
)

#predicting:
tree_predict = lapply(tree_model_list,predict,kc_valid)
tree_rmse = data.frame(lapply(tree_predict, calc_rmse, kc_valid$price))
tree_rmse = gather(tree_rmse)
colnames(tree_rmse) = c("model","RMSE")
tree_rmse$cp = c(0.001,0.01,0.02,0.1,0.2,0.001,0.01,0.02,0.1,0.2)
tree_rmse$minsplit = c(5,5,5,5,5,20,20,20,20,20)
view(tree_rmse)
#BEST MODEL:
tree_rmse[which(tree_rmse$RMSE == min(tree_rmse$RMSE), arr.ind = TRUE), ]
#tree graph:
rpart.plot(tree_model_list$model_1_5)

final_tree5_model = rpart(price ~ .,data = kc_train, cp = 0.001, minsplit = 5)
predict_final_tree5 = predict(final_tree5_model, kc_test)
RMSE_final_tree5 = calc_rmse(kc_test$price , predict_final_tree5)

tree_final_plot_df = data.frame(actual = kc_test$price , predicted = predict_final_tree5)
tree_final_plot_df %>%
  ggplot(aes(x = actual))+
  geom_point(aes(y = predicted),color = "darkred")+
  geom_abline(slope = 1)+
  labs(title = "acual price VS predicted price - Tree")
#------------------------------------
#part four:
contrast_plot_df_lm = data.frame(predict=predict_lm_validation$lm_model_intract,
                                 actual=kc_valid$price,
                                 type = "lm")
contrast_plot_df_knn = data.frame(predict = unlist(knn_norm_val_predict[11]),
                                  actual=kc_valid$price,
                                  type = "knn")
contrast_plot_df_tree = data.frame(predict = tree_predict$model_1_5,
                                   actual=kc_valid$price,
                                   type = "tree")
contrast_plot_df = rbind(contrast_plot_df_lm,contrast_plot_df_knn,contrast_plot_df_tree)
#plotting best models:
contrast_plot_df %>%
  ggplot(aes(x=actual))+
  geom_point(aes(y = predict),color = "darkgreen")+
  geom_abline(slope = 1)+
  facet_wrap(vars(type),dir = "h")





