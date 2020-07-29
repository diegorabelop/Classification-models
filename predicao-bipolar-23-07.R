# Importar o banco de dados##
library(haven)
banco_pelotas <- read_sav("C:/Users/TB e Internet/Downloads/banco_pelotas_30-04-VERSAO2.sav")

#remove missing in bipolar1
library(dplyr)
dt <- banco_pelotas[!is.na(banco_pelotas$bipolar1),]
#propor??o de TB
prop.table(table(dt$bipolar1)) 
#### excluir variaveis Deixei as subescalas SDQ dos pais e filhos aos 11 anos
#dt1 <- dt[-c(1:2, 3, 5, 6:7,11:13, 15:16, 18:20, 22:24, 27, 30:37, 40, 46:47, 49:56, 
#110:111, 117:119, 125:130, 133, 135:138, 140, 142:143, 145:156, 190:191, 193:196, 198:201, 203, 206:215, 222:231, 
#234:237, 243, 245:247, 248:258, 260:261, 270, 273:429, 435:437, 440:441, 449:453, 455, 458:459, 
#461:466, 468:471, 473:514, 515:526, 527:535, 537, 539, 541, 543, 545, 547, 
#549, 551, 553, 555:596, 598, 600, 602, 604:748, 749:893, 895:920, 925:930, 936, 942,
#949, 951:952, 957:958, 970:971, 975:976, 978:979, 983:985, 999, 1001, 1005, 1012: 1015, 1020)]

dt1 <- dt[,c(1034, 1, 238:242, 244, 259:271, 286, 258, 3, 342:360, 375, 377:424, 438:450, 953:956, 959:962, 963,986, 1023:1025)]
prop.table(table(dt1$bipolar1))

####Criando novas variaveis das drogas #####
sumFeatures <- function(x, ...){
  l <- length(x[is.na(x)]) 
  if (l == length(x)) y <- NA else y <- sum(x[!is.na(x)])
  return(y)
}

##Solventes
df_solvente <-data.frame(dt1$kc10, dt1$kc16)
solvente_lifetime <- apply(df_solvente, 1, sumFeatures)
solvente_lifetime[solvente_lifetime > 1] <- 1
df_solvente$solvente_lifetime <- solvente_lifetime
##Cocaina
df_cocaina <-data.frame(dt1$kc06, dt1$kc09, dt1$kc13, dt1$kc14)
cocaina_lifetime <- apply(df_cocaina, 1, sumFeatures)
cocaina_lifetime[cocaina_lifetime > 1] <- 1
df_cocaina$cocaina_lifetime <- cocaina_lifetime
##Inserindo colunas novas
dt1$solvente_lifetime <- solvente_lifetime
dt1$cocaina_lifetime <- cocaina_lifetime
##excluindo as variaveis de drogas usadas pra criar essas
dt1$kc06 <- NULL
dt1$kc09 <- NULL
dt1$kc13 <- NULL
dt1$kc14 <- NULL
dt1$kc10 <- NULL
dt1$kc16 <- NULL
dt1$nquest <- NULL
##Variavel Lmania
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(dt1,2,pMiss)

obj_variavel <- apply(dt1,2,pMiss)
variavel_8poc <- obj_variavel[obj_variavel < 8] ##selecionando so as com 7% de missing
variavel_8poc <- names(variavel_8poc)
library(stats) #selecionei colunas com 5% missing
dt2 <- dt1 [,c(variavel_8poc)]
pMiss_dataset2 <- function(x){sum(is.na(x))/length(x)*100} #Ja ta multiplicado por 100
apply(dt2,2,pMiss_dataset2)


# Ver as classes das variaveis
library(purrr)
map_chr(dt2, class)
#apply(dt2,2,pMiss_dataset2)
dt2$hamiltonscore <- NULL
# Ver as classes das variaveis
library(purrr)
map_chr(dt2, class)
##Mudar os fatores da classe pq tem variavel com "haven_label" CONVERSAO DE VARIAVEL
dt3 <- dt2
library(purrr)
dt3 <- map_df(dt3, function(x){
  if (length(levels(as.factor(x))) > 8) z <- as.numeric(x) else z <- as.factor(x)}
)
map_chr(dt3, class)

#transformando em numerico AS VARIAVEIS NÃO PEGAS PELA FUNÇAO
library(magrittr)
dt3$ZkSRQdepre_anx <- as.numeric(dt3$ZkSRQdepre_anx)
dt3$ZkSRQsomatic<- as.numeric(dt3$ZkSRQsomatic)
dt3$ZkSQRenergia <- as.numeric(dt3$ZkSQRenergia)
dt3$ZkSRQsentidepre <- as.numeric(dt3$ZkSRQsentidepre)


map_chr(dt3, class)
str(dt3)
dt3$hpconduct <- NULL
dt3$hphyper  <- NULL
dt3$hppeer <- NULL
dt3$hpprosoc <- NULL 
dt3$ZSco01 <- NULL
dt3$LSD <- NULL


### vARIAVEL DROGA QUE ERA PRA EXCLUIR
##Excluindo variaveis de drogas RELACIONADAS 
dt3$hc01 <- NULL
dt3$jc01 <- NULL
dt3$kfumar <- NULL
dt3$kfumante <- NULL
dt3$kc11 <- NULL
dt3$kc12 <- NULL
dt3$kc15 <- NULL
dt3$hcsolvente <- NULL
dt3$jcsolvente <- NULL
dt3$kc10 <- NULL 
dt3$hc07 <- NULL
dt3$jc06 <- NULL
dt3$kbebalc <- NULL
dt3$jc11c <- NULL
dt3$kc06 <- NULL
dt3$kc09 <- NULL
dt3$kc12 <- NULL 
dt3$kc13 <- NULL
dt3$kc14 <- NULL
dt3$jc11d <- NULL
dt3$kc05 <- NULL
dt3$Zjc02 <- NULL
### HOLD OUT onda 4 ####
# Separar em banco de dados em Treino e Teste 
library(caret)
set.seed(123)
datasetbipolar <- dt3 [sample (1:nrow (dt3), 3781, replace=FALSE), ]


## banco treino e teste ##
library(caret)
set.seed(123)
intrain <- createDataPartition(y = datasetbipolar$bipolar1, p= 0.7, list = FALSE)
dtTrain <-  datasetbipolar[intrain,]
dtTest <- datasetbipolar[-intrain,]
dim(dtTrain); dim(dtTest)
# Porcentagem do desfecho em cada banco
prop.table(table(datasetbipolar$bipolar1))
prop.table(table(dtTrain$bipolar1))
prop.table(table(dtTest$bipolar1))
#iMPUTA??O USANDO media e moda  no treino pra aplicar depois no teste, evitando data linkage

train_matrix <- dtTrain
test_matrix <- dtTest

train_matrix <- as.data.frame(train_matrix)
test_matrix <- as.data.frame(test_matrix)
class(train_matrix)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

for (i in names(train_matrix)){
  print(class(train_matrix[,i]))
  print(is.factor(train_matrix[,i]))
  if (is.factor(train_matrix[,i])) {
    
    mode_value = getmode(train_matrix[,i])
    print("cat")
    #recorded_values[1,i] = mode_value
    train_matrix[is.na(train_matrix[,i]),i] = mode_value
    test_matrix[is.na(test_matrix[,i]),i] = mode_value
    print(mode_value)
  } else {
    mean_value = mean(train_matrix[,i], na.rm=TRUE)
    #recorded_values[1,i] = mean_value
    train_matrix[is.na(train_matrix[,i]),i] = mean_value
    test_matrix[is.na(test_matrix[,i]),i] = mean_value
    print("num")
  }
  
}

map_int(test_matrix, function(x){length(x[is.na(x)])})
str(train_matrix)
anyNA(train_matrix)
#train_matrix$lobs_sm <- NULL ##tinha missing
#test_matrix$lobs_sm <- NULL

anyNA(train_matrix)
anyNA(test_matrix)

## O pacote doParallel n?o pode ser executado tudo com o Caret
library(randomForest)
library(ggplot2)
library(caret)
set.seed(101)
trainctrl <- trainControl(classProbs= TRUE, summaryFunction = twoClassSummary, savePredictions = TRUE)
rfFuncs$summary <- twoClassSummary
control <- rfeControl(functions=rfFuncs, method="cv", number = 10, returnResamp="final")
#Usei o comando abaixo pq minha vari?vel "suicidio" ta no meio das colunas. Tive que descrever todas. Tirei as variaveis colineares
results <- rfe(train_matrix[,c(2:53)], train_matrix$bipolar1, rfeControl=control, method="rf", metric = "ROC", trControl=trainctrl)
# summarize the results
print(results)
# list the chosen features
predictors(results)

# plot the results
plot(results, type=c("g", "o"))
# estimate variable importance
importance <- varImp(results, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

#### Novo Banco de Dados 
data_rfe <- train_matrix
prop.table(table(data_rfe$bipolar1))

data_testing <- test_matrix
prop.table(table(data_testing$bipolar1))
#selecionar o banco bom

### treinando o algoritmo
levels(data_rfe$bipolar1)
levels(data_rfe$bipolar1) <- c("no", "yes")
levels(data_testing$bipolar1)
levels(data_testing$bipolar1) <- c("no", "yes")

### Corrigindo imbalance class https://topepo.github.io/caret/subsampling-for-class-imbalances.html
library(caret)
set.seed(9560)
#down_train <- downSample(x = data_rfe[, -ncol(data_rfe)],                         y = data_rfe$bipolar1)
#table(down_train$bipolar1)

#set.seed(9560)
#up_train <- upSample(x = data_rfe[, -ncol(data_rfe)], y = data_rfe$bipolar1)
#table(up_train$bipolar1)

library(DMwR)
set.seed(9560)
smote_train <- SMOTE(bipolar1 ~ ., data  = data_rfe)                         
table(smote_train$bipolar1) 

library(ROSE)
set.seed(9560)
rose_train <- ROSE(bipolar1 ~ ., data  = data_rfe)$data                         
table(rose_train$bipolar1) 

#orig_fit <- train(bipolar1 ~ ., data = data_rfe, method = "rf", trControl = ctrl.rf, tuneGrid=grid.rf, metric = "ROC")


###Treinando com lasso ondaa 4 ####
# Com desequilibrio
library(caret)
library(glmnet)
lambda <- 10^seq(-3, 3, length = 100)
set.seed(108)
ctrl.rf_LASSO = trainControl(method="cv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary)
set.seed(5627) #downregulation
ctrl.rf_down_LASSO <- trainControl(method="cv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary, sampling = "down")
down_outside_LASSO <- train(bipolar1 ~., data = data_rfe, method = "glmnet", trControl = ctrl.rf_down_LASSO, tuneLength = 10, tuneGrid = expand.grid(alpha = 1, lambda = lambda), metric = "ROC")
set.seed(5627) #up regulation
ctrl.rf_up_LASSO <- trainControl(method="cv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary, sampling = "up")
up_outside_LASSO <- train(bipolar1 ~., data = data_rfe, method = "glmnet", trControl = ctrl.rf_down_LASSO, tuneLength = 10, tuneGrid=expand.grid(alpha = 1, lambda = lambda), metric = "ROC")
library(e1071)
test_pred_up_lasso <- predict(up_outside_LASSO, newdata = data_testing[,-1])
matrixConfusao_up_lasso <- confusionMatrix(test_pred_up_lasso, data_testing$bipolar1, positive = "yes")
matrixConfusao_up_lasso 
##Smote
set.seed(5627)
smote_outside_LASSO <- train(bipolar1 ~ ., data = smote_train, method = "glmnet", trControl = ctrl.rf_LASSO, tuneGrid=expand.grid(alpha = 1, lambda = lambda), tuneLength = 10, metric = "ROC")

#Rose
set.seed(5627)
rose_outside_LASSO <- train(bipolar1 ~ ., data = rose_train, method = "glmnet", trControl = ctrl.rf_LASSO, tuneLength = 10, tuneGrid=expand.grid(alpha = 1, lambda = lambda), metric = "ROC")

library(e1071)
test_pred_smote_lasso <- predict(smote_outside_LASSO, newdata = data_testing[,-1])
matrixConfusao_smote_lasso <- confusionMatrix(test_pred_smote_lasso, data_testing$bipolar1, positive = "yes")
matrixConfusao_smote_lasso 
# ROC
library(caret)
library(pROC)
p_smote_lasso <- predict (smote_outside_LASSO, data_testing[,-1])
p_prob_smote_lasso <- predict (smote_outside_LASSO, data_testing, type = "prob")
confusionMatrix(p_smote_lasso, data_testing$bipolar1)
print(confusionMatrix(p_smote_lasso, data_testing$bipolar1, positive = "yes"))
r_smote_lasso <- roc (data_testing$bipolar1, p_prob_smote_lasso[,"yes"])
plot(r_smote_lasso)

r_smote_lasso$auc

varImp(smote_outside_LASSO)


############## ---- com ROSE
# matrix de confusao 
library(e1071)
test_pred_rose_lasso <- predict(rose_outside_LASSO, newdata = data_testing[,-1])
matrixConfusao_rose_lasso <- confusionMatrix(test_pred_rose_lasso, data_testing$bipolar1, positive = "yes")
matrixConfusao_rose_lasso 
# ROC
library(caret)
library(pROC)
p_rose_lasso <- predict (rose_outside_LASSO, data_testing[,-1])
p_prob_rose_lasso <- predict (rose_outside_LASSO, data_testing[,-1], type = "prob")
confusionMatrix(p_rose_lasso, data_testing$bipolar1)
print(confusionMatrix(p_rose_lasso, data_testing$bipolar1, positive = "yes"))
r_rose_lasso <- roc (data_testing$bipolar1, p_prob_rose_lasso[,"yes"])
plot(r_rose_lasso)
r_rose_lasso$auc
varImp(rose_outside_LASSO)

library(caret)
library(pROC)
p_down_lasso <- predict (down_outside_LASSO, data_testing[,-1])
p_prob_down_lasso <- predict (down_outside_LASSO, data_testing[,-1], type = "prob")
confusionMatrix(p_down_lasso, data_testing$bipolar1)
print(confusionMatrix(p_down_lasso, data_testing$bipolar1, positive = "yes"))
r_down_lasso <- roc (data_testing$bipolar1, p_prob_down_lasso[,"yes"])
plot(r_down_lasso)
r_down_lasso$auc

p_up_lasso <- predict (up_outside_LASSO, data_testing[,-1])
p_prob_up_lasso <- predict (up_outside_LASSO, data_testing[,-1], type = "prob")
confusionMatrix(p_up_lasso, data_testing$bipolar1)
print(confusionMatrix(p_up_lasso, data_testing$bipolar1, positive = "yes"))
r_up_lasso <- roc (data_testing$bipolar1, p_prob_up_lasso[,"yes"])
plot(r_up_lasso)
r_up_lasso$auc







########RANDOM FOREST ####
set.seed(5627) #downregulation
ctrl.rf_down <- trainControl(method="cv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary, sampling = "down")
grid.rf_down = expand.grid(.mtry=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14))
down_outside <- train(bipolar1 ~., data = data_rfe, method = "rf", trControl = ctrl.rf_down, tuneLength = 10, tuneGrid=grid.rf_down, metric = "ROC")
set.seed(5627) #up regulation
grid.rf_down = expand.grid(.mtry=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14))
ctrl.rf_up <- trainControl(method="cv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary, sampling = "up")
up_outside <- train(bipolar1 ~., data = data_rfe, method = "rf", trControl = ctrl.rf_down, tuneLength = 10, tuneGrid=grid.rf_down, metric = "ROC")

library(e1071)
test_pred_up <- predict(up_outside, newdata = data_testing[,-1])
matrixConfusao_up <- confusionMatrix(test_pred_up, data_testing$bipolar1, positive = "yes")
matrixConfusao_up 

##Smote
set.seed(5627)
grid.rf_down = expand.grid(.mtry=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14))
ctrl.rf <- trainControl(method="cv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary)

smote_outside <- train(bipolar1 ~ ., data = smote_train, method = "rf", trControl = ctrl.rf, tuneGrid=grid.rf_down, tuneLength = 10, metric = "ROC")

#Rose
set.seed(5627)
grid.rf_down = expand.grid(.mtry=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14))
rose_outside <- train(bipolar1 ~ ., data = rose_train, method = "rf", trControl = ctrl.rf, tuneLength = 10, tuneGrid=grid.rf_down, metric = "ROC")

# matrix de confusao 
library(e1071)
test_pred_smote <- predict(smote_outside, newdata = data_testing[,-1])
matrixConfusao_smote <- confusionMatrix(test_pred_smote, data_testing$bipolar1, positive = "yes")
matrixConfusao_smote 
# ROC
library(caret)
library(pROC)
p_smote<- predict (smote_outside, data_testing[,-1])
p_prob_smote <- predict (smote_outside, data_testing, type = "prob")
confusionMatrix(p_smote, data_testing$bipolar1)
print(confusionMatrix(p_smote, data_testing$bipolar1, positive = "yes"))
r_smote <- roc (data_testing$bipolar1, p_prob_smote[,"yes"])
plot(r_smote)

r_smote$auc

varImp(smote_outside)


############## ---- com ROSE
# matrix de confusao 
library(e1071)
test_pred_rose <- predict(rose_outside, newdata = data_testing[,-1])
matrixConfusao_rose <- confusionMatrix(test_pred_rose, data_testing$bipolar1, positive = "yes")
matrixConfusao_rose 
# ROC
library(caret)
library(pROC)
p_rose <- predict (rose_outside, data_testing[,-1])
p_prob_rose <- predict (rose_outside, data_testing[,-1], type = "prob")
confusionMatrix(p_rose, data_testing$bipolar1)
print(confusionMatrix(p_rose, data_testing$bipolar1, positive = "yes"))
r_rose <- roc (data_testing$bipolar1, p_prob_rose[,"yes"])
plot(r_rose)
r_rose$auc
varImp(rose_outside)

library(caret)
library(pROC)
p_down <- predict (down_outside, data_testing[,-1])
p_prob_down <- predict (down_outside, data_testing[,-1], type = "prob")
confusionMatrix(p_down, data_testing$bipolar1)
print(confusionMatrix(p_down, data_testing$bipolar1, positive = "yes"))
r_down <- roc (data_testing$bipolar1, p_prob_down[,"yes"])
plot(r_down)
r_down$auc

p_up <- predict (up_outside, data_testing[,-1])
p_prob_up <- predict (up_outside, data_testing[,-1], type = "prob")
confusionMatrix(p_up, data_testing$bipolar1)
print(confusionMatrix(p_up, data_testing$bipolar1, positive = "yes"))
r_up <- roc (data_testing$bipolar1, p_prob_up[,"yes"])
plot(r_up)
r_up$auc




#### mudando o limiar da ROC da onda 4 ####
library(SDMTools)

# desfecho do conjunto de teste
# 0 = No, 1 = Yes
obs <- data_testing$bipolar1

# Os levels estão em ordem?
levels(obs)

levels(obs) <- c("0", "1")
obs <- as.numeric(as.character(obs))
obs

# predições do modelo em probabilidades
rf.predict <- predict(up_outside, data_testing[, -1], type = "prob")
predictions <- as.vector(rf.predict[, 2])
predictions

confusion_df <- data.frame(obs, predictions)
threshold_seq <- seq(0, 1, by = 0.01)

confMatrix <- function(i, obs, predictions, ...){
  require(caret)
  require(SDMTools)
  
  conf_matrix <- confusion.matrix(obs, predictions, threshold = i)
  cm_table <- as.table(conf_matrix)
  cm <- confusionMatrix(cm_table, positive = "1")
  p_acc <- cm$overall[6]
  acc <- cm$overall[1]
  null_acc <- cm$overall[5]
  ppv <- cm$byClass[3]
  npv <- cm$byClass[4]
  
  result <-  c("limiar" = i, cm$byClass[1], cm$byClass[2], 
               "AB" = as.numeric(c((cm$byClass[1]+cm$byClass[2]) / 2)), 
               acc, p_acc, null_acc, ppv, npv)
  result
}

matrixConfusao_up$byClass
library(purrr)
library(plyr)
confMatrix(0.5, obs, predictions)
result_list <- map(threshold_seq, confMatrix, obs = obs, predictions = predictions)
result_df <- ldply(result_list)

result_df
cat("O sensitivity do limiar com maior acurácia balanceada é", result_df$Sensitivity[which.max(result_df$AB)])
cat("O ESPECIFICIDADE do limiar com maior acurácia balanceada é", result_df$Specificity[which.max(result_df$AB)])
cat("O PPV do limiar com maior acurácia balanceada é", result_df$`Pos Pred Value`[which.max(result_df$AB)])
cat("O ACURACIA do limiar com maior acurácia balanceada é", result_df$Accuracy[which.max(result_df$AB)])
cat("O AB do limiar com maior acurácia balanceada é", result_df$AB[which.max(result_df$AB)])
cat("O limiar com maior Acurácia Balanceada é", result_df$limiar[which.max(result_df$AB)])
cat("O PPV do limiar com maior acurácia balanceada é", result_df$`Pos Pred Value`[which.max(result_df$AB)])


max(result_df5$Accuracy)
max(result_df5$AB)

{plot(result_df$limiar, result_df$AB, type = "l", ylim = c(min(result_df$Accuracy), max(result_df$Accuracy)))
  lines(result_df$limiar, result_df$`Pos Pred Value`, col = "red")}

plot(result_df$Sensitivity, result_df$Accuracy)
plot(result_df$Specificity, result_df$AccuracyPValue)

plot(result_df$Specificity, result_df$AccuracyPValue)
{plot(result_df$limiar, result_df$AccuracyPValue, type = "l", ylim = c(0, 0.5))
  abline(h = 0.05, col = "red")}

min(result_df$AccuracyPValue)


###iMPORTANCIA DAS VARIAVEIS

plotVarImp <- function(down_outside){
  require(ggplot2)
  varimp_df <- varImp(down_outside)
  varimp_df <- varimp_df$importance
  Variables <- row.names(varimp_df)
  varimp_df <- data.frame(Variables, "Overall" = varimp_df$Overall)
  varimp_df
  
  p <- ggplot(data = varimp_df, aes(x = reorder(varimp_df$Variable, varimp_df$Overall), y = Overall, fill = Variables)) + 
    labs(x = "Features", y = "Overall")
  
  p <- p + geom_bar(stat = "identity", color = "darkblue", fill = "steelblue") + theme_minimal() + coord_flip()
  
}
]



##### ONDA 1 com up RF ####
dt_onda1 <- dt[,c(1034, 30, 9:11, 15,18, 23:27, 989:990, 986, 1016:1019, 1021:1022)]
prop.table(table(dt_onda1$bipolar1))

pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(dt_onda1,2,pMiss)

obj_variavel_onda1 <- apply(dt_onda1,2,pMiss)
variavel_8poc_onda1 <- obj_variavel_onda1[obj_variavel_onda1 < 8] ##selecionando so as com 7% de missing
variavel_8poc_onda1 <- names(variavel_8poc_onda1)
library(stats) #selecionei colunas com 5% missing
dt2_onda1 <- dt_onda1 [,c(variavel_8poc_onda1)]
pMiss_dataset2 <- function(x){sum(is.na(x))/length(x)*100} #Ja ta multiplicado por 100
apply(dt2_onda1,2,pMiss_dataset2)


# Ver as classes das variaveis
library(purrr)
map_chr(dt2_onda1, class)
# Ver as classes das variaveis
library(purrr)
map_chr(dt2_onda1, class)
##Mudar os fatores da classe pq tem variavel com "haven_label" CONVERSAO DE VARIAVEL
dt3_onda1 <- dt2_onda1
library(purrr)
dt3_onda1 <- map_df(dt3_onda1, function(x){
  if (length(levels(as.factor(x))) > 8) z <- as.numeric(x) else z <- as.factor(x)}
)
map_chr(dt3_onda1, class)


### HOLD OUT onda 1 ####
# Separar em banco de dados em Treino e Teste 
library(caret)
set.seed(123)
datasetbipolar_onda1 <- dt3_onda1 [sample (1:nrow (dt3_onda1), 3781, replace=FALSE), ]


## banco treino e teste ##
library(caret)
set.seed(123)
intrain_onda1 <- createDataPartition(y = datasetbipolar_onda1$bipolar1, p= 0.7, list = FALSE)
dtTrain_onda1 <-  datasetbipolar_onda1[intrain_onda1,]
dtTest_onda1 <- datasetbipolar_onda1[-intrain_onda1,]
dim(dtTrain_onda1); dim(dtTest_onda1)
# Porcentagem do desfecho em cada banco
prop.table(table(datasetbipolar_onda1$bipolar1))
prop.table(table(dtTrain_onda1$bipolar1))
prop.table(table(dtTest_onda1$bipolar1))
#iMPUTA??O USANDO media e moda  no treino pra aplicar depois no teste, evitando data linkage

train_matrix_onda1 <- dtTrain_onda1
test_matrix_onda1 <- dtTest_onda1

train_matrix_onda1 <- as.data.frame(train_matrix_onda1)
test_matrix_onda1 <- as.data.frame(test_matrix_onda1)
class(train_matrix_onda1)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

for (i in names(train_matrix_onda1)){
  print(class(train_matrix_onda1[,i]))
  print(is.factor(train_matrix_onda1[,i]))
  if (is.factor(train_matrix_onda1[,i])) {
    
    mode_value = getmode(train_matrix_onda1[,i])
    print("cat")
    #recorded_values[1,i] = mode_value
    train_matrix_onda1[is.na(train_matrix_onda1[,i]),i] = mode_value
    test_matrix_onda1[is.na(test_matrix_onda1[,i]),i] = mode_value
    print(mode_value)
  } else {
    mean_value = mean(train_matrix_onda1[,i], na.rm=TRUE)
    #recorded_values[1,i] = mean_value
    train_matrix_onda1[is.na(train_matrix_onda1[,i]),i] = mean_value
    test_matrix_onda1[is.na(test_matrix_onda1[,i]),i] = mean_value
    print("num")
  }
  
}

map_int(test_matrix_onda1, function(x){length(x[is.na(x)])})
str(train_matrix_onda1)
anyNA(train_matrix_onda1)
#train_matrix$lobs_sm <- NULL ##tinha missing
#test_matrix$lobs_sm <- NULL

anyNA(train_matrix_onda1)
anyNA(test_matrix_onda1)

## Feature selection onda 1 ####
library(randomForest)
library(ggplot2)
library(caret)
set.seed(101)
trainctrl <- trainControl(classProbs= TRUE, summaryFunction = twoClassSummary, savePredictions = TRUE)
rfFuncs$summary <- twoClassSummary
control <- rfeControl(functions=rfFuncs, method="cv", number = 10, returnResamp="final")
#Usei o comando abaixo pq minha vari?vel "suicidio" ta no meio das colunas. Tive que descrever todas. Tirei as variaveis colineares
results_onda1 <- rfe(train_matrix_onda1[,c(2:19)], train_matrix_onda1$bipolar1, rfeControl=control, method="rf", metric = "ROC", trControl=trainctrl)
# summarize the results
print(results_onda1)
# list the chosen features
predictors(results_onda1)
# plot the results
plot(results_onda1, type=c("g", "o"))
# estimate variable importance
importance_onda1 <- varImp(results_onda1, scale=FALSE)
# summarize importance
print(importance_onda1)
# plot importance
plot(importance_onda1)

#### Novo Banco de Dados 
data_rfe_onda1 <- train_matrix_onda1
prop.table(table(data_rfe_onda1$bipolar1))

data_testing_onda1 <- test_matrix_onda1
prop.table(table(data_testing_onda1$bipolar1))
#selecionar o banco bom

### treinando o algoritmo
levels(data_rfe_onda1$bipolar1)
levels(data_rfe_onda1$bipolar1) <- c("no", "yes")
levels(data_testing_onda1$bipolar1)
levels(data_testing_onda1$bipolar1) <- c("no", "yes")

set.seed(5627) #up regulation
grid.rf_up = expand.grid(.mtry=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14))
ctrl.rf_up_onda1 <- trainControl(method="cv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary, sampling = "up")
up_outside_onda1 <- train(bipolar1 ~., data = data_rfe_onda1, method = "rf", trControl = ctrl.rf_up, tuneLength = 10, tuneGrid=grid.rf_up, metric = "ROC")

library(e1071)
test_pred_up_onda1 <- predict(up_outside_onda1, newdata = data_testing_onda1[,-1])
matrixConfusao_up_onda1 <- confusionMatrix(test_pred_up_onda1, data_testing_onda1$bipolar1, positive = "yes")
matrixConfusao_up_onda1 

p_up_onda1 <- predict (up_outside_onda1, data_testing_onda1[,-1])
p_prob_up_onda1 <- predict (up_outside_onda1, data_testing_onda1[,-1], type = "prob")
confusionMatrix(p_up_onda1, data_testing_onda1$bipolar1)
print(confusionMatrix(p_up_onda1, data_testing_onda1$bipolar1, positive = "yes"))
r_up_onda1 <- roc (data_testing_onda1$bipolar1, p_prob_up_onda1[,"yes"])
plot(r_up_onda1)
r_up_onda1$auc




### Onda 2 ####
dt_onda2 <- dt[,c(1034, 29, 40:41, 49, 58:90,133, 138, 140, 142, 145, 937:941, 986, 991)]
prop.table(table(dt_onda2$bipolar1))

pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(dt_onda2,2,pMiss)

obj_variavel_onda2 <- apply(dt_onda2,2,pMiss)
variavel_8poc_onda2 <- obj_variavel_onda2[obj_variavel_onda2 < 8] ##selecionando so as com 7% de missing
variavel_8poc_onda2 <- names(variavel_8poc_onda2)
library(stats) #selecionei colunas com 5% missing
dt_onda2 <- dt_onda2 [,c(variavel_8poc_onda2)]
pMiss_dataset2 <- function(x){sum(is.na(x))/length(x)*100} #Ja ta multiplicado por 100
apply(dt_onda2,2,pMiss_dataset2)


# Ver as classes das variaveis
library(purrr)
map_chr(dt_onda2, class)
# Ver as classes das variaveis
library(purrr)
map_chr(dt_onda2, class)
##Mudar os fatores da classe pq tem variavel com "haven_label" CONVERSAO DE VARIAVEL
dt3_onda2 <- dt_onda2
library(purrr)
dt3_onda2 <- map_df(dt3_onda2, function(x){
  if (length(levels(as.factor(x))) > 8) z <- as.numeric(x) else z <- as.factor(x)}
)
map_chr(dt3_onda2, class)


### HOLD OUT onda 2 ####
# Separar em banco de dados em Treino e Teste 
library(caret)
set.seed(123)
datasetbipolar_onda2 <- dt3_onda2 [sample (1:nrow (dt3_onda2), 3781, replace=FALSE), ]

## banco treino e teste ##
library(caret)
set.seed(123)
intrain_onda2 <- createDataPartition(y = datasetbipolar_onda2$bipolar1, p= 0.7, list = FALSE)
dtTrain_onda2 <-  datasetbipolar_onda2[intrain_onda2,]
dtTest_onda2 <- datasetbipolar_onda2[-intrain_onda2,]
dim(dtTrain_onda2); dim(dtTest_onda2)
# Porcentagem do desfecho em cada banco
prop.table(table(datasetbipolar_onda2$bipolar1))
prop.table(table(dtTrain_onda2$bipolar1))
prop.table(table(dtTest_onda2$bipolar1))
#iMPUTA??O USANDO media e moda  no treino pra aplicar depois no teste, evitando data linkage

train_matrix_onda2 <- dtTrain_onda2
test_matrix_onda2 <- dtTest_onda2

train_matrix_onda2 <- as.data.frame(train_matrix_onda2)
test_matrix_onda2 <- as.data.frame(test_matrix_onda2)
class(train_matrix_onda2)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

for (i in names(train_matrix_onda2)){
  print(class(train_matrix_onda2[,i]))
  print(is.factor(train_matrix_onda2[,i]))
  if (is.factor(train_matrix_onda2[,i])) {
    
    mode_value = getmode(train_matrix_onda2[,i])
    print("cat")
    #recorded_values[1,i] = mode_value
    train_matrix_onda2[is.na(train_matrix_onda2[,i]),i] = mode_value
    test_matrix_onda2[is.na(test_matrix_onda2[,i]),i] = mode_value
    print(mode_value)
  } else {
    mean_value = mean(train_matrix_onda2[,i], na.rm=TRUE)
    #recorded_values[1,i] = mean_value
    train_matrix_onda2[is.na(train_matrix_onda2[,i]),i] = mean_value
    test_matrix_onda2[is.na(test_matrix_onda2[,i]),i] = mean_value
    print("num")
  }
  
}

map_int(test_matrix_onda2, function(x){length(x[is.na(x)])})
str(train_matrix_onda2)
anyNA(train_matrix_onda2)
#train_matrix$lobs_sm <- NULL ##tinha missing
#test_matrix$lobs_sm <- NULL
anyNA(train_matrix_onda2)
anyNA(test_matrix_onda2)

train_matrix_onda2$hm202 <- NULL        
train_matrix_onda2$hm203 <- NULL             
train_matrix_onda2$hm204a <- NULL            
train_matrix_onda2$hm204b <- NULL            
train_matrix_onda2$hm204c <- NULL           
train_matrix_onda2$hm204d <- NULL            
train_matrix_onda2$hm205  <- NULL            
train_matrix_onda2$hm206 <- NULL     


test_matrix_onda2$hm202 <- NULL        
test_matrix_onda2$hm203 <- NULL             
test_matrix_onda2$hm204a <- NULL            
test_matrix_onda2$hm204b <- NULL            
test_matrix_onda2$hm204c <- NULL           
test_matrix_onda2$hm204d <- NULL            
test_matrix_onda2$hm205  <- NULL            
test_matrix_onda2$hm206 <- NULL     

## Feature selection onda 2 ####
library(randomForest)
library(ggplot2)
library(caret)
set.seed(101)
trainctrl <- trainControl(classProbs= TRUE, summaryFunction = twoClassSummary, savePredictions = TRUE)
rfFuncs$summary <- twoClassSummary
control <- rfeControl(functions=rfFuncs, method="cv", number = 10, returnResamp="final")
#Usei o comando abaixo pq minha vari?vel "suicidio" ta no meio das colunas. Tive que descrever todas. Tirei as variaveis colineares
results_onda2 <- rfe(train_matrix_onda2[,c(2:40)], train_matrix_onda2$bipolar1, rfeControl=control, method="rf", metric = "ROC", trControl=trainctrl)
# summarize the results
print(results_onda2)
# list the chosen features
predictors(results_onda2)
# plot the results
plot(results_onda2, type=c("g", "o"))
# estimate variable importance
importance_onda2 <- varImp(results_onda2, scale=FALSE)
# summarize importance
print(importance_onda2)
# plot importance
plot(importance_onda2)

#### Novo Banco de Dados 
data_rfe_onda2 <- train_matrix_onda2
prop.table(table(data_rfe_onda2$bipolar1))

data_testing_onda2 <- test_matrix_onda2
prop.table(table(data_testing_onda2$bipolar1))
#selecionar o banco bom

### treinando o algoritmo
levels(data_rfe_onda2$bipolar1)
levels(data_rfe_onda2$bipolar1) <- c("no", "yes")
levels(data_testing_onda2$bipolar1)
levels(data_testing_onda2$bipolar1) <- c("no", "yes")


set.seed(5627) #up regulation
grid.rf_up = expand.grid(.mtry=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14))
ctrl.rf_up_onda2 <- trainControl(method="cv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary, sampling = "up")
up_outside_onda2 <- train(bipolar1 ~., data = data_rfe_onda2, method = "rf", trControl = ctrl.rf_up, tuneLength = 10, tuneGrid=grid.rf_up, metric = "ROC")

library(e1071)
test_pred_up_onda2 <- predict(up_outside_onda2, newdata = data_testing_onda2[,-1])
matrixConfusao_up_onda2 <- confusionMatrix(test_pred_up_onda2, data_testing_onda2$bipolar1, positive = "yes")
matrixConfusao_up_onda2 

p_up_onda2 <- predict (up_outside_onda2, data_testing_onda2[,-1])
p_prob_up_onda2 <- predict (up_outside_onda2, data_testing_onda2[,-1], type = "prob")
confusionMatrix(p_up_onda2, data_testing_onda2$bipolar1)
print(confusionMatrix(p_up_onda2, data_testing_onda2$bipolar1, positive = "yes"))
r_up_onda2 <- roc (data_testing_onda2$bipolar1, p_prob_up_onda2[,"yes"])
plot(r_up_onda2)
r_up_onda2$auc

##### Onda 3 ####

dt_onda3 <- dt[,c(1034, 29, 986, 158:190, 192:194, 198, 201, 203, 205:206, 213, 944:948)] ## 986 é hist psiq familiar, 192 É REPETICAO ESCOLAR, 201 é alcool, 213 escolaridade materna
prop.table(table(dt_onda3$bipolar1))

pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(dt_onda3,2,pMiss)

obj_variavel_onda3 <- apply(dt_onda3,2,pMiss)
variavel_8poc_onda3 <- obj_variavel_onda3[obj_variavel_onda3 < 8] ##selecionando so as com 7% de missing
variavel_8poc_onda3 <- names(variavel_8poc_onda3)
library(stats) #selecionei colunas com 5% missing
dt2_onda3 <- dt_onda3 [,c(variavel_8poc_onda3)]
pMiss_dataset2 <- function(x){sum(is.na(x))/length(x)*100} #Ja ta multiplicado por 100
apply(dt2_onda3,2,pMiss_dataset2)


# Ver as classes das variaveis
library(purrr)
map_chr(dt2_onda3, class)
# Ver as classes das variaveis
library(purrr)
map_chr(dt2_onda3, class)
##Mudar os fatores da classe pq tem variavel com "haven_label" CONVERSAO DE VARIAVEL
dt3_onda3 <- dt2_onda3
library(purrr)
dt3_onda3 <- map_df(dt3_onda3, function(x){
  if (length(levels(as.factor(x))) > 8) z <- as.numeric(x) else z <- as.factor(x)}
)
map_chr(dt3_onda3, class)


### HOLD OUT onda 3 ####
# Separar em banco de dados em Treino e Teste 
library(caret)
set.seed(123)
datasetbipolar_onda3 <- dt3_onda3 [sample (1:nrow (dt3), 3781, replace=FALSE), ]


## banco treino e teste ##
library(caret)
set.seed(123)
intrain_onda3 <- createDataPartition(y = datasetbipolar_onda3$bipolar1, p= 0.7, list = FALSE)
dtTrain_onda3 <-  datasetbipolar_onda3[intrain_onda3,]
dtTest_onda3 <- datasetbipolar_onda3[-intrain_onda3,]
dim(dtTrain_onda3); dim(dtTest_onda3)
# Porcentagem do desfecho em cada banco
prop.table(table(datasetbipolar_onda3$bipolar1))
prop.table(table(dtTrain_onda3$bipolar1))
prop.table(table(dtTest_onda3$bipolar1))
#iMPUTA??O USANDO media e moda  no treino pra aplicar depois no teste, evitando data linkage

train_matrix_onda3 <- dtTrain_onda3
test_matrix_onda3 <- dtTest_onda3

train_matrix_onda3 <- as.data.frame(train_matrix_onda3)
test_matrix_onda3 <- as.data.frame(test_matrix_onda3)
class(train_matrix_onda3)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

for (i in names(train_matrix_onda3)){
  print(class(train_matrix_onda3[,i]))
  print(is.factor(train_matrix_onda3[,i]))
  if (is.factor(train_matrix_onda3[,i])) {
    
    mode_value = getmode(train_matrix_onda3[,i])
    print("cat")
    #recorded_values[1,i] = mode_value
    train_matrix_onda3[is.na(train_matrix_onda3[,i]),i] = mode_value
    test_matrix_onda3[is.na(test_matrix_onda3[,i]),i] = mode_value
    print(mode_value)
  } else {
    mean_value = mean(train_matrix_onda3[,i], na.rm=TRUE)
    #recorded_values[1,i] = mean_value
    train_matrix_onda3[is.na(train_matrix_onda3[,i]),i] = mean_value
    test_matrix_onda3[is.na(test_matrix_onda3[,i]),i] = mean_value
    print("num")
  }
  
}

map_int(test_matrix_onda3, function(x){length(x[is.na(x)])})
str(train_matrix_onda3)
anyNA(train_matrix_onda3)
#train_matrix$lobs_sm <- NULL ##tinha missing
#test_matrix$lobs_sm <- NULL

anyNA(train_matrix_onda3)
anyNA(test_matrix_onda3)

## Feature selection onda 1 ####
library(randomForest)
library(ggplot2)
library(caret)
set.seed(101)
trainctrl <- trainControl(classProbs= TRUE, summaryFunction = twoClassSummary, savePredictions = TRUE)
rfFuncs$summary <- twoClassSummary
control <- rfeControl(functions=rfFuncs, method="cv", number = 10, returnResamp="final")
#Usei o comando abaixo pq minha vari?vel "suicidio" ta no meio das colunas. Tive que descrever todas. Tirei as variaveis colineares
results_onda3 <- rfe(train_matrix_onda3[,c(2:46)], train_matrix_onda3$bipolar1, rfeControl=control, method="rf", metric = "ROC", trControl=trainctrl)
# summarize the results
print(results_onda3)
# list the chosen features
predictors(results_onda3)
# plot the results
plot(results_onda3, type=c("g", "o"))
# estimate variable importance
importance_onda3 <- varImp(results_onda3, scale=FALSE)
# summarize importance
print(importance_onda3)
# plot importance
plot(importance_onda3)

#### Novo Banco de Dados 
data_rfe_onda3 <- train_matrix_onda3
prop.table(table(data_rfe_onda3$bipolar1))

data_testing_onda3 <- test_matrix_onda3
prop.table(table(data_testing_onda3$bipolar1))
#selecionar o banco bom

### treinando o algoritmo
levels(data_rfe_onda3$bipolar1)
levels(data_rfe_onda3$bipolar1) <- c("no", "yes")
levels(data_testing_onda3$bipolar1)
levels(data_testing_onda3$bipolar1) <- c("no", "yes")


set.seed(5627) #up regulation
grid.rf_up = expand.grid(.mtry=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14))
ctrl.rf_up_onda3 <- trainControl(method="cv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary, sampling = "up")
up_outside_onda3 <- train(bipolar1 ~., data = data_rfe_onda3, method = "rf", trControl = ctrl.rf_up, tuneLength = 10, tuneGrid=grid.rf_up, metric = "ROC")

library(e1071)
test_pred_up_onda3 <- predict(up_outside_onda3, newdata = data_testing_onda3[,-1])
matrixConfusao_up_onda3 <- confusionMatrix(test_pred_up_onda3, data_testing_onda3$bipolar1, positive = "yes")
matrixConfusao_up_onda3 

p_up_onda3 <- predict (up_outside_onda3, data_testing_onda3[,-1])
p_prob_up_onda3 <- predict (up_outside_onda3, data_testing_onda3[,-1], type = "prob")
confusionMatrix(p_up_onda3, data_testing_onda3$bipolar1)
print(confusionMatrix(p_up_onda3, data_testing_onda3$bipolar1, positive = "yes"))
r_up_onda3 <- roc (data_testing_onda3$bipolar1, p_prob_up_onda3[,"yes"])
plot(r_up_onda3)
r_up_onda3$auc


#####Treinando com lasso onda 3 ####
# Com desequilibrio
library(caret)
library(glmnet)
lambda <- 10^seq(-3, 3, length = 100)
set.seed(108)
ctrl.rf_LASSO = trainControl(method="cv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary)
set.seed(5627)
smote_outside_LASSO_onda3 <- train(bipolar1 ~ ., data = smote_train_onda3, method = "glmnet", trControl = ctrl.rf_LASSO, tuneGrid=expand.grid(alpha = 1, lambda = lambda), tuneLength = 10, metric = "ROC")

##teste
library(e1071)
test_pred_smote_lasso_onda3 <- predict(smote_outside_LASSO_onda3, newdata = data_testing_onda3[,-1])
matrixConfusao_smote_lasso_onda3 <- confusionMatrix(test_pred_smote_lasso_onda3, data_testing_onda3$bipolar1, positive = "yes")
matrixConfusao_smote_lasso_onda3 
# ROC
library(caret)
library(pROC)
p_smote_lasso_onda3 <- predict (smote_outside_LASSO_onda3, data_testing_onda3[,-1])
p_prob_smote_lasso_onda3 <- predict (smote_outside_LASSO_onda3, data_testing_onda3, type = "prob")
confusionMatrix(p_smote_lasso_onda3, data_testing_onda3$bipolar1)
print(confusionMatrix(p_smote_lasso_onda3, data_testing_onda3$bipolar1, positive = "yes"))
r_smote_lasso_onda3 <- roc (data_testing_onda3$bipolar1, p_prob_smote_lasso_onda3[,"yes"])
plot(r_smote_lasso_onda3)
r_smote_lasso_onda3$auc
varImp(smote_outside_LASSO_onda3)



#####Onda 1 e 2 ####
dt_onda1e2 <- dt[,c(1033, 29, 9:11, 15,17, 22:26, 989:990, 986, 1016:1019, 1021:1022, 40, 48, 57:89,132, 139, 141:142, 144, 937:941, 986, 991)]
prop.table(table(dt_onda1e2$bipolar1))

pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(dt_onda1e2,2,pMiss)

obj_variavel_onda1e2 <- apply(dt_onda1e2,2,pMiss)
variavel_8poc_onda1e2 <- obj_variavel_onda1e2[obj_variavel_onda1e2 < 8] ##selecionando so as com 7% de missing
variavel_8poc_onda1e2 <- names(variavel_8poc_onda1e2)
library(stats) #selecionei colunas com 5% missing
dt2_onda1e2 <- dt_onda1e2 [,c(variavel_8poc_onda1e2)]
pMiss_dataset2 <- function(x){sum(is.na(x))/length(x)*100} #Ja ta multiplicado por 100
apply(dt2_onda1e2,2,pMiss_dataset2)


# Ver as classes das variaveis
library(purrr)
map_chr(dt2_onda1e2, class)
# Ver as classes das variaveis
library(purrr)
map_chr(dt2_onda1e2, class)
##Mudar os fatores da classe pq tem variavel com "haven_label" CONVERSAO DE VARIAVEL
dt3_onda1e2 <- dt2_onda1e2
library(purrr)
dt3_onda1e2 <- map_df(dt3_onda1e2, function(x){
  if (length(levels(as.factor(x))) > 8) z <- as.numeric(x) else z <- as.factor(x)}
)
map_chr(dt3_onda1e2, class)
dt3_onda1e2$hrenfa <- NULL

### HOLD OUT onda 1 E 2 ####
# Separar em banco de dados em Treino e Teste 
library(caret)
set.seed(123)
datasetbipolar_onda1e2 <- dt3_onda1e2 [sample (1:nrow (dt3_onda1e2), 3781, replace=FALSE), ]
## banco treino e teste ##
library(caret)
set.seed(123)
intrain_onda1e2 <- createDataPartition(y = datasetbipolar_onda1e2$bipolar1, p= 0.7, list = FALSE)
dtTrain_onda1e2 <-  datasetbipolar_onda1e2[intrain_onda1e2,]
dtTest_onda1e2 <- datasetbipolar_onda1e2[-intrain_onda1e2,]
dim(dtTrain_onda1e2); dim(dtTest_onda1e2)
# Porcentagem do desfecho em cada banco
prop.table(table(datasetbipolar_onda1e2$bipolar1))
prop.table(table(dtTrain_onda1e2$bipolar1))
prop.table(table(dtTest_onda1e2$bipolar1))
#iMPUTA??O USANDO media e moda  no treino pra aplicar depois no teste, evitando data linkage

train_matrix_onda1e2 <- dtTrain_onda1e2
test_matrix_onda1e2 <- dtTest_onda1e2

train_matrix_onda1e2 <- as.data.frame(train_matrix_onda1e2)
test_matrix_onda1e2 <- as.data.frame(test_matrix_onda1e2)
class(train_matrix_onda1e2)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

for (i in names(train_matrix_onda1e2)){
  print(class(train_matrix_onda1e2[,i]))
  print(is.factor(train_matrix_onda1e2[,i]))
  if (is.factor(train_matrix_onda1e2[,i])) {
    
    mode_value = getmode(train_matrix_onda1e2[,i])
    print("cat")
    #recorded_values[1,i] = mode_value
    train_matrix_onda1e2[is.na(train_matrix_onda1e2[,i]),i] = mode_value
    test_matrix_onda1e2[is.na(test_matrix_onda1e2[,i]),i] = mode_value
    print(mode_value)
  } else {
    mean_value = mean(train_matrix_onda1e2[,i], na.rm=TRUE)
    #recorded_values[1,i] = mean_value
    train_matrix_onda1e2[is.na(train_matrix_onda1e2[,i]),i] = mean_value
    test_matrix_onda1e2[is.na(test_matrix_onda1e2[,i]),i] = mean_value
    print("num")
  }
  
}

map_int(test_matrix_onda1e2, function(x){length(x[is.na(x)])})
str(train_matrix_onda1e2)
anyNA(train_matrix_onda1e2)
#train_matrix$lobs_sm <- NULL ##tinha missing
#test_matrix$lobs_sm <- NULL

anyNA(train_matrix_onda1e2)
anyNA(test_matrix_onda1e2)

data_rfe_onda1e2 <- train_matrix_onda1e2
prop.table(table(data_rfe_onda1e2$bipolar1))

data_testing_onda1e2 <- test_matrix_onda1e2
prop.table(table(data_testing_onda1e2$bipolar1))
#selecionar o banco bom

### treinando o algoritmo
levels(data_rfe_onda1e2$bipolar1)
levels(data_rfe_onda1e2$bipolar1) <- c("no", "yes")
levels(data_testing_onda1e2$bipolar1)
levels(data_testing_onda1e2$bipolar1) <- c("no", "yes")


library(DMwR)
set.seed(9560)
smote_train_onda1e2 <- SMOTE(bipolar1 ~ ., data  = data_rfe_onda1e2)                         
table(smote_train_onda1e2$bipolar1) 

#####Treinando com lasso onda 1 E 2 ####
# Com desequilibrio
library(caret)
library(glmnet)
lambda <- 10^seq(-3, 3, length = 100)
set.seed(108)
ctrl.rf_LASSO = trainControl(method="cv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary)
set.seed(5627)
smote_outside_LASSO_onda1e2 <- train(bipolar1 ~ ., data = smote_train_onda1e2, method = "glmnet", trControl = ctrl.rf_LASSO, tuneGrid=expand.grid(alpha = 1, lambda = lambda), tuneLength = 10, metric = "ROC")

##teste
library(e1071)
test_pred_smote_lasso_onda1e2 <- predict(smote_outside_LASSO_onda1e2, newdata = data_testing_onda1e2[,-1])
matrixConfusao_smote_lasso_onda1e2 <- confusionMatrix(test_pred_smote_lasso_onda1e2, data_testing_onda1e2$bipolar1, positive = "yes")
matrixConfusao_smote_lasso_onda1e2 
# ROC
library(caret)
library(pROC)
p_smote_lasso_onda1e2 <- predict (smote_outside_LASSO_onda1e2, data_testing_onda1e2[,-1])
p_prob_smote_lasso_onda1e2 <- predict (smote_outside_LASSO_onda1e2, data_testing_onda1e2, type = "prob")
confusionMatrix(p_smote_lasso_onda1e2, data_testing_onda1e2$bipolar1)
print(confusionMatrix(p_smote_lasso_onda1e2, data_testing_onda1e2$bipolar1, positive = "yes"))
r_smote_lasso_onda1e2 <- roc (data_testing_onda1e2$bipolar1, p_prob_smote_lasso_onda1e2[,"yes"])
plot(r_smote_lasso_onda1e2)
r_smote_lasso_onda1e2$auc
varImp(smote_outside_LASSO_onda1e2)

#### Onda 1, 2 e 3 ####
dt_onda1e2e3 <- dt[,c(1033, 29, 9:11, 15,17, 22:26, 989:990, 986, 1016:1019, 1021:1022, 40, 48, 57:89,132, 139, 141:142, 144, 937:941, 986, 991, 157:189, 191:193, 198,200, 202, 204, 205, 944:948)] #133 fumar na onda H. 145 porre. 
dt_onda1e2e3$hc02 <- NULL
dt_onda1e2e3$jc03 <- NULL
dt_onda1e2e3$ja006 <- NULL#repetencia escolar. hm048 tbm é repetencia
#jc07 usou alcool

pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(dt_onda1e2e3,2,pMiss)

obj_variavel_onda1e2e3 <- apply(dt_onda1e2e3,2,pMiss)
variavel_8poc_onda1e2e3 <- obj_variavel_onda1e2e3[obj_variavel_onda1e2e3 < 8] ##selecionando so as com 7% de missing
variavel_8poc_onda1e2e3 <- names(variavel_8poc_onda1e2e3)
library(stats) #selecionei colunas com 5% missing
dt2_onda1e2e3 <- dt_onda1e2e3 [,c(variavel_8poc_onda1e2e3)]
pMiss_dataset2 <- function(x){sum(is.na(x))/length(x)*100} #Ja ta multiplicado por 100
apply(dt2_onda1e2e3,2,pMiss_dataset2)


# Ver as classes das variaveis
library(purrr)
map_chr(dt2_onda1e2e3, class)
# Ver as classes das variaveis
library(purrr)
map_chr(dt2_onda1e2e3, class)
##Mudar os fatores da classe pq tem variavel com "haven_label" CONVERSAO DE VARIAVEL
dt3_onda1e2e3 <- dt2_onda1e2e3
library(purrr)
dt3_onda1e2e3 <- map_df(dt3_onda1e2e3, function(x){
  if (length(levels(as.factor(x))) > 8) z <- as.numeric(x) else z <- as.factor(x)}
)
map_chr(dt3_onda1e2e3, class)
dt3_onda1e2e3$hrenfa <- NULL

### HOLD OUT onda 1 E 2 E 3 ####
# Separar em banco de dados em Treino e Teste 
library(caret)
set.seed(123)
datasetbipolar_onda1e2e3 <- dt3_onda1e2e3 [sample (1:nrow (dt3_onda1e2e3), 3781, replace=FALSE), ]
## banco treino e teste ##
library(caret)
set.seed(123)
intrain_onda1e2e3 <- createDataPartition(y = datasetbipolar_onda1e2e3$bipolar1, p= 0.7, list = FALSE)
dtTrain_onda1e2e3 <-  datasetbipolar_onda1e2e3[intrain_onda1e2e3,]
dtTest_onda1e2e3 <- datasetbipolar_onda1e2e3[-intrain_onda1e2e3,]
dim(dtTrain_onda1e2e3); dim(dtTest_onda1e2e3)
# Porcentagem do desfecho em cada banco
prop.table(table(datasetbipolar_onda1e2e3$bipolar1))
prop.table(table(dtTrain_onda1e2e3$bipolar1))
prop.table(table(dtTest_onda1e2e3$bipolar1))
#iMPUTA??O USANDO media e moda  no treino pra aplicar depois no teste, evitando data linkage

train_matrix_onda1e2e3 <- dtTrain_onda1e2e3
test_matrix_onda1e2e3 <- dtTest_onda1e2e3

train_matrix_onda1e2e3 <- as.data.frame(train_matrix_onda1e2e3)
test_matrix_onda1e2e3 <- as.data.frame(test_matrix_onda1e2e3)
class(train_matrix_onda1e2e3)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

for (i in names(train_matrix_onda1e2e3)){
  print(class(train_matrix_onda1e2e3[,i]))
  print(is.factor(train_matrix_onda1e2e3[,i]))
  if (is.factor(train_matrix_onda1e2e3[,i])) {
    
    mode_value = getmode(train_matrix_onda1e2e3[,i])
    print("cat")
    #recorded_values[1,i] = mode_value
    train_matrix_onda1e2e3[is.na(train_matrix_onda1e2e3[,i]),i] = mode_value
    test_matrix_onda1e2e3[is.na(test_matrix_onda1e2e3[,i]),i] = mode_value
    print(mode_value)
  } else {
    mean_value = mean(train_matrix_onda1e2e3[,i], na.rm=TRUE)
    #recorded_values[1,i] = mean_value
    train_matrix_onda1e2e3[is.na(train_matrix_onda1e2e3[,i]),i] = mean_value
    test_matrix_onda1e2e3[is.na(test_matrix_onda1e2e3[,i]),i] = mean_value
    print("num")
  }
  
}

map_int(test_matrix_onda1e2e3, function(x){length(x[is.na(x)])})
str(train_matrix_onda1e2e3)
anyNA(train_matrix_onda1e2e3)
#train_matrix$lobs_sm <- NULL ##tinha missing
#test_matrix$lobs_sm <- NULL

anyNA(train_matrix_onda1e2e3)
anyNA(test_matrix_onda1e2e3)

data_rfe_onda1e2e3 <- train_matrix_onda1e2e3
prop.table(table(data_rfe_onda1e2e3$bipolar1))

data_testing_onda1e2e3 <- test_matrix_onda1e2e3
prop.table(table(data_testing_onda1e2e3$bipolar1))
#selecionar o banco bom

### treinando o algoritmo
levels(data_rfe_onda1e2e3$bipolar1)
levels(data_rfe_onda1e2e3$bipolar1) <- c("no", "yes")
levels(data_testing_onda1e2e3$bipolar1)
levels(data_testing_onda1e2e3$bipolar1) <- c("no", "yes")


library(DMwR)
set.seed(9560)
smote_train_onda1e2e3 <- SMOTE(bipolar1 ~ ., data  = data_rfe_onda1e2e3)                         
table(smote_train_onda1e2e3$bipolar1) 

#####Treinando com lasso onda 1 E 2 E 3 ####
# Com desequilibrio
library(caret)
library(glmnet)
lambda <- 10^seq(-3, 3, length = 100)
set.seed(108)
ctrl.rf_LASSO = trainControl(method="cv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary)
set.seed(5627)
smote_outside_LASSO_onda1e2e3 <- train(bipolar1 ~ ., data = smote_train_onda1e2e3, method = "glmnet", trControl = ctrl.rf_LASSO, tuneGrid=expand.grid(alpha = 1, lambda = lambda), tuneLength = 10, metric = "ROC")

##teste
library(e1071)
test_pred_smote_lasso_onda1e2e3 <- predict(smote_outside_LASSO_onda1e2e3, newdata = data_testing_onda1e2e3[,-1])
matrixConfusao_smote_lasso_onda1e2e3 <- confusionMatrix(test_pred_smote_lasso_onda1e2e3, data_testing_onda1e2e3$bipolar1, positive = "yes")
matrixConfusao_smote_lasso_onda1e2e3 
# ROC
library(caret)
library(pROC)
p_smote_lasso_onda1e2e3 <- predict (smote_outside_LASSO_onda1e2e3, data_testing_onda1e2e3[,-1])
p_prob_smote_lasso_onda1e2e3 <- predict (smote_outside_LASSO_onda1e2e3, data_testing_onda1e2e3, type = "prob")
confusionMatrix(p_smote_lasso_onda1e2e3, data_testing_onda1e2e3$bipolar1)
print(confusionMatrix(p_smote_lasso_onda1e2e3, data_testing_onda1e2e3$bipolar1, positive = "yes"))
r_smote_lasso_onda1e2e3 <- roc (data_testing_onda1e2e3$bipolar1, p_prob_smote_lasso_onda1e2e3[,"yes"])
plot(r_smote_lasso_onda1e2e3)
r_smote_lasso_onda1e2e3$auc
varImp(smote_outside_LASSO_onda1e2e3)

###### onda 1, 2 , 3 e 4 ####
dt_onda1e2e3e4 <- dt[,c(1033, 29, 9:11, 15,17, 22:26, 989:990, 986, 1016:1019, 1021:1022, 40, 48, 57:89,132, 139, 141:142, 144, 937:941, 986, 991, 157:189, 191:193, 198,200, 202, 204, 205, 944:948, 238:242, 244, 259:271, 286, 258, 3, 342:360, 375, 377:424, 438:450, 953:956, 959:962, 963,986, 1023:1025)] #133 fumar na onda H. 145 porre. 

####Criando novas variaveis das drogas #####
sumFeatures <- function(x, ...){
  l <- length(x[is.na(x)]) 
  if (l == length(x)) y <- NA else y <- sum(x[!is.na(x)])
  return(y)
}

##Solventes
df_solvente_onda1e2e3e4 <-data.frame(dt_onda1e2e3e4$kc10, dt_onda1e2e3e4$kc16)
solvente_lifetime_onda1e2e3e4 <- apply(df_solvente_onda1e2e3e4, 1, sumFeatures)
solvente_lifetime_onda1e2e3e4[solvente_lifetime_onda1e2e3e4 > 1] <- 1
df_solvente_onda1e2e3e4$solvente_lifetime_onda1e2e3e4 <- solvente_lifetime_onda1e2e3e4
##Cocaina
df_cocaina_onda1e2e3e4 <-data.frame(dt_onda1e2e3e4$kc06, dt_onda1e2e3e4$kc09, dt_onda1e2e3e4$kc13, dt_onda1e2e3e4$kc14)
cocaina_lifetime_onda1e2e3e4 <- apply(df_cocaina_onda1e2e3e4, 1, sumFeatures)
cocaina_lifetime_onda1e2e3e4[cocaina_lifetime_onda1e2e3e4 > 1] <- 1
df_cocaina_onda1e2e3e4$cocaina_lifetime_onda1e2e3e4 <- cocaina_lifetime_onda1e2e3e4
##Inserindo colunas novas
dt_onda1e2e3e4$solvente_lifetime_onda1e2e3e4 <- solvente_lifetime_onda1e2e3e4
dt_onda1e2e3e4$cocaina_lifetime_onda1e2e3e4 <- cocaina_lifetime_onda1e2e3e4
##excluindo as variaveis de drogas usadas pra criar essas
dt_onda1e2e3e4$kc06 <- NULL
dt_onda1e2e3e4$kc09 <- NULL
dt_onda1e2e3e4$kc13 <- NULL
dt_onda1e2e3e4$kc14 <- NULL
dt_onda1e2e3e4$kc10 <- NULL
dt_onda1e2e3e4$kc16 <- NULL
dt_onda1e2e3e4$nquest <- NULL
dt_onda1e2e3e4$hc02 <- NULL
dt_onda1e2e3e4$jc03 <- NULL
dt_onda1e2e3e4$ja006 <- NULL#repetencia escolar. hm048 tbm é repetencia
#jc07 usou alcool
#transformando em numerico AS VARIAVEIS NÃO PEGAS PELA FUNÇAO
library(magrittr)
dt_onda1e2e3e4$ZkSRQdepre_anx <- as.numeric(dt_onda1e2e3e4$ZkSRQdepre_anx)
dt_onda1e2e3e4$ZkSRQsomatic<- as.numeric(dt_onda1e2e3e4$ZkSRQsomatic)
dt_onda1e2e3e4$ZkSQRenergia <- as.numeric(dt_onda1e2e3e4$ZkSQRenergia)
dt_onda1e2e3e4$ZkSRQsentidepre <- as.numeric(dt_onda1e2e3e4$ZkSRQsentidepre)


map_chr(dt_onda1e2e3e4, class)
str(dt_onda1e2e3e4)
dt_onda1e2e3e4$hpconduct <- NULL
dt_onda1e2e3e4$hphyper  <- NULL
dt_onda1e2e3e4$hppeer <- NULL
dt_onda1e2e3e4$hpprosoc <- NULL 
dt_onda1e2e3e4$ZSco01 <- NULL
dt_onda1e2e3e4$LSD <- NULL


### vARIAVEL DROGA QUE ERA PRA EXCLUIR
##Excluindo variaveis de drogas RELACIONADAS 
dt_onda1e2e3e4$hc01 <- NULL
dt_onda1e2e3e4$jc01 <- NULL
dt_onda1e2e3e4$kfumar <- NULL
dt_onda1e2e3e4$kfumante <- NULL
dt_onda1e2e3e4$kc11 <- NULL
dt_onda1e2e3e4$kc12 <- NULL
dt_onda1e2e3e4$kc15 <- NULL
dt_onda1e2e3e4$hcsolvente <- NULL
dt_onda1e2e3e4$jcsolvente <- NULL
dt_onda1e2e3e4$kc10 <- NULL 
dt_onda1e2e3e4$hc07 <- NULL
dt_onda1e2e3e4$jc06 <- NULL
dt_onda1e2e3e4$kbebalc <- NULL
dt_onda1e2e3e4$jc11c <- NULL
dt_onda1e2e3e4$kc06 <- NULL
dt_onda1e2e3e4$kc09 <- NULL
dt_onda1e2e3e4$kc12 <- NULL 
dt_onda1e2e3e4$kc13 <- NULL
dt_onda1e2e3e4$kc14 <- NULL
dt_onda1e2e3e4$jc11d <- NULL
dt_onda1e2e3e4$kc05 <- NULL
dt_onda1e2e3e4$Zjc02 <- NULL
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(dt_onda1e2e3e4,2,pMiss)

obj_variavel_onda1e2e3e4 <- apply(dt_onda1e2e3e4,2,pMiss)
variavel_8poc_onda1e2e3e4 <- obj_variavel_onda1e2e3e4[obj_variavel_onda1e2e3e4 < 8] ##selecionando so as com 7% de missing
variavel_8poc_onda1e2e3e4 <- names(variavel_8poc_onda1e2e3e4)
library(stats) #selecionei colunas com 5% missing
dt2_onda1e2e3e4 <- dt_onda1e2e3e4 [,c(variavel_8poc_onda1e2e3e4)]
pMiss_dataset2 <- function(x){sum(is.na(x))/length(x)*100} #Ja ta multiplicado por 100
apply(dt2_onda1e2e3e4,2,pMiss_dataset2)


# Ver as classes das variaveis
library(purrr)
map_chr(dt2_onda1e2e3e4, class)
# Ver as classes das variaveis
library(purrr)
map_chr(dt2_onda1e2e3e4, class)
##Mudar os fatores da classe pq tem variavel com "haven_label" CONVERSAO DE VARIAVEL
dt3_onda1e2e3e4 <- dt2_onda1e2e3e4
library(purrr)
dt3_onda1e2e3e4 <- map_df(dt3_onda1e2e3e4, function(x){
  if (length(levels(as.factor(x))) > 8) z <- as.numeric(x) else z <- as.factor(x)}
)
#transformando em numerico AS VARIAVEIS NÃO PEGAS PELA FUNÇAO
library(magrittr)
dt3_onda1e2e3e4$ZkSRQdepre_anx <- as.numeric(dt3_onda1e2e3e4$ZkSRQdepre_anx)
dt3_onda1e2e3e4$ZkSRQsomatic<- as.numeric(dt3_onda1e2e3e4$ZkSRQsomatic)
dt3_onda1e2e3e4$ZkSQRenergia <- as.numeric(dt3_onda1e2e3e4$ZkSQRenergia)
dt3_onda1e2e3e4$ZkSRQsentidepre <- as.numeric(dt3_onda1e2e3e4$ZkSRQsentidepre)

map_chr(dt3_onda1e2e3e4, class)
dt3_onda1e2e3e4$hrenfa <- NULL

### HOLD OUT onda 1 E 2 E 3 e 4####
# Separar em banco de dados em Treino e Teste 
library(caret)
set.seed(123)
datasetbipolar_onda1e2e3e4 <- dt3_onda1e2e3e4 [sample (1:nrow (dt3_onda1e2e3e4), 3781, replace=FALSE), ]
## banco treino e teste ##
library(caret)
set.seed(123)
intrain_onda1e2e3e4 <- createDataPartition(y = datasetbipolar_onda1e2e3e4$bipolar1, p= 0.7, list = FALSE)
dtTrain_onda1e2e3e4 <-  datasetbipolar_onda1e2e3e4[intrain_onda1e2e3e4,]
dtTest_onda1e2e3e4 <- datasetbipolar_onda1e2e3e4[-intrain_onda1e2e3e4,]
dim(dtTrain_onda1e2e3e4); dim(dtTest_onda1e2e3e4)
# Porcentagem do desfecho em cada banco
prop.table(table(datasetbipolar_onda1e2e3e4$bipolar1))
prop.table(table(dtTrain_onda1e2e3e4$bipolar1))
prop.table(table(dtTest_onda1e2e3e4$bipolar1))
#iMPUTA??O USANDO media e moda  no treino pra aplicar depois no teste, evitando data linkage

train_matrix_onda1e2e3e4 <- dtTrain_onda1e2e3e4
test_matrix_onda1e2e3e4 <- dtTest_onda1e2e3e4

train_matrix_onda1e2e3e4 <- as.data.frame(train_matrix_onda1e2e3e4)
test_matrix_onda1e2e3e4 <- as.data.frame(test_matrix_onda1e2e3e4)
class(train_matrix_onda1e2e3e4)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

for (i in names(train_matrix_onda1e2e3e4)){
  print(class(train_matrix_onda1e2e3e4[,i]))
  print(is.factor(train_matrix_onda1e2e3e4[,i]))
  if (is.factor(train_matrix_onda1e2e3e4[,i])) {
    
    mode_value = getmode(train_matrix_onda1e2e3e4[,i])
    print("cat")
    #recorded_values[1,i] = mode_value
    train_matrix_onda1e2e3e4[is.na(train_matrix_onda1e2e3e4[,i]),i] = mode_value
    test_matrix_onda1e2e3e4[is.na(test_matrix_onda1e2e3e4[,i]),i] = mode_value
    print(mode_value)
  } else {
    mean_value = mean(train_matrix_onda1e2e3e4[,i], na.rm=TRUE)
    #recorded_values[1,i] = mean_value
    train_matrix_onda1e2e3e4[is.na(train_matrix_onda1e2e3e4[,i]),i] = mean_value
    test_matrix_onda1e2e3e4[is.na(test_matrix_onda1e2e3e4[,i]),i] = mean_value
    print("num")
  }
  
}

map_int(test_matrix_onda1e2e3e4, function(x){length(x[is.na(x)])})
str(train_matrix_onda1e2e3e4)
anyNA(train_matrix_onda1e2e3e4)
#train_matrix$lobs_sm <- NULL ##tinha missing
#test_matrix$lobs_sm <- NULL

anyNA(train_matrix_onda1e2e3e4)
anyNA(test_matrix_onda1e2e3e4)

data_rfe_onda1e2e3e4 <- train_matrix_onda1e2e3e4
prop.table(table(data_rfe_onda1e2e3e4$bipolar1))

data_testing_onda1e2e3e4 <- test_matrix_onda1e2e3e4
prop.table(table(data_testing_onda1e2e3e4$bipolar1))
#selecionar o banco bom

### treinando o algoritmo
levels(data_rfe_onda1e2e3e4$bipolar1)
levels(data_rfe_onda1e2e3e4$bipolar1) <- c("no", "yes")
levels(data_testing_onda1e2e3e4$bipolar1)
levels(data_testing_onda1e2e3e4$bipolar1) <- c("no", "yes")


library(DMwR)
set.seed(9560)
smote_train_onda1e2e3e4 <- SMOTE(bipolar1 ~ ., data  = data_rfe_onda1e2e3e4)                         
table(smote_train_onda1e2e3e4$bipolar1) 

#####Treinando com lasso onda 1 E 2 E 3 e 4 ####
# Com desequilibrio
library(caret)
library(glmnet)
lambda <- 10^seq(-3, 3, length = 100)
set.seed(108)
ctrl.rf_LASSO = trainControl(method="cv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary)
set.seed(5627)
smote_outside_LASSO_onda1e2e3e4 <- train(bipolar1 ~ ., data = smote_train_onda1e2e3e4, method = "glmnet", trControl = ctrl.rf_LASSO, tuneGrid=expand.grid(alpha = 1, lambda = lambda), tuneLength = 10, metric = "ROC")

##teste
library(e1071)
test_pred_smote_lasso_onda1e2e3e4 <- predict(smote_outside_LASSO_onda1e2e3e4, newdata = data_testing_onda1e2e3e4[,-1])
matrixConfusao_smote_lasso_onda1e2e3e4 <- confusionMatrix(test_pred_smote_lasso_onda1e2e3e4, data_testing_onda1e2e3e4$bipolar1, positive = "yes")
matrixConfusao_smote_lasso_onda1e2e3e4 
# ROC
library(caret)
library(pROC)
p_smote_lasso_onda1e2e3e4 <- predict (smote_outside_LASSO_onda1e2e3e4, data_testing_onda1e2e3e4[,-1])
p_prob_smote_lasso_onda1e2e3e4 <- predict (smote_outside_LASSO_onda1e2e3e4, data_testing_onda1e2e3e4, type = "prob")
confusionMatrix(p_smote_lasso_onda1e2e3e4, data_testing_onda1e2e3e4$bipolar1)
print(confusionMatrix(p_smote_lasso_onda1e2e3e4, data_testing_onda1e2e3e4$bipolar1, positive = "yes"))
r_smote_lasso_onda1e2e3e4 <- roc (data_testing_onda1e2e3e4$bipolar1, p_prob_smote_lasso_onda1e2e3e4[,"yes"])
plot(r_smote_lasso_onda1e2e3e4)
r_smote_lasso_onda1e2e3e4$auc
varImp(smote_outside_LASSO_onda1e2e3e4)

#mudando limiar
library(SDMTools)

# desfecho do conjunto de teste
# 0 = No, 1 = Yes
obs_onda1e2e3e4 <- data_testing_onda1e2e3e4$bipolar1

# Os levels estão em ordem?
levels(obs_onda1e2e3e4)

levels(obs_onda1e2e3e4) <- c("0", "1")
obs_onda1e2e3e4 <- as.numeric(as.character(obs_onda1e2e3e4))
obs_onda1e2e3e4

# predições do modelo em probabilidades
rf.predict_onda1e2e3e4 <- predict(smote_outside_LASSO_onda1e2e3e4, data_testing_onda1e2e3e4[, -1], type = "prob")
predictions_onda1e2e3e4 <- as.vector(rf.predict_onda1e2e3e4[, 2])
predictions_onda1e2e3e4

confusion_df_onda1e2e3e4 <- data.frame(obs_onda1e2e3e4, predictions_onda1e2e3e4)
threshold_seq_onda1e2e3e4 <- seq(0, 1, by = 0.01)

confMatrix_onda1e2e3e4 <- function(i, obs_onda1e2e3e4, predictions_onda1e2e3e4, ...){
  require(caret)
  require(SDMTools)
  
  conf_matrix <- confusion.matrix(obs_onda1e2e3e4, predictions_onda1e2e3e4, threshold = i)
  cm_table <- as.table(conf_matrix)
  cm <- confusionMatrix(cm_table, positive = "1")
  p_acc <- cm$overall[6]
  acc <- cm$overall[1]
  null_acc <- cm$overall[5]
  ppv <- cm$byClass[3]
  npv <- cm$byClass[4]
  
  result <-  c("limiar" = i, cm$byClass[1], cm$byClass[2], 
               "AB" = as.numeric(c((cm$byClass[1]+cm$byClass[2]) / 2)), 
               acc, p_acc, null_acc, ppv, npv)
  result
}

matrixConfusao_smote_lasso_onda1e2e3e4$byClass
library(purrr)
library(plyr)
confMatrix(0.5, obs_onda1e2e3e4, predictions_onda1e2e3e4)
result_list_onda1e2e3e4 <- map(threshold_seq_onda1e2e3e4, confMatrix_onda1e2e3e4, obs_onda1e2e3e4 = obs_onda1e2e3e4, predictions_onda1e2e3e4 = predictions_onda1e2e3e4)
result_df_onda1e2e3e4 <- ldply(result_list_onda1e2e3e4)

result_df_onda1e2e3e4
cat("O sensitivity do limiar com maior acurácia balanceada é", result_df_onda1e2e3e4$Sensitivity[which.max(result_df_onda1e2e3e4$AB)])
cat("O ESPECIFICIDADE do limiar com maior acurácia balanceada é", result_df_onda1e2e3e4$Specificity[which.max(result_df_onda1e2e3e4$AB)])
cat("O PPV do limiar com maior acurácia balanceada é", result_df_onda1e2e3e4$`Pos Pred Value`[which.max(result_df$AB)])
cat("O ACURACIA do limiar com maior acurácia balanceada é", result_df_onda1e2e3e4$Accuracy[which.max(result_df_onda1e2e3e4$AB)])
cat("O AB do limiar com maior acurácia balanceada é", result_df_onda1e2e3e4$AB[which.max(result_df_onda1e2e3e4$AB)])
cat("O limiar com maior Acurácia Balanceada é", result_df_onda1e2e3e4$limiar[which.max(result_df_onda1e2e3e4$AB)])
cat("O PPV do limiar com maior acurácia balanceada é", result_df_onda1e2e3e4$`Pos Pred Value`[which.max(result_df_onda1e2e3e4$AB)])


############ colunas por ondas #####
#1033, 29, 9:11, 15,17, 22:26, 989:990, 986, 1016:1019, 1021:1022, #onda 1
#40, 48, 57:89,132, 139, 141:142, 144, 937:941, 986, 991, #onda 2
#157:189, 191:193, 198,200, 202, 204, 205, 944:948 #onda 3
