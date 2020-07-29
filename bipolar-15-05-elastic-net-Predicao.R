
####______USANDO ElasticNet_______####

# Importar o banco de dados##
library(haven)
banco_pelotas <- read_sav("C:/Users/TB e Internet/Downloads/banco_pelotas_30-04-VERSAO2.sav")

#remove missing in lmania_puro
library(dplyr)
dt <- banco_pelotas[!is.na(banco_pelotas$lmania_puro),]
#propor??o de TB
prop.table(table(dt$lmania_puro)) 
#### excluir variaveis Deixei as subescalas SDQ dos pais e filhos aos 11 anos
dt1 <- dt[-c(1:2, 3, 5, 6:7,11:13, 15:16, 18:20, 22:24, 27, 30:37, 40, 46:47, 49:56, 
             110:111, 117:119, 125:131, 133, 135:140, 142:143, 145:156, 190:215, 222:231, 
             234:237, 243, 245:247, 248:258, 260:261, 270, 272:429, 435:453, 455, 458:459, 
             461:466, 468:471, 473:514, 515:526, 527:535, 537, 539, 541, 543, 545, 547, 
             549, 551, 553, 555:596, 598, 600, 602, 604:748, 749:893, 895:920, 925:930, 936, 942,
             949, 951:952, 957:958, 970:971, 975:976, 978:979, 983:985, 999, 1001, 1005, 1012: 1015, 1020)]


dt2 <- dt1[-c(149:153, 154:169, 173, 200:208, 210:212, 220:222,224, 226:229, 245:251, 253)] #excluindo variaveis da ultima onda
prop.table(table(dt2$lmania_puro))
##Variavel Lmania
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(dt2,2,pMiss)

obj_variavel <- apply(dt2,2,pMiss)
variavel_7.5poc <- obj_variavel[obj_variavel < 7.5] ##selecionando so as com 7% de missing
variavel_7.5poc <- names(variavel_7.5poc)
library(stats) #selecionei colunas com 5% missing
dt3 <- dt2 [,c(variavel_7.5poc)]
pMiss_dataset2 <- function(x){sum(is.na(x))/length(x)*100} #Ja ta multiplicado por 100
apply(dt3,2,pMiss_dataset2)

# Ver as classes das variaveis
library(purrr)
map_chr(dt3, class)
#apply(dt2,2,pMiss_dataset2)
dt3$hamiltonscore <- NULL
# Ver as classes das variaveis
library(purrr)
map_chr(dt3, class)
##Mudar os fatores da classe pq tem variavel com "haven_label" CONVERSAO DE VARIAVEL
dt4 <- dt3
library(purrr)
dt4 <- map_df(dt4, function(x){
  if (length(levels(as.factor(x))) > 8) z <- as.numeric(x) else z <- as.factor(x)}
)
map_chr(dt4, class)

dt4$hpemotion <-NULL
dt4$hpconduct <- NULL
dt4$hphyper  <- NULL
dt4$hppeer <- NULL
dt4$hpprosoc <- NULL 
dt4$ZSco01 <- NULL
dt4$Zjc02 <- NULL
library(magrittr)
con.names = c(149:152)
dt4[,con.names] = data.frame(sapply(dt4[,con.names], as.numeric))
dt4$hsemotion <- NULL
dt4$hsconduct <- NULL
dt4$hshyper <- NULL
dt4$hspeer <- NULL
dt4$hsprosoc <- NULL

map_chr(dt4, class)
str(dt4)

### HOLD OUT ####
# Separar em banco de dados em Treino e Teste 
library(caret)
set.seed(123)
datasetbipolar <- dt4 [sample (1:nrow (dt3), 3781, replace=FALSE), ]


## banco treino e teste ##
library(caret)
set.seed(123)
intrain <- createDataPartition(y = datasetbipolar$lmania_puro, p= 0.7, list = FALSE)
dtTrain <-  datasetbipolar[intrain,]
dtTest <- datasetbipolar[-intrain,]
dim(dtTrain); dim(dtTest)
# Porcentagem do desfecho em cada banco
prop.table(table(datasetbipolar$lmania_puro))
prop.table(table(dtTrain$lmania_puro))
prop.table(table(dtTest$lmania_puro))

##proporção de missing no treino e teste
library(VIM)
data_NA_training <- aggr(dtTrain, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(dtTrain), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern")) 

data_NA_testing <- aggr(dtTest, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(dtTest), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern")) 


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

##excluindo VD
data_rfe$Zjc07 <- NULL
data_testing$Zjc07 <- NULL
data_rfe$hpemotion <-NULL
data_rfe$hpconduct <- NULL
data_rfe$hphyper  <- NULL
data_rfe$hppeer <- NULL
data_rfe$hpprosoc <- NULL 
data_rfe$ZSco01 <- NULL
data_rfe$Zjc02 <- NULL
data_rfe$hsemotion <- NULL
data_rfe$hsconduct <- NULL
data_rfe$hshyper <- NULL
data_rfe$hspeer <- NULL
data_rfe$hsprosoc <- NULL

data_testing$hpemotion <-NULL
data_testing$hpconduct <- NULL
data_testing$hphyper  <- NULL
data_testing$hppeer <- NULL
data_testing$hpprosoc <- NULL 
data_testing$ZSco01 <- NULL
data_testing$Zjc02 <- NULL
data_testing$hsemotion <- NULL
data_testing$hsconduct <- NULL
data_testing$hshyper <- NULL
data_testing$hspeer <- NULL
data_testing$hsprosoc <- NULL

## O pacote doParallel n?o pode ser executado tudo com o Caret
library(randomForest)
library(ggplot2)
library(caret)
set.seed(101)
trainctrl <- trainControl(classProbs= TRUE, summaryFunction = twoClassSummary, savePredictions = TRUE)
rfFuncs$summary <- twoClassSummary
control <- rfeControl(functions=rfFuncs, method="cv", number = 10, returnResamp="final")
#Usei o comando abaixo pq minha vari?vel "suicidio" ta no meio das colunas. Tive que descrever todas. Tirei as variaveis colineares
results <- rfe(train_matrix[,c(1:168)], train_matrix$lmania_puro, rfeControl=control, method="rf", metric = "ROC", trControl=trainctrl)
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
prop.table(table(data_rfe$lmania_puro))

data_testing <- test_matrix
prop.table(table(data_testing$lmania_puro))
#selecionar o banco bom

### treinando o algoritmo
levels(data_rfe$lmania_puro)
levels(data_rfe$lmania_puro) <- c("no", "yes")
levels(data_testing$lmania_puro)
levels(data_testing$lmania_puro) <- c("no", "yes")

### Corrigindo imbalance class https://topepo.github.io/caret/subsampling-for-class-imbalances.html
library(caret)
set.seed(9560)
#down_train <- downSample(x = data_rfe[, -ncol(data_rfe)],                         y = data_rfe$lmania_puro)
#table(down_train$lmania_puro)

#set.seed(9560)
#up_train <- upSample(x = data_rfe[, -ncol(data_rfe)], y = data_rfe$lmania_puro)
#table(up_train$lmania_puro)

library(DMwR)
set.seed(9560)
smote_train <- SMOTE(lmania_puro ~ ., data  = data_rfe)                         
table(smote_train$lmania_puro) 

library(ROSE)
set.seed(9560)
rose_train <- ROSE(lmania_puro ~ ., data  = data_rfe)$data                         
table(rose_train$lmania_puro) 

#orig_fit <- train(lmania_puro ~ ., data = data_rfe, method = "rf", trControl = ctrl.rf, tuneGrid=grid.rf, metric = "ROC")


###Treinando com ELASTIC NET
# Com desequilibrio
library(caret)
library(glmnet)
set.seed(108)
ctrl.rf= trainControl(method="cv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary)
set.seed(5627) #downregulation
ctrl.rf_down <- trainControl(method="cv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary, sampling = "down")
down_outside <- train(lmania_puro ~., data = data_rfe, method = "glmnet", trControl = ctrl.rf_down, tuneLength = 10, metric = "ROC")
set.seed(5627) #up regulation
ctrl.rf_up <- trainControl(method="cv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary, sampling = "up")
up_outside <- train(lmania_puro ~., data = data_rfe, method = "glmnet", trControl = ctrl.rf_down, tuneLength = 10, metric = "ROC")
##Smote
set.seed(5627)
smote_outside <- train(lmania_puro ~ ., data = smote_train, method = "glmnet", trControl = ctrl.rf, tuneLength = 10, metric = "ROC")

#Rose
set.seed(5627)
rose_outside <- train(lmania_puro ~ ., data = rose_train, method = "glmnet", trControl = ctrl.rf, tuneLength = 10, metric = "ROC")


outside_models <- list(down = down_outside,
                       up = up_outside,
                       SMOTE = smote_outside,
                       ROSE = rose_outside)

resampling <- resamples(outside_models)
summary(resampling)
summary(resampling, metric = "ROC")
bwplot(resampling)


# matrix de confusao 
library(e1071)
test_pred_smote <- predict(smote_outside, newdata = data_testing[,-170])
matrixConfusao_smote <- confusionMatrix(test_pred_smote, data_testing$lmania_puro, positive = "yes")
matrixConfusao_smote 
# ROC
library(caret)
library(pROC)
p_smote<- predict (smote_outside, data_testing[,-170])
p_prob_smote <- predict (smote_outside, data_testing, type = "prob")
confusionMatrix(p_smote, data_testing$lmania_puro)
print(confusionMatrix(p_smote, data_testing$lmania_puro, positive = "yes"))
r_smote <- roc (data_testing$lmania_puro, p_prob_smote[,"yes"])
plot(r_smote)

r_smote$auc

varImp(smote_outside)


############## ---- com ROSE
# matrix de confusao 
library(e1071)
test_pred_rose <- predict(rose_outside, newdata = data_testing[,-170])
matrixConfusao_rose <- confusionMatrix(test_pred_rose, data_testing$lmania_puro, positive = "yes")
matrixConfusao_rose 
# ROC
library(caret)
library(pROC)
p_rose <- predict (rose_outside, data_testing[,-170])
p_prob_rose <- predict (rose_outside, data_testing[,-170], type = "prob")
confusionMatrix(p_rose, data_testing$lmania_puro)
print(confusionMatrix(p_rose, data_testing$lmania_puro, positive = "yes"))
r_rose <- roc (data_testing$lmania_puro, p_prob_rose[,"yes"])
plot(r_rose)
r_rose$auc
varImp(rose_outside)

library(caret)
library(pROC)
p_down <- predict (down_outside, data_testing[,-170])
p_prob_down <- predict (down_outside, data_testing[,-170], type = "prob")
confusionMatrix(p_down, data_testing$lmania_puro)
print(confusionMatrix(p_down, data_testing$lmania_puro, positive = "yes"))
r_down <- roc (data_testing$lmania_puro, p_prob_down[,"yes"])
plot(r_down)
r_down$auc

p_up <- predict (up_outside, data_testing[,-170])
p_prob_up <- predict (up_outside, data_testing[,-170], type = "prob")
confusionMatrix(p_up, data_testing$lmania_puro)
print(confusionMatrix(p_up, data_testing$lmania_puro, positive = "yes"))
r_up <- roc (data_testing$lmania_puro, p_prob_up[,"yes"])
plot(r_up)
r_up$auc
#### mudando limiar da ROC
library(SDMTools)

### COMEÇAR AQUI ####
# desfecho do conjunto de teste
# 0 = No, 1 = Yes
obs <- data_testing$lmania_puro

# Os levels estão em ordem?
levels(obs)

levels(obs) <- c("0", "1")
obs <- as.numeric(as.character(obs))
obs

# predições do modelo em probabilidades
rf.predict <- predict(smote_outside, data_testing[, -170], type = "prob")
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

matrixConfusao_rose$byClass
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


max(result_df$Accuracy)
max(result_df$AB)

{plot(result_df$limiar, result_df$AB, type = "l", ylim = c(min(result_df$Accuracy), max(result_df$Accuracy)))
  lines(result_df$limiar, result_df$`Pos Pred Value`, col = "red")}

plot(result_df$Sensitivity, result_df$Accuracy)
plot(result_df$Specificity, result_df$AccuracyPValue)

plot(result_df$Specificity, result_df$AccuracyPValue)
{plot(result_df$limiar, result_df$AccuracyPValue, type = "l", ylim = c(0, 0.5))
  abline(h = 0.05, col = "red")}

min(result_df$AccuracyPValue)

############PAREI








m.rf_down = train(suicidio ~ ., data = data_rfe, method = "rf", trControl = ctrl.rf, tuneGrid=grid.rf)
m.rf_down
summary(m.rf_down)
#m.rf_down$pred$mtry == 1
plot(m.rf_down)
# estimate variable importance
importance_rf_down <- varImp(m.rf_down, scale=FALSE)
# summarize importance
print(importance_rf_down)
# plot importance
plot(importance_rf_down)
# SD
m.rf_down_results <- m.rf_down$results
View(m.rf_down_results)
