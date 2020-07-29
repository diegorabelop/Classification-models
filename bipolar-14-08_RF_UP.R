
###LASSO####

# Importar o banco de dados##
library(haven)
banco_pelotas <- read_sav("C:/Users/TB e Internet/Downloads/banco_pelotas_30-04-VERSAO2.sav")
library(dplyr)
dt <- banco_pelotas[!is.na(banco_pelotas$bipolar1),]
#propor??o de TB
prop.table(table(dt$bipolar1))

####Criando novas variaveis das drogas #####
sumFeatures <- function(x, ...){
  l <- length(x[is.na(x)]) 
  if (l == length(x)) y <- NA else y <- sum(x[!is.na(x)])
  return(y)
}

#cigarro
df_fumar <- data.frame(dt$hc01, dt$jc01, dt$kfumar)
fumar_lifetime <- apply(df_fumar, 1, sumFeatures)

fumar_lifetime[fumar_lifetime > 1] <- 1
df_fumar$fumar_lifetime <- fumar_lifetime
## alucinogenos
df_alucinogeno <-data.frame(dt$kc11, dt$kc12, dt$kc15)
alucinogeno_lifetime <- apply(df_alucinogeno, 1, sumFeatures)

alucinogeno_lifetime[alucinogeno_lifetime > 1] <- 1
df_alucinogeno$alucinogeno_lifetime <- alucinogeno_lifetime
##Solventes
df_solvente <-data.frame(dt$hcsolvente, dt$jcsolvente, dt$kc10)
solvente_lifetime <- apply(df_solvente, 1, sumFeatures)
solvente_lifetime[solvente_lifetime > 1] <- 1
df_solvente$solvente_lifetime <- solvente_lifetime
#Alcool
df_alcool <-data.frame(dt$hc07, dt$jc06, dt$kbebalc)
alcool_lifetime <- apply(df_alcool, 1, sumFeatures)
alcool_lifetime[alcool_lifetime > 1] <- 1
df_alcool$alcool_lifetime <- alcool_lifetime
#Cocaina
df_cocaina <-data.frame(dt$jc11c, dt$kc06, dt$kc09, dt$kc12, dt$kc13, dt$kc14)
cocaina_lifetime <- apply(df_cocaina, 1, sumFeatures)
cocaina_lifetime[cocaina_lifetime > 1] <- 1
df_cocaina$cocaina_lifetime <- cocaina_lifetime
#Maconha
df_maconha <-data.frame(dt$jc11d, dt$kc05)
maconha_lifetime <- apply(df_maconha, 1, sumFeatures)
maconha_lifetime[maconha_lifetime > 1] <- 1
df_maconha$maconha_lifetime <- maconha_lifetime

##Inserindo colunas novas
dt$fumar_lifetime<- fumar_lifetime 
dt$alucino_lifetime <- alucinogeno_lifetime
dt$solvente_lifetime <- solvente_lifetime
dt$alcool_lifetime <- alcool_lifetime
dt$cocaina_lifetime <- cocaina_lifetime
dt$maconha_lifetime <- maconha_lifetime

#transformar em continuo
dt$ZkSQRenergia <- as.numeric(dt$ZkSQRenergia)
dt$ZkSRQsentidepre <- as.numeric(dt$ZkSRQsentidepre)
dt$ZkSRQsomatic <- as.numeric(dt$ZkSRQsomatic)
dt$ZkSRQsentidepre <- as.numeric(dt$ZkSRQsentidepre)
dt$ZkSRQdepre_anx <- as.numeric(dt$ZkSRQdepre_anx)


# Ver as classes das variaveis
library(purrr)
map_chr(dt, class)
# Ver as classes das variaveis
library(purrr)
map_chr(dt, class)
##Mudar os fatores da classe pq tem variavel com "haven_label" CONVERSAO DE VARIAVEL
library(purrr)
dt2 <- map_df(dt, function(x){
  if (length(levels(as.factor(x))) > 8) z <- as.numeric(x) else z <- as.factor(x)}
)
map_chr(dt2, class)

# Ver as classes das variaveis
library(purrr)
map_chr(dt2, class)
##Mudar os fatores da classe pq tem variavel com "haven_label" CONVERSAO DE VARIAVEL
dt3 <- dt2
library(purrr)
dt3 <- map_df(dt2, function(x){
  if (length(levels(as.factor(x))) > 8) z <- as.numeric(x) else z <- as.factor(x)}
)
map_chr(dt3, class)
dt3$kc06 <- NULL
dt3$kc09 <- NULL
dt3$kc13 <- NULL
dt3$kc14 <- NULL
dt3$kc10 <- NULL
dt3$kc16 <- NULL
dt3$nquest <- NULL
dt2$hamiltonscore <- NULL
dt3$hpconduct <- NULL
dt3$hphyper  <- NULL
dt3$hppeer <- NULL
dt3$hpprosoc <- NULL 
dt3$ZSco01 <- NULL
dt3$LSD <- NULL
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

### HOLD OUT ####
# Separar em banco de dados em Treino e Teste 
library(caret)
set.seed(123)
datasetbipolar <- dt3 [sample (1:nrow (dt3), 3781, replace=FALSE), ]
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
#transformar em continuo
dtTrain$ZkSQRenergia <- as.numeric(dtTrain$ZkSQRenergia)
dtTrain$ZkSRQsentidepre <- as.numeric(dtTrain$ZkSRQsentidepre)
dtTrain$ZkSRQsomatic <- as.numeric(dtTrain$ZkSRQsomatic)
dtTrain$ZkSRQsentidepre <- as.numeric(dtTrain$ZkSRQsentidepre)
dtTrain$ZkSRQdepre_anx <- as.numeric(dtTrain$ZkSRQdepre_anx)

dtTest$ZkSQRenergia <- as.numeric(dtTest$ZkSQRenergia)
dtTest$ZkSRQsentidepre <- as.numeric(dtTest$ZkSRQsentidepre)
dtTest$ZkSRQsomatic <- as.numeric(dtTest$ZkSRQsomatic)
dtTest$ZkSRQsentidepre <- as.numeric(dtTest$ZkSRQsentidepre)
dtTest$ZkSRQdepre_anx <- as.numeric(dtTest$ZkSRQdepre_anx)
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

treino_onda4 <- train_matrix[,c("bipolar1","ksentfel","kpesfel", "kcompfel", "kvidafel","kvidatri","ktabagismodiario", "kprosau", "kmorpar", "kmorprox","kprobdin","kmudcas", "kternam", "krixabai","kinsbai", "kbriga", "kapanhou","kassalto","kauditdic", "kmotpsiq3", "kmini17", "kmini1", "kmini2", "kmini13", "kmini14", "kmini15", "kmini33a","kmini33b", "kmini33c","kmini36a","kmini36b","kmini36c", "kmini36d","kmini40", "kmini41", "kmini51", "kmini52", "kmini53", "kmini54", "kmini55", "kmini56", "Zkqvfisico", "Zkqvpsico", "Zkqvsocial", "Zkqvambi","ZkSRQdepre_anx", "ZkSRQsomatic", "ZkSQRenergia", "ZkSRQsentidepre", "histpsiquiatri", "Zkescadol", "Zkibem", "Zkfeliz", "cocaina_lifetime", "maconha_lifetime", "alucino_lifetime", "solvente_lifetime", "alcool_lifetime")]
prop.table(table(treino_onda4$bipolar1))
teste_onda4 <- test_matrix[,c("bipolar1","ksentfel","kpesfel", "kcompfel", "kvidafel","kvidatri","ktabagismodiario", "kprosau", "kmorpar", "kmorprox","kprobdin","kmudcas", "kternam", "krixabai","kinsbai", "kbriga", "kapanhou","kassalto","kauditdic", "kmotpsiq3", "kmini17", "kmini1", "kmini2", "kmini13", "kmini14", "kmini15", "kmini33a","kmini33b", "kmini33c","kmini36a","kmini36b","kmini36c", "kmini36d","kmini40", "kmini41", "kmini51", "kmini52", "kmini53", "kmini54", "kmini55", "kmini56", "Zkqvfisico", "Zkqvpsico", "Zkqvsocial", "Zkqvambi","ZkSRQdepre_anx", "ZkSRQsomatic", "ZkSQRenergia", "ZkSRQsentidepre", "histpsiquiatri", "Zkescadol", "Zkibem", "Zkfeliz", "cocaina_lifetime", "maconha_lifetime", "alucino_lifetime", "solvente_lifetime", "alcool_lifetime")]
prop.table(table(teste_onda4$bipolar1))
prop.table(table(treino_onda4$bipolar1))
anyNA(treino_onda4)
anyNA(teste_onda4)
#Recursive Feature Selection
library(randomForest)
library(ggplot2)
library(caret)
set.seed(101)
trainctrl <- trainControl(classProbs= TRUE, summaryFunction = twoClassSummary, savePredictions = TRUE)
rfFuncs$summary <- twoClassSummary
control <- rfeControl(functions=rfFuncs, method="cv", number = 10, returnResamp="final")
#Usei o comando abaixo pq minha vari?vel "suicidio" ta no meio das colunas. Tive que descrever todas. Tirei as variaveis colineares
results <- rfe(treino_onda4[,c(2:58)], treino_onda4$bipolar1, rfeControl=control, method="rf", metric = "ROC", trControl=trainctrl)
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
data_rfe <- treino_onda4
prop.table(table(data_rfe$bipolar1))
data_testing <- teste_onda4
prop.table(table(data_testing$bipolar1))
### treinando o algoritmo
levels(data_rfe$bipolar1)
levels(data_rfe$bipolar1) <- c("no", "yes")
levels(data_testing$bipolar1)
levels(data_testing$bipolar1) <- c("no", "yes")


### Corrigindo imbalance class https://topepo.github.io/caret/subsampling-for-class-imbalances.html
library(caret)
set.seed(9560)
###Treinando com lasso ondaa 4 ####
# Com desequilibrio
library(caret)
library(glmnet)
lambda <- 10^seq(-3, 3, length = 100)
set.seed(108)
ctrl.rf_up_LASSO <- trainControl(method="cv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary, sampling = "up")
up_outside_LASSO <- train(bipolar1 ~., data = data_rfe, method = "glmnet", trControl = ctrl.rf_up_LASSO, tuneLength = 10, tuneGrid=expand.grid(alpha = 1, lambda = lambda), metric = "ROC")
##Com Smote
library(DMwR)
set.seed(9560)
smote_train <- SMOTE(bipolar1 ~ ., data  = data_rfe)                         
table(smote_train$bipolar1) 
##Smote
set.seed(5627)
ctrl.rf_LASSO_smote <- trainControl(method="cv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary)
smote_outside_LASSO <- train(bipolar1 ~ ., data = smote_train, method = "glmnet", trControl = ctrl.rf_LASSO_smote, tuneGrid=expand.grid(alpha = 1, lambda = lambda), tuneLength = 10, metric = "ROC")

library(ROSE)
set.seed(9560)
rose_train <- ROSE(bipolar1 ~ ., data  = data_rfe)$data                         
table(rose_train$bipolar1)
#rose
set.seed(5627)
ctrl.rf_LASSO_rose <- trainControl(method="cv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary)
rose_outside_LASSO <- train(bipolar1 ~ ., data = rose_train, method = "glmnet", trControl = ctrl.rf_LASSO_rose, tuneLength = 10, tuneGrid=expand.grid(alpha = 1, lambda = lambda), metric = "ROC")
beepr::beep()

##Com mil repetições
set.seed(5627) #downregulation
ctrl.rf_down_LASSO = trainControl(method="repeatedcv", number = 10, repeats = 1000, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary)
down_outside_LASSO <- train(bipolar1 ~., data = data_rfe, method = "glmnet", trControl = ctrl.rf_down_LASSO, tuneLength = 10, tuneGrid = expand.grid(alpha = 1, lambda = lambda), metric = "ROC")


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


####### Elastic net####
library(caret)
library(glmnet)
set.seed(108)
ctrl.rf_up_NET <- trainControl(method="cv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary, sampling = "up")
up_outside_NET <- train(bipolar1 ~., data = data_rfe, method = "glmnet", trControl = ctrl.rf_up_NET, tuneLength = 10, metric = "ROC")
ctrl.rf_smote_NET <- trainControl(method="cv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary)
smote_outside_NET <- train(bipolar1 ~., data = smote_train, method = "glmnet", trControl = ctrl.rf_smote_NET, tuneLength = 10, metric = "ROC")
ctrl.rf_rose_NET <- trainControl(method="cv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary)
rose_outside_NET <- train(bipolar1 ~., data = smote_train, method = "glmnet", trControl = ctrl.rf_rose_NET, tuneLength = 10, metric = "ROC")

# matrix de confusao 
library(e1071)
test_pred_rose_NET <- predict(rose_outside_NET, newdata = data_testing[,-1])
matrixConfusao_rose_NET <- confusionMatrix(test_pred_rose_NET, data_testing$bipolar1, positive = "yes")
matrixConfusao_rose_NET 
# ROC
library(caret)
library(pROC)
p_rose_NET <- predict (rose_outside_NET, data_testing[,-1])
p_prob_rose_NET <- predict (rose_outside_NET, data_testing[,-1], type = "prob")
confusionMatrix(p_rose_NET, data_testing$bipolar1)
print(confusionMatrix(p_rose_NET, data_testing$bipolar1, positive = "yes"))
r_rose_NET <- roc (data_testing$bipolar1, p_prob_rose_NET[,"yes"])
plot(r_rose_NET)
r_rose_NET$auc
varImp(rose_outside_NET)

##Smote
# matrix de confusao 
library(e1071)
test_pred_smote_NET <- predict(smote_outside_NET, newdata = data_testing[,-1])
matrixConfusao_smote_NET <- confusionMatrix(test_pred_smote_NET, data_testing$bipolar1, positive = "yes")
matrixConfusao_smote_NET 
# ROC
library(caret)
library(pROC)
p_smote_NET <- predict (smote_outside_NET, data_testing[,-1])
p_prob_smote_NET <- predict (smote_outside_NET, data_testing[,-1], type = "prob")
confusionMatrix(p_smote_NET, data_testing$bipolar1)
print(confusionMatrix(p_smote_NET, data_testing$bipolar1, positive = "yes"))
r_smote_NET <- roc (data_testing$bipolar1, p_prob_smote_NET[,"yes"])
plot(r_smote_NET)
r_smote_NET$auc
varImp(smote_outside_NET)
#####Random Forest ####
library(caret)
library(glmnet)
set.seed(108)
grid.rf <- expand.grid(.mtry=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14))
#up
set.seed(5627)
ctrl.rf_up_RF <- trainControl(method="cv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary, sampling = "up")
up_outside_RF <- train(bipolar1 ~ ., data = data_rfe, method = "rf", trControl = ctrl.rf_up_RF, tuneGrid=grid.rf, tuneLength = 10, metric = "ROC")
#smote
set.seed(5627)
ctrl.rf_smote_RF <- trainControl(method="cv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary)
smote_outside_RF <- train(bipolar1 ~ ., data = smote_train, method = "rf", trControl = ctrl.rf_smote_RF, tuneGrid=grid.rf, tuneLength = 10, metric = "ROC")
#rose
set.seed(5627)
ctrl.rf_rose_RF <- trainControl(method="cv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary)
rose_outside_RF <- train(bipolar1 ~ ., data = rose_train, method = "rf", trControl = ctrl.rf_rose_RF, tuneGrid=grid.rf, tuneLength = 10, metric = "ROC")
library(beepr)
beep()
# matrix de confusao 
library(e1071)
test_pred_up_RF <- predict(up_outside_RF, newdata = data_testing[,-1])
matrixConfusao_up_RF <- confusionMatrix(test_pred_up_RF, data_testing$bipolar1, positive = "yes")
matrixConfusao_up_RF 
# ROC
library(caret)
library(pROC)
p_up_RF <- predict (up_outside_RF, data_testing[,-1])
p_prob_up_RF <- predict (up_outside_RF, data_testing[,-1], type = "prob")
confusionMatrix(p_up_RF, data_testing$bipolar1)
print(confusionMatrix(p_up_RF, data_testing$bipolar1, positive = "yes"))
r_up_RF <- roc (data_testing$bipolar1, p_prob_up_RF[,"yes"])
plot(r_up_RF)
r_up_RF$auc
varImp(up_outside_RF)

# matrix de confusao 
library(e1071)
test_pred_rose_RF <- predict(rose_outside_RF, newdata = data_testing[,-1])
matrixConfusao_rose_RF <- confusionMatrix(test_pred_rose_RF, data_testing$bipolar1, positive = "yes")
matrixConfusao_rose_RF 
# ROC
library(caret)
library(pROC)
p_rose_RF <- predict (rose_outside_RF, data_testing[,-1])
p_prob_rose_RF <- predict (rose_outside_RF, data_testing[,-1], type = "prob")
confusionMatrix(p_rose_RF, data_testing$bipolar1)
print(confusionMatrix(p_rose_RF, data_testing$bipolar1, positive = "yes"))
r_rose_RF <- roc (data_testing$bipolar1, p_prob_rose_RF[,"yes"])
plot(r_rose_RF)
r_rose_RF$auc
varImp(rose_outside_RF)

##Smote
# matrix de confusao 
library(e1071)
test_pred_smote_RF <- predict(smote_outside_RF, newdata = data_testing[,-1])
matrixConfusao_smote_RF <- confusionMatrix(test_pred_smote_RF, data_testing$bipolar1, positive = "yes")
matrixConfusao_smote_RF 
# ROC
library(caret)
library(pROC)
p_smote_RF <- predict (smote_outside_RF, data_testing[,-1])
p_prob_smote_RF <- predict (smote_outside_RF, data_testing[,-1], type = "prob")
confusionMatrix(p_smote_RF, data_testing$bipolar1)
print(confusionMatrix(p_smote_RF, data_testing$bipolar1, positive = "yes"))
r_smote_RF <- roc (data_testing$bipolar1, p_prob_smote_RF[,"yes"])
plot(r_smote_RF)
r_smote_RF$auc
varImp(smote_outside_RF)



# matrix de confusao 
library(e1071)
test_pred_up_RF <- predict(up_outside_RF, newdata = data_testing[,-1])
matrixConfusao_up_RF <- confusionMatrix(test_pred_up_RF, data_testing$bipolar1, positive = "yes")
matrixConfusao_up_RF 
# ROC
library(caret)
library(pROC)
p_up_RF <- predict (up_outside_RF, data_testing[,-1])
p_prob_up_RF <- predict (up_outside_RF, data_testing[,-1], type = "prob")
confusionMatrix(p_up_RF, data_testing$bipolar1)
print(confusionMatrix(p_up_RF, data_testing$bipolar1, positive = "yes"))
r_up_RF <- roc (data_testing$bipolar1, p_prob_up_RF[,"yes"])
plot(r_up_RF)
r_up_RF$auc
varImp(up_outside_RF)
library(SDMTools)

### COMEÇAR AQUI ####
# desfecho do conjunto de teste
# 0 = No, 1 = Yes
obs <- data_testing$bipolar1

# Os levels estão em ordem?
levels(obs)

levels(obs) <- c("0", "1")
obs <- as.numeric(as.character(obs))
obs

# predições do modelo em probabilidades
rf.predict <- predict(up_outside_RF, data_testing[, -1], type = "prob")
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

matrixConfusao_up_RF$byClass
library(purrr)
library(plyr)
confMatrix(0.5, obs, predictions)
result_list <- map(threshold_seq, confMatrix, obs = obs, predictions = predictions)
result_df <- ldply(result_list)

result_df
