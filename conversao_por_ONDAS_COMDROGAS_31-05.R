
####______USANDO LASSO_______####

# Importar o banco de dados##
library(haven)
banco_pelotas <- read_sav("C:/Users/TB e Internet/Downloads/banco_pelotas_30-04-VERSAO3-predicaoPORondas.sav")

#remove missing in lmania_puro
library(dplyr)
dt <- banco_pelotas[!is.na(banco_pelotas$lmania_puro),]
#propor??o de TB
prop.table(table(dt$lmania_puro)) 
#### excluir variaveis Deixei as subescalas SDQ dos pais e filhos aos 11 anos
dt1 <- dt[-c(1:2, 3, 5, 6:7,11:13, 15:16, 18:20, 22:24, 27, 30:37, 40, 46:47, 49:56, 110:111, 117:119, 125:131, 133, 135:136, 138, 140, 142:143, 145:156, 190, 193, 194,  201, 203, 206, 207, 209:215, 222:231, 
             234:237, 243, 245:247, 248:258, 260:261, 270, 273:357, 360:429, 435:437, 440:441, 458:459, 461:466, 468:471, 473:514, 515:526, 527:535, 537, 539, 541, 543, 545, 547, 
             549, 551, 553, 555:596, 598, 600, 602, 604:748, 749:893, 895:920, 925:930, 936, 942, 949, 951:952, 957:958, 970:971, 975:976, 978:979, 983:985, 999, 1001, 1005, 1012: 1015, 1020)]


#dt2 <- dt1[-c(149:153, 154:169, 173, 200:208, 210:212, 220:222,224, 226:229, 245:251, 253)] #excluindo variaveis da ultima onda
prop.table(table(dt1$lmania_puro))


####Criando novas variaveis das drogas #####
sumFeatures <- function(x, ...){
  l <- length(x[is.na(x)]) 
  if (l == length(x)) y <- NA else y <- sum(x[!is.na(x)])
  return(y)
}

#cigarro
df_fumar <- data.frame(dt1$hc01, dt1$jc01, dt1$kfumar)
fumar_lifetime <- apply(df_fumar, 1, sumFeatures)

fumar_lifetime[fumar_lifetime > 1] <- 1
df_fumar$fumar_lifetime <- fumar_lifetime
## alucinogenos
df_alucinogeno <-data.frame(dt1$kc11, dt1$kc12, dt1$kc15)
alucinogeno_lifetime <- apply(df_alucinogeno, 1, sumFeatures)

alucinogeno_lifetime[alucinogeno_lifetime > 1] <- 1
df_alucinogeno$alucinogeno_lifetime <- alucinogeno_lifetime
##Solventes
df_solvente <-data.frame(dt1$hcsolvente, dt1$jcsolvente, dt1$kc10)
solvente_lifetime <- apply(df_solvente, 1, sumFeatures)
solvente_lifetime[solvente_lifetime > 1] <- 1
df_solvente$solvente_lifetime <- solvente_lifetime
#Alcool
df_alcool <-data.frame(dt1$hc07, dt1$jc06, dt1$kbebalc)
alcool_lifetime <- apply(df_alcool, 1, sumFeatures)
alcool_lifetime[alcool_lifetime > 1] <- 1
df_alcool$alcool_lifetime <- alcool_lifetime
#Cocaina
df_cocaina <-data.frame(dt1$jc11c, dt1$kc06, dt1$kc09, dt1$kc12, dt1$kc13, dt1$kc14)
cocaina_lifetime <- apply(df_cocaina, 1, sumFeatures)
cocaina_lifetime[cocaina_lifetime > 1] <- 1
df_cocaina$cocaina_lifetime <- cocaina_lifetime
#Maconha
df_maconha <-data.frame(dt1$jc11d, dt1$kc05)
maconha_lifetime <- apply(df_maconha, 1, sumFeatures)
maconha_lifetime[maconha_lifetime > 1] <- 1
df_maconha$maconha_lifetime <- maconha_lifetime

##Inserindo colunas novas
dt1$fumar_lifetime<- fumar_lifetime 
dt1$alucino_lifetime <- alucinogeno_lifetime
dt1$solvente_lifetime <- solvente_lifetime
dt1$alcool_lifetime <- alcool_lifetime
dt1$cocaina_lifetime <- cocaina_lifetime
dt1$maconha_lifetime <- maconha_lifetime

#### Porcentagem de missing####
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(dt1,2,pMiss)

obj_variavel <- apply(dt1,2,pMiss)
variavel_7.5poc <- obj_variavel[obj_variavel < 7.5] ##selecionando so as com 7% de missing
variavel_7.5poc <- names(variavel_7.5poc)
library(stats) #selecionei colunas com 5% missing
dt2 <- dt1 [,c(variavel_7.5poc)]
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
dt3 <- map_df(dt2, function(x){
  if (length(levels(as.factor(x))) > 8) z <- as.numeric(x) else z <- as.factor(x)}
)
map_chr(dt3, class)

dt3$hpemotion <-NULL
dt3$hpconduct <- NULL
dt3$hphyper  <- NULL
dt3$hppeer <- NULL
dt3$hpprosoc <- NULL 
dt3$ZSco01 <- NULL
dt3$Zjc02 <- NULL
library(magrittr)
con.names = c(180:203, 227:230)
dt3[,con.names] = data.frame(sapply(dt3[,con.names], as.numeric))
dt3$hsemotion <- NULL
dt3$hsconduct <- NULL
dt3$hshyper <- NULL
dt3$hspeer <- NULL
dt3$hsprosoc <- NULL
dt3$kc17a <- NULL
dt3$Zjc07 <- NULL

###EXCLUINDO VARIAVEL drogas de cada onda pra evitar overfitting

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

map_chr(dt3, class)
str(dt3)

##mudando desfecho pro final
dt3 <- dt3[, c(1:219, 221:225, 220)]
##Excluindo variaveil de droga
dt3$ecstasy  <- NULL
dt3$opioides <- NULL
dt3$solvente<- NULL
dt3$maconha<- NULL
dt3$LSD <- NULL
dt3$cocaina <- NULL

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

####criando banco ONDA 1 #### 
train_onda1 <- train_matrix[c(2:11,198:203, 219)] 
test_onda1 <- test_matrix[c(2:11,208:203, 219)]


### CRIANDO banco com a ONDA 2 ############
train_onda2<- train_matrix[c(12:65, 150:160, 190, 219)] #hISTORIA PSIQUIATRICA DA FAMILIA É A COLUNA 187
test_onda2<- test_matrix[c(12:65, 150:160, 190, 219)] #hISTORIA PSIQUIATRICA DA FAMILIA É A COLUNA 187
##Coluna 224 É O DESFECHO
##EXCLUINDO A COLUNA zhc08
test_onda2$Zhc08 <- NULL
train_onda2$Zhc08 <- NULL
## O pacote doParallel n?o pode ser executado tudo com o Caret
library(randomForest)
library(ggplot2)
library(caret)
set.seed(101)
trainctrl <- trainControl(classProbs= TRUE, summaryFunction = twoClassSummary, savePredictions = TRUE)
rfFuncs$summary <- twoClassSummary
control <- rfeControl(functions=rfFuncs, method="cv", number = 10, returnResamp="final")
#Usei o comando abaixo pq minha vari?vel "suicidio" ta no meio das colunas. Tive que descrever todas. Tirei as variaveis colineares
results_onda2 <- rfe(train_onda2[,c(1:65)], train_onda2$lmania_puro, rfeControl=control, method="rf", metric = "ROC", trControl=trainctrl)
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
data_rfe_onda2 <- train_onda2
prop.table(table(data_rfe_onda2$lmania_puro))

data_testing_onda2 <- test_onda2
prop.table(table(data_testing_onda2$lmania_puro))
#selecionar o banco bom

### treinando o algoritmo
levels(data_rfe_onda2$lmania_puro)
levels(data_rfe_onda2$lmania_puro) <- c("no", "yes")
levels(data_testing_onda2$lmania_puro)
levels(data_testing_onda2$lmania_puro) <- c("no", "yes")

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
smote_train_onda2 <- SMOTE(lmania_puro ~ ., data  = data_rfe_onda2)                         
table(smote_train_onda2$lmania_puro) 


#orig_fit <- train(lmania_puro ~ ., data = data_rfe, method = "rf", trControl = ctrl.rf, tuneGrid=grid.rf, metric = "ROC")
### treinando o algoritmo
###Treinando com lasso
# Com desequilibrio
library(caret)
library(glmnet)
lambda <- 10^seq(-3, 3, length = 100)
set.seed(108)
ctrl.rf= trainControl(method="cv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary)
#Smote
set.seed(5627)
smote_outside_onda2 <- train(lmania_puro ~ ., data = smote_train_onda2, method = "glmnet", trControl = ctrl.rf, tuneGrid=expand.grid(alpha = 1, lambda = lambda), tuneLength = 10, metric = "ROC")

smote_outside_onda2_RANDOM <- train(lmania_puro ~ ., data = smote_train_onda2, method = "rf", trControl = ctrl.rf, tuneGrid=expand.grid(alpha = 1, lambda = lambda), tuneLength = 10, metric = "ROC")

# matrix de confusao 
library(e1071)
test_pred_smote_onda2 <- predict(smote_outside_onda2, newdata = data_testing_onda2[,-66])
matrixConfusao_smote_onda2 <- confusionMatrix(test_pred_smote_onda2, data_testing_onda2$lmania_puro, positive = "yes")
matrixConfusao_smote_onda2 
# ROC
library(caret)
library(pROC)
p_smote_onda2 <- predict (smote_outside_onda2, data_testing_onda2[,-66])
p_prob_smote_onda2 <- predict (smote_outside_onda2, data_testing_onda2, type = "prob")
confusionMatrix(p_smote_onda2, data_testing_onda2$lmania_puro)
print(confusionMatrix(p_smote_onda2, data_testing_onda2$lmania_puro, positive = "yes"))
r_smote_onda2 <- roc (data_testing_onda2$lmania_puro, p_prob_smote_onda2[,"yes"])
plot(r_smote_onda2)

r_smote_onda2$auc

varImp(smote_outside_onda2)

######################### FAZENDO DA ONDA 3#############

train_onda3 <- train_matrix[c(66:104, 146, 161:165,187, 219)] 
test_onda3 <- test_matrix[c(66:104, 146, 161:165, 187, 219)] 

#Feature selection
library(randomForest)
library(ggplot2)
library(caret)
set.seed(101)
results_onda3 <- rfe(train_onda3[,c(1:46)], train_onda3$lmania_puro, rfeControl=control, method="rf", metric = "ROC", trControl=trainctrl)
# summarize the results
print(results_onda3)
# list the chosen features
predictors(results_onda3)
# plot the results
plot(results_onda3, type=c("g", "o"))
# estimate variable importance
importance_onda3 <- varImp(results_onda2, scale=FALSE)
# summarize importance
print(importance_onda3)
# plot importance
plot(importance_onda3)
#### Novo Banco de Dados 
data_rfe_onda3 <- train_onda3
prop.table(table(data_rfe_onda3$lmania_puro))

data_testing_onda3 <- test_onda3
prop.table(table(data_testing_onda3$lmania_puro))
levels(data_rfe_onda3$lmania_puro)
levels(data_rfe_onda3$lmania_puro) <- c("no", "yes")
levels(data_testing_onda3$lmania_puro)
levels(data_testing_onda3$lmania_puro) <- c("no", "yes")

##Corrigindo class imbalance
library(DMwR)
set.seed(9560)
smote_train_onda3 <- SMOTE(lmania_puro ~ ., data  = data_rfe_onda3)                         
table(smote_train_onda3$lmania_puro) 

# Treino
library(caret)
library(glmnet)
lambda <- 10^seq(-3, 3, length = 100)
set.seed(108)
ctrl.rf= trainControl(method="cv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary)
#Smote
set.seed(5627)
smote_outside_onda3 <- train(lmania_puro ~ ., data = smote_train_onda3, method = "glmnet", trControl = ctrl.rf, tuneGrid=expand.grid(alpha = 1, lambda = lambda), tuneLength = 10, metric = "ROC")

# matrix de confusao 
library(e1071)
test_pred_smote_onda3 <- predict(smote_outside_onda3, newdata = data_testing_onda3[,-47])
matrixConfusao_smote_onda3 <- confusionMatrix(test_pred_smote_onda3, data_testing_onda3$lmania_puro, positive = "yes")
matrixConfusao_smote_onda3 
# ROC
library(caret)
library(pROC)
p_smote_onda3 <- predict (smote_outside_onda3, data_testing_onda3[,-47])
p_prob_smote_onda3 <- predict (smote_outside_onda3, data_testing_onda3, type = "prob")
confusionMatrix(p_smote_onda3, data_testing_onda3$lmania_puro)
print(confusionMatrix(p_smote_onda3, data_testing_onda3$lmania_puro, positive = "yes"))
r_smote_onda3 <- roc (data_testing_onda3$lmania_puro, p_prob_smote_onda3[,"yes"])
plot(r_smote_onda3)

r_smote_onda3$auc

varImp(smote_outside_onda3)



######################### FAZENDO DA ONDA 4#############

train_onda4 <- train_matrix[c(110:123, 147:148, 166:174, 187, 204:206, 214:218, 219)]
  
test_onda4 <- test_matrix[c(110:123, 147:148, 166:174, 187, 204:206, 214:218, 219)]


#Feature selection
library(randomForest)
library(ggplot2)
library(caret)
set.seed(101)
results_onda4 <- rfe(train_onda4[,c(1:34)], train_onda4$lmania_puro, rfeControl=control, method="rf", metric = "ROC", trControl=trainctrl)
# summarize the results
print(results_onda4)
# list the chosen features
predictors(results_onda4)
# plot the results
plot(results_onda4, type=c("g", "o"))
# estimate variable importance
importance_onda4 <- varImp(results_onda4, scale=FALSE)
# summarize importance
print(importance_onda4)
# plot importance
plot(importance_onda4)


#### Novo Banco de Dados 
data_rfe_onda4 <- train_onda4
prop.table(table(data_rfe_onda4$lmania_puro))

data_testing_onda4 <- test_onda4
prop.table(table(data_testing_onda4$lmania_puro))
levels(data_rfe_onda4$lmania_puro)
levels(data_rfe_onda4$lmania_puro) <- c("no", "yes")
levels(data_testing_onda4$lmania_puro)
levels(data_testing_onda4$lmania_puro) <- c("no", "yes")

##Corrigindo class imbalance
library(DMwR)
set.seed(9560)
smote_train_onda4 <- SMOTE(lmania_puro ~ ., data  = data_rfe_onda4)                         
table(smote_train_onda4$lmania_puro) 

# Treino
library(caret)
library(glmnet)
lambda <- 10^seq(-3, 3, length = 100)
set.seed(108)
ctrl.rf= trainControl(method="cv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary)
#Smote
set.seed(5627)
smote_outside_onda4 <- train(lmania_puro ~ ., data = smote_train_onda4, method = "glmnet", trControl = ctrl.rf, tuneGrid=expand.grid(alpha = 1, lambda = lambda), tuneLength = 10, metric = "ROC")

# matrix de confusao 
library(e1071)
test_pred_smote_onda4 <- predict(smote_outside_onda4, newdata = data_testing_onda4[,-35])
matrixConfusao_smote_onda4 <- confusionMatrix(test_pred_smote_onda4, data_testing_onda4$lmania_puro, positive = "yes")
matrixConfusao_smote_onda4 
# ROC
library(caret)
library(pROC)
p_smote_onda4 <- predict (smote_outside_onda4, data_testing_onda4[,-35])
p_prob_smote_onda4 <- predict (smote_outside_onda4, data_testing_onda4, type = "prob")
confusionMatrix(p_smote_onda4, data_testing_onda4$lmania_puro)
print(confusionMatrix(p_smote_onda4, data_testing_onda4$lmania_puro, positive = "yes"))
r_smote_onda4 <- roc (data_testing_onda4$lmania_puro, p_prob_smote_onda4[,"yes"])
plot(r_smote_onda4)

r_smote_onda4$auc

varImp(smote_outside_onda4)

##### Fazendo PREDIÇÃO agrupando ONDAS ######

#####Onda 1 com onda 2 ####
train_onda_1e2 <-  train_matrix[c(2:11,198:203, 12:65, 150:160, 187, 190, 219)]
test_onda_1e2 <- test_matrix[c(2:11,198:203, 12:65, 150:160, 187, 190, 219)]


#Feature selection
library(randomForest)
library(ggplot2)
library(caret)
set.seed(101)
results_onda_1e2 <- rfe(train_onda_1e2[,c(1:83)], train_onda_1e2$lmania_puro, rfeControl=control, method="rf", metric = "ROC", trControl=trainctrl)
# summarize the results
print(results_onda_1e2)
# list the chosen features
predictors(results_onda_1e2)
# plot the results
plot(results_onda_1e2, type=c("g", "o"))
# estimate variable importance
importance_onda_1e2  <- varImp(results_onda_1e2, scale=FALSE)
# summarize importance
print(importance_onda_1e2)
# plot importance
plot(importance_onda_1e2)


#### Novo Banco de Dados 
data_rfe_onda_1e2  <- train_onda_1e2
prop.table(table(data_rfe_onda_1e2$lmania_puro))

data_testing_onda_1e2  <- test_onda_1e2
prop.table(table(data_testing_onda_1e2$lmania_puro))
levels(data_rfe_onda_1e2$lmania_puro)
levels(data_rfe_onda_1e2$lmania_puro) <- c("no", "yes")
levels(data_testing_onda_1e2$lmania_puro)
levels(data_testing_onda_1e2$lmania_puro) <- c("no", "yes")

##Corrigindo class imbalance
library(DMwR)
set.seed(9560)
smote_train_onda_1e2  <- SMOTE(lmania_puro ~ ., data  = data_rfe_onda_1e2)                         
table(smote_train_onda_1e2$lmania_puro) 

# Treino
library(caret)
library(glmnet)
lambda <- 10^seq(-3, 3, length = 100)
set.seed(108)
ctrl.rf= trainControl(method="cv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary)
#Smote
set.seed(5627)
smote_outside_onda_1e2 <- train(lmania_puro ~ ., data = smote_train_onda_1e2, method = "glmnet", trControl = ctrl.rf, tuneGrid=expand.grid(alpha = 1, lambda = lambda), tuneLength = 10, metric = "ROC")

# matrix de confusao 
library(e1071)
test_pred_smote_onda_1e2  <- predict(smote_outside_onda_1e2, newdata = data_testing_onda_1e2 [,-84])
matrixConfusao_smote_onda_1e2  <- confusionMatrix(test_pred_smote_onda_1e2, data_testing_onda_1e2$lmania_puro, positive = "yes")
matrixConfusao_smote_onda_1e2 
# ROC
library(caret)
library(pROC)
p_smote_onda_1e2 <- predict (smote_outside_onda_1e2, data_testing_onda_1e2[,-84])
p_prob_smote_onda_1e2 <- predict (smote_outside_onda_1e2, data_testing_onda_1e2, type = "prob")
confusionMatrix(p_smote_onda_1e2, data_testing_onda_1e2$lmania_puro)
print(confusionMatrix(p_smote_onda_1e2, data_testing_onda_1e2$lmania_puro, positive = "yes"))
r_smote_onda_1e2 <- roc (data_testing_onda_1e2$lmania_puro, p_prob_smote_onda_1e2[,"yes"])
plot(r_smote_onda_1e2)

r_smote_onda_1e2$auc


##########Onda 1, 2 e 3 #####

train_onda_1e2e3 <-  train_matrix[c(2:11,198:203, 12:65, 150:160, 190, 66:104, 146, 161:165,187, 219)]
test_onda_1e2e3 <- test_matrix[c(2:11,198:203, 12:65, 150:160, 190, 66:104, 146, 161:165,187, 219)]


###Feature selection
library(randomForest)
library(ggplot2)
library(caret)
set.seed(101)
results_onda_1e2e3 <- rfe(train_onda_1e2e3[,c(1:128)], train_onda_1e2e3$lmania_puro, rfeControl=control, method="rf", metric = "ROC", trControl=trainctrl)
# summarize the results
print(results_onda_1e2e3)
# list the chosen features
predictors(results_onda_1e2e3)
# plot the results
plot(results_onda_1e2e3, type=c("g", "o"))
# estimate variable importance
importance_onda_1e2e3  <- varImp(results_onda_1e2e3, scale=FALSE)
# summarize importance
print(importance_onda_1e2e3)
# plot importance
plot(importance_onda_1e2e3)


#### Novo Banco de Dados PAREI
data_rfe_onda_1e2e3  <- train_onda_1e2e3[,c("Zhsemotion", "Zjphyper", "hm189", "Zjpconduc",  "Zapesorn", "Zhphyper", "hm220", "Zhsconduct", "jm111", "hm214", "Zhpemotion","Zacompr", "Zjppeer", "jm136", "hm208", "Zhpconduct", "lmania_puro")]
prop.table(table(data_rfe_onda_1e2e3$lmania_puro))

data_testing_onda_1e2e3  <- test_onda_1e2e3[,c("Zhsemotion", "Zjphyper", "hm189", "Zjpconduc",  "Zapesorn", "Zhphyper", "hm220", "Zhsconduct", "jm111", "hm214", "Zhpemotion","Zacompr", "Zjppeer", "jm136", "hm208", "Zhpconduct", "lmania_puro")]
prop.table(table(data_testing_onda_1e2e3$lmania_puro))
levels(data_rfe_onda_1e2e3$lmania_puro)
levels(data_rfe_onda_1e2e3$lmania_puro) <- c("no", "yes")
levels(data_testing_onda_1e2e3$lmania_puro)
levels(data_testing_onda_1e2e3$lmania_puro) <- c("no", "yes")

##Corrigindo class imbalance
library(DMwR)
set.seed(9560)
smote_train_onda_1e2e3  <- SMOTE(lmania_puro ~ ., data  = data_rfe_onda_1e2e3)                         
table(smote_train_onda_1e2e3$lmania_puro) 

# Treino
library(caret)
library(glmnet)
lambda <- 10^seq(-3, 3, length = 100)
set.seed(108)
ctrl.rf= trainControl(method="cv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary)
#Smote
set.seed(5627)
smote_outside_onda_1e2e3 <- train(lmania_puro ~ ., data = smote_train_onda_1e2e3, method = "glmnet", trControl = ctrl.rf, tuneGrid=expand.grid(alpha = 1, lambda = lambda), tuneLength = 10, metric = "ROC")

# matrix de confusao 
library(e1071)
test_pred_smote_onda_1e2e3  <- predict(smote_outside_onda_1e2e3, newdata = data_testing_onda_1e2e3 [,-129])
matrixConfusao_smote_onda_1e2e3  <- confusionMatrix(test_pred_smote_onda_1e2e3, data_testing_onda_1e2e3$lmania_puro, positive = "yes")
matrixConfusao_smote_onda_1e2e3 
# ROC
library(caret)
library(pROC)
p_smote_onda_1e2e3 <- predict (smote_outside_onda_1e2e3, data_testing_onda_1e2e3[,-129])
p_prob_smote_onda_1e2e3 <- predict (smote_outside_onda_1e2e3, data_testing_onda_1e2e3, type = "prob")
confusionMatrix(p_smote_onda_1e2e3, data_testing_onda_1e2e3$lmania_puro)
print(confusionMatrix(p_smote_onda_1e2e3, data_testing_onda_1e2e3$lmania_puro, positive = "yes"))
r_smote_onda_1e2e3 <- roc (data_testing_onda_1e2e3$lmania_puro, p_prob_smote_onda_1e2e3[,"yes"])
plot(r_smote_onda_1e2e3)

r_smote_onda_1e2e3$auc


#######Onda 1, 2, 3 e 4 #####

train_onda_1e2e3e4 <-  train_matrix[c(2:11, 12:65, 66:104,110:123, 146, 147:148, 150:160, 161:165, 166:174, 187, 190, 198:203, 204:206, 214:218, 219
                                      )]
                                    
test_onda_1e2e3e4 <-  test_matrix[c(2:11, 12:65, 66:104,110:123, 146, 147:148, 150:160, 161:165, 166:174, 187, 190, 198:203, 204:206, 214:218, 219
                                  )]

###Feature selection
library(randomForest)
library(ggplot2)
library(caret)
set.seed(101)
results_onda_1e2e3e4 <- rfe(train_onda_1e2e3e4[,c(1:161)], train_onda_1e2e3e4$lmania_puro, rfeControl=control, method="rf", metric = "ROC", trControl=trainctrl)
# summarize the results
print(results_onda_1e2e3e4)
# list the chosen features
predictors(results_onda_1e2e3e4)
# plot the results
plot(results_onda_1e2e3e4, type=c("g", "o"))
# estimate variable importance
importance_onda_1e2e3e4  <- varImp(results_onda_1e2e3e4, scale=FALSE)
# summarize importance
print(importance_onda_1e2e3e4)
# plot importance
plot(importance_onda_1e2e3e4)


#### Novo Banco de Dados 
data_rfe_onda_1e2e3e4  <- train_onda_1e2e3e4
prop.table(table(data_rfe_onda_1e2e3e4$lmania_puro))

data_testing_onda_1e2e3e4  <- test_onda_1e2e3e4
prop.table(table(data_testing_onda_1e2e3e4$lmania_puro))
levels(data_rfe_onda_1e2e3e4$lmania_puro)
levels(data_rfe_onda_1e2e3e4$lmania_puro) <- c("no", "yes")
levels(data_testing_onda_1e2e3e4$lmania_puro)
levels(data_testing_onda_1e2e3e4$lmania_puro) <- c("no", "yes")

##Corrigindo class imbalance
library(DMwR)
set.seed(9560)
smote_train_onda_1e2e3e4  <- SMOTE(lmania_puro ~ ., data  = data_rfe_onda_1e2e3e4)                         
table(smote_train_onda_1e2e3e4$lmania_puro) 

# Treino
library(caret)
library(glmnet)
lambda <- 10^seq(-3, 3, length = 100)
set.seed(108)
ctrl.rf= trainControl(method="cv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary)
#Smote
set.seed(5627)
smote_outside_onda_1e2e3e4 <- train(lmania_puro ~ ., data = smote_train_onda_1e2e3e4, method = "glmnet", trControl = ctrl.rf, tuneGrid=expand.grid(alpha = 1, lambda = lambda), tuneLength = 10, metric = "ROC")

# matrix de confusao 
library(e1071)
test_pred_smote_onda_1e2e3e4  <- predict(smote_outside_onda_1e2e3e4, newdata = data_testing_onda_1e2e3e4 [,-162])
matrixConfusao_smote_onda_1e2e3e4  <- confusionMatrix(test_pred_smote_onda_1e2e3e4, data_testing_onda_1e2e3e4$lmania_puro, positive = "yes")
matrixConfusao_smote_onda_1e2e3e4 
# ROC
library(caret)
library(pROC)
p_smote_onda_1e2e3e4 <- predict (smote_outside_onda_1e2e3e4, data_testing_onda_1e2e3e4[,-162])
p_prob_smote_onda_1e2e3e4 <- predict (smote_outside_onda_1e2e3e4, data_testing_onda_1e2e3e4, type = "prob")
confusionMatrix(p_smote_onda_1e2e3e4, data_testing_onda_1e2e3e4$lmania_puro)
print(confusionMatrix(p_smote_onda_1e2e3e4, data_testing_onda_1e2e3e4$lmania_puro, positive = "yes"))
r_smote_onda_1e2e3e4 <- roc (data_testing_onda_1e2e3e4$lmania_puro, p_prob_smote_onda_1e2e3e4[,"yes"])
plot(r_smote_onda_1e2e3e4)

r_smote_onda_1e2e3e4$auc


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
rf.predict <- predict(down_outside, data_testing[, -170], type = "prob")
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

matrixConfusao_down$byClass
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

############ ELASTIC NET








