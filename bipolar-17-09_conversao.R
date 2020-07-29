### TB tipo 1 ###
ksoma <- banco_pelotas_DX$kmini23+ banco_pelotas_DX$kmini24+ banco_pelotas_DX$kmini25+ banco_pelotas_DX$kmini26+ banco_pelotas_DX$kmini27 + banco_pelotas_DX$kmini28 + banco_pelotas_DX$kmini29
banco_pelotas_DX$ksoma<- ksoma 
banco_pelotas_DX$klmania1 <- ifelse((banco_pelotas_DX$kmini19>=1 | banco_pelotas_DX$kmini21>=1) & banco_pelotas_DX$ksoma>= 3, 1, 0) 
banco_pelotas_DX$klmania2 <- ifelse((banco_pelotas_DX$kmini19<1 | banco_pelotas_DX$kmini20<1) & banco_pelotas_DX$ksoma>= 4, 1, 0) 
kD1_D2_D3 <- banco_pelotas_DX$klmania1 + banco_pelotas_DX$klmania2
banco_pelotas_DX$kD1_D2_D3 <- kD1_D2_D3
banco_pelotas_DX$kD4 <- ifelse((banco_pelotas_DX$kmini30==1 & banco_pelotas_DX$kmini32>=1) | banco_pelotas_DX$kmini31==1, 1, 0)
banco_pelotas_DX$kTB1 <- ifelse(banco_pelotas_DX$kD1_D2_D3>=1 & banco_pelotas_DX$kD4>=1, 1, 0)
banco_pelotas_DX$khipomania <- ifelse(banco_pelotas_DX$kD1_D2_D3>=1 & banco_pelotas_DX$kD4==0, 1, 0)

#### Diagnóstico de depressão presente sem prejuizo
banco_pelotas_DX$kA1 <- ifelse(banco_pelotas_DX$kmini1==1 & banco_pelotas_DX$kmini1a==1, 1, 0)
banco_pelotas_DX$kA2 <- ifelse(banco_pelotas_DX$kmini64==1 & banco_pelotas_DX$kmini2a==1, 1, 0)
banco_pelotas_DX$kA3 <- banco_pelotas_DX$kmini3 + banco_pelotas_DX$kmini4 + banco_pelotas_DX$kmini5 + banco_pelotas_DX$kmini6 + banco_pelotas_DX$kmini7 + banco_pelotas_DX$kmini8 + banco_pelotas_DX$kmini10
banco_pelotas_DX$kA4 <- ifelse((banco_pelotas_DX$kA1==0 | banco_pelotas_DX$kA2==0) & banco_pelotas_DX$kA3>=4, 1, 0)
banco_pelotas_DX$kA4.1 <- ifelse((banco_pelotas_DX$kA1==1 & banco_pelotas_DX$kA2==1) & banco_pelotas_DX$kA3>=3, 1, 0)
banco_pelotas_DX$kA5 <- ifelse(banco_pelotas_DX$kmini10==1 & banco_pelotas_DX$kmini11==1, 1, 0)
##prejuizo e l074

##TB TIPO II
banco_pelotas_DX$kTBII <- ifelse(banco_pelotas_DX$khipomania==1 & ((banco_pelotas_DX$kA4==1 | banco_pelotas_DX$kA4.1==1 | banco_pelotas_DX$kA5==1) & banco_pelotas_DX$kmini12>=2), 1, 0)
#TB NOS nao foi feito na onda K pq nao sabia se durou mais que 4 dias

banco_pelotas_DX$kTB <- ifelse(banco_pelotas_DX$kTB1==1 | banco_pelotas_DX$kTBII==1, 1, 0)

#### TB tipo 1 ###
soma <- banco_pelotas$l085+ banco_pelotas$l086+ banco_pelotas$l087+ banco_pelotas$l088+ banco_pelotas$l089+ banco_pelotas$l090+ banco_pelotas$l091
banco_pelotas$soma<- soma 
banco_pelotas$lmania1 <- ifelse((banco_pelotas$l081>=1 | banco_pelotas$l083>=1) & banco_pelotas$soma>= 3, 1, 0) 
banco_pelotas$lmania2 <- ifelse((banco_pelotas$l081<1 | banco_pelotas$l082<1) & banco_pelotas$soma>= 4, 1, 0) 
D1_D2_D3 <- banco_pelotas$lmania1 + banco_pelotas$lmania2
banco_pelotas$D1_D2_D3 <- D1_D2_D3
banco_pelotas$D4 <- ifelse((banco_pelotas$l092==1 & banco_pelotas$l094>=1) | banco_pelotas$l093==1, 1, 0)
banco_pelotas$TB1 <- ifelse(banco_pelotas$D1_D2_D3>=1 & banco_pelotas$D4>=1, 1, 0)
banco_pelotas$hipomania <- ifelse(banco_pelotas$D1_D2_D3>=1 & banco_pelotas$D4==0, 1, 0)

#### Diagnóstico de depressão presente sem prejuizo
banco_pelotas$A1 <- ifelse(banco_pelotas$l063==1 & banco_pelotas$l063a==1, 1, 0)
banco_pelotas$A2 <- ifelse(banco_pelotas$l064==1 & banco_pelotas$l064a==1, 1, 0)
banco_pelotas$A3 <- banco_pelotas$l065 + banco_pelotas$l066 + banco_pelotas$l067 + banco_pelotas$l068 + banco_pelotas$l069 + banco_pelotas$l070 + banco_pelotas$l071
banco_pelotas$A4 <- ifelse((banco_pelotas$A1==0 | banco_pelotas$A2==0) & banco_pelotas$A3>=4, 1, 0)
banco_pelotas$A4.1 <- ifelse((banco_pelotas$A1==1 & banco_pelotas$A2==1) & banco_pelotas$A3>=3, 1, 0)
banco_pelotas$A5 <- ifelse(banco_pelotas$l072==1 & banco_pelotas$l073==1, 1, 0)
##prejuizo e l074

#Depressão passado sem prejuizo
#### 
#banco_pelotas$A2.p <- ifelse(banco_pelotas$l064b==1 & banco_pelotas$l064c==1, 1, 0)
#banco_pelotas$A3 <- banco_pelotas$l065 + banco_pelotas$l066 + banco_pelotas$l067 + banco_pelotas$l068 + banco_pelotas$l069 + banco_pelotas$l070 + banco_pelotas$l071
#banco_pelotas$A4 <- ifelse((banco_pelotas$A1==0 | banco_pelotas$A2==0) & banco_pelotas$A3>=4, 1, 0)
#banco_pelotas$A4.1 <- ifelse((banco_pelotas$A1==1 & banco_pelotas$A2==1) & banco_pelotas$A3>=3, 1, 0)

##TB TIPO II
banco_pelotas$TBII <- ifelse(banco_pelotas$hipomania==1 & ((banco_pelotas$A4==1 | banco_pelotas$A4.1==1 | banco_pelotas$A5==1) & banco_pelotas$l074>=1), 1, 0)
banco_pelotas$TBII.p <- ifelse(banco_pelotas$hipomania==1 & banco_pelotas$kdepreprej==1, 1, 0)
# FALTA O TB NOS
#TB NOS preenche critério pra hipomania exceto pela duração + sintomas depressivos
banco_pelotas_DX$TB.NOS <- ifelse(((banco_pelotas_DX$l081==1 | banco_pelotas_DX$l081a==1) & (banco_pelotas_DX$soma>=2)) | 
                                    ((banco_pelotas_DX$l083==1 | banco_pelotas_DX$l084==1) &(banco_pelotas_DX$soma>=3)) & 
                                    (banco_pelotas_DX$l092a==1) & (banco_pelotas_DX$l094>=1), 1, 0)




#testando
#banco_pelotas$TB.NOS <- ifelse(((banco_pelotas$D1_D2_D3>=1 & (banco_pelotas$l092a==0 & banco_pelotas$l094>=1))) & ((banco_pelotas$A4==1 | banco_pelotas$A4.1==1 | banco_pelotas$A5==1) & banco_pelotas$l074>=1), 1, 0)
##Combinando variaveis
banco_pelotas_DX$TB <- ifelse(banco_pelotas_DX$TB1==1 | banco_pelotas_DX$TBII==1 | banco_pelotas_DX$TBII.p==1 | banco_pelotas_DX$TB.NOS==1, 1, 0)
#banco com a variavel com os zeros 
library(haven)
banco_pelotas_DX1 <- read_sav("banco_pelotas_DX_16-09.sav")
library(dplyr)
dt <- banco_pelotas_DX1[!is.na(banco_pelotas_DX1$TB),]
#propor??o de TB
prop.table(table(dt$TB))
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


dt1 <- dt[,c("ahipert", "adiabet", "ainfeccao", "aanemia", "aintern", "afumou", "abebalc", "avivmar", "sexonovo", "aalcool", "Zaidadmae", "Zapesorn","Zacompr", "Zapcrn",  "Zarenfam", "hm046", "hm114a", "hm114b", "hm114c","hm114d", "hm114e", "hprocom", "hm175","hm176", "hm177", "hm178", "hm179",  "hm180", "hm181", "hm182", "hm183","hm184", "hm185", "hm186", "hm187", "hm188", "hm189", "hm190", "hm191", "hm192", "hm193", "hm194", "hm195", "hm196", "hm197", "hm198", "hm199", "hm201", "hm206", "hm207", "hm208",  "hm209", "hm210", "hm211", "hm212", "hm213", "hm214", "hm215", "hm216",  "hm217", "hm218", "hm219", "hm220",  "hm221", "hm222", "hm223", "hm224", "hm225", "hc01",  "hc06m", "hc07", "Zhpemotion", "Zhpconduct", "Zhphyper", "Zhppeer", "Zhpprosoc", "Zhsemotion", "Zhsconduct", "Zhshyper", "Zhspeer", "Zhsprosoc", "histpsiquiatri", "Zhtsono", 
             "jm111", "jm112", "jm113", "jm114", "jm115", "jm116", "jm117", "jm118", "jm119", "jm120", "jm121", "jm122", "jm123", "jm124", "jm125", "jm126", "jm127", "jm128", "jm129", "jm130", "jm131", "jm132", "jm133", "jm134", "jm135", "jm136", "jm137", "jm138", "jm139a", "jm139b", "jm139c", "jm139d", "jm140", "ja006", "jc01", "jc04", "jc05", "jc06", "jc08", "jc11g", "jcorpel5", "Zjpemotio", "Zjpconduc", "Zjphyper", "Zjppeer", "Zjpprosoc", 
             "histpsiquiatri", "TB","ktabagismodiario", "Zkqitotbr", "kprocom", "kprosau", "kmorte", "kprobdin","kmudcas", "kternam", "kinsbai", "kbriga", "kapanhou","kassalto","kauditdic", "Zkqvfisico", "Zkqvpsico", "Zkqvsocial", "Zkqvambi","ZkSRQdepre_anx", "ZkSRQsomatic", "ZkSQRenergia", "ZkSRQsentidepre", "histpsiquiatri", "Zkescadol", "Zkibem", "Zkfeliz", "cocaina_lifetime", "maconha_lifetime", "alucino_lifetime", "solvente_lifetime", "alcool_lifetime",
             "risco_suicidio", "kfobsoc", "kansger", "ktdah")]

#transformar em continuo
dt1$ZkSQRenergia <- as.numeric(dt1$ZkSQRenergia)
dt1$ZkSRQsentidepre <- as.numeric(dt1$ZkSRQsentidepre)
dt1$ZkSRQsomatic <- as.numeric(dt1$ZkSRQsomatic)
dt1$ZkSRQsentidepre <- as.numeric(dt1$ZkSRQsentidepre)
dt1$ZkSRQdepre_anx <- as.numeric(dt1$ZkSRQdepre_anx)
dt1$Zlauditpreocup <- as.numeric(dt1$Zlauditpreocup)
dt1$Zkqitotbr <- as.numeric(dt1$Zkqitotbr)
dt1$soma <- NULL
dt1$SRQ_20_22a <- NULL
dt1$SNAITH_HAMILTON <- NULL
dt1$ESCALA_TRANSVERSAL_SINTOMAS <- NULL
dt1$lmotivo_perda_sm <- NULL               
dt1$lmotivo_ce_sm <- NULL                        
dt1$CESD <- NULL
dt1$nquest <- NULL
dt1$dv <- NULL
# Ver as classes das variaveis
library(purrr)
map_chr(dt1, class)
dt1 <- as.data.frame(dt1)
##Mudar os fatores da classe pq tem variavel com "haven_label" CONVERSAO DE VARIAVEL
library(purrr)
dt2 <- map_df(dt1, function(x){
  if (length(levels(as.factor(x))) > 8) z <- as.numeric(x) else z <- as.factor(x)}
)
map_chr(dt2, class)
# Ver as classes das variaveis
library(purrr)
map_chr(dt2, class)
##Mudar os fatores da classe pq tem variavel com "haven_label" CONVERSAO DE VARIAVEL
dt3 <- dt2
map_chr(dt3, class)
dt3$ZkSQRenergia <- as.numeric(dt3$ZkSQRenergia)
dt3$ZkSRQsentidepre <- as.numeric(dt3$ZkSRQsentidepre)
dt3$ZkSRQsomatic <- as.numeric(dt3$ZkSRQsomatic)
dt3$ZkSRQsentidepre <- as.numeric(dt3$ZkSRQsentidepre)
dt3$ZkSRQdepre_anx <- as.numeric(dt3$ZkSRQdepre_anx)
dt3$Zkqitotbr <- as.numeric(dt3$Zkqitotbr)
#REORDERNAR desfecho na frente
dt3 <- dt3[,c(130,1:129, 131:163)]
### HOLD OUT ####
# Separar em banco de dados em Treino e Teste 
library(caret)
set.seed(123)
datasetbipolar <- dt3 [sample (1:nrow (dt3), 3691, replace=FALSE), ]
## banco treino e teste ##
library(caret)
set.seed(123)
intrain <- createDataPartition(y = datasetbipolar$TB, p= 0.8, list = FALSE)
dtTrain <-  datasetbipolar[intrain,]
dtTest <- datasetbipolar[-intrain,]
dim(dtTrain); dim(dtTest)
# Porcentagem do desfecho em cada banco
prop.table(table(datasetbipolar$TB))
prop.table(table(dtTrain$TB))
prop.table(table(dtTest$TB))

#####Imputação ####
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

treino_onda4 <- train_matrix[,c("TB","ktabagismodiario","Zkqitotbr", "kprocom", "kprosau", 
                                "kmorte", "kprobdin","kmudcas", "kternam", "kinsbai", "kbriga",
                                "kapanhou","kassalto","kauditdic", "Zkqvfisico", "Zkqvpsico", "Zkqvsocial",
                                "Zkqvambi","ZkSRQdepre_anx", "ZkSRQsomatic", "ZkSQRenergia", "ZkSRQsentidepre",
                                "histpsiquiatri", "Zkescadol", "Zkibem", "Zkfeliz", "cocaina_lifetime", 
                                "maconha_lifetime", "alucino_lifetime", "solvente_lifetime", "alcool_lifetime",
                                "risco_suicidio", "kfobsoc", "kansger", "ktdah"
)]
prop.table(table(treino_onda4$TB))
teste_onda4 <- test_matrix[,c("TB","ktabagismodiario","Zkqitotbr", "kprocom", "kprosau", 
                              "kmorte", "kprobdin","kmudcas", "kternam", "kinsbai", "kbriga",
                              "kapanhou","kassalto","kauditdic", "Zkqvfisico", "Zkqvpsico", "Zkqvsocial",
                              "Zkqvambi","ZkSRQdepre_anx", "ZkSRQsomatic", "ZkSQRenergia", "ZkSRQsentidepre",
                              "histpsiquiatri", "Zkescadol", "Zkibem", "Zkfeliz", "cocaina_lifetime", 
                              "maconha_lifetime", "alucino_lifetime", "solvente_lifetime", "alcool_lifetime",
                              "risco_suicidio", "kfobsoc", "kansger", "ktdah" 
)]
prop.table(table(teste_onda4$TB))
prop.table(table(treino_onda4$TB))
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
results <- rfe(treino_onda4[,c(2:35)], treino_onda4$TB, rfeControl=control, method="rf", metric = "ROC", trControl=trainctrl)
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
prop.table(table(data_rfe$TB))
data_testing <- teste_onda4
prop.table(table(data_testing$TB))
### treinando o algoritmo
levels(data_rfe$TB)
levels(data_rfe$TB) <- c("no", "yes")
levels(data_testing$TB)
levels(data_testing$TB) <- c("no", "yes")
## Corrigindo imbalance class https://topepo.github.io/caret/subsampling-for-class-imbalances.html
library(caret)
set.seed(9560)
###Treinando com lasso ondaa 4 ####
# Com desequilibrio

library(caret)
library(glmnet)
lambda <- 10^seq(-3, 3, length = 100)
set.seed(108)
ctrl.rf_up_LASSO <- trainControl(method="repeatedcv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary, sampling = "up")
up_outside_LASSO <- train(TB ~., data = data_rfe, method = "glmnet", trControl = ctrl.rf_up_LASSO, tuneLength = 10, tuneGrid=expand.grid(alpha = 1, lambda = lambda), metric = "ROC")
##Com Smote
library(DMwR)
set.seed(9560)
smote_train <- SMOTE(TB ~ ., data  = data_rfe)                         
table(smote_train$TB) 
##Smote
set.seed(5627)
ctrl.rf_LASSO_smote <- trainControl(method="repeatedcv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary)
smote_outside_LASSO <- train(TB ~ ., data = smote_train, method = "glmnet", trControl = ctrl.rf_LASSO_smote, tuneGrid=expand.grid(alpha = 1, lambda = lambda), tuneLength = 10, metric = "ROC")
#rose
library(ROSE)
set.seed(9560)
rose_train <- ROSE(TB ~ ., data  = data_rfe)$data                         
table(rose_train$TB)
set.seed(5627)
ctrl.rf_LASSO_rose <- trainControl(method="repeatedcv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary)
rose_outside_LASSO <- train(TB ~ ., data = rose_train, method = "glmnet", trControl = ctrl.rf_LASSO_rose, tuneGrid=expand.grid(alpha = 1, lambda = lambda), tuneLength = 10, metric = "ROC")

library(e1071)
test_pred_smote_LASSO <- predict(smote_outside_LASSO, newdata = data_testing[,-1])
matrixConfusao_smote_LASSO  <- confusionMatrix(test_pred_smote_LASSO, data_testing$TB, positive = "yes")
matrixConfusao_smote_LASSO 
# ROC
library(caret)
library(pROC)
p_smote_LASSO <- predict (smote_outside_LASSO, data_testing[,-1])
p_prob_smote_LASSO <- predict (smote_outside_LASSO, data_testing[,-1], type = "prob")
confusionMatrix(p_smote_LASSO, data_testing$TB)
print(confusionMatrix(p_smote_LASSO, data_testing$TB, positive = "yes"))
r_smote_LASSO <- roc (data_testing$TB, p_prob_smote_LASSO[,"yes"])
plot(r_smote_LASSO)
r_smote_LASSO$auc

library(e1071)
test_pred_rose_LASSO <- predict(rose_outside_LASSO, newdata = data_testing[,-1])
matrixConfusao_rose_LASSO  <- confusionMatrix(test_pred_rose_LASSO, data_testing$TB, positive = "yes")
matrixConfusao_rose_LASSO 
# ROC
library(caret)
library(pROC)
p_rose_LASSO <- predict (rose_outside_LASSO, data_testing[,-1])
p_prob_rose_LASSO <- predict (rose_outside_LASSO, data_testing[,-1], type = "prob")
confusionMatrix(p_rose_LASSO, data_testing$TB)
print(confusionMatrix(p_rose_LASSO, data_testing$TB, positive = "yes"))
r_rose_LASSO <- roc (data_testing$TB, p_prob_rose_LASSO[,"yes"])
plot(r_rose_LASSO)
r_rose_LASSO$auc

library(e1071)
test_pred_up_LASSO <- predict(up_outside_LASSO, newdata = data_testing[,-1])
matrixConfusao_up_LASSO  <- confusionMatrix(test_pred_up_LASSO, data_testing$TB, positive = "yes")
matrixConfusao_up_LASSO 
# ROC
library(caret)
library(pROC)
p_up_LASSO <- predict (up_outside_LASSO, data_testing[,-1])
p_prob_up_LASSO <- predict (up_outside_LASSO, data_testing[,-1], type = "prob")
confusionMatrix(p_up_LASSO, data_testing$TB)
print(confusionMatrix(p_up_LASSO, data_testing$TB, positive = "yes"))
r_up_LASSO <- roc (data_testing$TB, p_prob_up_LASSO[,"yes"])
plot(r_up_LASSO)
r_up_LASSO$auc


####### Elastic net####
library(caret)
library(glmnet)
set.seed(108)
ctrl.rf_up_NET <- trainControl(method="repeatedcv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary, sampling = "up")
up_outside_NET <- train(TB ~., data = data_rfe, method = "glmnet", trControl = ctrl.rf_up_NET, tuneLength = 10, metric = "ROC")
ctrl.rf_smote_NET <- trainControl(method="repeatedcv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary)
smote_outside_NET <- train(TB ~., data = smote_train, method = "glmnet", trControl = ctrl.rf_smote_NET, tuneLength = 10, metric = "ROC")
ctrl.rf_rose_NET <- trainControl(method="repeatedcv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary)
rose_outside_NET <- train(TB ~., data = rose_train, method = "glmnet", trControl = ctrl.rf_rose_NET, tuneLength = 10, metric = "ROC")


##Smote
# matrix de confusao 
library(e1071)
test_pred_smote_NET <- predict(smote_outside_NET, newdata = data_testing[,-1])
matrixConfusao_smote_NET <- confusionMatrix(test_pred_smote_NET, data_testing$TB, positive = "yes")
matrixConfusao_smote_NET 
# ROC
library(caret)
library(pROC)
p_smote_NET <- predict (smote_outside_NET, data_testing[,-1])
p_prob_smote_NET <- predict (smote_outside_NET, data_testing[,-1], type = "prob")
confusionMatrix(p_smote_NET, data_testing$TB)
print(confusionMatrix(p_smote_NET, data_testing$TB, positive = "yes"))
r_smote_NET <- roc (data_testing$TB, p_prob_smote_NET[,"yes"])
plot(r_smote_NET)
r_smote_NET$auc
varImp(smote_outside_NET)
#######
library(SDMTools)

### mudando limiar onda 4 ####
# desfecho do conjunto de teste
# 0 = No, 1 = Yes
obs <- data_testing$TB

# Os levels estão em ordem?
levels(obs)

levels(obs) <- c("0", "1")
obs <- as.numeric(as.character(obs))
obs

# predições do modelo em probabilidades
rf.predict <- predict(smote_outside_LASSO, data_testing[, -1], type = "prob")
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

matrixConfusao_smote_lasso$byClass
library(purrr)
library(plyr)
confMatrix(0.5, obs, predictions)
result_list <- map(threshold_seq, confMatrix, obs = obs, predictions = predictions)
result_df <- ldply(result_list)

result_df



########### Onda 1 #####
treino_onda1 <- train_matrix[,c("TB","histpsiquiatri", "ahipert", "adiabet", "ainfeccao", "aanemia", "aintern", "afumou", "abebalc", "avivmar", "sexonovo", "aalcool", "Zaidadmae", "Zapesorn","Zacompr", "Zapcrn",  "Zarenfam" 
)]
prop.table(table(treino_onda1$TB))
teste_onda1 <- test_matrix[,c("TB","histpsiquiatri", "ahipert", "adiabet", "ainfeccao", "aanemia", "aintern", "afumou", "abebalc", "avivmar", "sexonovo", "aalcool", "Zaidadmae", "Zapesorn","Zacompr", "Zapcrn",  "Zarenfam" 
)]
prop.table(table(teste_onda1$TB))
prop.table(table(treino_onda1$TB))
anyNA(treino_onda1)
anyNA(teste_onda1)

### treinando o algoritmo
levels(treino_onda1$TB)
levels(treino_onda1$TB) <- c("no", "yes")
levels(teste_onda1$TB)
levels(teste_onda1$TB) <- c("no", "yes")
## Corrigindo imbalance class https://topepo.github.io/caret/subsampling-for-class-imbalances.html
library(caret)
set.seed(9560)
# Com desequilibrio
##Com Smote
library(DMwR)
set.seed(9560)
smote_train_onda1 <- SMOTE(TB ~ ., data  = treino_onda1)                         
table(smote_train_onda1$TB) 

####### Elastic net####
library(caret)
library(glmnet)
set.seed(108)
ctrl.rf_smote_NET <- trainControl(method="repeatedcv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary)
smote_outside_NET_onda1 <- train(TB ~., data = smote_train_onda1, method = "glmnet", trControl = ctrl.rf_smote_NET, tuneLength = 10, metric = "ROC")

##Smote
# matrix de confusao 
library(e1071)
test_pred_smote_NET_onda1 <- predict(smote_outside_NET_onda1, newdata = teste_onda1[,-1])
matrixConfusao_smote_NET_onda1 <- confusionMatrix(test_pred_smote_NET_onda1, teste_onda1$TB, positive = "yes")
matrixConfusao_smote_NET_onda1 
# ROC
library(caret)
library(pROC)
p_smote_NET_onda1 <- predict (smote_outside_NET_onda1, teste_onda1[,-1])
p_prob_smote_NET_onda1 <- predict (smote_outside_NET_onda1, teste_onda1[,-1], type = "prob")
confusionMatrix(p_smote_NET_onda1, teste_onda1$TB)
print(confusionMatrix(p_smote_NET_onda1, teste_onda1$TB, positive = "yes"))
r_smote_NET_onda1 <- roc (teste_onda1$TB, p_prob_smote_NET_onda1[,"yes"])
plot(r_smote_NET_onda1)
r_smote_NET_onda1$auc
varImp(smote_outside_NET_onda1)

#mundando limiar onda 1 ####
library(SDMTools)
obs_1 <- teste_onda1$TB

# Os levels estão em ordem?
levels(obs_1)

levels(obs_1) <- c("0", "1")
obs_1 <- as.numeric(as.character(obs))
obs_1

# predições do modelo em probabilidades
rf.predict_onda1 <- predict(smote_outside_NET_onda1, teste_onda1[, -1], type = "prob")
predictions_onda1 <- as.vector(rf.predict_onda1[, 2])
predictions_onda1

confusion_df_onda1 <- data.frame(obs_1, predictions_onda1)
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

matrixConfusao_smote_NET_onda1$byClass
library(purrr)
library(plyr)
confMatrix(0.5, obs, predictions)
result_list_onda1 <- map(threshold_seq, confMatrix, obs = obs_1, predictions = predictions_onda1)
result_df_onda1 <- ldply(result_list_onda1)

result_df_onda1

######Onda 2 ####
treino_onda2 <- train_matrix[,c("TB","histpsiquiatri", "sexonovo",  "hm046", "hm114a", "hm114b", "hm114c","hm114d", "hm114e", "hprocom", "hm175","hm176", "hm177", "hm178", "hm179",  "hm180", "hm181", "hm182", "hm183","hm184", "hm185", "hm186", "hm187", "hm188", "hm189", "hm190", "hm191", "hm192", "hm193", "hm194", "hm195", "hm196", "hm197", "hm198", "hm199", "hm201", "hm206", "hm207", "hm208",  "hm209", "hm210", "hm211", "hm212", "hm213", "hm214", "hm215", "hm216",  "hm217", "hm218", "hm219", "hm220",  "hm221", "hm222", "hm223", "hm224", "hm225", "hc01",  "hc06m", "hc07", "Zhpemotion", "Zhpconduct", "Zhphyper", "Zhppeer", "Zhpprosoc", "Zhsemotion", "Zhsconduct", "Zhshyper", "Zhspeer", "Zhsprosoc", "Zhtsono")]
prop.table(table(treino_onda2$TB))
teste_onda2 <- test_matrix[,c("TB","histpsiquiatri", "sexonovo",  "hm046", "hm114a", "hm114b", "hm114c","hm114d", "hm114e", "hprocom", "hm175","hm176", "hm177", "hm178", "hm179",  "hm180", "hm181", "hm182", "hm183","hm184", "hm185", "hm186", "hm187", "hm188", "hm189", "hm190", "hm191", "hm192", "hm193", "hm194", "hm195", "hm196", "hm197", "hm198", "hm199", "hm201", "hm206", "hm207", "hm208",  "hm209", "hm210", "hm211", "hm212", "hm213", "hm214", "hm215", "hm216",  "hm217", "hm218", "hm219", "hm220",  "hm221", "hm222", "hm223", "hm224", "hm225", "hc01",  "hc06m", "hc07", "Zhpemotion", "Zhpconduct", "Zhphyper", "Zhppeer", "Zhpprosoc", "Zhsemotion", "Zhsconduct", "Zhshyper", "Zhspeer", "Zhsprosoc", "Zhtsono")]

prop.table(table(teste_onda2$TB))
prop.table(table(treino_onda2$TB))
anyNA(treino_onda2)
anyNA(teste_onda2)

### treinando o algoritmo
levels(treino_onda2$TB)
levels(treino_onda2$TB) <- c("no", "yes")
levels(teste_onda2$TB)
levels(teste_onda2$TB) <- c("no", "yes")
## Corrigindo imbalance class https://topepo.github.io/caret/subsampling-for-class-imbalances.html
library(caret)
set.seed(9560)
# Com desequilibrio
##Com Smote
library(DMwR)
set.seed(9560)
smote_train_onda2 <- SMOTE(TB ~ ., data  = treino_onda2)                         
table(smote_train_onda2$TB) 
library(caret)
library(glmnet)
set.seed(108)
ctrl.rf_smote_NET <- trainControl(method="repeatedcv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary)
smote_outside_NET_onda2 <- train(TB ~., data = smote_train_onda2, method = "glmnet", trControl = ctrl.rf_smote_NET, tuneLength = 10, metric = "ROC")

##Smote
# matrix de confusao 
library(e1071)
test_pred_smote_NET_onda2 <- predict(smote_outside_NET_onda2, newdata = teste_onda2[,-1])
matrixConfusao_smote_NET_onda2 <- confusionMatrix(test_pred_smote_NET_onda2, teste_onda2$TB, positive = "yes")
matrixConfusao_smote_NET_onda2 
# ROC
library(caret)
library(pROC)
p_smote_NET_onda2 <- predict (smote_outside_NET_onda2, teste_onda2[,-1])
p_prob_smote_NET_onda2 <- predict (smote_outside_NET_onda2, teste_onda2[,-1], type = "prob")
confusionMatrix(p_smote_NET_onda2, teste_onda2$TB)
print(confusionMatrix(p_smote_NET_onda2, teste_onda2$TB, positive = "yes"))
r_smote_NET_onda2 <- roc (teste_onda2$TB, p_prob_smote_NET_onda2[,"yes"])
plot(r_smote_NET_onda2)
r_smote_NET_onda2$auc
varImp(smote_outside_NET_onda2)
### mudando limiar onda 2 #### 
#Ver o limiar 0.33
# desfecho do conjunto de teste
# 0 = No, 1 = Yes
library(SDMTools)
obs_2 <- teste_onda2$TB

# Os levels estão em ordem?
levels(obs_2)

levels(obs_2) <- c("0", "1")
obs_2 <- as.numeric(as.character(obs))
obs_2

# predições do modelo em probabilidades
rf.predict_onda2 <- predict(smote_outside_NET_onda2, teste_onda2[, -1], type = "prob")
predictions_onda2 <- as.vector(rf.predict_onda2[, 2])
predictions_onda2

confusion_df_onda2 <- data.frame(obs_2, predictions_onda2)
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

matrixConfusao_smote_NET_onda2$byClass
library(purrr)
library(plyr)
confMatrix(0.5, obs, predictions)
result_list_onda2 <- map(threshold_seq, confMatrix, obs = obs_2, predictions = predictions_onda2)
result_df_onda2 <- ldply(result_list_onda2)

result_df_onda2



##


##### Onda 3 ####
treino_onda3 <- train_matrix[,c("TB","histpsiquiatri", "sexonovo", "jm111", "jm112", "jm113", "jm114", "jm115", "jm116", "jm117", "jm118", "jm119", "jm120", "jm121", "jm122", "jm123", "jm124", "jm125", "jm126", "jm127", "jm128", "jm129", "jm130", "jm131", "jm132", "jm133", "jm134", "jm135", "jm136", "jm137", "jm138", "jm139a", "jm139b", "jm139c", "jm139d", "jm140", "ja006", "jc01", "jc04", "jc05", "jc06", "jc08", "jc11g", "jcorpel5", "Zjpemotio", "Zjpconduc", "Zjphyper", "Zjppeer", "Zjpprosoc")]
prop.table(table(treino_onda3$TB))
teste_onda3 <- test_matrix[,c("TB","histpsiquiatri", "sexonovo", "jm111", "jm112", "jm113", "jm114", "jm115", "jm116", "jm117", "jm118", "jm119", "jm120", "jm121", "jm122", "jm123", "jm124", "jm125", "jm126", "jm127", "jm128", "jm129", "jm130", "jm131", "jm132", "jm133", "jm134", "jm135", "jm136", "jm137", "jm138", "jm139a", "jm139b", "jm139c", "jm139d", "jm140", "ja006", "jc01", "jc04", "jc05", "jc06", "jc08", "jc11g", "jcorpel5", "Zjpemotio", "Zjpconduc", "Zjphyper", "Zjppeer", "Zjpprosoc")]
prop.table(table(teste_onda3$TB))
prop.table(table(treino_onda3$TB))
anyNA(treino_onda3)
anyNA(teste_onda3)

### treinando o algoritmo
levels(treino_onda3$TB)
levels(treino_onda3$TB) <- c("no", "yes")
levels(teste_onda3$TB)
levels(teste_onda3$TB) <- c("no", "yes")
## Corrigindo imbalance class https://topepo.github.io/caret/subsampling-for-class-imbalances.html
library(caret)
set.seed(9560)
# Com desequilibrio
##Com Smote
library(DMwR)
set.seed(9560)
smote_train_onda3 <- SMOTE(TB ~ ., data  = treino_onda3)                         
table(smote_train_onda3$TB) 

####### Elastic net####
library(caret)
library(glmnet)
set.seed(108)
ctrl.rf_smote_NET <- trainControl(method="repeatedcv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary)
smote_outside_NET_onda3 <- train(TB ~., data = smote_train_onda3, method = "glmnet", trControl = ctrl.rf_smote_NET, tuneLength = 10, metric = "ROC")

##Smote
# matrix de confusao 
library(e1071)
test_pred_smote_NET_onda3 <- predict(smote_outside_NET_onda3, newdata = teste_onda3[,-1])
matrixConfusao_smote_NET_onda3 <- confusionMatrix(test_pred_smote_NET_onda3, teste_onda3$TB, positive = "yes")
matrixConfusao_smote_NET_onda3 
# ROC
library(caret)
library(pROC)
p_smote_NET_onda3 <- predict (smote_outside_NET_onda3, teste_onda3[,-1])
p_prob_smote_NET_onda3 <- predict (smote_outside_NET_onda3, teste_onda3[,-1], type = "prob")
confusionMatrix(p_smote_NET_onda3, teste_onda3$TB)
print(confusionMatrix(p_smote_NET_onda3, teste_onda3$TB, positive = "yes"))
r_smote_NET_onda3 <- roc (teste_onda3$TB, p_prob_smote_NET_onda3[,"yes"])
plot(r_smote_NET_onda3)
r_smote_NET_onda3$auc
varImp(smote_outside_NET_onda3)

### colocando Intervalo de confiança
pred <- predict(smote_outside_NET, data.matrix(data_testing[, -1]), type = "prob")
predictions2 <- as.vector(pred[, 2])
rocobj2 <- roc(data_testing$TB, p_prob, ci=TRUE, of="auc", percent = FALSE)
plot(rocobj2,percent = FALSE, ci=TRUE, print.auc=TRUE)

## AGRUPANDO AUCS e colocando cores ####
library(RColorBrewer)
require(pROC)
glm.predict4 <- predict(smote_outside_NET, teste_onda4[, -1], type = "prob")
predictions4 <- as.vector(glm.predict4[, 2])
rocobj4 <- roc(data_testing$TB, predictions4, ci=TRUE, of="auc", percent = FALSE)
plot(rocobj4,percent = FALSE, ci=TRUE, print.auc=FALSE, col="royalblue3") #print.auc se for TRUE, serve pra colocar valor de auc

require(pROC)
glm.predict1 <- predict(smote_outside_NET_onda1, teste_onda1[, -1], type = "prob")
predictions1 <- as.vector(glm.predict1[, 2])
rocobj1 <- roc(teste_onda2$TB, predictions1, ci=TRUE, of="auc", percent = FALSE)
plot(rocobj1,percent = FALSE, ci=TRUE, print.auc=FALSE, add=TRUE, col="firebrick4")

require(pROC)
glm.predict2 <- predict(smote_outside_NET_onda2, teste_onda2[, -1], type = "prob")
predictions2 <- as.vector(glm.predict2[, 2])
rocobj2 <- roc(teste_onda2$TB, predictions2, ci=TRUE, of="auc", percent = FALSE)
plot(rocobj2,percent = FALSE, ci=TRUE, print.auc=FALSE, add=TRUE, col="forestgreen")
require(pROC)
glm.predict3 <- predict(smote_outside_NET_onda3, teste_onda3[, -1], type = "prob")
predictions3 <- as.vector(glm.predict3[, 2])
rocobj3 <- roc(teste_onda3$TB, predictions3, ci=TRUE, of="auc", percent = FALSE)
plot(rocobj3,percent = FALSE, ci=TRUE, print.auc=FALSE, add=TRUE, col="gray0")
