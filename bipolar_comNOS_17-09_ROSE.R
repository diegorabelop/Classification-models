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
banco_pelotas_DX1 <- read_sav("banco_pelotas_DX_17-09.sav")
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
datasetbipolar <- dt3 [sample (1:nrow (dt3), 3778, replace=FALSE), ]
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
rose_outside_NET <- train(TB ~., data = rose_train, method = "glmnet", trControl = ctrl.rf_rose_NET, tuneLength = 10, metric = c("ROC","Accuracy"))
ctrl.rf_rose_NET_g <- trainControl(method="repeatedcv", number = 10, classProbs = TRUE,savePredictions = TRUE)
rose_outside_NET_accuracy <- train(TB ~., data = rose_train, method = "glmnet", trControl = ctrl.rf_rose_NET_g, tuneLength = 10)

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

# matrix de confusao 
library(e1071)
test_pred_up_NET <- predict(up_outside_NET, newdata = data_testing[,-1])
matrixConfusao_up_NET <- confusionMatrix(test_pred_up_NET, data_testing$TB, positive = "yes")
matrixConfusao_up_NET 
# ROC
library(caret)
library(pROC)
p_up_NET <- predict (up_outside_NET, data_testing[,-1])
p_prob_up_NET <- predict (up_outside_NET, data_testing[,-1], type = "prob")
confusionMatrix(p_up_NET, data_testing$TB)
print(confusionMatrix(p_up_NET, data_testing$TB, positive = "yes"))
r_up_NET <- roc (data_testing$TB, p_prob_up_NET[,"yes"])
plot(r_up_NET)
r_up_NET$auc

# matrix de confusao 
library(e1071)
test_pred_rose_NET <- predict(rose_outside_NET, newdata = data_testing[,-1])
matrixConfusao_rose_NET <- confusionMatrix(test_pred_rose_NET, data_testing$TB, positive = "yes")
matrixConfusao_rose_NET 
# ROC
library(caret)
library(pROC)
p_rose_NET <- predict (rose_outside_NET, data_testing[,-1])
p_prob_rose_NET <- predict (rose_outside_NET, data_testing[,-1], type = "prob")
confusionMatrix(p_rose_NET, data_testing$TB)
print(confusionMatrix(p_rose_NET, data_testing$TB, positive = "yes"))
r_rose_NET <- roc (data_testing$TB, p_prob_rose_NET[,"yes"])
plot(r_rose_NET)
r_rose_NET$auc

rose_outside_NET_onda4 <- rose_outside_NET
matrixConfusao_rose_NET_onda4 <- matrixConfusao_rose_NET

#######
library(SDMTools)

### mudando limiar onda 4 ####
# desfecho do conjunto de teste
# 0 = No, 1 = Yes
library(SDMTools)
obs_4 <- teste_onda4$TB

# Os levels estão em ordem?
levels(obs_4)

levels(obs_4) <- c("0", "1")
obs_4 <- as.numeric(as.character(obs))
obs_4

# predições do modelo em probabilidades
rf.predict_onda4 <- predict(rose_outside_NET_onda4, teste_onda4[, -1], type = "prob")
predictions_onda4 <- as.vector(rf.predict_onda4[, 2])
predictions_onda4

confusion_df_onda4 <- data.frame(obs_4, predictions_onda4)
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

matrixConfusao_rose_NET_onda4$byClass
library(purrr)
library(plyr)
confMatrix(0.5, obs, predictions)
result_list_onda4 <- map(threshold_seq, confMatrix, obs = obs_4, predictions = predictions_onda4)
result_df_onda4 <- ldply(result_list_onda4)

result_df_onda4

##plot dos limiares
plotPerformancesMeasures <- function(result_df_onda4){
  require(ggplot2)
  require(reshape2)
  
  colnames(result_df_onda4)
  result_selected <- result_df_onda4[, c("limiar", "AB", "Pos Pred Value", "Neg Pred Value")]
  colnames(result_selected) <- c("Cutoff", "Balanced Accuracy", "Pos Pred Value", "Neg Pred Value")
  result_long <- melt(result_selected, id.vars = "Cutoff")
  
  p1 <-ggplot(result_long, aes(x=Cutoff, y=value, group=variable)) +
    geom_line(aes(color=variable)) +
    geom_point(aes(color=variable)) +
    theme(panel.background = element_blank(), 
          axis.line = element_line(colour = "grey"),
          legend.title = element_blank())
  p1
  
  result_selected <- result_df_onda4[, c("limiar", "AB", "Sensitivity", "Specificity")]
  colnames(result_selected) <- c("Cutoff", "Balanced Accuracy", "Sensitivity", "Specificity")
  result_long <- melt(result_selected, id.vars = "Cutoff")
  
  p2 <-ggplot(result_long, aes(x=Cutoff, y=value, group=variable)) +
    geom_line(aes(color=variable)) +
    geom_point(aes(color=variable)) +
    theme(panel.background = element_blank(), 
          axis.line = element_line(colour = "grey"),
          legend.title = element_blank())
  
  p2
  
  return(list("BA_PPV_NPV_Vs_Cutoff" = p1 , "BA_Sen_Spe_Vs_Cutoff" = p2))
  
}

getPerformanceVsCutoff <- function(confusion_df, threshold_seq, positive_class){
  message("positive_class = as.character(0) ou as.character(1)")
  
  require(purrr)
  require(plyr)
  
  
  confMatrix <- function(i, obs, predictions, ...){
    require(caret)
    require(SDMTools)
    
    conf_matrix <- confusion.matrix(obs, predictions, threshold = i)
    cm_table <- as.table(conf_matrix)
    cm <- confusionMatrix(cm_table, positive = positive_class)
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
  
  result_list_onda4 <- map(threshold_seq, confMatrix, obs = obs_4, predictions = predictions_onda4) #aqui se muda em cada modelo
  result_df_onda4 <- ldply(result_list_onda4)
  return(result_df_onda4)
}

result_df_onda4 <- getPerformanceVsCutoff(result_df_onda4, threshold_seq, "1")
plots_onda4 <- plotPerformancesMeasures(result_df_onda4)
plots_onda4$BA_PPV_NPV_Vs_Cutoff
plots_onda4$BA_Sen_Spe_Vs_Cutoff




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
library(ROSE)
set.seed(9560)
rose_train_onda1 <- ROSE(TB ~ ., data  = treino_onda1)$data                         
table(rose_train_onda1$TB) 

####### Elastic net####
library(caret)
library(glmnet)
set.seed(108)
ctrl.rf_rose_NET <- trainControl(method="repeatedcv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary)
rose_outside_NET_onda1 <- train(TB ~., data = rose_train_onda1, method = "glmnet", trControl = ctrl.rf_rose_NET, tuneLength = 10, metric = "ROC")

##rose
# matrix de confusao 
library(e1071)
test_pred_rose_NET_onda1 <- predict(rose_outside_NET_onda1, newdata = teste_onda1[,-1])
matrixConfusao_rose_NET_onda1 <- confusionMatrix(test_pred_rose_NET_onda1, teste_onda1$TB, positive = "yes")
matrixConfusao_rose_NET_onda1 
# ROC
library(caret)
library(pROC)
p_rose_NET_onda1 <- predict (rose_outside_NET_onda1, teste_onda1[,-1])
p_prob_rose_NET_onda1 <- predict (rose_outside_NET_onda1, teste_onda1[,-1], type = "prob")
confusionMatrix(p_rose_NET_onda1, teste_onda1$TB)
print(confusionMatrix(p_rose_NET_onda1, teste_onda1$TB, positive = "yes"))
r_rose_NET_onda1 <- roc (teste_onda1$TB, p_prob_rose_NET_onda1[,"yes"])
plot(r_rose_NET_onda1)
r_rose_NET_onda1$auc
varImp(rose_outside_NET_onda1)

#mundando limiar onda 1 ####
library(SDMTools)
obs_1 <- teste_onda1$TB

# Os levels estão em ordem?
levels(obs_1)

levels(obs_1) <- c("0", "1")
obs_1 <- as.numeric(as.character(obs))
obs_1

# predições do modelo em probabilidades
rf.predict_onda1 <- predict(rose_outside_NET_onda1, teste_onda1[, -1], type = "prob")
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

matrixConfusao_rose_NET_onda1$byClass
library(purrr)
library(plyr)
confMatrix(0.5, obs, predictions)
result_list_onda1 <- map(threshold_seq, confMatrix, obs = obs_1, predictions = predictions_onda1)
result_df_onda1 <- ldply(result_list_onda1)

result_df_onda1

##plot dos limiares
plotPerformancesMeasures <- function(result_df_onda1){
  require(ggplot2)
  require(reshape2)
  
  colnames(result_df_onda1)
  result_selected <- result_df_onda1[, c("limiar", "AB", "Pos Pred Value", "Neg Pred Value")]
  colnames(result_selected) <- c("Cutoff", "Balanced Accuracy", "Pos Pred Value", "Neg Pred Value")
  result_long <- melt(result_selected, id.vars = "Cutoff")
  
  p1 <-ggplot(result_long, aes(x=Cutoff, y=value, group=variable)) +
    geom_line(aes(color=variable)) +
    geom_point(aes(color=variable)) +
    theme(panel.background = element_blank(), 
          axis.line = element_line(colour = "grey"),
          legend.title = element_blank())
  p1
  
  result_selected <- result_df_onda1[, c("limiar", "AB", "Sensitivity", "Specificity")]
  colnames(result_selected) <- c("Cutoff", "Balanced Accuracy", "Sensitivity", "Specificity")
  result_long <- melt(result_selected, id.vars = "Cutoff")
  
  p2 <-ggplot(result_long, aes(x=Cutoff, y=value, group=variable)) +
    geom_line(aes(color=variable)) +
    geom_point(aes(color=variable)) +
    theme(panel.background = element_blank(), 
          axis.line = element_line(colour = "grey"),
          legend.title = element_blank())
  
  p2
  
  return(list("BA_PPV_NPV_Vs_Cutoff" = p1 , "BA_Sen_Spe_Vs_Cutoff" = p2))
  
}

getPerformanceVsCutoff <- function(confusion_df, threshold_seq, positive_class){
  message("positive_class = as.character(0) ou as.character(1)")
  
  require(purrr)
  require(plyr)
  
  
  confMatrix <- function(i, obs, predictions, ...){
    require(caret)
    require(SDMTools)
    
    conf_matrix <- confusion.matrix(obs, predictions, threshold = i)
    cm_table <- as.table(conf_matrix)
    cm <- confusionMatrix(cm_table, positive = positive_class)
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
  
  result_list_onda1 <- map(threshold_seq, confMatrix, obs = obs_1, predictions = predictions_onda1) #aqui se muda em cada modelo
  result_df_onda1 <- ldply(result_list_onda1)
  return(result_df_onda1)
}

result_df_onda1 <- getPerformanceVsCutoff(result_df_onda1, threshold_seq, "1")
plots_onda1 <- plotPerformancesMeasures(result_df_onda1)
plots_onda1$BA_PPV_NPV_Vs_Cutoff
plots_onda1$BA_Sen_Spe_Vs_Cutoff


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
##Com ROSE
library(ROSE)
set.seed(9560)
rose_train_onda2 <- ROSE(TB ~ ., data  = treino_onda2)$data                         
table(rose_train_onda2$TB) 
library(caret)
library(glmnet)
set.seed(108)
ctrl.rf_rose_NET <- trainControl(method="repeatedcv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary)
rose_outside_NET_onda2 <- train(TB ~., data = rose_train_onda2, method = "glmnet", trControl = ctrl.rf_rose_NET, tuneLength = 10, metric = "ROC")

##rose
# matrix de confusao 
library(e1071)
test_pred_rose_NET_onda2 <- predict(rose_outside_NET_onda2, newdata = teste_onda2[,-1])
matrixConfusao_rose_NET_onda2 <- confusionMatrix(test_pred_rose_NET_onda2, teste_onda2$TB, positive = "yes")
matrixConfusao_rose_NET_onda2 
# ROC
library(caret)
library(pROC)
p_rose_NET_onda2 <- predict (rose_outside_NET_onda2, teste_onda2[,-1])
p_prob_rose_NET_onda2 <- predict (rose_outside_NET_onda2, teste_onda2[,-1], type = "prob")
confusionMatrix(p_rose_NET_onda2, teste_onda2$TB)
print(confusionMatrix(p_rose_NET_onda2, teste_onda2$TB, positive = "yes"))
r_rose_NET_onda2 <- roc (teste_onda2$TB, p_prob_rose_NET_onda2[,"yes"])
plot(r_rose_NET_onda2)
r_rose_NET_onda2$auc
varImp(rose_outside_NET_onda2)
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
rf.predict_onda2 <- predict(rose_outside_NET_onda2, teste_onda2[, -1], type = "prob")
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

matrixConfusao_rose_NET_onda2$byClass
library(purrr)
library(plyr)
confMatrix(0.5, obs, predictions)
result_list_onda2 <- map(threshold_seq, confMatrix, obs = obs_2, predictions = predictions_onda2)
result_df_onda2 <- ldply(result_list_onda2)

result_df_onda2
### plot limiar onda 2 ####
plotPerformancesMeasures <- function(result_df_onda2){
  require(ggplot2)
  require(reshape2)
  
  colnames(result_df_onda2)
  result_selected <- result_df_onda2[, c("limiar", "AB", "Pos Pred Value", "Neg Pred Value")]
  colnames(result_selected) <- c("Cutoff", "Balanced Accuracy", "Pos Pred Value", "Neg Pred Value")
  result_long <- melt(result_selected, id.vars = "Cutoff")
  
  p1 <-ggplot(result_long, aes(x=Cutoff, y=value, group=variable)) +
    geom_line(aes(color=variable)) +
    geom_point(aes(color=variable)) +
    theme(panel.background = element_blank(), 
          axis.line = element_line(colour = "grey"),
          legend.title = element_blank())
  p1
  
  result_selected <- result_df_onda2[, c("limiar", "AB", "Sensitivity", "Specificity")]
  colnames(result_selected) <- c("Cutoff", "Balanced Accuracy", "Sensitivity", "Specificity")
  result_long <- melt(result_selected, id.vars = "Cutoff")
  
  p2 <-ggplot(result_long, aes(x=Cutoff, y=value, group=variable)) +
    geom_line(aes(color=variable)) +
    geom_point(aes(color=variable)) +
    theme(panel.background = element_blank(), 
          axis.line = element_line(colour = "grey"),
          legend.title = element_blank())
  
  p2
  
  return(list("BA_PPV_NPV_Vs_Cutoff" = p1 , "BA_Sen_Spe_Vs_Cutoff" = p2))
  
}

getPerformanceVsCutoff <- function(confusion_df, threshold_seq, positive_class){
  message("positive_class = as.character(0) ou as.character(1)")
  
  require(purrr)
  require(plyr)
  
  
  confMatrix <- function(i, obs, predictions, ...){
    require(caret)
    require(SDMTools)
    
    conf_matrix <- confusion.matrix(obs, predictions, threshold = i)
    cm_table <- as.table(conf_matrix)
    cm <- confusionMatrix(cm_table, positive = positive_class)
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
  
  result_list_onda2 <- map(threshold_seq, confMatrix, obs = obs_1, predictions = predictions_onda2) #aqui se muda em cada modelo
  result_df_onda2 <- ldply(result_list_onda2)
  return(result_df_onda2)
}

result_df_onda2 <- getPerformanceVsCutoff(result_df_onda2, threshold_seq, "1")
plots_onda2 <- plotPerformancesMeasures(result_df_onda2)
plots_onda2$BA_PPV_NPV_Vs_Cutoff
plots_onda2$BA_Sen_Spe_Vs_Cutoff
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
##Com rose
library(ROSE)
set.seed(9560)
rose_train_onda3 <- ROSE(TB ~ ., data  = treino_onda3)$data                         
table(rose_train_onda3$TB) 

####### Elastic net####
library(caret)
library(glmnet)
set.seed(108)
ctrl.rf_rose_NET <- trainControl(method="repeatedcv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary)
rose_outside_NET_onda3 <- train(TB ~., data = rose_train_onda3, method = "glmnet", trControl = ctrl.rf_rose_NET, tuneLength = 10, metric = "ROC")

##rose
# matrix de confusao 
library(e1071)
test_pred_rose_NET_onda3 <- predict(rose_outside_NET_onda3, newdata = teste_onda3[,-1])
matrixConfusao_rose_NET_onda3 <- confusionMatrix(test_pred_rose_NET_onda3, teste_onda3$TB, positive = "yes")
matrixConfusao_rose_NET_onda3 
# ROC
library(caret)
library(pROC)
p_rose_NET_onda3 <- predict (rose_outside_NET_onda3, teste_onda3[,-1])
p_prob_rose_NET_onda3 <- predict (rose_outside_NET_onda3, teste_onda3[,-1], type = "prob")
confusionMatrix(p_rose_NET_onda3, teste_onda3$TB)
print(confusionMatrix(p_rose_NET_onda3, teste_onda3$TB, positive = "yes"))
r_rose_NET_onda3 <- roc (teste_onda3$TB, p_prob_rose_NET_onda3[,"yes"])
plot(r_rose_NET_onda3)
r_rose_NET_onda3$auc
varImp(rose_outside_NET_onda3)
library(SDMTools)
obs_3 <- teste_onda3$TB

# Os levels estão em ordem?
levels(obs_3)

levels(obs_3) <- c("0", "1")
obs_3 <- as.numeric(as.character(obs))
obs_3

# predições do modelo em probabilidades
rf.predict_onda3 <- predict(rose_outside_NET_onda3, teste_onda3[, -1], type = "prob")
predictions_onda3 <- as.vector(rf.predict_onda3[, 2])
predictions_onda3

confusion_df_onda3 <- data.frame(obs_3, predictions_onda3)
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

matrixConfusao_rose_NET_onda3$byClass
library(purrr)
library(plyr)
confMatrix(0.5, obs, predictions)
result_list_onda3 <- map(threshold_seq, confMatrix, obs = obs_3, predictions = predictions_onda3)
result_df_onda3 <- ldply(result_list_onda3)

result_df_onda3

result_df_onda3
plotPerformancesMeasures <- function(result_df_onda3){
  require(ggplot2)
  require(reshape2)
  
  colnames(result_df_onda3)
  result_selected <- result_df_onda3[, c("limiar", "AB", "Pos Pred Value", "Neg Pred Value")]
  colnames(result_selected) <- c("Cutoff", "Balanced Accuracy", "Pos Pred Value", "Neg Pred Value")
  result_long <- melt(result_selected, id.vars = "Cutoff")
  
  p1 <-ggplot(result_long, aes(x=Cutoff, y=value, group=variable)) +
    geom_line(aes(color=variable)) +
    geom_point(aes(color=variable)) +
    theme(panel.background = element_blank(), 
          axis.line = element_line(colour = "grey"),
          legend.title = element_blank())
  p1
  
  result_selected <- result_df_onda3[, c("limiar", "AB", "Sensitivity", "Specificity")]
  colnames(result_selected) <- c("Cutoff", "Balanced Accuracy", "Sensitivity", "Specificity")
  result_long <- melt(result_selected, id.vars = "Cutoff")
  
  p2 <-ggplot(result_long, aes(x=Cutoff, y=value, group=variable)) +
    geom_line(aes(color=variable)) +
    geom_point(aes(color=variable)) +
    theme(panel.background = element_blank(), 
          axis.line = element_line(colour = "grey"),
          legend.title = element_blank())
  
  p2
  
  return(list("BA_PPV_NPV_Vs_Cutoff" = p1 , "BA_Sen_Spe_Vs_Cutoff" = p2))
  
}

getPerformanceVsCutoff <- function(confusion_df, threshold_seq, positive_class){
  message("positive_class = as.character(0) ou as.character(1)")
  
  require(purrr)
  require(plyr)
  
  
  confMatrix <- function(i, obs, predictions, ...){
    require(caret)
    require(SDMTools)
    
    conf_matrix <- confusion.matrix(obs, predictions, threshold = i)
    cm_table <- as.table(conf_matrix)
    cm <- confusionMatrix(cm_table, positive = positive_class)
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
  
  result_list_onda3 <- map(threshold_seq, confMatrix, obs = obs_1, predictions = predictions_onda3) #aqui se muda em cada modelo
  result_df_onda3 <- ldply(result_list_onda3)
  return(result_df_onda3)
}

result_df_onda3 <- getPerformanceVsCutoff(result_df_onda3, threshold_seq, "1")
plots_onda3 <- plotPerformancesMeasures(result_df_onda3)
plots_onda3$BA_PPV_NPV_Vs_Cutoff
plots_onda3$BA_Sen_Spe_Vs_Cutoff



### colocando Intervalo de confiança
pred <- predict(rose_outside_NET, data.matrix(data_testing[, -1]), type = "prob")
predictions2 <- as.vector(pred[, 2])
rocobj2 <- roc(data_testing$TB, p_prob, ci=TRUE, of="auc", percent = FALSE)
plot(rocobj2,percent = FALSE, ci=TRUE, print.auc=TRUE)

## AGRUPANDO AUCS e colocando cores ####
library(RColorBrewer)
require(pROC)
glm.predict4 <- predict(rose_outside_NET, teste_onda4[, -1], type = "prob")
predictions4 <- as.vector(glm.predict4[, 2])
rocobj4 <- roc(data_testing$TB, predictions4, ci=TRUE, of="auc", percent = FALSE)
plot(rocobj4,percent = FALSE, ci=TRUE, print.auc=FALSE, col="darkblue") #print.auc se for TRUE, serve pra colocar valor de auc

require(pROC)
glm.predict1 <- predict(rose_outside_NET_onda1, teste_onda1[, -1], type = "prob")
predictions1 <- as.vector(glm.predict1[, 2])
rocobj1 <- roc(teste_onda2$TB, predictions1, ci=TRUE, of="auc", percent = FALSE)
plot(rocobj1,percent = FALSE, ci=TRUE, print.auc=TRUE, add=TRUE, col="darkorange3")

require(pROC)
glm.predict2 <- predict(rose_outside_NET_onda2, teste_onda2[, -1], type = "prob")
predictions2 <- as.vector(glm.predict2[, 2])
rocobj2 <- roc(teste_onda2$TB, predictions2, ci=TRUE, of="auc", percent = FALSE)
plot(rocobj2,percent = FALSE, ci=TRUE, print.auc=FALSE, add=TRUE, col="forestgreen")
require(pROC)
glm.predict3 <- predict(rose_outside_NET_onda3, teste_onda3[, -1], type = "prob")
predictions3 <- as.vector(glm.predict3[, 2])
rocobj3 <- roc(teste_onda3$TB, predictions3, ci=TRUE, of="auc", percent = FALSE)
plot(rocobj3,percent = FALSE, ci=TRUE, print.auc=TRUE, add=TRUE, col="firebrick1")


###Plot de risco ####
# Cerfique-se que suas classes observados são do tipo factor
obs <- teste_onda4$TB
obs <- as.factor(obs)
levels(obs)
table(obs)
model.predict <-  predict(rose_outside_NET, teste_onda4[, -1], type = "prob")
pred <- as.vector(model.predict[, 2])
pred
# Carrega as funções
getQuintis <- function(predictions_probabilities, response_factor){
  require(Hmisc)
  require(ggplot2)
  
  pred <- predictions_probabilities
  resp_factor <- response_factor
  
  predresp_df <- data.frame("ID" = 1:length(pred), pred, resp_factor)
  predresp_df <- predresp_df[order(predresp_df$pred, decreasing = TRUE), ]
  
  qq <- Hmisc::cut2(predresp_df$pred, g = 5)
  levels(qq) <- c("5", "4", "3", "2", "1")
  qq <- as.numeric(qq)
  
  df <- data.frame("ID" = predresp_df$ID, "pred" = predresp_df$pred, "quintis" = qq, "desfecho" = predresp_df$resp_factor)
  return(df)
}

plotRisk <- function(df, ylabel = "ylabel"){
  # Plot Concentration of risk curve
  
  df_table <- table(df$quintis, df$desfecho)
  #df_soma <- apply(df_table, 1, sum)
  total_yes <- sum(df_table[, 2])
  #df_table <- cbind(df_table, "Soma" = df_soma)
  
  df_chart <- data.frame("x_axis" = as.numeric(row.names(df_table)), "yes" = df_table[, 2]/total_yes*100)
  
  cat("Estao na mesma ordem?")
  cat(colnames(df_table)[1:2], " | ", levels(df$desfecho))
  
  library(ggplot2)
  p1 <- ggplot(data=df_chart, aes(x=x_axis, y = yes)) +
    geom_bar(stat = "identity") + labs(x = "Quintile of Predicted Risk", y = ylabel) +
    scale_x_reverse() + ylim(0, 100) + theme_classic()
  
  return("plot" = p1)
}  


# A função gera o dataframe de cada individuo
df <- getQuintis(pred, obs)
df

# Para relacionar esses dados ao banco principal de análise (my_data)
new_df <- data.frame(banco_pelotas_DX1[df$ID, ], df$quintis)

# A função plota o gráfico
# A proporção O gráfico relaciona os quintis do score
plotRisk(df, "Probability of Bipolar Disorder (%)")

my.data <- new_df[(new_df$TB==1) & (new_df$df.quintis==1 | new_df$df.quintis==5), ]
new_df$df.quintis <- as.factor(new_df$df.quintis)

new_df1 <- new_df[(new_df$TB==1), ]

library(psych)
new_df5 <- new_df
new_df5$quintil <- ifelse(new_df5$df.quintis>=1 & new_df5$df.quintis<=4, 1,5)
new_df5$quintil <- as.factor(new_df5$quintil)

library(psych)
#describeBy(sociodem, sociodem$cluster)
aov1 <- aov(kqitotbr ~ quintil, data = new_df5)
summary(aov1)
TukeyHSD(aov1)
wilcox.test(kqitotbr ~ quintil, data = new_df5)

aov2 <- aov(kescadol ~ quintil, data = new_df5)#escolaridade onda 18
summary(aov2)
TukeyHSD(aov2)
shapiro.test(new_df5$kescadol)
wilcox.test(kescadol ~ quintil, data = new_df5)

aov3 <- aov(lescoljovemano ~ quintil, data = new_df5) #escolaridade onda 22
summary(aov3)
TukeyHSD(aov3)
wilcox.test(lescoljovemano ~ quintil, data = new_df5, exact = FALSE)

#renda aoos 22
aov4 <- aov(lrenfam ~ quintil, data = new_df5) #escolaridade onda 22
summary(aov4)
TukeyHSD(aov4)
shapiro.test(new_df5$lrenfam)
wilcox.test(lrenfam ~ quintil, data = new_df5)

aov5 <- aov(lSRQsomatic ~ quintil, data = new_df5) #escolaridade onda 22
summary(aov5)
TukeyHSD(aov5)
shapiro.test(new_df5$lSRQsomatic)
wilcox.test(lSRQsomatic ~ quintil, data = new_df5)

aov6 <- aov(lSRQansiedade ~ quintil, data = new_df5) 
summary(aov6)
TukeyHSD(aov6)
wilcox.test(lSRQansiedade ~ quintil, data = new_df5, exact = FALSE)

aov7 <- aov(lSRQenergiavital ~ quintil, data = new_df5) #escolaridade onda 22
summary(aov7)
TukeyHSD(aov7)
wilcox.test(lSRQenergiavital ~ quintil, data = new_df5, exact = FALSE)

aov8 <- aov(lSRQpens_depre ~ quintil, data = new_df5) 
summary(aov8)
TukeyHSD(aov8)
wilcox.test(lSRQpens_depre ~ quintil, data = new_df5, exact = FALSE)

aov9 <- aov(hamiltonscore ~ quintil, data = new_df5) 
summary(aov9)
TukeyHSD(aov9)
shapiro.test(new_df5$hamiltonscore)
wilcox.test(hamiltonscore ~ quintil, data = new_df5)

###### deu ##
aov10 <- aov(lbestarcont ~ quintil, data = new_df5) 
summary(aov10)
TukeyHSD(aov10)
shapiro.test(new_df5$hamiltonscore)
wilcox.test(lbestarcont ~ quintil, data = new_df5, exact = FALSE)

aov11 <- aov(lduracao ~ quintil, data = new_df5) 
summary(aov11)
TukeyHSD(aov11)
wilcox.test(lduracao ~ quintil, data = new_df5, exact = FALSE)

new_df5$ld011 <- as.factor(new_df5$ld011)
chisq.test(x = new_df5$histpsiquiatri, y = new_df5$quintil)
chisq.test(x = new_df5$ld006, y = new_df5$quintil)
chisq.test(x = new_df5$ld011todos, y = new_df5$quintil)
new_df5$ld088 <- as.factor(new_df5$ld088)
new_df5$ld088 <- as.factor(new_df5$ld089)
chisq.test(x = new_df5$ld088, y = new_df5$quintil)
chisq.test(x = new_df5$ld089, y = new_df5$quintil)
chisq.test(x = new_df5$ld095, y = new_df5$quintil)
chisq.test(x = new_df5$lpsqi_dic, y = new_df5$quintil)
chisq.test(x = new_df5$lfagerscat, y = new_df5$quintil)
chisq.test(x = new_df5$lauditdep, y = new_df5$quintil)
chisq.test(x = new_df5$suicidio, y = new_df5$quintil)
chisq.test(x = new_df5$l079, y = new_df5$quintil) #tentativa suicidio
chisq.test(x = new_df5$l078, y = new_df5$quintil) #plano suicidio
chisq.test(x = new_df5$l077, y = new_df5$quintil) #ideação suicida
chisq.test(x = new_df5$anedonia, y = new_df5$quintil)
chisq.test(x = new_df5$lsrqdic, y = new_df5$quintil)
chisq.test(x = new_df5$kSRQdic, y = new_df5$quintil)
chisq.test(x = new_df5$LSD, y = new_df5$quintil)
###DEU SOLVENTE
chisq.test(x = new_df5$solvente, y = new_df5$quintil)
chisq.test(x = new_df5$cocaina, y = new_df5$quintil)
chisq.test(x = new_df5$maconha, y = new_df5$quintil)
chisq.test(x = new_df5$ecstasy, y = new_df5$quintil)

#####DEU###
chisq.test(x = new_df5$opioides, y = new_df5$quintil)
chisq.test(x = new_df5$sexonovo, y = new_df5$quintil)


####sociodemo##### 
sex_table <- table(datasetbipolar$sexonovo, datasetbipolar$TB)
sex_table
chisq.test(x = datasetbipolar$sexonovo, y = datasetbipolar$TB)

table(banco_pelotas_DX1$ld011todos, banco_pelotas_DX1$TB) #status laboral
chisq.test(banco_pelotas_DX1$ld011todos, banco_pelotas_DX1$TB)
table(banco_pelotas_DX1$ld023a, banco_pelotas_DX1$TB)#status civil
chisq.test(banco_pelotas_DX1$ld023a, banco_pelotas_DX1$TB)

table(banco_pelotas_DX1$labep3, banco_pelotas_DX1$TB)#status economico
chisq.test(banco_pelotas_DX1$labep3, banco_pelotas_DX1$TB)
shapiro.test(banco_pelotas_DX1$lescoljovemano) #ANOS DE ESTUDO
wilcox.test(lescoljovemano ~ TB, data = banco_pelotas_DX1, exact = FALSE)
#CALCULANDO OS QUINTIS
escolaridade <- banco_pelotas_DX1[(banco_pelotas_DX1$TB==1), ]
quantile(escolaridade$lescoljovemano)

###### correlação da onda 4 ####
library(dplyr)
new_data_cor <- data_rfe[c("Zkqitotbr", "Zkqvpsico", "Zkqvsocial", "Zkqvambi","Zkqvfisico" ,"ZkSRQdepre_anx", "ZkSRQsomatic", "ZkSQRenergia", "ZkSRQsentidepre", "Zkescadol","Zkibem", "Zkfeliz")]

names(new_data_cor)[1] <- "Intellectual functioning"
names(new_data_cor)[2] <- "Quality of life - Psychological"
names(new_data_cor)[3] <- "Quality of life - Social"
names(new_data_cor)[4] <- "Quality of life - Environment"
names(new_data_cor)[5] <- "Quality of life - Physical "
names(new_data_cor)[6] <- "Anxiety feelings"
names(new_data_cor)[7] <- "Somatic symptoms"
names(new_data_cor)[8] <- "Vital energy"
names(new_data_cor)[9] <- "Depressive thoughts"
names(new_data_cor)[10] <- "Years of education"
names(new_data_cor)[11] <- "socio-economic status index"
names(new_data_cor)[12] <- "Subjective Happiness"

library(corrplot)
corr <- cor(new_data_cor, method = c("spearman"))
corr
p.mat <- cor.mtest(new_data_cor)$p
res1_corr <- cor.mtest(new_data_cor, conf.level = .99)


library(corrplot)
#corrplot(eptrauma, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45) 

plot_cor <- corrplot(corr, p.mat = res1_corr$p, method = "color", type = "upper", pch.cex = .9,
                     pch.col = "white", order = "original", tl.col = "black")


#### Plotando erros padrão como Benson pediu####

ctrl.rf_rose_NET_ACCURACIA <- trainControl(method="repeatedcv", number = 10, classProbs = TRUE,savePredictions = TRUE)
rose_outside_NET_accuracy_onda4 <- train(TB ~., data = rose_train, method = "glmnet", trControl = ctrl.rf_rose_NET_ACCURACIA, tuneLength = 10)
ctrl.rf_rose_NET_80_20_5fold <- trainControl(method="repeatedcv", number = 5, classProbs = TRUE,savePredictions = TRUE)
rose_outside_NET_80_20_5fold <- train(TB ~., data = rose_train, method = "glmnet", trControl = ctrl.rf_rose_NET_80_20_5fold, tuneLength = 10)

ctrl.rf_rose_NET_80_20_LOOCV <- trainControl(method="LOOCV", classProbs = TRUE,savePredictions = TRUE)
rose_outsideNET_8020_LOOCV <- train(TB ~., data = rose_train, method = "glmnet", trControl = ctrl.rf_rose_NET_80_20_LOOCV, tuneLength = 10)

library(e1071)
test_pred_rose_NET_80_20_10cv <- predict(rose_outside_NET_accuracy_onda4, newdata = data_testing[,-1])
matrixConfusao_rose_NET_80_20_10cv <- confusionMatrix(test_pred_rose_NET_80_20_10cv, data_testing$TB, positive = "yes")
matrixConfusao_rose_NET_80_20_10cv 
library(e1071)
test_pred_rose_NET_80_20_5cv <- predict(rose_outside_NET_80_20_5fold, newdata = data_testing[,-1])
matrixConfusao_rose_NET_80_20_5cv <- confusionMatrix(test_pred_rose_NET_80_20_5cv, data_testing$TB, positive = "yes")
matrixConfusao_rose_NET_80_20_5cv 
library(e1071)
test_pred_rose_NET_80_20_LOOCV <- predict(rose_outsideNET_8020_LOOCV, newdata = data_testing[,-1])
matrixConfusao_rose_NET_80_20_LOOCV <- confusionMatrix(test_pred_rose_NET_80_20_LOOCV, data_testing$TB, positive = "yes")
matrixConfusao_rose_NET_80_20_LOOCV


library(caret)
set.seed(123)
intrain_50 <- createDataPartition(y = datasetbipolar$TB, p= .5, list = FALSE)
dtTrain_50 <-  datasetbipolar[intrain_50,]
dtTest_50 <- datasetbipolar[-intrain_50,]
dim(dtTrain_50); dim(dtTest_50)
# Porcentagem do desfecho em cada banco
prop.table(table(datasetbipolar$TB))
prop.table(table(dtTrain_50$TB))
prop.table(table(dtTest_50$TB))

#####Imputação ####
#iMPUTA??O USANDO media e moda  no treino pra aplicar depois no teste, evitando data linkage
train_matrix_50 <- dtTrain_50
test_matrix_50 <- dtTest_50
train_matrix_50 <- as.data.frame(train_matrix_50)
test_matrix_50 <- as.data.frame(test_matrix_50)
class(train_matrix_50)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

for (i in names(train_matrix_50)){
  print(class(train_matrix_50[,i]))
  print(is.factor(train_matrix_50[,i]))
  if (is.factor(train_matrix_50[,i])) {
    
    mode_value = getmode(train_matrix_50[,i])
    print("cat")
    #recorded_values[1,i] = mode_value
    train_matrix_50[is.na(train_matrix_50[,i]),i] = mode_value
    test_matrix_50[is.na(test_matrix_50[,i]),i] = mode_value
    print(mode_value)
  } else {
    mean_value = mean(train_matrix_50[,i], na.rm=TRUE)
    #recorded_values[1,i] = mean_value
    train_matrix_50[is.na(train_matrix_50[,i]),i] = mean_value
    test_matrix_50[is.na(test_matrix_50[,i]),i] = mean_value
    print("num")
  }
  
}

map_int(test_matrix_50, function(x){length(x[is.na(x)])})
str(train_matrix_50)
anyNA(train_matrix_50)
anyNA(test_matrix_50)

treino_onda4_50 <- train_matrix_50[,c("TB","ktabagismodiario","Zkqitotbr", "kprocom", "kprosau", 
                                      "kmorte", "kprobdin","kmudcas", "kternam", "kinsbai", "kbriga",
                                      "kapanhou","kassalto","kauditdic", "Zkqvfisico", "Zkqvpsico", "Zkqvsocial",
                                      "Zkqvambi","ZkSRQdepre_anx", "ZkSRQsomatic", "ZkSQRenergia", "ZkSRQsentidepre",
                                      "histpsiquiatri", "Zkescadol", "Zkibem", "Zkfeliz", "cocaina_lifetime", 
                                      "maconha_lifetime", "alucino_lifetime", "solvente_lifetime", "alcool_lifetime",
                                      "risco_suicidio", "kfobsoc", "kansger", "ktdah"
)]
prop.table(table(treino_onda4_50$TB))
teste_onda4_50 <- test_matrix_50[,c("TB","ktabagismodiario","Zkqitotbr", "kprocom", "kprosau", 
                                    "kmorte", "kprobdin","kmudcas", "kternam", "kinsbai", "kbriga",
                                    "kapanhou","kassalto","kauditdic", "Zkqvfisico", "Zkqvpsico", "Zkqvsocial",
                                    "Zkqvambi","ZkSRQdepre_anx", "ZkSRQsomatic", "ZkSQRenergia", "ZkSRQsentidepre",
                                    "histpsiquiatri", "Zkescadol", "Zkibem", "Zkfeliz", "cocaina_lifetime", 
                                    "maconha_lifetime", "alucino_lifetime", "solvente_lifetime", "alcool_lifetime",
                                    "risco_suicidio", "kfobsoc", "kansger", "ktdah" 
)]
prop.table(table(teste_onda4_50$TB))
prop.table(table(treino_onda4_50$TB))
anyNA(treino_onda4_50)
anyNA(teste_onda4_50)
### treinando o algoritmo
data_rfe_50 <- treino_onda4_50
levels(data_rfe_50$TB)
levels(data_rfe_50$TB) <- c("no", "yes")
levels(teste_onda4_50$TB)
levels(teste_onda4_50$TB) <- c("no", "yes")
#rose
library(ROSE)
set.seed(9560)
rose_train_50 <- ROSE(TB ~ ., data  = data_rfe_50)$data                         
table(rose_train_50$TB)

##treinando com 50-50 split ####
ctrl.rf_rose_NET_50 <- trainControl(method="repeatedcv", number = 10, classProbs = TRUE,savePredictions = TRUE)
rose_outside_NET_onda4_50 <- train(TB ~., data = rose_train_50, method = "glmnet", trControl = ctrl.rf_rose_NET_50, tuneLength = 10)
ctrl.rf_rose_NET_50_5fold <- trainControl(method="repeatedcv", number = 5, classProbs = TRUE,savePredictions = TRUE)
rose_outside_NET_onda4_50_5fold <- train(TB ~., data = rose_train_50, method = "glmnet", trControl = ctrl.rf_rose_NET_50_5fold, tuneLength = 10)
ctrl.rf_rose_NET_50_LOOCV <- trainControl(method="LOOCV", classProbs = TRUE,savePredictions = TRUE)
rose_outside_NET_onda4_50_LOOCV <- train(TB ~., data = rose_train_50, method = "glmnet", trControl = ctrl.rf_rose_NET_50_LOOCV, tuneLength = 10)

library(e1071)
test_pred_rose_NET_50_10CV <- predict(rose_outside_NET_onda4_50, newdata = teste_onda4_50[,-1])
matrixConfusao_rose_NET_50_10CV <- confusionMatrix(test_pred_rose_NET_50_10CV, teste_onda4_50$TB, positive = "yes")
matrixConfusao_rose_NET_50_10CV
library(e1071)
test_pred_rose_NET_50_5CV <- predict(rose_outside_NET_onda4_50_5fold, newdata = teste_onda4_50[,-1])
matrixConfusao_rose_NET_50_5CV <- confusionMatrix(test_pred_rose_NET_50_5CV, teste_onda4_50$TB, positive = "yes")
matrixConfusao_rose_NET_50_5CV

library(e1071)
test_pred_rose_NET_50_LOOCV <- predict(rose_outside_NET_onda4_50_LOOCV, newdata = teste_onda4_50[,-1])
matrixConfusao_rose_NET_50_LOOCV <- confusionMatrix(test_pred_rose_NET_50_LOOCV, teste_onda4_50$TB, positive = "yes")
matrixConfusao_rose_NET_50_LOOCV

accuracy_data <- data.frame(rose_outside_NET_accuracy_onda4$results$Accuracy, 
                             rose_outside_NET_80_20_5fold$results$Accuracy, 
                             rose_outsideNET_8020_LOOCV$results$Accuracy, 
                             rose_outside_NET_onda4_50$results$Accuracy, 
                             rose_outside_NET_onda4_50_5fold$results$Accuracy, 
                             rose_outside_NET_onda4_50_LOOCV$results$Accuracy)

accuracy_data1 <- accuracy_data
#rename names of columns
names(accuracy_data1)[1] <- "80_20_10CV"
names(accuracy_data1)[2] <- "80_20_5CV"
names(accuracy_data1)[3] <- "80_20_LOOCV"
names(accuracy_data1)[4] <- "50_50_10CV"
names(accuracy_data1)[5] <- "50_50_5CV"
names(accuracy_data1)[6] <- "50_50_LOOCV"

accuracy_data2 <- accuracy_data1
accuracy_data2$`80_20_10CV` <- 0.7709 - accuracy_data2$`80_20_10CV`
accuracy_data2$`80_20_5CV` <- 0.7722 - accuracy_data2$`80_20_5CV` 
accuracy_data2$`80_20_LOOCV` <- 0.7722 - accuracy_data2$`80_20_LOOCV`
accuracy_data2$`50_50_10CV` <- 0.7479 - accuracy_data2$`50_50_10CV`
accuracy_data2$`50_50_5CV` <- 0.7463 - accuracy_data2$`50_50_5CV`
accuracy_data2$`50_50_LOOCV` <- 0.7474 - accuracy_data2$`50_50_LOOCV` 

##juntando as linhas e colunas
library(tidyr)
library(dplyr)
error_data <- accuracy_data2 %>% gather(model, error, c("80_20_10CV", "80_20_5CV", "80_20_LOOCV", "50_50_10CV", "50_50_5CV", "50_50_LOOCV"))

error_data <- error_data %>%
 filter(error >= 0.011 & error <= 0.111)

library(ggplot2)
ggplot(error_data) +
 aes(x = model, y = error) +
 geom_boxplot(fill = "#0c4c8a") +
 labs(x = "Models with different splits and cross-validation methods", y = "Estimation error on the prediction accuracy") +
 theme_classic()

#######Permutation#####
ctrl.rf_rose_NET <- trainControl(method="repeatedcv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary)
rose_outside_NET <- train(TB ~., data = rose_train, method = "glmnet", trControl = ctrl.rf_rose_NET, tuneLength = 10, metric = "ROC")

library(ggplot2)
library(caret)
ctrl <- trainControl(method="repeatedcv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary)
# Modelo treinado normalmente
rose_outside_NET <- train(TB ~., data = rose_train, method = "glmnet", trControl = ctrl.rf_rose_NET, tuneLength = 10, metric = "ROC")

# Carregue
# return a frame of the deviance scores on the permuted data
permutation_test = function(dataf, ycol, nperm) {
  browser()
  nrows = dim(dataf)[1]
  y = dataf[[ycol]]
  X = dataf[, setdiff(colnames(dataf), ycol), drop=FALSE]
  
  aucs <- numeric(nperm)
  
# Substitua a linha abaixo pelo seu trainControl
  ctrl <- trainControl(method="repeatedcv", number = 10, classProbs = TRUE,savePredictions = TRUE, summaryFunction = twoClassSummary)
  
  for(i in seq_len(nperm)) {
    # random order of rows
    ord = sample.int(nrows, size=nrows, replace=FALSE)
    dff <- data.frame("y"=y[ord], X)
    
    ###############
    # Manter os parâmetros x e y como estão. Modificar tuneGrid.
    #model <- train(x = df[, -1], y = df$y, method = "glmnet", trControl = ctrl)
    model <- train(x = dff[, -1], y = dff$y, method = "glmnet", trControl = ctrl, 
          tuneLength = 10, metric = "ROC")
    
    #print(summary(model))
    aucs[[i]] = max(model$results$ROC)
    cat(i, "\r")
  }
  aucs
}

plotHistogram <- function(x, xlab, performance_model){
  library(ggplot2)
  df <- data.frame("var" = x)
  vertical <- performance_model
  p <- ggplot(df, aes(x = var)) + geom_histogram(bins = 10) + theme_classic() + geom_vline(aes(xintercept=vertical),
                                                                                           color="orange", linetype="dashed", size=1) + labs(x = xlab)
  p
  
}

# Realiza o teste de permutação
# accuracies_perm <- permutation_test(dataset_treino, "desfecho", numero_permutacoes)

set.seed(2501)
aucs_perm <- permutation_test(rose_train, "TB", 2)
# Acurácia para comparação
# cm é a matriz de confusão com base nos dados de teste
test_accuracy <- cm$overall[1]
# Plota histograma das permutacoes
plotHistogram(accuracies_perm, 'Accuracy', test_accuracy)

library(pROC)
