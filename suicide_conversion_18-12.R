#Loading the dataset
library(haven)
banco_pelotas <- read_sav("C:/Users/TB e Internet/Downloads/banco_pelotas_suicidio_12-02.sav")
banco_pelotas$conversao <- ifelse(banco_pelotas$kmini17==0 & banco_pelotas$l079==1, 1, 0)
#exclui quem tentou suicidio aos 18 anos no spss
library(haven)
suicidio_conversao <- read_sav("suicidio_conversao.sav")
table(suicidio_conversao$conversao)
## deleting missing in the outcome. 
library(dplyr)
dt <- suicidio_conversao[!is.na(suicidio_conversao$conversao),]
#Proportion of classes
prop.table(table(dt$conversao))
table(dt$conversao)

####Creating new variables for drug use #####
sumFeatures <- function(x, ...){
  l <- length(x[is.na(x)]) 
  if (l == length(x)) y <- NA else y <- sum(x[!is.na(x)])
  return(y)
}

#smoking
df_fumar <- data.frame(dt$hc01, dt$jc01, dt$kfumante)
fumar_lifetime <- apply(df_fumar, 1, sumFeatures)
fumar_lifetime[fumar_lifetime > 1] <- 1
df_fumar$fumar_lifetime <- fumar_lifetime
## hallucinogenic
df_alucinogeno <-data.frame(dt$kc11, dt$kc12, dt$kc15)
alucinogeno_lifetime <- apply(df_alucinogeno, 1, sumFeatures)
alucinogeno_lifetime[alucinogeno_lifetime > 1] <- 1
df_alucinogeno$alucinogeno_lifetime <- alucinogeno_lifetime
##Solvent use
df_solvente <-data.frame(dt$hcsolvente, dt$jcsolvente, dt$kc10)
solvente_lifetime <- apply(df_solvente, 1, sumFeatures)
solvente_lifetime[solvente_lifetime > 1] <- 1
df_solvente$solvente_lifetime <- solvente_lifetime
#Alcohol use
df_alcool <-data.frame(dt$hc07, dt$jc06, dt$kbebalc)
alcool_lifetime <- apply(df_alcool, 1, sumFeatures)
alcool_lifetime[alcool_lifetime > 1] <- 1
df_alcool$alcool_lifetime <- alcool_lifetime
#Crack-cocaine use
df_cocaina <-data.frame(dt$jc11c, dt$kc06, dt$kc09, dt$kc12, dt$kc13, dt$kc14)
cocaina_lifetime <- apply(df_cocaina, 1, sumFeatures)
cocaina_lifetime[cocaina_lifetime > 1] <- 1
df_cocaina$cocaina_lifetime <- cocaina_lifetime
#Marijuana use
df_maconha <-data.frame(dt$jc11d, dt$kc05)
maconha_lifetime <- apply(df_maconha, 1, sumFeatures)
maconha_lifetime[maconha_lifetime > 1] <- 1
df_maconha$maconha_lifetime <- maconha_lifetime
##Inserting new columns related drug use
dt$fumar_lifetime<- fumar_lifetime 
dt$alucino_lifetime <- alucinogeno_lifetime
dt$solvente_lifetime <- solvente_lifetime
dt$alcool_lifetime <- alcool_lifetime
dt$cocaina_lifetime <- cocaina_lifetime
dt$maconha_lifetime <- maconha_lifetime
  
#selecting columns for a new dataset 
dt1 <- dt[,c("conversao", "ahipert", "adiabet", "ainfeccao", "aanemia", "aintern", "afumou", "abebalc", "avivmar", "sexonovo", "aalcool", "aidadmae", "apesorn","acompr", "apcrn",  "arenfam", "hm046", "hm114a", "hm114b", "hm114c","hm114d", "hm114e", "hprocom", "hm175","hm176", "hm177", "hm178", "hm179",  "hm180", "hm181", "hm182", "hm183","hm184", "hm185", "hm186", "hm187", "hm188", "hm189", "hm190", "hm191", "hm192", "hm193", "hm194", "hm195", "hm196", "hm197", "hm198", "hm199", "hm201", "hm206", "hm207", "hm208",  "hm209", "hm210", "hm211", "hm212", "hm213", "hm214", "hm215", "hm216",  "hm217", "hm218", "hm219", "hm220",  "hm221", "hm222", "hm223", "hm224", "hm225", "hc01",  "hc06m", "hc07", "Zhpemotion", "Zhpconduct", "Zhphyper", "Zhppeer", "Zhpprosoc", "Zhsemotion", "Zhsconduct", "Zhshyper", "Zhspeer", "Zhsprosoc", "histpsiquiatri", "Zhtsono", 
             "jc01", "jc04", "jc05", "jc06","jcorpel5", "Zjpemotio", "Zjpconduc", "Zjphyper", "Zjppeer", "Zjpprosoc", 
             "histpsiquiatri","ktabagismodiario", "Zkqitotbr", "kprocom", "kprosau", "kmorte", "kprobdin","kmudcas", "kternam", "kinsbai", "kbriga", "kapanhou","kassalto","kauditdic", "Zkqvfisico", "Zkqvpsico", "Zkqvsocial", "Zkqvambi","ZkSRQdepre_anx", "ZkSRQsomatic", "ZkSQRenergia", "ZkSRQsentidepre", "histpsiquiatri", "Zkescadol", "kibem", "Zkfeliz", "cocaina_lifetime", "maconha_lifetime", "alucino_lifetime", "solvente_lifetime", "alcool_lifetime",
             "kdepredsm","kmaniamini", "kfobsoc", "kansger", "ktdah")]

#converting into z-score variables that I haven't done yet
dt1$aidadmae <- scale(dt1$aidadmae, center = TRUE, scale = TRUE)
dt1$apesorn <- scale(dt1$apesorn, center = TRUE, scale = TRUE)
dt1$acompr <- scale(dt1$acompr, center = TRUE, scale = TRUE)
dt1$apcrn <- scale(dt1$apcrn, center = TRUE, scale = TRUE)
dt1$arenfam <- scale(dt1$arenfam, center = TRUE, scale = TRUE)
dt1$kibem <- scale(dt1$kibem, center = TRUE, scale = TRUE)

#transforming as numeric
dt1$ZkSQRenergia <- as.numeric(dt1$ZkSQRenergia)
dt1$ZkSRQsentidepre <- as.numeric(dt1$ZkSRQsentidepre)
dt1$ZkSRQsomatic <- as.numeric(dt1$ZkSRQsomatic)
dt1$ZkSRQsentidepre <- as.numeric(dt1$ZkSRQsentidepre)
dt1$ZkSRQdepre_anx <- as.numeric(dt1$ZkSRQdepre_anx)
dt1$Zkqitotbr <- as.numeric(dt1$Zkqitotbr)
#deleting
dt1$soma <- NULL
dt1$nquest <- NULL
dt1$dv <- NULL
dt1$jc08 <- NULL
# Finding the class of the variables
library(purrr)
map_chr(dt1, class)
dt1 <- as.data.frame(dt1)
##Variable type conversion
library(purrr)
dt2 <- map_df(dt1, function(x){
  if (length(levels(as.factor(x))) > 8) z <- as.numeric(x) else z <- as.factor(x)}
)
map_chr(dt2, class)
# Finding the class of the variables again
library(purrr)
map_chr(dt2, class)
##transforming as numeric
dt3 <- dt2
map_chr(dt3, class)
dt3$ZkSQRenergia <- as.numeric(dt3$ZkSQRenergia)
dt3$ZkSRQsentidepre <- as.numeric(dt3$ZkSRQsentidepre)
dt3$ZkSRQsomatic <- as.numeric(dt3$ZkSRQsomatic)
dt3$ZkSRQsentidepre <- as.numeric(dt3$ZkSRQsentidepre)
dt3$ZkSRQdepre_anx <- as.numeric(dt3$ZkSRQdepre_anx)
dt3$Zkqitotbr <- as.numeric(dt3$Zkqitotbr)

### HOLD OUT ####
library(caret)
set.seed(123)
datasetsuicidio <- dt3 [sample (1:nrow (dt3), 3408, replace=FALSE), ]
## training and testing with 80% for training##
library(caret)
set.seed(123)
intrain <- createDataPartition(y = datasetsuicidio$conversao, p= 0.8, list = FALSE)
dtTrain <-  datasetsuicidio[intrain,]
dtTest <- datasetsuicidio[-intrain,]
dim(dtTrain); dim(dtTest)
# Porcentage of outcome 
prop.table(table(datasetsuicidio$conversao))
prop.table(table(dtTrain$conversao))
prop.table(table(dtTest$conversao))
dtTrain <- as.data.frame(dtTrain)
dtTest <- as.data.frame(dtTest)
## parallel process
library(doParallel)
cl <- makeCluster(4)
registerDoParallel(cl)
foreach(i=1:4) %dopar% sqrt(i)


library(mlr)
library(adabag)
library(rpart)
imp = impute(dtTrain, target = "Ozone", cols = list(Solar.R = imputeHist(),
                                                       Wind = imputeLearner("classif.rpart")), dummy.cols = c("Solar.R", "Wind"))
summary(imp$data)

rpart_imp <- impute(dtTrain, classes = list(numeric = imputeLearner(makeLearner("regr.rpart")),
                    factor = imputeLearner(makeLearner("classif.rpart"))),
                    dummy.classes = c("numeric","factor"),
                    dummy.type = "numeric")
summary(rpart_imp$data)

#Imputation using bagImpute (caret) without double dipping
library(purrr)
library(d3heatmap)
library(caret)
library(RANN)
dtTrain_imp <- dtTrain
dtTest_imp <- dtTest

## dummy variables
dtTrain_imp <- dummyVars(conversao~., data = dtTrain)

preProcValues2 <- preProcess(method = "BoxCox", dtTrain_imp)


gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

nrow(gbmGrid)

set.seed(825)
gbmFit2 <- train(conversao ~ ., data = dtTrain_imp, 
                 method = "gbm", 
                 trControl = fitControl, 
                 verbose = FALSE, 
                 ## Now specify the exact models 
                 ## to evaluate:
                 tuneGrid = gbmGrid)
gbmFit2


#preProcValues <- dt %>% select(-GroupCTR.BD, -Sex) %>% preProcess(method = c("center", "scale"))
preProc <- preProcess(method="knnImpute", dtTrain_imp[, -1])
preProc <- preProcess(method="bagImpute", dtTrain[, -1])

dtTrain_imp[, -1] <- predict(preProc, dtTrain_imp[, -1])
dtTest[, -1] <- predict(preProc, dtTest[, -1])

#Run automated Machine Learning
y <- "went_on_backorder"
x <- setdiff(names(df), c(y, "sku"))
aml <- h2o.automl(y = y, x = x,
                  training_frame = df,
                  max_models = 10, stopping_metric = "AUC",
                  seed = 123, balance_classes = TRUE, sort_metric = "AUC", nfolds = 10)
lb <- aml@leaderboard
#we will view a snapshot of the top models.
print(lb)
print(lb, n = nrow(lb))
# Get model ids for all models in the AutoML Leaderboard
model_ids <- as.data.frame(aml@leaderboard$model_id)[,1]
# If you need to generate predictions on a test set, you can make
# predictions directly on the `"H2OAutoML"` object, or on the leader
# model object directly

pred <- h2o.predict(aml, test)  # predict(aml, test) also works

# or:
pred <- h2o.predict(aml@leader, test)
# Get the "All Models" Stacked Ensemble model
se <- h2o.getModel(grep("StackedEnsemble_AllModels", model_ids, value = TRUE)[1])
# Get the Stacked Ensemble metalearner model
metalearner <- h2o.getModel(se@model$metalearner$name)
h2o.varimp(metalearner)
h2o.varimp_plot(metalearner)
#Saving the model
h2o.saveModel(aml@leader, path = "./product_backorders_model_bin")
h2o.download_mojo(aml@leader, path = "./")
