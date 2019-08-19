set.seed(1234)

load("datos/replicatedData.RData")

source("0_loadLibraries.R")
loadpkg("glmnet")
loadpkg("caret")
loadpkg("pROC")
loadpkg("kableExtra")
loadpkg("magick")

df<-rep.data.balanced

foldDoubleKVal<-function(df){
  all.train.test <- sparse.model.matrix(as.formula(paste("status~", paste(colnames(df)[colnames(df)!="status"], sep="", collapse = "+"))),df)
  pb<-txtProgressBar(min=1, max =10, style = 3 )
  # Input 2. Set the fractions of the dataframe you want to split into training, 
  # validation, and test.
  fractionTraining   <- 0.60
  fractionValidation <- 0.20
  fractionTest       <- 0.20
  # Compute sample sizes.
  sampleSizeTraining   <- floor(fractionTraining   * nrow(all.train.test))
  sampleSizeValidation <- floor(fractionValidation * nrow(all.train.test))
  sampleSizeTest       <- floor(fractionTest       * nrow(all.train.test))
  auc.list<-c()
  accuracy<-c()
  accuracy.balanced<-c()
  best.alpha<-c()
  best.lambda<-c()
  for(j in 1:10) {
    # Create the randomly-sampled indices for the dataframe. Use setdiff() to
    # avoid overlapping subsets of indices.
    indicesTraining    <- sort(sample(seq_len(nrow(all.train.test)), size=sampleSizeTraining))
    indicesNotTraining <- setdiff(seq_len(nrow(all.train.test)), indicesTraining)
    indicesValidation  <- sort(sample(indicesNotTraining, size=sampleSizeValidation))
    indicesTest        <- setdiff(indicesNotTraining, indicesValidation)
    
    # Finally, output the three dataframes for training, validation and test.
    x.train <- all.train.test[indicesTraining, ]
    x.val <- all.train.test[indicesValidation, ]
    x.test <- all.train.test[indicesTest, ]
    
    y.train <- df[indicesTraining, ]$status
    y.val <- df[indicesValidation, ]$status
    y.test <- df[indicesTest, ]$status
    
    # Vemos el alpha para el que obtenemos el mejor resultado
    lista<-list()
    aucs<-c()
    lambdas<-c()
    
    for (i in 0:10) { 
      lambdas<-c(lambdas,cv.glmnet(x.train, y.train, family = "binomial",
                                   nfold = 10, type.measure = "auc", paralle = TRUE, alpha = 1)$lambda.1se)
      lista[[(i+1)]]<-glmnet(x.train, y.train, family = "binomial",lambda = lambdas[i+1],alpha = i/10)
      
      aucs<-c(aucs,roc(y.val, as.numeric(predict(lista[[(i+1)]], x.val, type = "response"))))
      
    }
    
    best.alpha<-c(best.alpha, (which.min(lambdas)-1)/10)
    best.lambda<-c(best.lambda, min(lambdas))
    
    lasso.model<-glmnet(x.train, y.train, family = "binomial",lambda = min(lambdas),alpha = (which.min(lambdas)-1)/10)
    lasso.prob <- predict(lasso.model,type="class", newx = x.test)
    #Sacamos la matriz de confusion
    cm = confusionMatrix(as.factor(lasso.prob),as.factor(y.test))
    accuracy.balanced<-c(accuracy.balanced,cm$byClass["Balanced Accuracy"])
    accuracy<-c(accuracy, cm$byClass["Balanced Accuracy"])
    auc.list<-c(auc.list, roc(y.test, as.numeric(lasso.prob))$auc[1])
    setTxtProgressBar(pb,j)
    
  }
  
  return(data.frame(auc=auc.list, accuracy= accuracy, balanced.accuracy = accuracy.balanced, alpha = best.alpha, lambda = best.lambda))
}

resDoubleValidation<-foldDoubleKVal(df)


kable(resDoubleValidation, format = "latex")%>%
  kable_styling() %>%
  save_kable(file="results/ML/10DoubleValidationLASSO.jpg")

cat("\n The mean AUC for lasso/ridge is ", mean(resDoubleValidation$accuracy))
save(resDoubleValidation, file="datos/resultadosLASSORIDGE.RData")


train_ind <- sample(seq_len(nrow(df)), size = floor(0.85 * nrow(df)))
data.train<-model.matrix(status~.,df[train_ind, ])
fit.lasso <- glmnet(data.train, df[train_ind, ]$status, family="gaussian", alpha=1)
fit.ridge <- glmnet(data.train, df[train_ind, ]$status, family="gaussian", alpha=0)
fit.elnet <- glmnet(data.train, df[train_ind, ]$status, family="gaussian", alpha=.5)


#Plot solution paths:
jpeg("results/ML/lasso-ridge-performance.jpg")
par(mfrow=c(3,2))
# For plotting options, type '?plot.glmnet' in R console
plot(fit.lasso, xvar="lambda")
plot(fit.lasso, main="LASSO")

plot(fit.ridge, xvar="lambda")
plot(fit.ridge, main="Ridge")

plot(fit.elnet, xvar="lambda")
plot(fit.elnet, main="Elastic Net")

dev.off()

