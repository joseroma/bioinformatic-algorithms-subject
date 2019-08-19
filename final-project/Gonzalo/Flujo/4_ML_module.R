
load("datos/replicatedData.RData")
source("0_loadLibraries.R")
loadpkg("rpart")
loadpkg("caret")
loadpkg("pROC")
loadpkg("nnet")
loadpkg("kableExtra")
loadpkg("magick")

rep.data.balanced$time<-as.numeric(rep.data.balanced$time)
rep.data.balanced$status<-as.factor(rep.data.balanced$status)
crossValidation.models <-function(data,k) {
  
  auc.glm<-list()
  auc.dt<-list()
  auc.nnet<-list()
  cf<-createFolds(data$status, k=k)
  formula=as.formula(paste("status", ".", sep="~"))
  
  for(i in 1:k) {
    
    test <-data[unlist(cf[i]),]
    train <-data[-unlist(cf[i]),]
    train$status <-as.factor(train$status)
    test$status <-as.factor(test$status)
    
    #dt
    modelo.dt<-rpart(formula,data=train,control=rpart.control(minsplit=1, cp=0.007));
    mod.estimado.dt<-predict(modelo.dt, newdata=test, type="matrix");
    mod.estimado.dt<-as.matrix(mod.estimado.dt)
    if(is.factor(mod.estimado.dt)) mod.estimado.dt<-ifelse(mod.estimado.dt=="X1", 1, 0)
    new.auc.dt<-roc(as.numeric(test$status), as.numeric(mod.estimado.dt[,dim(mod.estimado.dt)[2]]), smooth=F, auc=T)$auc
    auc.dt<-c(auc.dt,new.auc.dt)
    
    #Regresión logística
    modelo.glm<-glm(formula, train, family=binomial("logit"));
    modelo.glm$xlevels$PAM50mRNA <- union(modelo.glm$xlevels$PAM50mRNA, levels(test$PAM50mRNA))
    mod.estimado.glm<-predict(modelo.glm, newdata=test, type="response");
    if(is.factor(mod.estimado.glm)) mod.estimado.dt<-ifelse(mod.estimado.glm=="X1", 1, 0)
    new.auc.glm<-roc(as.numeric(test$status), mod.estimado.glm, smooth=F, auc=T)$auc
    auc.glm<-c(auc.glm,new.auc.glm)
    
    #Neural network
    modelo.nnet<-nnet(formula,data=train, size=5, decay=0, maxit=7, trace=F);
    mod.estimado.nnet<-predict(modelo.nnet, newdata=test, type="raw");
    if(is.factor(mod.estimado.nnet)) mod.estimado.dt<-ifelse(mod.estimado.nnet=="X1", 1, 0)
    new.auc.nnet<-roc(as.numeric(test$status), mod.estimado.nnet, smooth=F, auc=T)$auc
    auc.nnet<-c(auc.nnet,new.auc.nnet)
    
  }
  
  all.means<-data.frame(method=c("glm","dt","nnet"), metrics=c(mean(unlist(auc.glm)), mean(unlist(auc.dt)), mean(unlist(auc.nnet))))
  
  return(all.means) 
}

res<-crossValidation.models(rep.data.balanced,8)

kable(res, format = "latex")%>%
  kable_styling() %>%
  save_kable(file="results/ML/10DoubleValidationMLmethods.jpg")

load("datos/resultadosLASSORIDGE.RData")
mean(resDoubleValidation$auc)

if(sum(mean(resDoubleValidation$auc),res[,"metrics"][1])>sum(res[,"metrics"][2:3])){
  cat("\nLos resultados indican que tenemos dependencias lineares en nuestro conjunto de datos.\n
      Por lo que tenemos mejores resultados para LASSO/RIDGE/GLM",
      "\nLASSO/RIDGE: ", mean(resDoubleValidation$auc), "\nGLM: ", res[,"metrics"][1],
      "\nDT: ", res[,"metrics"][2],"\nNNET: ", res[,"metrics"][3])
}else if(sum(mean(resDoubleValidation$auc),res[,"metrics"][1])<sum(res[,"metrics"][2:3])){
  cat("\nLos resultados indican que tenemos dependencias no lineares en nuestro conjunto de datos.\n
      Por lo que tenemos mejores resultados para redes neuronales.",
      "\nLASSO/RIDGE: ", mean(resDoubleValidation$auc), "\nGLM: ", res[,"metrics"][1],
      "\nDT: ", res[,"metrics"][2],"\nNNET: ", res[,"metrics"][3])
}
