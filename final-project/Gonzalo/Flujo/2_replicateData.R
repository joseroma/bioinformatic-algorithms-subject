#Aquí vamos a replicar los datos de supervivencia

load(file="datos/survival_plus_expression.RData")
options(warn=-1)
source("0_loadLibraries.R")
loadpkg("ROSE")

args = commandArgs(trailingOnly=TRUE)
make_balance<-TRUE
if(length(args)==0){
  step <- round(as.numeric(max(clinicos.expresion$time, na.rm = TRUE))/140,0)
}else if(length(args)==1){
  if(is.na(as.numeric(args[1]))){
    make_balance<-as.logical(args[1])
    step <- round(as.numeric(max(clinicos.expresion$time, na.rm = TRUE))/140,0)
  }else{
    step<-as.numeric(agrs[1])
  }
  
}else if(length(args)==2){
  make_balance<-as.logical(args[2])
  step <- as.numeric(args[1])
}

replicate.cum <- function(step, data, min.time, max.time){
  data.cum <- data.frame()
  pb<-txtProgressBar(min=0, max =dim(clinicos.expresion)[1], style = 3 )
  for( i in 1:dim(clinicos.expresion)[1]){
    
    linea <- data[i,]
    for( j in seq(min.time, max.time, by=step)){
      lin <- linea
      if(as.numeric(linea$time) >j){
          lin$time <- j
          lin$status <- 0
      }else{
        if(as.numeric(linea$status)==2){
          lin$time <- j
          lin$status <- 1
        }
      }
      data.cum <- rbind(data.cum, lin)
    }
    setTxtProgressBar(pb,i)
  }
  return(data.cum)
}


undersample_ds <- function(x, classCol, nsamples_class){
  for (i in 1:length(unique(x[, classCol]))){
    class.i <- unique(x[, classCol])[i]
    if((sum(x[, classCol] == class.i) - nsamples_class) != 0){
      x <- x[-sample(which(x[, classCol] == class.i), 
                     sum(x[, classCol] == class.i) - nsamples_class), ]
    }
  }
  return(x)
}

load("datos/geneSignificativos.RData")

genes<-clinicos.expresion[,colnames(clinicos.expresion) %in% genes.mas.significativos]

list.var<-c("time", "status","Gender", "ERstatus", "PRstatus", "HER2Finalstatus", "Tumor", "Node", "AJCCStage", "Metastasis", "PAM50mRNA")
variab<-clinicos.expresion[,colnames(clinicos.expresion) %in% list.var]
for (i in list.var[list.var %in% c("Gender","status", "ERstatus", "PRstatus", "HER2Finalstatus", "Tumor", "Node", "AJCCStage", "Metastasis", "PAM50mRNA")]) {
  variab[,i]<-as.factor(variab[,i])
}

df<-cbind(variab, genes)


rep.data <- replicate.cum(step = step, df, 
                            min.time = as.numeric(min(clinicos.expresion$time, na.rm = TRUE)),
                            max.time = as.numeric(max(clinicos.expresion$time, na.rm = TRUE)))


rep.data.balanced<-data.frame()
if(make_balance==TRUE){
rep.data.balanced <- undersample_ds(rep.data, "status", table(rep.data$status)[[2]])
}

save(rep.data, rep.data.balanced, file="datos/replicatedData.RData")
