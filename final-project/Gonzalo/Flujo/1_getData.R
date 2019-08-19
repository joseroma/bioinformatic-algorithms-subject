options(warn=0)
source("0_loadLibraries.R")
loadpkg("readxl")
loadpkg("RTCGAToolbox")
loadpkg("tibble")
loadpkg("dplyr")
loadpkg("limma")
loadpkg("edgeR")
loadpkg("calibrate")

args = commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  pval<-0.01
  lfc<-4
}else{
  pval<-as.numeric(args[2])
  lfc<-as.numeric(args[1])
}

######################

# En vez de esto me descargo un monton de datos clínicos de TCGA
temp <- tempfile()
download.file("https://media.nature.com/original/nature-assets/nature/journal/v490/n7418/extref/nature11412-s2.zip",temp)
unzip(temp)
sample_data <- read_excel("nature11412-s2/Supplementary Tables 1-4.xls", sheet = 1, skip = 1)
unlink("nature11412-s2",recursive = T,force = T)
rm(temp)
unlink("20160128*",force = T)


sample_data_1<-sample_data
#Quitamos columnas
sample_data<-sample_data[,!names(sample_data) %in% c("Days to Date of Last Contact",
                                                     "Survival Data Form","Tumor--T1 Coded",
                                                     "Metastasis-Coded", "Converted Stage",
                                                     "Days to date of Death",
                                                     "SigClust Unsupervised mRNA"         
                                                     ,"SigClust Intrinsic mRNA"    ,         "miRNA Clusters"      ,               
                                                     "methylation Clusters"  ,              "RPPA Clusters" ,                     
                                                     "CN Clusters"                ,         "Integrated Clusters (with PAM50)" ,  
                                                     "Integrated Clusters (no exp)"  ,      "Integrated Clusters (unsup exp)"  )]

sample_data<-as.data.frame(sample_data[!(apply(sample_data, 1, function(y) any(y == "NA"))),])

######################

brcaData <- getFirehoseData(dataset="BRCA", runDate="20160128",gistic2Date="20160128",forceDownload=F, clinical =TRUE, RNASeq2GeneNorm  =TRUE)
brca_rnaseq <- getData(brcaData,type = "RNASeq2GeneNorm")
brca_rnaseq.tumour <- brca_rnaseq[, which(as.numeric(substr(colnames(brca_rnaseq), 14,15)) < 10)]
colnames(brca_rnaseq.tumour) <- substr(colnames(brca_rnaseq.tumour), 1,12)
brca_rnaseq.tumour <- brca_rnaseq.tumour[, !duplicated(colnames(brca_rnaseq.tumour))]




tnbc_samples <- sample_data %>% dplyr::filter(`ER Status` == "Negative" & `PR Status` == "Negative" & `HER2 Final Status` == "Negative" & `PAM50 mRNA` != "Luminal A")
tnbc_barcodes <- tnbc_samples$`Complete TCGA ID`
brca_rnaseq.tnbc <- brca_rnaseq.tumour[, which(colnames(brca_rnaseq.tumour) %in% tnbc_barcodes)]

luminal_samples <- sample_data %>% dplyr::filter(`PAM50 mRNA` == "Luminal A")
luminal_barcodes <- luminal_samples$`Complete TCGA ID`
brca_rnaseq.luminal <- brca_rnaseq.tumour[, which(colnames(brca_rnaseq.tumour) %in% luminal_barcodes)]

rnaseq.for.de <- cbind(brca_rnaseq.luminal, brca_rnaseq.tnbc)
counts = rnaseq.for.de[apply(rnaseq.for.de,1,function(x) sum(x==0))<ncol(rnaseq.for.de)*0.8,]

df.l <- data_frame("sample" = colnames(brca_rnaseq.luminal), "status" = rep(0, length(colnames(brca_rnaseq.luminal))) )
df.t <- data_frame("sample" = colnames(brca_rnaseq.tnbc), "status" = rep(1, length(colnames(brca_rnaseq.tnbc))) )
df <- rbind(df.l,df.t)
design <- model.matrix(~ status, data = df)

dge <- DGEList(counts=counts)
A <- rowSums(dge$counts)
isexpr <- A > 100 # Keeping genes with total counts more than 100.
dge <- calcNormFactors(dge)
v <- voom(dge[isexpr,], design, plot=FALSE)

fit <- lmFit(v, design)
fit <- eBayes(fit)



diff.exp.df <- topTable(fit, coef = "status", n = Inf, sort = "p", p = 0.01) # Positive log-fold-changes mean higher expression in d1

diff.exp.df$gene.name <- rownames(diff.exp.df)

save(diff.exp.df, pval, lfc, file="datos/expressionData.RData")

menor <- diff.exp.df[diff.exp.df$logFC<(-lfc),]
mayor <- diff.exp.df[diff.exp.df$logFC>lfc,]
sobreexp <- rbind(menor,mayor)
sobreexp <- sobreexp[ sobreexp$adj.P.Val<pval,]

genes.model <- counts[sobreexp$gene.name,]
genes.model <- t(genes.model)
genes.model<- data.frame(genes.model)


# aqui analisis funcional

genes.model$`Complete TCGA ID` <- rownames(genes.model)
compact<- sample_data[c(sample_data$`Complete TCGA ID`) %in% genes.model$`Complete TCGA ID`,]
join <- inner_join(compact, genes.model)

clinicos.sin.quitar.vacios<- sample_data_1
clinicos.completos.sin.exp <- sample_data
clinicos.reducidos <- compact
clinicos.expresion <- join

cat("Los datos originales tienen unas dimensiones de: ", dim(clinicos.sin.quitar.vacios)[1],"x",dim(clinicos.sin.quitar.vacios)[2],
    "\nLos datos filtrados tienen unas dimensiones de: ", dim(clinicos.completos.sin.exp)[1],"x",dim(clinicos.completos.sin.exp)[2],
    "\nLos datos reducidos tienen unas dimensiones de: ", dim(clinicos.reducidos)[1],"x",dim(clinicos.reducidos)[2],
    "\nLos datos finales tienen unas dimensiones de: ", dim(clinicos.expresion)[1],"x",dim(clinicos.expresion)[2], "\n"
    )

colnames(clinicos.expresion)[which(names(clinicos.expresion) == "Age at Initial Pathologic Diagnosis")] <- "AgeatInitialPathologicDiagnosis"
colnames(clinicos.expresion)[which(names(clinicos.expresion) == "OS Time")] <- "time"
colnames(clinicos.expresion)[which(names(clinicos.expresion) == "OS event")] <- "status"
colnames(clinicos.expresion)[which(names(clinicos.expresion) == "Vital Status")] <- "VitalStatus"
colnames(clinicos.expresion)[which(names(clinicos.expresion) == "ER Status")] <- "ERstatus"
colnames(clinicos.expresion)[which(names(clinicos.expresion) == "PR Status")] <- "PRstatus"
colnames(clinicos.expresion)[which(names(clinicos.expresion) == "AJCC Stage")] <- "AJCCStage"
colnames(clinicos.expresion)[which(names(clinicos.expresion) == "Complete TCGA ID")] <- "CompleteTCGAID"
colnames(clinicos.expresion)[which(names(clinicos.expresion) == "PAM50 mRNA")] <- "PAM50mRNA"
colnames(clinicos.expresion)[which(names(clinicos.expresion) == "HER2 Final Status")] <- "HER2Finalstatus"

save(clinicos.sin.quitar.vacios,clinicos.completos.sin.exp,clinicos.reducidos,clinicos.expresion, file="datos/survival_plus_expression.RData")
