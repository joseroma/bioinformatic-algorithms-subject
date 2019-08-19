library("readxl")
library("RTCGAToolbox")
library("tibble")
library("dplyr")
library("limma")
library("edgeR")
library("calibrate")

brcaData <- getFirehoseData(dataset="BRCA", runDate="20160128",gistic2Date="20160128",forceDownload=F, clinical =FALSE, RNASeq2GeneNorm  =TRUE)
brca_rnaseq <- getData(brcaData,type = "RNASeq2GeneNorm")
brca_rnaseq.tumour <- brca_rnaseq[, which(as.numeric(substr(colnames(brca_rnaseq), 14,15)) < 10)]
colnames(brca_rnaseq.tumour) <- substr(colnames(brca_rnaseq.tumour), 1,12)
brca_rnaseq.tumour <- brca_rnaseq.tumour[, !duplicated(colnames(brca_rnaseq.tumour))]
temp <- tempfile()
download.file("https://media.nature.com/original/nature-assets/nature/journal/v490/n7418/extref/nature11412-s2.zip",temp)
unzip(temp)
sample_data <- read_excel("nature11412-s2/Supplementary Tables 1-4.xls", sheet = 1, skip = 1)
unlink("nature11412-s2",recursive = T,force = T)
rm(temp)
unlink("20160128*",force = T)

tnbc_samples <- sample_data %>% dplyr::filter(`ER Status` == "Negative" & `PR Status` == "Negative" & `HER2 Final Status` == "Negative" & `PAM50 mRNA` != "Luminal A")
tnbc_barcodes <- tnbc_samples$`Complete TCGA ID`

luminal_samples <- sample_data %>% dplyr::filter(`PAM50 mRNA` == "Luminal A")
luminal_barcodes <- luminal_samples$`Complete TCGA ID`

basal_samples <- sample_data %>% dplyr::filter(`PAM50 mRNA` == "Basal-like")
basal_barcodes <- basal_samples$`Complete TCGA ID`

brca_rnaseq.tnbc <- brca_rnaseq.tumour[, which(colnames(brca_rnaseq.tumour) %in% tnbc_barcodes)]

brca_rnaseq.luminal <- brca_rnaseq.tumour[, which(colnames(brca_rnaseq.tumour) %in% luminal_barcodes)]

brca_rnaseq.basal <- brca_rnaseq.tumour[, which(colnames(brca_rnaseq.tumour) %in% basal_barcodes)]

d1 = brca_rnaseq.tnbc
d2 = brca_rnaseq.luminal

rnaseq.for.de <- cbind(d1, d2)
counts = rnaseq.for.de[apply(rnaseq.for.de,1,function(x) sum(x==0))<ncol(rnaseq.for.de)*0.8,]

df.l <- data_frame("sample" = colnames(d1), "status" = rep(0, length(colnames(d1))) )
df.t <- data_frame("sample" = colnames(d2), "status" = rep(1, length(colnames(d2))) )
df <- rbind(df.t,df.l)
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

menor <- diff.exp.df[diff.exp.df$logFC<(-2),]
mayor <- diff.exp.df[diff.exp.df$logFC>2,]
sobreexp <- rbind(menor,mayor)
sobreexp <- sobreexp[ sobreexp$adj.P.Val<0.01,]

genes.model <- counts[sobreexp$gene.name,]
genes.model <- t(genes.model)
genes.model<- data.frame(genes.model)

genes.model$`Complete TCGA ID` <- rownames(genes.model)
compact<- sample_data[c(sample_data$`Complete TCGA ID`) %in% genes.model$`Complete TCGA ID`,]
join <- inner_join(compact, genes.model)

clinicos.completos <- sample_data
clinicos.reducidos <- compact
clinicos.expresion <- join