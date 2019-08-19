options(warn=0)
source("0_loadLibraries.R")
loadpkg("limma") # Differential gene expression analysis (DEA)
loadpkg("edgeR")
loadpkg("calibrate") # To label the volcano plot
loadpkg("dplyr")


if(file.exists("datos/expressionData.RData")){
  load( file="datos/expressionData.RData")
  cat("El lfc para los datos descargados es ", lfc," y el p-valor es de ", pval, "\n")
  argo = commandArgs(trailingOnly=TRUE)
  if(length(argo)==0){
    cat("\nSe mantienen los valores con los que se descargaron los datos.\n")
  }else{
    lfc = as.numeric(argo[1])
    pval = as.numeric(argo[2])
    cat("\nEl lfc que se muestra es ", lfc," y el p-valor es de ", pval, "\n")
  }
  
  tab = data.frame(logFC = diff.exp.df$logFC, negLogPval= -log10(diff.exp.df$adj.P.Val))
  tab2 = data.frame(logFC = diff.exp.df$logFC, negLogPval= -log10(diff.exp.df$adj.P.Val), Gene=diff.exp.df$gene.name)
  
  write.csv(filter(tab2, abs(logFC) > lfc & negLogPval > -log10(pval)), "datos/dea.csv") # write output
  
  pdf(file = "results/volcano.pdf", width = 9, height = 4.5)
  par(mar = c(5, 4, 4, 5))
  plot(tab, pch = 16, cex = 0.6, xlab = expression(log[2]~fold~change), ylab = expression(-log[10]~pvalue))
  #signGenes = (abs(tab$logFC) > lfc & tab$negLogPval > -log10(pval))
  points(tab[(abs(tab$logFC) > lfc), ], pch = 16, cex = 0.8, col = "orange") 
  points(tab[(tab$negLogPval > -log10(pval)), ], pch = 16, cex = 0.8, col = "green") 
  points(tab[(abs(tab$logFC) > lfc & tab$negLogPval > -log10(pval)), ], pch = 16, cex = 0.8, col = "red") 
  abline(h = -log10(pval), col = "green3", lty = 2) 
  abline(v = c(-lfc, lfc), col = "blue", lty = 2) 
  mtext(paste("pval =", pval), side = 4, at = -log10(pval), cex = 0.8, line = 0.5, las = 1) 
  mtext(c(paste("-", lfc, "fold"), paste("+", lfc, "fold")), side = 3, at = c(-lfc, lfc), cex = 0.8, line = 0.5)
  with(subset(tab2, negLogPval > -log10(pval) & abs(logFC)>lfc), textxy(logFC, negLogPval, labs=Gene, cex=.4))
  
}else{
  cat("No se puede generar el volcano plot sin descargar los datos \n")
}

