options(warn=0)
source("0_loadLibraries.R")
loadpkg("limma") # Differential gene expression analysis (DEA)
loadpkg("edgeR")
loadpkg("calibrate") # To label the volcano plot
loadpkg("dplyr")
loadpkg("clusterProfiler")
loadpkg("pathview")
loadpkg("ggplot2")
loadpkg("survival")
loadpkg("survminer")
loadpkg("FSelector")

if(file.exists("datos/expressionData.RData")){
  
  load( file="datos/expressionData.RData")
  cat("El lfc para los datos descargados es ", lfc," y el p-valor es de ", pval, "\n")
  
  summarize_cp = function(res, comparison) {
    summaries = data.frame()
    for (ont in names(res)) {
      ontsum = summary(res[[ont]])
      ontsum$ont = ont
      summaries = rbind(summaries, ontsum)
    }
    summaries$comparison = comparison
    return(summaries)
  }
  
  enrich_cp = function(res, comparison, type="over") {
    res = res %>% data.frame()  %>% left_join(entrezsymbol, by = "hgnc_symbol") %>% filter(!is.na(entrezgene))
    # universe = brcaData@GISTIC@AllByGene$Gene.Symbol
    if(type=="all"){
      res <- res %>% filter(abs(logFC) > lfc & adj.P.Val < pval) # lfc and pval threshold defined above in the volcano plot
      genes = res$entrezgene
      
      mf = enrichGO(genes, OrgDb = orgdb, ont = "MF", pAdjustMethod = "BH",
                    qvalueCutoff = 1, pvalueCutoff = 1)
      cc = enrichGO(genes,  OrgDb = orgdb, ont = "CC", pAdjustMethod = "BH",
                    qvalueCutoff = 1, pvalueCutoff = 1)
      bp = enrichGO(genes,  OrgDb = orgdb, ont = "BP", pAdjustMethod = "BH",
                    qvalueCutoff = 1, pvalueCutoff = 1)
      kg = enrichKEGG(gene = genes, organism = keggname, pvalueCutoff = 1,
                      qvalueCutoff = 1, pAdjustMethod = "BH")
      all = list(mf = mf, cc = cc, bp = bp, kg = kg)
      all[["summary"]] = summarize_cp(all, comparison)
      return(all)
    }
    if(type=="over"){
      res.over <- res %>% filter(logFC > lfc  & adj.P.Val < pval)
      genes = res.over$entrezgene
      mf = enrichGO(genes, OrgDb = orgdb, ont = "MF", pAdjustMethod = "BH",
                    qvalueCutoff = 1, pvalueCutoff = 1)
      cc = enrichGO(genes,  OrgDb = orgdb, ont = "CC", pAdjustMethod = "BH",
                    qvalueCutoff = 1, pvalueCutoff = 1)
      bp = enrichGO(genes,  OrgDb = orgdb, ont = "BP", pAdjustMethod = "BH",
                    qvalueCutoff = 1, pvalueCutoff = 1)
      kg = enrichKEGG(gene = genes, organism = keggname, pvalueCutoff = 1,
                      qvalueCutoff = 1, pAdjustMethod = "BH")
      all = list(mf = mf, cc = cc, bp = bp, kg = kg)
      all[["summary"]] = summarize_cp(all, comparison)
      return(all)
    }
    
    if(type=="under"){
      res.under <- res %>% filter(logFC < -lfc & adj.P.Val < pval)
      genes = res.under$entrezgene
      mf = enrichGO(genes, OrgDb = orgdb, ont = "MF", pAdjustMethod = "BH",
                    qvalueCutoff = 1, pvalueCutoff = 1)
      cc = enrichGO(genes,  OrgDb = orgdb, ont = "CC", pAdjustMethod = "BH",
                    qvalueCutoff = 1, pvalueCutoff = 1)
      bp = enrichGO(genes,  OrgDb = orgdb, ont = "BP", pAdjustMethod = "BH",
                    qvalueCutoff = 1, pvalueCutoff = 1)
      kg = enrichKEGG(gene = genes, organism = keggname, pvalueCutoff = 1,
                      qvalueCutoff = 1, pAdjustMethod = "BH")
      all = list(mf = mf, cc = cc, bp = bp, kg = kg)
      all[["summary"]] = summarize_cp(all, comparison)
      return(all)
    }
  }
  
  convert_enriched_ids = function(res, entrezsymbol) {
    res = res %>% mutate(geneID = strsplit(as.character(geneID), "/")) %>% tidyr::unnest(geneID) %>% 
      left_join(entrezsymbol, by = c(geneID = "entrezgene")) %>% group_by(ID, 
                                                                          Description, GeneRatio, BgRatio, pvalue, p.adjust, qvalue, Count, ont, 
                                                                          comparison) %>% summarise(geneID = paste(geneID, collapse = "/"), symbol = paste(hgnc_symbol, 
                                                                                                                                                           collapse = "/"))
    return(res)
  }
  
  
  orgdb = "org.Hs.eg.db"
  biomart_dataset = "hsapiens_gene_ensembl"
  keggname = "hsa"
  mart = biomaRt::useMart(biomart = "ensembl", dataset = biomart_dataset)
  entrezsymbol = biomaRt::getBM(attributes = c("entrezgene", "hgnc_symbol"), mart = mart)
  entrezsymbol$entrezgene = as.character(entrezsymbol$entrezgene)
  res = diff.exp.df
  names(res) <- c("logFC", "AveExpr", "t", "P.Value", "adj.P.Val", "B", "hgnc_symbol")
  enrich_rs = enrich_cp(res, "TNBC/LumA", type="all")
  enrich_summary = enrich_rs$summary %>% arrange(p.adjust)
  enrich_summary = convert_enriched_ids(enrich_summary,entrezsymbol = entrezsymbol) %>% arrange(p.adjust)
  write.csv(enrich_summary, "datos/tnbc_luma_enrichment.csv")
  
  ggsave(filename = "results/enrichment/tnbc_luma_enrichment_GO.jpg",dotplot(enrich_rs$kg, x="count", showCategory=10), height = 10, width = 8)
  
  
  #Ahora vamos a filtrar por aquellos genes que parecen mas importantes en el modelo de cox
  
  load("datos/survival_plus_expression.RData")
  
  # Sacamos mejores variables exhaustive
  ranking.var<-function(df,indep.var){
    indep.var<-"Surv(time,status)"
    names.col<- colnames(df)[!colnames(df) %in% c("Node-Coded", "time", "status", "VitalStatus")]
    pvalue<-c()
    formula<-c()
    for (i in names.col) {
      ec.final<-as.formula(c("Surv(time,status) ~", i))
      cox.data.norm<-coxph(ec.final, data = df)
      cox.zph<-cox.zph(cox.data.norm)
      #Comprobamos que la variable cumplen la proporcionalidad de riesgos
      if(cox.zph$table[nrow(cox.zph$table),][3]>0.05){
        
        formula<-c(formula,  i)
        pvalue<-c(pvalue,summary(cox.data.norm)$logtest["pvalue"])
        
      }
    }
    dat.res<-data.frame(variable=formula, pvalue=pvalue)
    dat.res<-dat.res[order(dat.res$pvalue),]
    return(dat.res$variable)
  }
  clinicos.expresion$status<-as.numeric(clinicos.expresion$status)
  clinicos.expresion$time<-as.numeric(clinicos.expresion$time)
  for (i in colnames(clinicos.expresion)[17:ncol(clinicos.expresion)]) {
    clinicos.expresion[,i]<-as.numeric(clinicos.expresion[,i])
  }
  args = commandArgs(trailingOnly=TRUE)
  if(args[1]=="NONE"){
    variables<-colnames(clinicos.expresion)
  }else if(args[1]=="RFI"){
    
    clinicos.expresion$CompleteTCGAID<-NULL
    clinicos.expresion$AgeatInitialPathologicDiagnosis<-as.numeric(clinicos.expresion$AgeatInitialPathologicDiagnosis)
    formula<-as.simple.formula(paste(colnames(clinicos.expresion[!colnames(clinicos.expresion) %in% c("Node-Coded", "CompleteTCGAID","VitalStatus")]),collapse = "+"),"status")
    weights <- random.forest.importance(formula, clinicos.expresion)
    variables<-cutoff.k(weigths,50)
  #AQUI ME HE QUEDADO AÑADIENDO UN FEATURE SELECTION (EN EL PORTATIL TENGO EJEMPLO)
    }else{
    variables<-ranking.var(clinicos.expresion, "Surv(time,status)")
  }
  
  
  #Ahora nos quedamos solo con los genes (quitamos datos clinicos) y con los genes que cumplen la proporcionalidad de riesgos
  variables<-as.vector(variables)
  select.var<-variables[variables %in% rownames(res)][1:30]
  genes.mas.significativos<-select.var
  save(genes.mas.significativos, file="datos/geneSignificativos.RData")
  res1 <- diff.exp.df[rownames(diff.exp.df) %in% select.var, ]
  names(res1) <- c("logFC", "AveExpr", "t", "P.Value", "adj.P.Val", "B", "hgnc_symbol")
  enrich_rs_1 = enrich_cp(res1, "TNBC/LumA", type="all")
  enrich_summary_1 = enrich_rs_1$summary %>% arrange(p.adjust)
  enrich_summary_1 = convert_enriched_ids(enrich_summary_1,entrezsymbol = entrezsymbol) %>% arrange(p.adjust)
  write.csv(enrich_summary_1, "datos/REDUCED_tnbc_luma_enrichment.csv")
  ggsave(filename = "results/enrichment/reduced_tnbc_luma_enrichment_GO.jpg",dotplot(enrich_rs_1$kg, x="count", showCategory=10), height = 10, width = 8)
  
  
  #######   KEGGG
  
  
  viewkeggpath = function(path, enrichment){
    # Get the genes in path from the enrichment summary
    genesymbols=strsplit(as.character(enrich_summary[which(enrichment$ID ==path),12]), split = "/")[[1]]
    geneids=strsplit(as.character(enrich_summary[which(enrichment$ID ==path),11]), split = "/")[[1]]
    ll = data.frame(geneid=geneids, gene.name=genesymbols)
    # dataframe with the genes and the log2FC
    kk = filter(diff.exp.df, gene.name %in% genesymbols) %>% dplyr::select(gene.name, logFC)
    kk = kk %>%right_join(ll,by="gene.name")
    gene.vector <- kk$logFC
    names(gene.vector) = kk$geneid
    pathview(gene.data  = gene.vector ,pathway.id = path,species = "hsa",limit = list(gene=max(abs(gene.vector)), cpd=1))
  }
  
  viewkeggpath(path="hsa04915", enrichment = enrich_summary)
  system('mv hsa04915.xml ./results/enrichment/hsa04915.xml')
  system('mv hsa04915.png ./results/enrichment/hsa04915_KEGG.png')
  system('mv hsa04915.pathview.png ./results/enrichment/hsa04915_KEGG.pathview.png')
  viewkeggpath(path="hsa04915", enrichment = enrich_summary_1)
  system('mv hsa04915.xml ./results/enrichment/reduced_hsa04915.xml')
  system('mv hsa04915.png ./results/enrichment/reduced_hsa04915_KEGG.png')
  system('mv hsa04915.pathview.png ./results/enrichment/reduced_hsa04915_KEEG.pathview.png')
  
}else{
  cat("No se puede generar el volcano plot sin descargar los datos \n")
}

