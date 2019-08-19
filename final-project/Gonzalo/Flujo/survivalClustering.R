load("datos/survival_plus_expression.RData")
options(warn=0)
source("0_loadLibraries.R")
colnames(clinicos.expresion)
loadpkg("survival")
loadpkg("survminer")
loadpkg("hier.part")
loadpkg("FSelector")
loadpkg("cluster")
loadpkg("pec")
loadpkg("factoextra")
loadpkg("kableExtra")
loadpkg("magick")

df<-clinicos.expresion

#Estratificamos las variables necesarias
df$age<-cut(as.numeric(df$`AgeatInitialPathologicDiagnosis`), breaks = c(0,35,60,100))
df$time<-as.numeric(df$time)
df$status<-as.numeric(df$status)
df$AJCC<-as.factor(df$AJCCStage)

# Vemos las estratificaciones de las variables de interes
fit.age<-survfit(Surv(time, status) ~ age, data=df) 
fit.node<-survfit(Surv(time, status) ~ Node, data=df) 
fit.meta<-survfit(Surv(time, status) ~ Metastasis, data=df)
fit.ajcc<-survfit(Surv(time, status) ~ AJCC, data=df) 


#Ploteamos los resultados

splots<-list()

splots[[1]] <- ggsurvplot(fit.age, censor= TRUE,  main="Estratificado: edad", pval = TRUE)
splots[[2]] <- ggsurvplot(fit.node, censor= TRUE,  main="Estratificado: nodos", pval = TRUE)
splots[[3]] <- ggsurvplot(fit.meta, conf.int = TRUE, censor= TRUE,  main="Estratificado: Metastasis", pval = TRUE)
splots[[4]] <- ggsurvplot(fit.ajcc,  censor= TRUE,  main="Estratificado: Metastasis", pval = TRUE)

ggsave("results/survival/overallSurvivalSomeStrats.png", arrange_ggsurvplots(splots, print = TRUE, ncol =2, nrow = 2),
       device = 'png', width = 13, height = 10)



# Generar 2 grupos de pacientes Hierarchical clustering


# Se obtiene el dendrograma de hierarchical clustering para elegir el número de

#expresion <- scale(df, center = TRUE, scale = TRUE)
matriz_distancias <- dist(x = df, method = "manhattan")
hc_completo <- hclust(d = matriz_distancias, method = "complete")
hc_average  <- hclust(d = matriz_distancias, method = "average")
hc_single   <- hclust(d = matriz_distancias, method = "single")

complet<-c()
average<-c()
single<-c()
for (i in 2:15) {
  complet<-c(complet, mean(silhouette(cutree(hc_completo, k=i), (matriz_distancias))[,"sil_width"]))
  average<-c(average, mean(silhouette(cutree(hc_average, k=i), (matriz_distancias))[,"sil_width"]))
  single<-c(single, mean(silhouette(cutree(hc_single, k=i), (matriz_distancias))[,"sil_width"]))
}



kable(rbind(complet=complet, average=average, single=single), format = "latex")%>%
  kable_styling() %>%
  save_kable(file="results/ML/silhouettes.jpg")


jpeg("results/survival/clusterOptions.jpg")
par(mfrow = c(3, 1))
plot(hc_completo, ylab = "", xlab = "", sub = "",
     main = "Linkage completo", cex = 0.8)
plot(hc_average, ylab = "", xlab = "", sub = "",
     main = "Linkage average", cex = 0.8)
plot(hc_single, ylab = "", xlab = "", sub = "",
     main = "Linkage single", cex = 0.8)
dev.off()


clusters <- cutree(tree = hc_completo, k = 2)

group.1<-df[clusters,]
group.2<-df[-clusters,]


# Vemos las estratificaciones de las variables de interes
fit.age.1<-survfit(Surv(time, status) ~ age, data=group.1) 
fit.node.1<-survfit(Surv(time, status) ~ Node, data=group.1) 
fit.ajcc.1<-survfit(Surv(time, status) ~ AJCC, data=group.1) 

fit.age.2<-survfit(Surv(time, status) ~ age, data=group.2) 
fit.node.2<-survfit(Surv(time, status) ~ Node, data=group.2) 
fit.ajcc.2<-survfit(Surv(time, status) ~ AJCC, data=group.2) 




splots1<-list()

splots1[[1]] <- ggsurvplot(fit.age.1, censor= TRUE,  main="Estratificado: edad", pval = TRUE)
splots1[[2]] <- ggsurvplot(fit.node.1, censor= TRUE,  main="Estratificado: nodos", pval = TRUE)
splots1[[3]] <- ggsurvplot(fit.ajcc.1,  censor= TRUE,  main="Estratificado: AJCC", pval = TRUE)
splots1[[4]] <- ggsurvplot(fit.age.2, censor= TRUE,  main="Estratificado: edad", pval = TRUE)
splots1[[5]] <- ggsurvplot(fit.node.2, censor= TRUE,  main="Estratificado: nodos", pval = TRUE)
splots1[[6]] <- ggsurvplot(fit.ajcc.2,  censor= TRUE,  main="Estratificado: AJCC", pval = TRUE)

ggsave("results/survival/overallSurvivalSomeStratsClust.png", arrange_ggsurvplots(splots1, print = TRUE, ncol =2, nrow = 3),
       device = 'png', width = 13, height = 10)


