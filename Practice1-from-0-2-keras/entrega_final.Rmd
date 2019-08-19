---
title: "Práctica 1: Analisis de parametros para redes neuronales profundas en KERAS"
author: "Jose Rodriguez Maldonado"
date: "Octubre de 2018"
output:
  html_document:
    toc: true
    toc_float: true
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(knitr)
library(kableExtra)
```
**NOTA IMPORTANTE**: Algunas gráficas pueden no haberse cargado bien. Si refresca la página en el punto que te encuentres, deberías mantenerte en el mismo sitio y la gráfica te debería aparecerte correctamente. Un saludo.


#Introducción

Esta primera práctica se va a dividir en dos partes. 

Para la primera parte, se probaran diferentes combinaciones de parámetros y  cómo afectan al modelo. 

En la segunda parte, se hará lo mismo para un conjunto de variables seleccionadas del set de datos **Breast Cancer**. Se harán diferentes prubas sobre las posibles capas que podemos llegar a introducir y los parámetros que podemos probar. Finalmente, se compararan los resultados obtenidos por el modelo de DeepLearning con los obtenidos para los diferentes algoritmos de Machine Learning del año pasado. 

#MNIST

##¿Qué vamos a hacer?

Para esta práctica se probarán las siguientes variables sobre el modelo de *Deep Learning*.

- **Numero de capas interas** del modelo 
- **Numero de neuronas** para cada capa del modelo
- Valor de **dropout**

##Codigo

###Recopilar datos

Esta parte es igual que la que podemos encontrar en el tutorial sobre cómo utilizar keras visto en clase. 

```{r eval=FALSE, results='hide', message=FALSE, warning=FALSE}
library(keras)
mnist <- dataset_mnist()  
x_train <- mnist$train$x
y_train <- mnist$train$y 
x_test <- mnist$test$x
y_test <- mnist$test$y

x_train<-array_reshape(x_train, c(nrow(x_train),784))
x_test<-array_reshape(x_test, c(nrow(x_test),784))

x_train <- x_train / 255
x_test <- x_test / 255

y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)
```

###Numero de capas y de neuronas

Vamos a probar los siguientes valores:
```{r results='hide', message=FALSE, warning=FALSE}
second_layer = c(1400, 700, 300, 50, 40, 30 , 20 ,15)
third_layer = c(784, 400, 130, 30, 25 , 20 , 15 , 10)
fourth_layer = c(784, 600, 400, 200, 150 , 80 , 60 , 20)
fifth_layer = c(256, 150, 80, 30, 25 , 20 , 15 , 10)
sixth_layer = c(150, 160, 100, 70, 60, 50, 40, 30 )
df <- data.frame(  l2=second_layer, l3=third_layer, l4=fourth_layer, l5= fifth_layer , l6=sixth_layer)

dropout = c(0.3,0.3,0.3,0.3,0.3, 0.3, 0.3, 0.3)
nnet_comb<-c(2,5,7)
```

Estos datos estan ajustados para poder ver:


- Que ocurre si las neuronas de una red son mayores que el número de entradas.
- ¿Entrena la red mejor disminuyendo el numero de neuronas por capa gradualmente?
- ¿Cómo afecta aumentar o disminuir el número de capas?

En total tenemos 15 combinaciones de parámetros: 5 configuraciones de neuronas y 3 número de capas. En mi ordenador el tiempo que puede tardar en probar todas estas combinaciones oscila entre los 24-25 minutos.

Para probar estos parámetros he utilizado las siguientes funciones:
```{r echo=FALSE}
load("savedData/MNIST_capas_neuronas.RData")
```

```{r eval=FALSE}
calcula_modelo<-function(x_train=x_train, x_test=x_test, y_train=y_train, y_test=y_test, num_capas=2, vector=c(256,180), drop.out=c(0.4,0.3), activation_func ){
  t <- proc.time() # Inicia el cronÃ³metro
  modelo <- keras_model_sequential()
  i<-1
  while(i<num_capas+1){
    if(i==1){
    modelo %>% 
        layer_dense(units = as.integer(vector[i]), activation = "relu", input_shape = c(784)) %>% 
        layer_dropout(rate = drop.out[[i]]) 
      i<-i+1
    }else{
    modelo %>%
      layer_dense(units = as.integer(vector[i]), activation = "relu", input_shape = c(784)) %>% 
        layer_dropout(rate = drop.out[[i]]) 
      i<-i+1
    }
  }
  
  modelo %>%
    layer_dense(units = 10, activation = activation_func)
  
  modelo %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_rmsprop(),
    metrics = c("accuracy"))
  
  #Repetir solo el FIT
  train_data<-modelo %>% fit(
    x_train, y_train, epochs = 10, batch_size = 128, view_metrics = FALSE, 
    validation_split = 0.2)
  
  var<-modelo %>% evaluate(x_test, y_test,verbose = 0)

  proc.time()-t  
  
  return(c(var$acc,var$loss, train_data$metrics$acc[10], train_data$metrics$loss[10], t["elapsed"]))
}


aplicar_dif_layers_lapply_prep<-function(x_train=x_train, x_test=x_test, y_train=y_train, y_test=y_test,num_comb=c(2,3,4),df=df, j, dropout, activation_func='softmax'){
  resul<-c()
  if(lengths(df)[1] != 1 && lengths(dropout)[1] != 1){
    resul<-"POR PROBLEMAS DE COMP. NO VOT A PROBAR DROPS Y DF A LA VEZ"
  }else{
   
    if(lengths(df)[1] != 1){
      print("NNET COMB")
      resul<-c(unlist(lapply(1:length(num_comb), function(p) calcula_modelo(x_train=x_train, x_test=x_test, y_train=y_train, y_test=y_test,num_comb[p],vector=df[,j], drop.out = dropout, activation_func))))
    }
    if(lengths(dropout)[1] != 1){
      vect_drop<-c()
      print("DROP COMB")
      for(m in 1:length(dropout)){
        vect_drop<-c(vect_drop, dropout[[m]][j])
      }
      resul<-c(unlist(lapply(1:length(num_comb), function(p) calcula_modelo(x_train=x_train, x_test=x_test, y_train=y_train, y_test=y_test,num_comb[p],vector=df, drop.out = vect_drop, activation_func))))
    }
  }
  return(resul)
}

resultado_lapply<-as.matrix(lapply(1:length(df), function(i) aplicar_dif_layers_lapply_prep(x_train=x_train, x_test=x_test, y_train=y_train, y_test=y_test,nnet_comb,df, i, dropout)))

data_frame<-data.frame(t(matrix(unlist(lapply(1:length(df), function(i) resultado_lapply[i])), nrow = 5*length(nnet_comb) )))

nombres_data_frame<-c()
for (e in 1:length(nnet_comb)) {
  nombres_data_frame <- c(nombres_data_frame, paste("ACC",e, sep=""))
  nombres_data_frame <- c(nombres_data_frame, paste("LOSS",e, sep=""))
  nombres_data_frame <- c(nombres_data_frame, paste("TRAIN_ACC",e, sep=""))
  nombres_data_frame <- c(nombres_data_frame, paste("TRAIN_LOSS",e, sep=""))
  nombres_data_frame <- c(nombres_data_frame, paste("TIEMPO",e, sep=""))
}
colnames(data_frame)<-nombres_data_frame


```

##Resultados
Se representan los datos para tratar de ver con mas claridad los resultados obtenidos.


###Capas y combinaciones de neuronas
```{r echo=FALSE, results='hide', message=FALSE, warning=FALSE}
load(file="savedData/MNIST_plotly_data.RData")
library("ggplot2")
library("plotly")
Sys.setenv("plotly_username"="pedroroma")
Sys.setenv("plotly_api_key"="uEVDAEsRx9W89BEdeUff")
```

```{r results='hide', message=FALSE, warning=FALSE}
axx <- list(
  gridcolor='rgb(255, 255, 255)',
  zerolinecolor='rgb(255, 255, 255)',
  showbackground=TRUE,
  backgroundcolor='rgb(230, 230,230)'
)

plot_acc_3 <- plot_ly(showscale = FALSE, scene='scene100') %>%
  add_surface(z = ~acc_values_3, opacity=0.98) %>%
  add_surface(z = ~acc_values_val_3, opacity=0.85, colorscale = list(c(0,"rgb(255,112,183)"),c(1,"rgb(128,0,64)")))

plot_loss_3 <- plot_ly(showscale = FALSE, scene='scene200') %>%
  add_surface(z = ~loss_values_3, opacity=0.98) %>%
  add_surface(z = ~loss_values_val_3, opacity=0.85, colorscale = list(c(0,"rgb(107,184,214)"),c(1,"rgb(0,90,124)")))

plot_acc_2 <- plot_ly(showscale = FALSE, scene='scene300') %>%
  add_surface(z = ~acc_values_2, opacity=0.98) %>%
  add_surface(z = ~acc_values_val_2, opacity=0.85, colorscale = list(c(0,"rgb(255,112,183)"),c(1,"rgb(128,0,64)")))

plot_loss_2 <- plot_ly(showscale = FALSE, scene='scene400') %>%
  add_surface(z = ~loss_values_2, opacity=0.98) %>%
  add_surface(z = ~loss_values_val_2, opacity=0.85, colorscale = list(c(0,"rgb(107,184,214)"),c(1,"rgb(0,90,124)")))






three_layers<-subplot(plot_acc_3, plot_loss_3)%>%
  layout(title = "3D Subplots ACC & LOSS",
         scene100 = list(domain=list(x=c(0,0.5),y=c(0.25,0.75)),
                      xaxis=axx, yaxis=axx, zaxis=axx,
                      aspectmode='cube'),
         scene200 = list(domain=list(x=c(0.51,1),y=c(0.25,0.75)),
                       xaxis=axx, yaxis=axx, zaxis=axx,
                       aspectmode='cube'))

two_layers<-subplot(plot_acc_2, plot_loss_2)%>%
  layout(title = "3D Subplots ACC & LOSS",
         scene300 = list(domain=list(x=c(0,0.5),y=c(0.25,0.75)),
                      xaxis=axx, yaxis=axx, zaxis=axx,
                      aspectmode='cube'),
         scene400 = list(domain=list(x=c(0.51,1),y=c(0.25,0.75)),
                       xaxis=axx, yaxis=axx, zaxis=axx,
                       aspectmode='cube'))

capas.tres<- api_create(three_layers, filename="3 Layers COMB")
capas.dos<- api_create(two_layers, filename="2 Layers COMB")
```
```{r}
capas.tres

```

En estas gráficas podemos ver de un color naranja-rojo-rosa los valores obtenidos para el train. Mientras que los obtenidos para el test estan de un color Morado-Azul-Verde-Amarillo. A la izquierda podemos ver el ACC y a la derecha el LOSS. En el eje X tenemos las diferentes combinaciones de neuronas que hemos probado. Ej, el valor cero en la gráfica corresponde con el primer numero de neuronas que se ha probado. Mientras que para el eje y tenemos las diferentes combinaciones de neuronas que hemos usado para cada capa. Ej. para y= 0 en x=0 tenemos 2 capas, la primera y una interna que tendrás como valores 1400 y 700 respectivamente. Y finalmente, el eje z se corresponde con los valores de ACC y LOSS para cada caso.

Los resultados empeoran conforme vamos aumentando el número de capas. Especialmente, vamos a tener peores resultados cuando tenemos mas capas con pequeño número de neuronas por capa. Pero no podemos sacar mas información, a si que voy a quitar de la gráfica los resultados de tener 8 capas. Para poder apreciar mas claramente la diferencia entre el resto de resultados.
```{r}
capas.dos
```


- Que ocurre si las neuronas de una red son mayores que el número de entradas.
- ¿Cómo afecta aumentar o disminuir el número de capas?

Para un número de capas de neuronas **bajo** con **alto** numero de neuronas por capa obtenemos mejores resultados. 

Pero para valores **altos** de *neuronas por capa*, la diferencia entre el ACC de entrenamiento y el ACC de test aumenta considerablemente. Esto es indicativo de **overfitting**. 

Este overfitting se va **reduciendo** 

- conforme **aumenta** el número de capas, manteniendo altos numero de neuronas
- conforme **disminuye** número de neuronas por capa . 

Para un bajo número de neuronas por capa se aprecia que el *acc *de **train** *disminye* mucho mas que el *acc* de **test**. Esto podría ser indicativo de **underfitting**. 

Podemos estar ante un caso en el que, por no haber tenido suficiente tiempo para entrenar; el modelo no halla podido aprender los patrones relevantes que necesita de los datos de train.

Aunque para este caso parece ser mas razonable pensar que esta situación se produzca como consecuencia del aumento del LOSS. Se nota como crece conforme aumentamos el número de capas.


- ¿Entrena la red mejor disminuyendo el numero de neuronas por capa gradualmente?

Si comparamos un caso de este tipo tenemos que fijarnos concretamente en Y=1(descenso brusco) y Y=2(descenso gradual).

Para ambos casos se nota que el ACC de train es mucho mayor para bajo numero de capas neuronales. Lo que es indicativo de un ligero **overfitting**, pero este overfitting parece estar provocado por el número de neuronas . Esta diferencia se disminuye conforme aumentamos el número de capas neuronales. Como se ha **comentado ya anteriormente**.

Se nota que para altos numeros de capas neuronales (es cuando se puede apreciar las diferencias en el descenso), ofrece peores resultados un descenso brusco de las capas neuronales, que un descenso gradual. Como nos indican los valores de ACC correspondientes. Esto podría ser debido a que el modelo al tener un mayor número de capas con bajo número de neuronas por capa "mezcla" los patrones que aprende provocando que aprenda peor de los datos. 


###Dropout


Para el dropout vamos a probar las siguientes combinaciones:

```{r}
dropout_1<-seq(0.1, 0.9, by=0.2)
drop1<-c()
drop2<-c()
cont<-0
for(i in 1:length(dropout_1)){
    for(j in 1:length(dropout_1)){
      drop1<-c(drop1,dropout_1[i])
      drop2<-c(drop2,dropout_1[j])
      cont<-cont+1
    }
}
drop_comb<-list(drop1,drop2)

nnet_comb_num = c(784, 600, 400, 200)
nnet_comb<-c(rep(2, length(dropout_1)))
```
He decidido escoger 
Volviendo a ejecutar la función anterior podemos probar las diferentes cofiguraciones.
```{r echo=FALSE}
load(file= "savedData/MNIST_plotly__drop_data.RData")
```

```{r eval=FALSE}
resultado_lapply<-as.matrix(lapply(1:length(df), function(i) aplicar_dif_layers_lapply_prep((x_train=x_train, x_test=x_test, y_train=y_train, y_test=y_test,nnet_comb,df = nnet_comb_num, i, drop_values)))


drop.data.frame<-data.frame(t(matrix(unlist(lapply(1:length(df), function(i) resultado_lapply[i])), nrow = 5*length(nnet_comb) )))

nombres_data_frame<-c()
for (e in 1:length(nnet_comb)) {
  nombres_data_frame <- c(nombres_data_frame, paste("ACC",e, sep=""))
  nombres_data_frame <- c(nombres_data_frame, paste("LOSS",e, sep=""))
  nombres_data_frame <- c(nombres_data_frame, paste("TRAIN_ACC",e, sep=""))
  nombres_data_frame <- c(nombres_data_frame, paste("TRAIN_LOSS",e, sep=""))
  nombres_data_frame <- c(nombres_data_frame, paste("TIEMPO",e, sep=""))
}
colnames(drop.data.frame)<-nombres_data_frame

```

Pasamos a plotear los resultados obtenidos:

```{r results='hide', message=FALSE, warning=FALSE}

plot_acc <- plot_ly(showscale = FALSE, scene='scene10') %>%
  add_surface(z = ~drop_acc_values, opacity=0.98) %>%
  add_surface(z = ~drop_acc_values_val, opacity=0.85, colorscale = list(c(0,"rgb(255,112,183)"),c(1,"rgb(128,0,64)")))

plot_loss <- plot_ly(showscale = FALSE, scene='scene20') %>%
  add_surface(z = ~drop_loss_values, opacity=0.98) %>%
  add_surface(z = ~drop_loss_values_val, opacity=0.85, colorscale = list(c(0,"rgb(107,184,214)"),c(1,"rgb(0,90,124)")))

drop_scenario<-subplot(plot_acc, plot_loss)%>%
  layout(title = "3D Subplots ACC & LOSS",
         scene10 = list(domain=list(x=c(0,0.5),y=c(0.25,0.75)),
                      xaxis=axx, yaxis=axx, zaxis=axx,
                      aspectmode='cube'),
         scene20 = list(domain=list(x=c(0.51,1),y=c(0.25,0.75)),
                       xaxis=axx, yaxis=axx, zaxis=axx,
                       aspectmode='cube'))

capas.dos <- api_create(drop_scenario, filename="ACC and LOSS DROPOUT")
```

```{r}
capas.dos
```
Al igual que antes podemos encontrar a la izquierda los diferentes valores para el ACC (Rosa-TRAIN, AZUL-Test). Y a la derecha los valores para el LOSS. en 'X' la el dropout usado por la capa incial y en 'y' el dropout usado por la primera capa interna.

Se puede apreciar que para el dropout de la capa incial apenas vamos a notar cambios independiente de cual sea. Mientras que para el dropout de la capa interna si que podemos notar como el acc de train va disminuyendo gradualmente conforme aumentamos el dropout. Además de aumentar el loss, al eliminar algunos patrones que había aprendido el modelo de mas. Acercando los valores de ACC para train y test, y por consiguiente el **overfitting**. Diría que el mejor valor que le podríamos asignar al loss se encuentra en esa franja en la que el ACC de test se pone amarillo (asciende muy muy levemente) mientras que el ACC de train disminuye (disminuyendo el **overfitting**).


#Datos hospital (Breast cancer)

##¿Qué vamos a hacer?

Para poder trabajar con todos estos datos, vamos a dividirlos. Una vez divididos se eligen para cada subconjunto las 8000 variables mas significativas, se descartan el resto. Habiendo realizado esta pequeña transformación ya tendremos un conjunto de datos con el que será muy fácil trabajar.
Vamos a filtrar otra vez las variables que hemos obtenido anteriormente, para quedarnos solo con: 30, 200 y 800. 

Tras unas pequeñas pruebas, he comprobado que el tiempo de cómputo necesario para este conjunto de datos es muhco menor que el que necesitabamos anteriormente. Esto me va a permitir añadir mas condiciones. Me parece muy interesante comprobar cuanto se afectan los datos en función de las variables que hemos seleccionado (30,200 y 800). 

Además, se va a agrandar un poco mas el conjunto de pruebas que le vamos a hacer a estos datos. Al no tener muy clara que combinación podría resultar ser la mejor. Ampliar el rango de busqueda va a permitir poder ver para que valores se obtienen mejores resutlados, además de dar una idea general de que resultados podemos obtener con este modelo.

```{r echo=FALSE}
load(file="savedData/reduced_matrix_lung.RData")

```
##Leemos los datos

```{r eval=FALSE}
test.set<-read.csv("BreastCancer/BreastCancer/breastCancer_test.data", header=FALSE, sep=',')
train.set<-read.csv("BreastCancer/BreastCancer/breastCancer_train.data", header=FALSE, sep=',')
data.names<- read.csv("BreastCancer/BreastCancer/breastCancer.names")
all.data <- rbind(train.set,test.set)

v.dependiente <- 'relapse'
colnames(train.set)[24482] <-v.dependiente
colnames(test.set)[24482] <-v.dependiente
colnames(all.data)[24482] <-v.dependiente

train.set$relapse <- ifelse(train.set$relapse == v.dependiente, 1, 0)
test.set$relapse <- ifelse(test.set$relapse == v.dependiente, 1, 0)

#Pasamos a partir los datos

dim.train<-dim(train.set)
dim.test<-dim(test.set)
len_corte<-round(dim.train[2]/2) 
part.1.train<-train.set[,1:(len_corte-1)]
part.2.train<-train.set[,len_corte:(dim.train[2]-1)]
part.1.test<-test.set[,1:(len_corte-1)] 
part.2.test<-test.set[,len_corte:(dim.test[2]-1)]
relapse.train<-train.set[,24482]
relapse.test<-test.set[,24482]

part.1.train<-cbind.data.frame(part.1.train,relapse.train)
part.1.train<-cbind.data.frame(part.1.train,relapse.train)
part.2.train<-cbind.data.frame(part.2.train,relapse.train)
part.1.test<-cbind.data.frame(part.1.test,relapse.test)
part.2.test<-cbind.data.frame(part.2.test,relapse.test)

weight.var.1<-information.gain(relapse.train~., part.1.train)
weight.var.2<-information.gain(relapse.train~., part.2.train)
selected.var.1<-cutoff.k(weight.var.1,8000)
selected.var.2<-cutoff.k(weight.var.2,8000)
dataframe.1<-part.1.train[,selected.var.1]
dataframe.2<-part.2.train[,selected.var.2]

#Re-filtramos las variables obtenidas
train.set.reduced<-cbind.data.frame(dataframe.1, dataframe.2)
weight.reduce.vars<-information.gain(relapse.train~., train.set.reduced)
reduccion.30.var<-cutoff.k(weight.reduce.vars,30)
reduccion.200.var<-cutoff.k(weight.reduce.vars,200)
reduccion.800.var<-cutoff.k(weight.reduce.vars,800)

reduced.train.30.var<-train.set[,reduccion.30.var]
reduced.train.200.var<-train.set[,reduccion.200.var]
reduced.train.800.var<-train.set[,reduccion.800.var]
reduced.test.30.var<-test.set[,reduccion.30.var]
reduced.test.200.var<-test.set[,reduccion.200.var]
reduced.test.800.var<-test.set[,reduccion.800.var]


```
##Combinaciones seleccionadas
Ahora pasamos a entrenar el modelo para todas estas posibles combinaciones.  
```{r eval=FALSE}
x_train_800<- as.matrix(reduced.train.800.var)
x_test_800<-as.matrix(reduced.test.800.var)
x_train_200<- as.matrix(reduced.train.200.var)
x_test_200<-as.matrix(reduced.test.200.var)
x_train_30<- as.matrix(reduced.train.30.var)
x_test_30<-as.matrix(reduced.test.30.var)
y_train<-to_categorical(relapse.train,2)
y_test<-to_categorical(relapse.test,2)

zero_layer = c(6000, 4000, 2800, 1000, 600 , 200 , 120 , 20)
first_layer = c(4000, 2500, 1800, 1000, 600 , 200 , 120 , 20)
second_layer = c(2000, 1200, 800, 500, 200 , 80 , 50 , 20)
third_layer = c(1000, 800, 600, 300, 100 , 80 , 30 , 20)
fourth_layer = c(800, 600, 400, 300, 200 , 120 , 70 , 20)
fifth_layer = c(700, 600, 500, 300, 200 , 120 , 70 , 50)
sixth_layer = c(200, 150, 80, 30, 25 , 20 , 15 , 10 )
seventh_layer = c(150, 100, 80, 50, 40 , 30 , 20 , 10 )
eigth_layer = c(120, 90, 70, 50, 40 , 30 , 15 , 10 )
ninth_layer = c(100, 80, 60, 50, 40 , 30 , 15 , 10 )
tenth_layer = c(80, 60, 50, 40, 30 , 20 , 15 , 10 )
dropout = c(0.3,0.3,0.3,0.3,0.3, 0.3, 0.3, 0.3)
nnet_comb<-c(1,2,3,4,5,6,7)
df <- data.frame( l0=zero_layer,l1=first_layer, l2=second_layer, l3=third_layer, 
                   l4=fourth_layer, l5= fifth_layer , l6=sixth_layer, l7=seventh_layer, l8=eigth_layer,
                   l9=ninth_layer, l10=tenth_layer)

resultado_lapply_800<-as.matrix(lapply(1:length(df), function(i) aplicar_dif_layers_lapply_prep(x_train=x_train_800, x_test=x_test_800, y_train=y_train, y_test=y_test,nnet_comb,df, i, dropout, activation_func = 'sigmoid')))
resultado_lapply_200<-as.matrix(lapply(1:length(df), function(i) aplicar_dif_layers_lapply_prep(x_train=x_train_200, x_test=x_test_200, y_train=y_train, y_test=y_test,nnet_comb,df, i, dropout, activation_func = 'sigmoid')))
resultado_lapply_30<-as.matrix(lapply(1:length(df), function(i) aplicar_dif_layers_lapply_prep(x_train=x_train_30, x_test=x_test_30, y_train=y_train, y_test=y_test,nnet_comb,df, i, dropout, activation_func = 'sigmoid')))

drop.data.frame_800<-data.frame(t(matrix(unlist(lapply(1:length(df), function(i) resultado_lapply_800[i])), nrow = 5*length(nnet_comb) )))
drop.data.frame_200<-data.frame(t(matrix(unlist(lapply(1:length(df), function(i) resultado_lapply_200[i])), nrow = 5*length(nnet_comb) )))
drop.data.frame_30<-data.frame(t(matrix(unlist(lapply(1:length(df), function(i) resultado_lapply_30[i])), nrow = 5*length(nnet_comb) )))

nombres_data_frame<-c()
for (e in 1:length(nnet_comb)) {
  nombres_data_frame <- c(nombres_data_frame, paste("ACC",e, sep=""))
  nombres_data_frame <- c(nombres_data_frame, paste("LOSS",e, sep=""))
  nombres_data_frame <- c(nombres_data_frame, paste("TRAIN_ACC",e, sep=""))
  nombres_data_frame <- c(nombres_data_frame, paste("TRAIN_LOSS",e, sep=""))
  nombres_data_frame <- c(nombres_data_frame, paste("TIEMPO",e, sep=""))
}
colnames(drop.data.frame_800)<-nombres_data_frame
colnames(drop.data.frame_200)<-nombres_data_frame
colnames(drop.data.frame_30)<-nombres_data_frame

```
##Gráficas
Se muestran a continuación los resutlados graficamente.

```{r echo=FALSE}
load(file="savedData/Val_plotly_no_se_ve_nada.RData")
```
```{r results='hide', message=FALSE, warning=FALSE}
library(plotly)

plot.acc.30<-plot_ly(showscale = FALSE, scene='scene28') %>%
  add_surface(z = ~acc.values.30, opacity=0.98) %>%
  add_surface(z = ~acc.values.val.30, opacity=0.85, colorscale = list(c(0,"rgb(255,112,183)"),c(1,"rgb(128,0,64)")))

plot.acc.200<-plot_ly(showscale = FALSE, scene='scene29') %>%
  add_surface(z = ~acc.values.200, opacity=0.98)%>%
  add_surface(z = ~acc.values.val.200, opacity=0.85,colorscale = list(c(0,"rgb(255,112,183)"),c(1,"rgb(128,0,64)")))

plot.acc.800<-plot_ly(showscale = FALSE,scene='scene30') %>%
  add_surface(z = ~acc.values.800, opacity=0.98) %>%
  add_surface(z = ~acc.values.val.800, opacity=0.85, colorscale = list(c(0,"rgb(255,112,183)"),c(1,"rgb(128,0,64)")))


plot.loss.30 <- plot_ly(showscale = FALSE,scene='scene31') %>%
  add_surface(z = ~loss.values.30, opacity=0.98) %>%
  add_surface(z = ~loss.values.val.30, opacity=0.85, colorscale = list(c(0,"rgb(255,112,183)"),c(1,"rgb(128,0,64)")))

plot.loss.200<-plot_ly(showscale = FALSE,scene='scene32') %>%
  add_surface(z = ~loss.values.200, opacity=0.98) %>%
  add_surface(z = ~loss.values.val.200, opacity=0.85, colorscale = list(c(0,"rgb(255,112,183)"),c(1,"rgb(128,0,64)")))

plot.loss.800<-plot_ly(showscale = FALSE,scene='scene33') %>%
  add_surface(z = ~loss.values.800, opacity=0.98) %>%
  add_surface(z = ~loss.values.val.800, opacity=0.85, colorscale = list(c(0,"rgb(255,112,183)"),c(1,"rgb(128,0,64)")))

p.acc<-subplot(plot.acc.30, plot.acc.200, plot.acc.800)%>%
  layout(title = "3D Subplots",
         scene28 = list(domain=list(x=c(0,0.35),y=c(0.25,0.75)),
                      xaxis=axx, yaxis=axx, zaxis=axx,
                      aspectmode='cube'),
         scene29 = list(domain=list(x=c(0.35,0.6),y=c(0.25,0.75)),
                       xaxis=axx, yaxis=axx, zaxis=axx,
                       aspectmode='cube'),
         scene30 = list(domain=list(x=c(0.6,1),y=c(0.25,0.75)),
                       xaxis=axx, yaxis=axx, zaxis=axx,
                       aspectmode='cube'))

p.loss<-subplot(plot.loss.30, plot.loss.200, plot.loss.800)%>%
  layout(title = "3D Subplots",
         scene31 = list(domain=list(x=c(0,0.35),y=c(0.25,0.75)),
                      xaxis=axx, yaxis=axx, zaxis=axx,
                      aspectmode='cube'),
         scene32 = list(domain=list(x=c(0.35,0.6),y=c(0.25,0.75)),
                       xaxis=axx, yaxis=axx, zaxis=axx,
                       aspectmode='cube'),
         scene33 = list(domain=list(x=c(0.6,1),y=c(0.25,0.75)),
                       xaxis=axx, yaxis=axx, zaxis=axx,
                       aspectmode='cube'))

lung.plot.acc <-api_create(p.acc, filename="LUNG PLOT ACC TRAIN TEST 1")

lung.plot.loss <- api_create(p.loss, filename="LUNG PLOT LOSS TRAIN TEST 1")
```

```{r}
lung.plot.acc
```
```{r}
lung.plot.loss
```
En estas gráficas se muestran todo igual que antes, la única diferencia es que aquí tenemos tres gráficas juntas. En este caso, de izquierda a derecha vamos a encontrar los resultados obtenidos para 30, 200 y 800 variables respectivamente. Arriba quedan los valores para el ACC y abajo los del LOSS.

Es muy dificil poder encontrar alguna clase de patrón como ocurria para el caso de MNIST. 
```{r  echo=FALSE, message=FALSE, warning=FALSE}
library(plotly)

plot.acc.30A<-plot_ly(showscale = FALSE, scene='scene1') %>%
  add_surface(z = ~acc.values.30, opacity=0.98)

plot.acc.200A<-plot_ly(showscale = FALSE, scene='scene2') %>%
  add_surface(z = ~acc.values.200, opacity=0.98)

plot.acc.800A<-plot_ly(showscale = FALSE,scene='scene3') %>%
  add_surface(z = ~acc.values.800, opacity=0.98)


plot.loss.30A <- plot_ly(showscale = FALSE,scene='scene4') %>%
  add_surface(z = ~loss.values.30, opacity=0.98) 

plot.loss.200A <-plot_ly(showscale = FALSE,scene='scene5') %>%
  add_surface(z = ~loss.values.200, opacity=0.98) 

plot.loss.800A<-plot_ly(showscale = FALSE,scene='scene6') %>%
  add_surface(z = ~loss.values.800, opacity=0.98) 

p.accA<-subplot(plot.acc.30A, plot.acc.200A, plot.acc.800A)%>%
  layout(title = "3D Subplots",
         scene = list(domain=list(x=c(0,0.35),y=c(0.25,0.75)),
                      xaxis=axx, yaxis=axx, zaxis=axx,
                      aspectmode='cube'),
         scene2 = list(domain=list(x=c(0.35,0.6),y=c(0.25,0.75)),
                       xaxis=axx, yaxis=axx, zaxis=axx,
                       aspectmode='cube'),
         scene3 = list(domain=list(x=c(0.6,1),y=c(0.25,0.75)),
                       xaxis=axx, yaxis=axx, zaxis=axx,
                       aspectmode='cube'))

p.lossA<-subplot(plot.loss.30A, plot.loss.200A, plot.loss.800A)%>%
  layout(title = "3D Subplots",
         scene4 = list(domain=list(x=c(0,0.35),y=c(0.25,0.75)),
                      xaxis=axx, yaxis=axx, zaxis=axx,
                      aspectmode='cube'),
         scene5 = list(domain=list(x=c(0.35,0.6),y=c(0.25,0.75)),
                       xaxis=axx, yaxis=axx, zaxis=axx,
                       aspectmode='cube'),
         scene6 = list(domain=list(x=c(0.6,1),y=c(0.25,0.75)),
                       xaxis=axx, yaxis=axx, zaxis=axx,
                       aspectmode='cube'))

lung.plot.accA = api_create(p.accA, filename="LUNG PLOT ACC")

lung.plot.lossA = api_create(p.lossA, filename="LUNG PLOT LOSS")
```
Probamos a quitarles los valores de TRAIN para ver mejor la gráfica.
```{r}
lung.plot.accA
```
```{r}
lung.plot.lossA
```
Podemos apreciar un pequeño matiz al comprobar los datos resultantes, y es que:

- Para los datos con 30 variables, dificilmente vamos a obtener altos valores de ACC. Además, los valores mas altos tienen el ACC train bastante mas alto, indicando un claro **overfitting**.
- Los valores con 800 variables tampoco llegan a tener altos valores para el ACC, y los pocos que tienen son valores de ACC mas grandes que los de train, por lo que no ofrece resultados que puedan interesarnos tampoco.
- Pero en cambio, para los valores obtenidos con las 200 variables mas significativas si que encontramos valores de ACC interesantes.

Concretamente, encontramos algunos resultados como:
```{r echo=FALSE}
load(file="savedData/tabla_TODO_200_VAR.RData")
```

```{r}

data.frame.new<-data.frame(
  drop.data.frame_200$ACC1, 
  drop.data.frame_200$LOSS1, 
  drop.data.frame_200$TRAIN_ACC1,   drop.data.frame_200$TRAIN_LOSS1,
  drop.data.frame_200$TIEMPO1)
i<-2
for(i in c(2,3,4,5,6,7)){
  data.temp<-data.frame(
    drop.data.frame_200[paste("ACC", i,sep="")],
    drop.data.frame_200[paste("LOSS", i,sep="")],
    drop.data.frame_200[paste("TRAIN_ACC", i,sep="")],
    drop.data.frame_200[paste("TRAIN_LOSS", i,sep="")],
    drop.data.frame_200[paste("TIEMPO", i,sep="")])
  colnames(data.temp)<-colnames(data.frame.new)
    data.frame.new<-rbind(data.frame.new, data.temp)
    data.temp<-0
    
}

ndx = order(data.frame.new$drop.data.frame_200.ACC1, decreasing=T)

data.order<-data.frame.new[ndx,]
colnames(data.order)<-c("ACC", "LOSS", "ACC_TRAIN", "LOSS_TRAIN", "TIEMPO")
kable(data.order[1:15,]) %>%
  kable_styling(bootstrap_options = "striped", full_width = F)
```
Aquí podemos ver algunos de los 15 mejores resultados obtenidos para 200 variables.


##10 Cross-Validation & AUC

Se va a usar la misma medida que utilizamos el año pasado para medir que tan bueno era un modelo. Para ello se ha reutilizado la función que habíamos utilizado anteriormente. Quedando tal que así:
```{r echo=FALSE}
load(file="savedData/AUC_VAL_200.RData")
```


```{r eval=FALSE}
AUC <-function(mod.estimado, last.col) {
  roc.curve <-roc(last.col, mod.estimado, smooth=FALSE, auc=TRUE)
  auc.result <-roc.curve$auc
  return(auc.result)
}


calcula_modelo<-function(x_train=x_train, x_test=x_test, y_train=y_train, y_test=y_test, num_capas=2, vector=c(256,180), drop.out=c(0.4,0.3), activation_func, k_folds=0 ){
  t <- proc.time() # Inicia el cronómetro
  modelo <- keras_model_sequential()
  i<-1
  while(i<num_capas+1){
    if(i==1){
      modelo %>% 
        layer_dense(units = as.integer(vector[i]), activation = "relu", input_shape = c(dim(x_train)[2])) %>% 
        layer_dropout(rate = drop.out[[i]]) 
      i<-i+1
    }else{
      modelo %>%
        layer_dense(units = as.integer(vector[i]), activation = "relu") %>% 
        layer_dropout(rate = drop.out[[i]]) 
      i<-i+1
    }
  }
  
  modelo %>%
    layer_dense(units = ncol(y_train), activation = activation_func)
  
  modelo %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_rmsprop(),
    metrics = c("accuracy"))
  if(k_folds==0){
  train_data<-modelo %>% fit(x_train, y_train, epochs = 10, batch_size = 128, view_metrics = FALSE, 
                             validation_split = 0.2)
  var<-modelo %>% evaluate(test, y_test,verbose = 0)
  
  acc.train<-train_data$metrics$acc[10]
  loss.train<- train_data$metrics$loss[10]
  acc.test<-var$acc
  loss.test<-var$loss
  }else{
    all.data.data<-rbind(x_train,x_test)
    all.data.relapse<-rbind(y_train,y_test)
    cf<-createFolds(all.data.relapse[,-1], k=k_folds)
    res.acc<-c()
    res.loss<-c()
    res.test.acc<-c()
    res.test.loss<-c()
    for(i in 1:k_folds) {
      test <-as.matrix(all.data.data[unlist(cf[i]),])
      y_test <-as.matrix(all.data.relapse[unlist(cf[i]),])
      train <-as.matrix(all.data.data[-unlist(cf[i]),])
      y_train<-as.matrix(all.data.relapse[-unlist(cf[i]),])
      resul<-modelo %>% fit(train, y_train, epochs = 10, batch_size = 128, view_metrics = FALSE, 
                                 validation_split = 0.2)
      res.acc<-c(res.acc, resul$metrics$acc[10])
      res.loss<-c(res.loss, resul$metrics$loss[10])
      #Predicted class
      predicted_clases<-modelo %>% predict_classes(test)
      #yhat_keras_class_vec <- predict_classes(object = modelo, x = as.matrix(test)) %>%
      #  as.vector()
      # Predicted Class Probability
      val.auc<-AUC(predicted_clases,  y_test[,1])
      
      
      res.test.acc<-c(res.test.acc, val.auc)
    }
    
    acc.train<-mean(res.acc)
    loss.train<-NaN
    acc.test<-NaN
    loss.test<-NaN
    
  }

  proc.time()-t  
  
  return(c(acc.train,loss.train, acc.test,loss.test, t["elapsed"]))
}
```


```{r echo=FALSE}
load(file="savedData/AUC_VAL_200.RData")
```

Utilizando estas funciónes hemos obtenido los siguientes resultados:
```{r}

auc_val<-matrix(unlist(lapply(1:7, function(i) drop.data.frame_200[paste("ACC",i,sep = "")])))
dim(auc_val) <- c(11, 7)
auc_plot<-plot_ly(showscale = FALSE) %>%
  add_surface(z = ~auc_val, opacity=0.98)

auc.surface = api_create(auc_plot, filename="AUC VAL")
auc.surface
```
Los ejes utilizados para esta gráfica son los mismo que se han explicado anteriormente, de combinaciones de capas y de neuronas.


Se comprueba para el AUC que:

- Para los valores altos de capas neuronales los resultados son un tanto peores.
- Se nota un leve parecido entre las gráficas de ACC y AUC. Pero, el AUC nos va a dar resultados mas claros a la hora de interpretar los resultados.
- Los mejores resultados se obtienen para las primeras combinaciones de neuronas y pocas o muchas capas. Y conforme disminuimos el número de neuronas empezamos a obtener mejores resultados para valores intermedios de capas neuronales. 

```{r}

ndx = order(auc_val, decreasing=T)
ndx
data.order<-auc_val[ndx]

kable(data.order[1:10]) %>%
  kable_styling(bootstrap_options = "striped", full_width = F)
```
Aqui podemos ver los 10 mejores resultados de AUC obtenidos para este conjunto de datos.

##ML-CARET

A continuación recordamos los resultados que obtuvimos para los modelos utilizados el año pasado. Esta vez desde alto nivel utilizando el paquete CARET. Para ello se han usado las funciones implementadas el año pasado. No voy a entrar mucho en detalle, me voy a centrar mas en los resultados que se obtienen.

Vamos a coger de referencia la siguiente formula:
```{r echo=FALSE}
load(file="savedData/val_para_pruebas.RData")
```

```{r}
formula
```

Esta formula la hemos obtenido seleccionando las 5 variables mas significativas que arroja FSelector. Obteniendo lo siguientes resultados:
```{r echo=FALSE, message=FALSE, warning=FALSE}

compute.models <-function(formula, elige.modelo, train.data, test.data) {

  if(elige.modelo=="glm") {
    modelo<-glm(formula, train.data, family=binomial("logit"));
    mod.estimado<-predict(modelo, newdata=test.data, type="response");
  }
  else if(elige.modelo=="dt") {
    modelo<-rpart(formula,data=train.data,control=rpart.control(minsplit=3, cp=0.111));
    mod.estimado<-predict(modelo, newdata=test.data, type="matrix");
    mod.estimado<-as.matrix(mod.estimado)
  }
  else if(elige.modelo=="svm") {
    modelo<-svm(formula, data=train.data, cost=1, gamma=1, probality=T);
    mod.estimado<-predict(modelo, newdata=test.data, probability=T);
  }
  else if(elige.modelo=="nnet") { 
    modelo<-nnet(formula,data=train.data, size=1, decay=0.1, maxit=3, trace=F);
    mod.estimado<-predict(modelo, newdata=test.data, type="raw");
  }
  
  if(is.factor(mod.estimado)) mod.estimado<-ifelse(mod.estimado=="X1", 1, 0)
  ifelse(elige.modelo=="dt", roc.test<-roc(as.numeric(test.data$relapse), as.numeric(mod.estimado[,dim(mod.estimado)[2]]), smooth=F, auc=T), roc.test<-roc(as.numeric(test.data$relapse), mod.estimado, smooth=F, auc=T))
  auc.test <-roc.test$auc
  return(list(auc=auc.test,roc=roc.test)) 
}

crossValidation<-function(full.matrix, k, formula) {
  
  auc.glm<-list()
  auc.svm<-list()
  auc.dt<-list()
  auc.nnet<-list()
  cf<-createFolds(full.matrix$relapse, k=k)
  
  for(i in 1:k) {
    
    test <-full.matrix[unlist(cf[i]),]
    train <-full.matrix[-unlist(cf[i]),]
    train$relapse <-as.factor(train$relapse)
    test$relapse <-as.factor(test$relapse)
    
    auc.glm<-c(auc.glm,compute.models(formula, "glm", train, test )$auc);
    auc.svm<-c(auc.svm,compute.models(formula, "svm", train, test)$auc);
    auc.dt<-c(auc.dt,compute.models(formula, "dt", train, test)$auc);
    auc.nnet<-c(auc.nnet,compute.models(formula, "nnet", train, test)$auc);
  }
  
  mean.glm=mean(unlist(auc.glm))
  mean.svm=mean(unlist(auc.svm))
  mean.dt=mean(unlist(auc.dt))
  mean.nnet=mean(unlist(auc.nnet))
  
  return(list(mean.glm, mean.svm, mean.dt, mean.nnet))
}
```

```{r warning=FALSE}
kable(tabla) %>%
  kable_styling(bootstrap_options = "striped", full_width = F)
```

Aqui se muestran los resultados obtenidos para **GLM**, **SVM**, **DT** y **NNET** respectivamente.

Como podemos observar el AUC puede llegar, para esta pequeña demostración, a rondar 0.85 de AUC para el caso de **SVM**.


##Conclusiones

De cara a la conclusión final toca valorar los resultados obtenidos por nuestro modelo de Deep Learning. El mejor resultado de todas las combinaciones es de 0.96 de AUC, donde podemos notar que hay un poco de overfitting (Hay cierta diferencia significativa entre los ACC de train y test). Pero podemos llegar a obtener un AUC de en torno al 0.9 sin tener overfitting. Aunque han sido solo unas pocas combinaciones de entre todas las que hemos probado.

Por otro lado nos encontramos con los resultados que hemos obtenido sin apenas haber procesado los datos, solo cogiendo como formula las 5 variables mas significativas en la que fácilmente alcanzavamos un AUC de 0.88.

A la hora de ver los resultados no me parece una opción tan buena el haber usado **Deep Learning** para este conjunto de datos. Debido a que ha supuesto una grán dificultad mejorar levemente este accuracy, y podríamos haber obtenido iguales, o incluso mejores, resultados con otro modelo mas sencillo.

A diferencia de lo que ocurria para el caso de MNIST, donde se obtenían resultados soprendentemente buenos, en *Breast cancer* no tenemos suficientes datos como para poder utilizar un modelo tan complejo para clasificar los datos.

No solo en lo referente a resultado, sino también a rendimiento, un *modelo mas sencillo* parece la **mejor opción**. En lo que al **tiempo y rendimiento** respecta, mi ordenador ha sufrido al principio *estragos* para poder ejecutar las diferentes combinaciones que quería probar en MNIST. Estos problemas se han reducido sensiblemente al usar **Breast cancer**; debido a lo pequeño que se quedaba el dataframe tras procesarlo. He necesitado estar tomando medidas de tiempo para MNIST, aunque me han sido también muy útiles para *Breast cancer*. Desde descartar ejemplos que no me aportaban información, hacer estimaciones muy claras de cuanto podría tardar 'x' funcion en ejecutarse par una combinación, detección de errores ...


