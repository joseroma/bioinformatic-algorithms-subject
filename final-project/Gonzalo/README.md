---
title: "Workflow"
author:
- name: Jose Rodrvguez Maldonado
  affiliation: Trabajo final. Herramientas y algoritmos en Bioinformática
  email: jrmaj0a@uma.es
date: "Octubre de 2018"
output:
  prettydoc::html_pretty:
    self_contained: false
    keep_md: true
    df_print: paged
    theme: cayman
    highlight: github
---



Vamos a tener diferentes formas de ejecutar este workflow. En caso de duda, o si nos equivocamos, nos aparecerá una ayuda como esta:
```{}
usage: ./work.sh [-svbr] [-l number] [-p number] [-t number]
  -s 	 Saltamos la descarga y enriquecimiento de los datos
  -v     Dibujar volcano plot
  -r     Saltamos análisis de supervivencia y clusters de pacientes
  -b 	 No balanceamos los datos
  -l <number>  	Fija el LogFC
  -f <COX,RFI,NONE>    Elegimos el método de selección de variables.
  -p number  	Elige P-value
  -t number  	Elige el paso (step) para replicar los datos

```


Algunos ejemplos de como ejecutar el flujo:

Ejecutar todo el flujo con los parámetros por defecto y guardando el volcano plot.

``./work.sh -v``

Ejecutar todo el flujo con los parámetros por defecto y guardando el volcano plot, pero sin tener que volver a descargar los datos.

``./work.sh -v -s``

Ejecutar todo el flujo con los parámetros por defecto y guardando el volcano plot, pero sin tener que volver a descargar los datos y definiendo un paso.

``./work.sh -v -s -t 5``

Ejecutar todo el flujo con los parámetros por defecto y guardando el volcano plot, pero sin tener que volver a descargar los datos, definiendo un paso, el p-value y el lfc.

``./work.sh -v -s -t 5 -p 0.001 -l 2``

Como vemos, podemos combinar las diferentes opciones que tenemos a nuestro gusto.
