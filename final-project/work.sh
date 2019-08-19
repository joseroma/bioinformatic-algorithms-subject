#!/bin/bash
printinfo()
{
    echo -e "\e[36m\e[1m$1\e[0m"
}

printinfo "Empieza el flujo"


set -e
cd `dirname $0`
mkdir -p .datos

data_bring=true

args=`getopt s $*`

set -- $args
for i
do
	case "$i" in
		"-s") data_bring=false ;;
		*) echo ""
	esac
done


if $data_bring; 
	then
		Rscript 1_getData.R
fi

Rscript 2_subettingData.R

mkdir -p results



if $data_bring;
	then
		Rscript 3_DEA.R "$2"
	else
		Rscript 3_DEA.R "$3"
fi

for last; do true; done


if [ "$last" == "volcano" ]; 
then
	#statements
	Rscript 4-1_Volcano-analysis.R
	printinfo "Guardado el volcano en la carpeta results"
else
	printinfo "Saltamos el volcano"

fi


if [ "$last" == "G2K" ]; 
then
	#statements
	Rscript 4-2_enrichment-analysis-GO-AND-KEGG.R
	printinfo "Guardado el GO and Kegg en la carpeta results"
else
	echo ""

fi

if [ "$last" == "pathways" ]; 
then
	#statements
	Rscript 4-2_enrichment-analysis-GO-AND-KEGG.R
	Rscript 4-3_pathways.R
	prininfo "Guardado el GO and Kegg en la carpeta results"
else
	if [ "$last" == "pathways" ]; 
then
	#statements
	printinfo "Saltamos Go y Kegg analysis"
	printinfo "Saltamos pathways"
else
	printinfo "Saltamos pathways"

fi

fi

