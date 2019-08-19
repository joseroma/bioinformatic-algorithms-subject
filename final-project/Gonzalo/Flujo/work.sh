#!/bin/bash


printinfo()
{
    echo -e "\x1B[32m\x1B[1m$1\x1B[0m"
}

printwarning()
{
    echo -e "\x1B[33m\x1B[1m$1\x1B[0m"
}

function usage {
    printinfo "usage: $0 [-svbr] [-l number] [-p number] [-t number]"
    printinfo "  -s 	 Saltamos la descarga y enriquecimiento de los datos"
    printinfo "  -v     Dibujar volcano plot"
    printinfo "  -r     Saltamos análisis de supervivencia y clusters de pacientes"
    printinfo "  -b 	 No balanceamos los datos"
    printinfo "  -l <number>  	Fija el LogFC"
    printinfo "  -f <COX,RFI,NONE>    Elegimos el método de selección de variables."
    printinfo "  -p number  	Elige P-value"
    printinfo "  -t number  	Elige el paso (step) para replicar los datos"
    exit 1
}


set -e
cd `dirname $0`
mkdir -p datos

let refstep=20

survival=true
step=10
referencestep=10
data_bring=true
plot_volcano=false
balance_data=TRUE
feature_selection=COX
pval=0.01
lfc=2

while getopts "svbrl:p:t:f:" o; do
    case "${o}" in
        s)
            data_bring=false
            ;;
        b)
            balance_data=FALSE
            ;;
        v)
            plot_volcano=true
            ;;
        r)
            survival=false
            ;;
        
		l)
            lfc=${OPTARG}
            ;;
        f)
            feature_selection=${OPTARG}
            ;;
        p)
            pval=${OPTARG}
            ;;
        t)
            step=${OPTARG}
            ;;
        *)
            usage
            ;;
    esac
done
shift $((OPTIND-1))


mkdir -p results
mkdir -p results/enrichment
mkdir -p results/ML
mkdir -p results/survival

if $data_bring; 
	then
		Rscript 1_getData.R $lfc $pval	
			lection
		mv 20160128-BRCA-RNAseq2GeneNorm.txt datos/20160128-BRCA-RNAseq2GeneNorm.txt
		mv 20160128-BRCA-Clinical.txt datos/20160128-BRCA-Clinical.txt
fi

if $plot_volcano;
	then
		Rscript 1_1Volcano.R $lfc $pval
fi


printinfo "Pasamos a replicar los datos."


if(($step  > $refstep));
	then
		printwarning "El STEP esta a $step"
		printwarning "Un STEP alto probablemente generará errores en los modelos predictivos"
		printwarning "STEP recomendado --> $referencestep"
fi
Rscript 2_replicateData.R $step $balance_data


Rscript 3_lasso-ridge.R
Rscript 4_ML_module.R

if $plot_volcano;
	then
		Rscript survivalClustering.R
fi




echo "Fin del flujo de trabajo"

