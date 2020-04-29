#!/bin/bash

#SBATCH --nodes=1
#SBATCH --time=03:00:00
#SBATCH --partition=normal
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=k.m.lang@uvt.nl
#SBATCH --output=msg/%x-%A_%a.out
#SBATCH --error=msg/%x-%A_%a.err
#SBATCH --job-name=exp5

## Load modules:
module load 2019
module load pre2019
module load stopos
module load R/3.5.1-foss-2018b

## Define variables:
exp=1                    # Experiment number
id0=$SLURM_ARRAY_JOB_ID  # Master job ID for the current job
id1=$SLURM_ARRAY_TASK_ID # Array index for the current job
np=`sara-get-num-cores`  # Number of available processors
projDir=$HOME/impDv/2020 # Project directory

## Define the source directory:
inDir=$projDir/code

## Define and create (as necessary) the final output directory:
outDir=$projDir/output/$SLURM_JOB_NAME/

if [ ! -d "$outDir" ]; then
    mkdir -p $outDir
fi

## Define and create the temporary output directory:
tmpOut="$TMPDIR"/output/
mkdir -p $tmpOut

## Store the stopos pool's name in the environment variable STOPOS_POOL:
export STOPOS_POOL=$projDir/parms/pool

## Allow worker nodes to find my personal R packages:
export R_LIBS=$HOME/rPackages

## Don't let BLAS multithread:
export GOTO_NUM_THREADS=1

## Execute my script on $np - 1 processors:
for i in `seq 1 $(($np - 1))`; do
    (
	## Get the next line or parameters from the stopos pool:
	stopos next

	## Did we get a line? If not, break the loop:
	if [ "$STOPOS_RC" != "OK" ]; then
	    break
	fi

	## Call the R script with the replication number from the stopos pool:
	Rscript --vanilla $inDir/runImpDvSim.R $STOPOS_VALUE $exp $tmpOut

	## Remove the used parameter line from the stopos pool:
	stopos remove
    )& 
done
wait

## Compress the output directory:
tar -czf "$TMPDIR"/output-$id0\_$id1.tar.gz -C $tmpOut .

## Copy ouput from scratch to output directory:
cp "$TMPDIR"/output-$id0\_$id1.tar.gz $outDir
