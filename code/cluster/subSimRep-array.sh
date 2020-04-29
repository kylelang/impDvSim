#!/bin/bash

#SBATCH --nodes=1
#SBATCH --ntasks=15
#SBATCH --time=00:45:00
#SBATCH --partition=normal
#SBATCH --mail-type=BEGIN,END
#SBATCH --mail-user=k.m.lang@uvt.nl
#SBATCH --output=msg/%x-%A_%a.out
#SBATCH --error=msg/%x-%A_%a.err
#SBATCH --job-name=exp5

## Define variables:
id0=$SLURM_ARRAY_JOB_ID   # Master job ID for the current job
id1=$SLURM_ARRAY_TASK_ID  # Array index for the current job
np=$SLURM_NTASKS          # Number of requested processors
exp=5                     # Experiment number

## Define the source directory:
inDir=$HOME/miben/repLiLin/code

## Define and create (as necessary) the final output directory:
outDir=$HOME/miben/repLiLin/output/smallS2/$SLURM_JOB_NAME/

if [ ! -d "$outDir" ]; then
    mkdir -p $outDir
fi

## Define and create the temporary output directory:
tmpOut="$TMPDIR"/output/
mkdir -p $tmpOut

## Load modules:
module load 2019
module load R/3.5.1-foss-2018b 

## Allow worker nodes to find my personal R packages:
export R_LIBS=$HOME/rPackages

## Don't let BLAS multithread:
export GOTO_NUM_THREADS=1

## Execute my script on $np processors:
for i in `seq 1 $np`; do
    rp=$((($id1 - 1) * $np + i)) # Compute the current replication number
    Rscript --vanilla $inDir/repLiLinSim-serial.R $rp $exp $tmpOut &
done
wait

## Compress the output directory:
tar -czf "$TMPDIR"/output-$id0\_$id1.tar.gz -C $tmpOut .

## Copy ouput from scratch to output directory:
cp "$TMPDIR"/output-$id0\_$id1.tar.gz $outDir
