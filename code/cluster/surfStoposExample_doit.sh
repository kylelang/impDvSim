#!/bin/bash

# This script expects one parameter, for example 8_135_138
# and will use that to construct input and output file names
# to be used by the program plink.
# This script calls plink, and takes care that input and output
# files are in $TMPDIR. When plink finishes, the output
# files are copied to the home directory.
#
# The bonus: Using the timetorun variable we arrange 
# that if plink is still running 300 seconds before the
# specified walltime, it will be killed (command: timeout) 
# so that the copy of the output files has 300 seconds to complete.

(( timetorun = `sacct -n -o timelimitraw -j $SLURM_JOBID` - 300 ))

module load plink

case=$1 # case for example 8_135_138

famfile="$TMPDIR"/foo.fam  # generated in the job that calls this script
mapfile=project.chr${case}.dosage.map
dosagefile=project.chr${case}.dosage.gz
outfile=out_chr${case}
outdir=$HOME/project1/out

mkdir -p $outdir

datadir=/home/datashar/share/dosages

# copy dosage-file and map-file to $TMPDIR:

cp $datadir/$dosagefile "$TMPDIR"
cp $datadir/$mapfile "$TMPDIR"

# call plink
# plink will create outputfiles with names starting with 'out_chr3_4_18'.

timeout $timetorun plink \
  --dosage "$TMPDIR"/$dosagefile Zin format=2 \
  --fam $famfile \
  --maf 0.05 \
  --geno 0.05 \
  --mind 0.05 --hwe 0.000001 --logistic \
  --out "$TMPDIR"/$outfile \
  --map "$TMPDIR"/$mapfile

#compress the outputfiles:
gzip "$TMPDIR"/$outfile*

# copy the output files to the $HOME directory:
cp "$TMPDIR"/$outfile* $outdir
