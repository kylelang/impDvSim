#SBATCH --nodes=1 --time=1:00:00
cd $HOME/project1
module load stopos

# copy the fam-file:

cp $HOME/famfiles/foo.fam "$TMPDIR"

# put the name of the pool in the environment variable
# STOPOS_POOL

export STOPOS_POOL=project1/pool

# determine the number of cores available on the node the job is running on:

ncores=`sara-get-num-cores`

# start as many doit's as there are cores, every doit will start an instance of plink:

for ((i=1; i<=ncores; i++)) ; do
(
# get a line form the pool:
  stopos next

# test if we got a line. If not: leave the loop
  if [ "$STOPOS_RC" != "OK" ]; then
    break
  fi
# call the doit script, using the line we just got from stopos ($STOPOS_VALUE)

  $HOME/project1/jobs/doit $STOPOS_VALUE

# remove the line we used

  stopos remove

# put this commands in the background: hence the & :
) &
done

# wait until all background processes are done
wait
