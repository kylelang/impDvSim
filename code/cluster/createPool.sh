#!/bin/bash

### Title:    Create Stopos Pool
### Author:   Kyle M. Lang
### Created:  2020-04-28
### Modified: 2020-04-28

parmDir="$1"

module load pre2019
module load stopos

stopos create -p $parmDir/pool
stopos add $parmDir/repList.txt -p $parmDir/pool
