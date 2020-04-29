### Title:    Run Imputed DV Simulation
### Author:   Kyle M. Lang
### Created:  2015-09-21
### Modified: 2020-04-28

## Make sure we have a clean environment:
rm(list = ls(all = TRUE))

## Extract commandline arguments
args <- commandArgs(trailingOnly = TRUE) 

## Initialize the environment:
source("init.R")

## Run one replication of the simulation:
doRep(rp = rp, conds = conds, parms = parms)
