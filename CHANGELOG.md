# Change Log
All notable changes to the **impDvSim** project (and some not so notable ones)
will be documented in this file. The format of this log is based on [Keep a
Changelog][kacl].

## 2020-04-29

### Added
- Number of imputations as a simulation parameter
- Scripts to run simulation on Lisa cluster using Stopos to control 
  parallelization

### Changed
- Missing data are now generated via a logistic regression model (the relevant 
  routines are defined in *code/simMissingness.R*)
- Updated analysis scripts
- Archived old versions of analysis scripts in *code/analysis/pre2019/*

### Fixed
- Cleaned redundant scripts out of the *code* directory

## 2019-11-11

### Added
- This ChangeLog
- A ToDo file
- @2Lu-Hove invited as collaborator

### Fixed
- Got the code cleaned and ready to be run/modified

[kacl]: http://keepachangelog.com/
