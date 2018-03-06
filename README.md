# Gottlieb2016
This is a replication exercise (in R) of a published survey experiment on accountability (Gottlieb 2016)

The main file in the repo is Gottlieb2016.R which contains the script to replicate all of the tables in Gottlieb (2016) which was an analysis conducted by the original author in Stata. This script produces several TeX files which I then compiled into a single Tex file (Tables.Tex ==> Tables.pdf).


Methods used:

*Mixed (random and fixed) Effects fit with Restricted Maximum Likelihood

Randomization Inference



In this repo you will also find the original datasets (.dta)-- survey, commune, and restitutions--as well as the author's code in Stata (.do) and the survey questionaire (citoyen.survey_03.03.14a.pdf)in French. Note that Table 1 in the paper is not replicated by the author's own Stata code. My own code (in R) replicates the Stata code in Table 1 (not the paper's) and both the State code and published results for all other tables.
