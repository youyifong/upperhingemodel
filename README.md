# upperhingemodel

This project contains the script and R code for the Monte Carlo studies and real data analysis in Elder and Fong (2018). The estimation  methods themselves are implemented in the R package chngpt (https://cran.r-project.org/web/packages/chngpt) version 2018.10-17 or later. The R package kyotil (https://cran.r-project.org/web/packages/kyotil) is also needed for MC studies.


---------------------------------------------------
Monte Carlo studies for speed comparison in Sec 4.1:

There are two options:
    1) If you are in a SLURM distributed computing environment, you can execute the superscript file without any parameters, which calls runscript through configuration files under the config directory. This requests 100 CPUs to run 100 jobs. 
    2) Otherwise, you can directly execute the runscript file with two parameters: runscript 0 1. This runs 1 job. 
  
In both cases, the number of MC replicates each job performs is controlled by the bs parameter in runscript.

To analyze Monte Carlo results, source fastgrid2_performance_MC_summary.R from an R prompt, which creates summary tables.


---------------------------------------------------
Monte Carlo studies for estimation and CI in Sec 4.2 and 4.3

Execute the estscript file to run MC. The number of MC replicates is controlled by the MC_rep parameter in run_sim.R.

To analyze Monte Carlo results, run the content of read_sims.R script from an R prompt. The summary tables in latex will be printed to the standard output.


---------------------------------------------------
The data_example.R file replicates the data analysis example.



References:

Elder and Fong (2018) Faster Grid Search Algorithm and Model-Robust Inference for Upper Hinge Generalized Linear Models. Under review.
