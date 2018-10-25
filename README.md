# upperhingemodel

This project contains the script and R code for the Monte Carlo studies and real data analysis in Elder and Fong (2018). The estimation  methods themselves are implemented in the R package chngpt (https://cran.r-project.org/web/packages/chngpt) version 2018.10-17 or later.

To perform Monte Carlo studies, run submit.sh in a xxx distributed computing environment. It calls the call_sim.h file xxx. call_sim.h then runs run_sim.R with proper arguments. The Monte Carlo results are saved in files organized into subdirectories. 

To analyze Monte Carlo results, source read_sims.R, which creates summary tables.

The data_example.R file replicates the data analysis example.


References:

Elder and Fong (2018) Faster Grid Search Algorithm and Model-Robust Inference for Upper Hinge Generalized Linear Models. Under review.
