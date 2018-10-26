###########################################################
#### R-Script for Submitting Simulations for change point 2
#### Author : Adam Elder
##########################################################

library(chngpt)
source("sim_fast_boot.R")


### getting the tak ID which will determine which 
### simulaiton settings to use when running the simulation
### depending on you servers setup, this code chunk
### will likely fail unless you are using a 
### Sun Grid Engine on your cluster.
#syst.envr <- as.numeric(Sys.getenv("SGE_TASK_ID"))
#
#if(!is.na(syst.envr)){
#  job.id <- as.numeric(Sys.getenv("SGE_TASK_ID"))
#}else{
#  cat("No Job ID found, choosing the answer to
#      life the universe and everything \n")
#  job.id <- 2
#}
Args <- commandArgs(trailingOnly=TRUE) 
job.id=as.numeric(Args[1])



## Set the number of Monte Carlo replicates to be used in the
## simulation here. To speed simulation reduce this number : 

MC_rep <- 2 # 10^4 for real

## Creating a dataframe with the different simulation results
var_combos <- list(c("bootstrap"), c("bootstrap", "model"), 
                   c("model"), c("model", "modrob"))
sim_mods <- c("up_hinge", "near_up_hinge", "up_hinge", "near_up_hinge")
sample_sizes <- c(50, 100, 500, 2000)

sim_df <- data.frame( "Model" = rep(sim_mods, each = 4),
                      "SS" = rep(sample_sizes, times = 4))
sim_df$fam_type <- rep(c("gaussian", "gaussian", "binomial", "binomial"), each = 4)
sim_df$var_type <- rep(1:4, each = 4)


job_params <- sim_df[job.id, ]
cat("Parameters for this simulation \n")
print(job_params)

set.seed(2040)
# Running a simulation 
all_results <- run_sims(model = job_params[1, 1], fam_type = job_params$fam_type, ss = job_params[1, 2],
                        mc_runs = MC_rep, conf_type = var_combos[[job_params$var_type]])

if(!file.exists("sim_res")){dir.create("sim_res")}
all_runs <- all_results[[1]]
tr_values <- all_results$truth
save(all_runs, tr_values, file = paste0("sim_res/all_runs_", job_params[1, 1],
                            "_ss_", job_params[1, 2],"_ft_", job_params$fam_type,
                            "_vt_", job_params$var_type, ".Rdata"))

### Reading out the rsults
#results <- all_results[[2]]
#mod_types <- names(results)
#fin_res <- results[[mod_types[1]]]
#for(bb in mod_types[-1]){
#  fin_res <- rbind(fin_res, results[[bb]])
#}
#d_fin_res <- cbind("var_type" = rep(mod_types, each = 3), fin_res)
### Writing the rsults to both a .csv file, and 
### saving all of the simulation results as a 
### .Rdata file.  
#write.csv(d_fin_res, paste0("sim_res/run_mod_", job_params[1, 1],
#                            "_ss_", job_params[1, 2],"_ft_", job_params$fam_type,
#                            "_vt_", job_params$var_type, ".csv"))
