###########################################################
#### R-Script for Running Simulations for change point 2
#### Author : Adam Elder
##########################################################

library(chngpt)
library(splines)

expit <- function(x) exp(x)/(1 + exp(x))

covered <- function(truth, ci){
  return(as.numeric(truth >= ci[1] && truth <= ci[2]))
}
apr_up_hg <- function(slope, cpt, int, a, x){
  int + slope * (x - cpt) - (slope/a) * log(1 + exp(a * (x - cpt) ))
}

get_est_cov <- function(model, truth){
  ## This function takes the model fits, and outputs the
  ## coefficient estimates, standard error, and an 
  ## indicator if the 95% CI contains the truth.
  if(model$threshold.type == "upperhinge"){
    b_x <- "(x-chngpt)-"; b_z <- "z"; co <- "chngpt"
  }else if (model$threshold.type == "segmented"){
    b_x <- "x"; b_z <- "z"; co <- "chngpt"
  }
  if (model$var.type == "bootstrap"){
    mod_sd <- apply(model$vcov$symm, 2, diff)/(1.96*2)
    e_ci_b <- c(model$coefficients[b_x], mod_sd[b_x], model$vcov$symm[, b_x])
    e_ci_bz<- c(model$coefficients[b_z], mod_sd[b_z], model$vcov$symm[, b_z])
    e_ci_e <- c(model$coefficients[co], mod_sd[co], model$vcov$symm[, co])
  }else if(model$var.type %in% c("model", "robust")){
    mod_sum <- summary(model)
    mod_sd <- sqrt(diag(model$vcov))
    e_ci_b <- c(mod_sum$coefficients[b_x, 1], mod_sd[b_x], mod_sum$coefficients[b_x, 3:4])
    e_ci_bz<- c(mod_sum$coefficients[b_z, 1], mod_sd[b_z], mod_sum$coefficients[b_z, 3:4])
    e_ci_e <- c(mod_sum$chngpt[1], mod_sd[co], mod_sum$chngpt[2:3])
  }
  cov_est <- c(e_ci_b[1], e_ci_b[2], covered(truth[1], e_ci_b[3:4]),
               e_ci_bz[1], e_ci_bz[2], covered(truth[2], e_ci_bz[3:4]),
               e_ci_e[1], e_ci_e[2], covered(truth[3], e_ci_e[3:4]))
  names(cov_est) <- paste0(rep(c("Beta", "Beta_z", "Cpt"), each = 3), 
                           rep(c(" est", " se", " cov"), times = 3))
  return(cov_est)
}

est_mbsc <- function(mat, tr_v, type, ave_meas = mean, cut_ext = FALSE){
  ## This function takes all the simulated results for specific
  ## parameter estimate, and sumarizes the results as 
  ## Mean, Biase, Standard error, Coverage, and IQR. 
  mat_1 <- mat[, 1]; mat_2 <- mat[, 2]; mat_3 <- mat[, 3]
  #Removing all NA observations
  n_na_mat_1 <- mat_1[!is.na(mat_1)]; n_na_mat_2 <- mat_2[!is.na(mat_2)]
  n_na_mat_3 <- mat_3[!is.na(mat_3)]
  ## Determining the number of observations to ignore.
  num_obs <- length(n_na_mat_1); num_excl <- floor(num_obs/20)
  
  if(cut_ext == TRUE){
    ## Cutting out extreme values if requested
    ne_ests <- sort(n_na_mat_1)[-c(0:num_excl, (num_obs - num_excl):(num_obs))]
  }else{
    ne_ests <- n_na_mat_1
  }
  if (type == "est"){
    div <- tr_v
  }else if(type == "cpt"){
    ## using interquartlie range as denominator 
    ## for the bias of changepoint.
    div <- quantile(ne_ests, c(0.75)) - quantile(ne_ests, c(0.25))
  }
  res <- c(ave_meas(ne_ests), 100 * (ave_meas(ne_ests) - tr_v)/div,
           sd(ne_ests), mean(n_na_mat_2), mean(n_na_mat_3), IQR(ne_ests))
  names(res) <- c("Mean", "Bias", "MC SD", "Mean SE", "COV", "IQR")
  return(res)
}

gen_data <- function(mod, sample_size = 100, mod_type = "linear"){
  ## This function is used to generate our data, one can specify 
  ## the data generating mechanism by both the model type (linear
  ## versus binomial), and the relationship between X and Y (upper 
  ## hinge, or near upper hinge).
  if (mod == "up_hinge"){
    x <- runif(n = sample_size, min = -1, max = 6)
    z <- runif(n = sample_size, min =  2, max = 4)
    if( mod_type == "logit"){
        logit_prb <- z + pmin(x - 3, 0)
        y <- rbinom(n = sample_size, size = 1, prob = expit(logit_prb))
      }else{
        y <- z + pmin(x - 3, 0) + rnorm(n = sample_size)
      }
  }
  if (mod == "near_up_hinge"){
    x <- runif(n = sample_size, min = -1, max = 6)
    z <- runif(n = sample_size, min = 2, max = 4)
    y <- z + apr_up_hg(1, 3, 0, 5, x) +  rnorm(n = sample_size)
    if (mod_type == "logit"){
         logit_prb <- z + apr_up_hg(1, 3, 0, 5, x)
         y <- rbinom(n = sample_size, size = 1, prob = expit(logit_prb))
     }
  }
  return(data.frame(y, z,x))
}

convert_table <- function(table){
  ## This function was used to create a table of all the summary results
  ## by combining various measures and rounding them appropriately.
  uhg_data <- c(paste0(round(table[1, 1], 2), "(", round(table[1, 2], 0), ")"),
                paste0(round(table[1, 5], 3), "(",
                       round(table[1, 3], 2), "/", round(table[1, 6], 2), ")"),
                paste0(round(table[1, 4], 2)))
  seg_data <- c(paste0(round(table[2, 1], 2), "(", round(table[2, 2], 0), ")"),
                paste0(round(table[2, 5], 3), "(",
                       round(table[2, 3], 2), "/", round(table[2, 6], 2), ")"),
                paste0(round(table[2, 4], 2)))
  res <- c(seg_data, uhg_data, round(c(table[2, 3]/table[1, 3], table[2, 6]/table[1, 6]), 3))
  names(res) <- c(paste0(rep(c("Seg ", "Uph "), each = 3),
                         rep(c("Est(Bias)", "Cov(MC SD/IQR)", "Mean SE"), times = 2)),
                  "SE Ratio", "IQR Ratio")
  return(res)
}

run_sims <- function(model, ss, mc_runs = 1000, fam_type = "gaussian",
                     conf_type = "bootstrap"){
  ## This function is the workhorse of the simulation.  Most other functions
  ## in this script are called within this function to help it run.
  fc <- 0
  if(fam_type == "gaussian"){gen_meth <- "linear"; est_meth <- "fastgrid2"; ee <- FALSE}
  if(fam_type == "binomial"){gen_meth <- "logit"; est_meth <- "smoothapprox"; ee <- TRUE}
  ## Finding true parameter values by running a model with a very large sample size:
  large_data <- gen_data(mod = model, sample_size = 10000, mod_type = gen_meth)
  big_up <- chngptm(formula.1 = y ~ z, formula.2 = ~x,
                    family = fam_type, large_data,
                    type = "upperhinge", est.method = est_meth,
                    var.type = "model")
  true_b <- big_up$coefficients["(x-chngpt)-"]
  true_bz <- big_up$coefficients["z"]
  true_e <- big_up$coefficients["chngpt"]
  true_vals <- c(true_b, true_bz, true_e)
  cat(true_b, true_bz, true_e, "\n")
  sim_res <- rep(list(matrix(NA, nrow = mc_runs, ncol = 18)), length(conf_type))
  names(sim_res) <- conf_type
  for(mc_idx in 1:mc_runs){
    ## now running simulations many times
    set.seed(mc_idx)
    tryCatch(
      {
        if(mc_idx%%500 == 0){ cat(mc_idx, " ")}
        ## first generating data
        mc_data <- gen_data(mod = model, sample_size = ss, mod_type = gen_meth)
        ## Then fitting the model using one of the three variance types. 
        if ("bootstrap" %in% conf_type){
          uph_mod <- chngptm(formula.1 = y ~ z, formula.2 = ~x,
                             family = fam_type, mc_data,
                             type = "upperhinge", est.method = est_meth,
                             var.type = "bootstrap", ci.bootstrap.size = 1000)

          seg_mod <- chngptm(formula.1 = y ~ z, formula.2 = ~x,
                             family = fam_type, mc_data,
                             type = "segmented", est.method = est_meth,
                             var.type = "bootstrap", ci.bootstrap.size = 1000)
          sim_res[["bootstrap"]][mc_idx, ] <- c(get_est_cov(uph_mod, true_vals),
                                                get_est_cov(seg_mod, true_vals))
        }
        if ("modrob" %in% conf_type){
          uph_mod2 <- chngptm(formula.1=y~z, formula.2=~x, family = fam_type, mc_data,
                              type="upperhinge", est.method=est_meth, var.type="robust",
                              aux.fit=glm(y~z+ns(x,2), family = fam_type, mc_data))

          seg_mod2 <- chngptm(formula.1=y~z, formula.2=~x, family = fam_type, mc_data,
                              type="segmented", est.method=est_meth, var.type="robust",
                              aux.fit=glm(y~z+ns(x,2), family = fam_type, mc_data))
          if( is.null(uph_mod2$vcov) | is.null(seg_mod2$vcov)){
            sim_res[["modrob"]][mc_idx, ] <- rep(NA, 18)
            fc <- fc + 1
          }else{
            sim_res[["modrob"]][mc_idx, ] <- c(get_est_cov(uph_mod2, true_vals),
                                               get_est_cov(seg_mod2, true_vals))
          }
        }
        if ("model" %in% conf_type){
          uph_mod3 <- chngptm(formula.1=y~z, formula.2=~x, family = fam_type, mc_data,
                              type="upperhinge", est.method = est_meth, var.type="model")
          seg_mod3 <- chngptm(formula.1=y~z, formula.2=~x, family = fam_type, mc_data,
                              type="segmented", est.method = est_meth, var.type="model")
          if( is.null(uph_mod3$vcov) | is.null(seg_mod3$vcov)){
            sim_res[["model"]][mc_idx, ] <- rep(NA, 18)
            fc <- fc + 1
          }else{
            sim_res[["model"]][mc_idx, ] <- c(get_est_cov(uph_mod3, true_vals),
                                              get_est_cov(seg_mod3, true_vals))
          }
        }
      },
      ## Here, if the model fails, the try_catch function
      ## will catch this error, and run the data with a new
      ## dataset.  Failures normally occur due to perfect 
      ## seperation. 
      error = function(error_message){
        warning(error_message)
        message(error_message)
        cat("woops, the current seed is ",  mc_idx, "\n")
      }
    )
  }
  ## The remaining portion of this function is devoted to converting
  ## the simulated data into a readable table.  
  final_res_mat <- matrix(NA, nrow = 6, ncol = 6)
  colnames(final_res_mat) <- c("Mean_est", "percent Bias", "MC SD",
                               "Mean_SE", "MC coverage", "MC IQR")
  rownames(final_res_mat) <- c("UH_B", "Seg_B", "UH_Bz", "Seg_Bz", "UH_e", "Seg_e")
  final_res <- rep(list(final_res_mat), length(conf_type))
  final_result <- rep(list(NA), length(conf_type))
  names(final_res) <- names(final_result) <- conf_type
  if(fam_type == "gaussian"){a_m <- mean}else
    if(fam_type == "binomial"){
      a_m <- median
    }
  cat( "\n" )
#  browser()
  for(bb in conf_type){
    final_res[[bb]][1, ] <- est_mbsc(sim_res[[bb]][, 1:3], true_b, "est", a_m, cut_ext = ee)
    final_res[[bb]][2, ] <- est_mbsc(sim_res[[bb]][, 10:12], true_b, "est", a_m, cut_ext = ee)
    final_res[[bb]][3, ] <- est_mbsc(sim_res[[bb]][, 4:6], true_bz, "est", a_m, cut_ext = ee)
    final_res[[bb]][4, ] <- est_mbsc(sim_res[[bb]][, 13:15], true_bz, "est", a_m, cut_ext = ee)
    final_res[[bb]][5, ] <- est_mbsc(sim_res[[bb]][, 7:9], true_e, "cpt", a_m, cut_ext = ee)
    final_res[[bb]][6, ] <- est_mbsc(sim_res[[bb]][, 16:18], true_e, "cpt", a_m, cut_ext = ee)
    print(final_res[[bb]])
    final_result[[bb]] <- rbind("Slope" = convert_table(final_res[[bb]][1:2, ]),
                                "BetaZ" = convert_table(final_res[[bb]][3:4, ]),
                                "Thrsh" = convert_table(final_res[[bb]][5:6, ]))
  }
  cat( "\n" )
  cat("faild simulations (no variances)", fc, "\n")
  return(list(sim_res, final_result, "truth" = true_vals))
}
