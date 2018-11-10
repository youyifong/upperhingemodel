###########################################################
#### R-Script for reading simulation results
#### Author : Adam Elder
##########################################################



library(tidyverse)
library(kableExtra)
library(kyotil)

mod_types <- list("linear boot" = c("up_hinge_", "gaussian_"),
                  "linear modrob" = c("near_up_hinge_", "gaussian_"),
                  "binomial modrob" = c("up_hinge_", "binomial_"),
                  "binomial mod/modrob" = c("near_up_hinge_", "binomial_"))

c_name <- paste0(rep(c("Beta", "Beta_z", "Cpt"), each = 3), rep(c(" est", " se", "cov"), times = 3))
all_names <- paste0(rep(c_name, times = 2), rep(c(" uph", " seg"), each = 9))

est_mbsc <- function(mat, tr_v, type, ave_meas = mean, cut_ext = FALSE){
  mat_1 <- mat[, 1]; mat_2 <- mat[, 2]; mat_3 <- mat[, 3]

  #Removing all NA observations
  n_na_mat_1 <- mat_1[!is.na(mat_1)]; n_na_mat_2 <- mat_2[!is.na(mat_2)]
  n_na_mat_3 <- mat_3[!is.na(mat_3)]
  ## Determining the number of observations to ignore.
  num_obs <- length(n_na_mat_1); num_excl <- floor(num_obs/40)

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

get_line <- function(var_type, ss, param){
  ## This function gives a summary of the simulations completed
  ## using run_sims (from sim_fast_boot.R).  Depending on the
  ## data generating mechanism, different summaries will be given
  p_idx <- which(param == c("Beta", "Beta_z", "Cpt"))
  if (p_idx == 3){tp <- "cpt"}else{tp <- "est"}
  if (p_idx <= 2){meas <- mean; ce <- FALSE}else{meas <- median; ce <- TRUE}
  ## Read in the data set
  load(paste0("sim_res/all_runs_", mod_types[[var_type]][1], "ss_", ss, "_ft_",
              mod_types[[var_type]][2],"vt_", var_type, ".Rdata"))
  if(var_type %in% c(2, 4)){
    trv <- tr_values[p_idx]
  }else{
    trv <- c(1, 1, 3)[p_idx]
    }
  if (var_type %in% c(1, 3)){
    ##Selecting the reqired subset of the simulations for
    ##the summary of choice
    my_obs <- all_runs[[1]]
    seg_data <- my_obs[, 9 + (3 * p_idx - 2):(3 * p_idx)]
    uph_data <- my_obs[,     (3 * p_idx - 2):(3 * p_idx)]
    ## Summarizing the simulations (below)
    seg_val <- est_mbsc(seg_data, type = tp, tr_v = trv,
                        ave_meas = meas, cut_ext = ce)
    seg_sum <- round(seg_val, c(2, 0, 2, 2, 3, 2))
    uph_val <- est_mbsc(uph_data, type = tp, tr_v = trv,
                        ave_meas = meas, cut_ext = ce)
    uph_sum <- round(uph_val, c(2, 0, 2, 2, 3, 2))
    ## Placing the summary in a readable vector.
    if (p_idx == 3){
      est_bias_seg <- paste0(seg_sum[1])
      est_bias_uph <- paste0(uph_sum[1])
    }else{
      est_bias_seg <- paste0(seg_sum[1], " (", seg_sum[2], ")")
      est_bias_uph <- paste0(uph_sum[1], " (", uph_sum[2], ")")
      }
    all_res <- c(est_bias_seg,
                 paste0(seg_sum[5]), paste0(seg_sum[3]),
                 est_bias_uph,
                 paste0(uph_sum[5]), paste0(uph_sum[3]),
                 paste0(round(seg_val[3]/uph_val[3], 2)),
                 paste0(round(seg_val[6]/uph_val[6], 2)))
    ## Giving names to each value
    names(all_res) <- c(paste0(rep(c("Seg", "Uph"), each = 3),
                             rep(c(" Est(%Bias)", " Coverage", " MC SD"), times = 2)),
                        "SE Ratio", "IQR Ratio")

  }else if (var_type %in% c(2, 4)){
    ve_names <- names(all_runs)
    for(jj in 1:length(ve_names)){
      sub_data <- all_runs[[ve_names[jj]]]
      sub_data <- sub_data[, (3 * p_idx - 2):(3 * p_idx)]
      sum_data <- round(est_mbsc(sub_data, type = tp, tr_v = trv,
                                 ave_meas = meas, cut_ext = ce),
                               c(2, 0, 2, 2, 3, 2))
      if (jj == 1){
        if (p_idx == 3){
          est_bias <- paste0(sum_data[1])
        }else{
          est_bias <- paste0(sum_data[1], " (", sum_data[2], ")")
        }
        all_res <- c(est_bias,
                     paste0(sum_data[3]),
                     paste0(sum_data[5]), paste0(sum_data[4]))
        names(all_res) <- c("Est (% Bias)", "MC SD", paste0(ve_names[jj], " Cov (SE)"))
      } else {
        new_obs <- c(paste0(sum_data[5]),paste0(sum_data[4]))
        names(new_obs) <- paste0(ve_names[jj], c(" Cov", " (SE)"))
        all_res <- c(all_res,  new_obs)
      }
    }
  }
  return(all_res)
}

mk_tbl <- function(samp_sizes, params, v_typ){
  ## This function creates tables line by line using
  ## the get_line function.
  num_params <- length(params)
  n_ss       <- length(samp_sizes)
  comp_df    <- NULL
  for(pp in 1:num_params){
    sub_df <- c("ss" = samp_sizes[1],
                get_line(var_type = v_typ, ss = samp_sizes[1],
                         param = params[pp]))
    for(ss in 2:n_ss){
      sub_df <- rbind(sub_df, c("ss" = samp_sizes[ss],
                                get_line(var_type = v_typ, ss = samp_sizes[ss],
                                         param = params[pp])))
    }
    row.names(sub_df) <- rep(params[pp], n_ss)
    comp_df <- rbind(comp_df, sub_df)
  }
  return(comp_df)
}

## An example of how to convert the table created using mk_tbl into a Latex table.
res_1 <- mk_tbl(c(50, 100, 500), c("Beta_z", "Beta", "Cpt"), 1)[, -9]
row.names(res_1) <- NULL
colnames(res_1) <- c("n", rep(c(" Est(%Bias)", " Coverage", " MC SD"), times = 2),
                     "SD Ratio")

knitr::kable(res_1, format = "latex", booktabs = TRUE, align = c("r", rep("c", 7))) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  add_header_above(c(" ", "Segmented Model" = 3, "Upper Hinge Model" = 3, " " = 1)) %>%
  group_rows('Beta_Z', 1, 3) %>% 
  group_rows("Beta", 4, 6) %>%
  group_rows("Chpt", 7, 9)

res_2 <- mk_tbl(c(50, 100, 500), c("Beta_z", "Beta", "Cpt"), 2)
row.names(res_2) <- NULL
colnames(res_2) <- c("n", "Est(%Bias)", "MC SD", "Coverage", "SE", "Coverage", "SE")
res_2 <- res_2[, c(1, 2, 3, 6, 7, 4, 5)]

knitr::kable(res_2, format = "latex", booktabs = TRUE, align = c("r", rep("c", 6))) %>%
  kable_styling(latex_options = c("striped"), full_width = T) %>%
  add_header_above(c(" " = 3, "Model Based" = 2, "Bootstrap Based" = 2)) %>%
  group_rows("Beta_Z", 1, 3) %>% group_rows("Beta", 4, 6) %>%
  group_rows("Chpt", 7, 9)

res_3 <- mk_tbl(c(50, 100, 500, 2000), c("Beta_z", "Beta", "Cpt"), 3)
row.names(res_3) <- NULL
  colnames(res_3) <- c("n", rep(c(" Est(%Bias)", " Coverage", " MC SD"), times = 2),
                       "SE Ratio", "IQR Ratio")

knitr::kable(res_3, format = "latex", booktabs = TRUE, align = c("r", rep("c", 8))) %>%
  kable_styling(latex_options = c("striped", "scale_down")) %>%
  add_header_above(c(" ", "Segmented Model" = 3, "Upper Hinge Model" = 3, " " = 2)) %>%
  group_rows("Beta_Z", 1, 4) %>% group_rows("Beta", 5, 8) %>%
  group_rows("Chpt", 9, 12)

res_4 <- mk_tbl(c(50, 100, 500, 2000), c("Beta_z", "Beta", "Cpt"), 4)
row.names(res_4) <- NULL
colnames(res_4) <- c("n", "Est(%Bias)", "MC SD", 
                    "Coverage", "SE","Coverage", "SE")

knitr::kable(res_4, format = "latex", booktabs = TRUE, align = c("r", rep("c", 6))) %>%
  kable_styling(latex_options = c("striped"), full_width = T) %>%
  add_header_above(c(" " = 3, "Model Based" = 2, "Model Robust" = 2)) %>%
  group_rows("Beta_Z", 1, 4) %>% group_rows("Beta", 5, 8) %>%
  group_rows("Chpt", 9, 12)
