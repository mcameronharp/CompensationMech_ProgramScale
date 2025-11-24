library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(cowplot)
library(magrittr)
library(parallel)
library(pracma)

#----------- Set-up: Input parameters --------------------#
#Number of jurisdictions and observations per jurisdiction
#Set observations per jurisdiction
jur_size_vec <- c(25, 40, 50, 80)
#jur_size_vec <- c(1)

#Set net returns distribution
#Cover Cropping
cc_nr_mean <- 1
cc_nr_sd <- 1
#Conservation tillage
nt_nr_mean <- 1
nt_nr_sd <- 1

#Set soil carbon sequestration distribution
c_outc_distr_choice <- 2 #IF this equals "1", then it's a uniform draw. If it's "2" then it's a normal draw.
#Lower and upper bounds when it's a uniform distribution draw
unif_outc_lb <- 0
unif_outc_ub <- 1
#Mean and standard deviation when it's a normal distribution draw
#Made it so it has the same mean and standard deviation as the uniform by default
norm_outc_mean <- 0.5
norm_outc_sd <- 0.2

#Set measurement error parameter values
e_nr_mean <- 0 #Mean of policy makers' observation error in estimating the net returns of producers
e_nr_sd_vec <- c(seq(from = 0.5, to = 1.5, by = 0.05))
e_c0_mean <- 0 #Mean of error for policy makers' estimate of initial soil carbon stock
e_c0_sd_vec <- c(seq(from = 0.1, to = .3, by = 0.01))   #Standard deviation of error for policy makers' estimate of initial soil carbon stock

#Chose number of replicates for each combination of model parameters
replicates <- 100
#Define function inputs as columns of an expanded matrix based on the "_vec" variables
jur_size <- rep(expand.grid(jur_size_vec, e_nr_sd_vec, e_c0_sd_vec)[ , 1], replicates)
e_nr_sd <- rep(expand.grid(jur_size_vec, e_nr_sd_vec, e_c0_sd_vec)[ , 2], replicates)
e_c0_sd <- rep(expand.grid(jur_size_vec, e_nr_sd_vec, e_c0_sd_vec)[ , 3], replicates)
######

#Set policy incentives
#ppp_payment <- 0.5 #payment-for-practice incentive
ppco2_payment <- 1 #payment-per-unit-sequestered incentive
theta <- 0.1 #reverse auction efficiency measure. If a producers nr is negative, they bid (1+theta)*|nr| to adopt

#Set whether unbiased jurisdictional baselines should be used, '1' means to use unbiased baselines
use_unbiased_bl <- 0
use_reverse_auction <- 0 #Set equal to one if wanting to use reverse auction enrollment mechanism

#Number of model runs
total_model_runs <- length(jur_size)
run_num <- seq(1, total_model_runs, by = 1)

#--Call R script containing the jurisdictional model
source("model_function_revised_fixbudget.R")
#Load in the program that determines the action-based payment to equalize total program cost with results-based program
source("act_based_payment_finder.R")

system.time(output_list <- mcmapply(toy_model_run, run_num, jur_size, e_nr_sd, e_c0_sd,
                                    MoreArgs=list(cc_nr_mean, cc_nr_sd,
                                                  c_outc_distr_choice, unif_outc_lb, unif_outc_ub, norm_outc_mean, norm_outc_sd,
                                                  e_nr_mean, e_c0_mean,
                                                  ppco2_payment, theta, use_unbiased_bl, use_reverse_auction),
                                    mc.preschedule = TRUE,
                                    mc.cores = getOption("mc.cores", 2L)))

output_df <- data.frame(matrix(ncol=49,nrow=1,
                               dimnames=list(NULL, c(
                                 #Input parameters and baseline outcomes
                                 "run_num", "num_jur", "jur_size",
                                 "cc_nr_mean", "cc_nr_sd",
                                 "e_c0_mean", "e_c0_sd",
                                 "e_nr_mean", "e_nr_sd",
                                 "c_outc_distr_choice",
                                 "unif_outc_lb", "unif_outc_ub",
                                 "norm_outc_mean", "norm_outc_sd",
                                 "ppp_payment", "ppco2_payment", "theta",
                                 "use_unbiased_bl",
                                 "total_bl_adopt", "total_bl_adopt_hat",
                                 "total_bl_outc",
                                 #PPP outputs
                                 "total_add_outc_act",
                                 "total_jur_enrolled_act",
                                 "total_plots_enrolled_act",
                                 "total_credits_act",
                                 "total_spur_adopt_act",
                                 "total_eff_gain_act",
                                 "total_typ2_credits_act",
                                 "total_payments_rec_act",
                                 "total_jur_exp_act",
                                 "total_jur_eff_incl_act",
                                 "total_jur_eff_excl_act",
                                 "total_jur_ineff_incl_act",
                                 "total_jur_ineff_excl_act",
                                 #PPCO2 outputs
                                 "total_add_outc_res",
                                 "total_jur_enrolled_res",
                                 "total_plots_enrolled_res",
                                 "total_credits_res",
                                 "total_spur_adopt_res",
                                 "total_eff_gain_res",
                                 "total_typ2_credits_res",
                                 "total_overest_adopt_res",
                                 "total_overest_true_outc_res",
                                 "total_payments_rec_res",
                                 "total_jur_exp_res",
                                 "total_jur_eff_incl_res",
                                 "total_jur_eff_excl_res",
                                 "total_jur_ineff_incl_res",
                                 "total_jur_ineff_excl_res"
                               ))))

#Add output to the output dataframe
for (i in 1:ncol(output_list)) {
  output_df <- rbind(output_df, output_list[ , i]) #So we're binding columns of the output_list as rows in output_df
}
output_df <- output_df %>% dplyr::filter(!is.na(run_num)) #Get rid of the row with all missing values

#Save output
cur_date <- Sys.Date()
saveRDS(output_df, file = paste0("/homes/mcameronharp/Downloads/applied_theory_paper_results/jurmodel_fig3pos1_", replicates, "reps_fixedbuget_", cur_date, ".rds"))
