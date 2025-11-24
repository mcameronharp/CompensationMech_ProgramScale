library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(cowplot)
library(magrittr)
library(parallel)
library(pracma)
library(scales)

#Load in the function to run the model and output individual level results
source("C:/Users/Micah/Dropbox/2022 EDF carbon market/applied theory paper/app.theory.sim/figure_scripts/newfigure_histograms_participants_errors_vs_true_outcomes/model function_revised_12-21.R")

#----------- Set-up: Input parameters --------------------#
#Number of jurisdictions and observations per jurisdiction
#Set observations per jurisdiction
#jur_size_vec <- c(20, 25, 40, 50, 80, 100)
jur_size_vec <- c(1)

#Set net returns distribution
#Cover Cropping
cc_nr_mean <- -0.5
cc_nr_sd <- 1
#Conservation tillage
nt_nr_mean <- -0.5
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
e_nr_sd_vec <- c(0, 0.5, 1)
#e_nr_sd_vec <- 0
e_c0_mean <- 0 #Mean of error for policy makers' estimate of initial soil carbon stock
e_c0_sd_vec <- c(0, .1, .2)    #Standard deviation of error for policy makers' estimate of initial soil carbon stock

#Chose number of replicates for each combination of model parameters
replicates <- 1
#Define function inputs as columns of an expanded matrix based on the "_vec" variables
jur_size <- rep(expand.grid(jur_size_vec, e_nr_sd_vec, e_c0_sd_vec)[ , 1], replicates)
e_nr_sd <- rep(expand.grid(jur_size_vec, e_nr_sd_vec, e_c0_sd_vec)[ , 2], replicates)
e_c0_sd <- rep(expand.grid(jur_size_vec, e_nr_sd_vec, e_c0_sd_vec)[ , 3], replicates)
######

#Set policy incentives
ppp_payment <- 0.5 #payment-for-practice incentive
ppco2_payment <- 1 #payment-per-unit-sequestered incentive
theta <- 0.1 #reverse auction efficiency measure. If a producers nr is negative, they bid (1+theta)*|nr| to adopt

#Set whether unbiased jurisdictional baselines should be used, '1' means to use unbiased baselines
use_unbiased_bl <- 0
use_reverse_auction <- 0 #Set equal to one if wanting to use reverse auction enrollment mechanism

#Number of model runs
total_model_runs <- length(jur_size)
run_num <- seq(1, total_model_runs, by = 1)

output_list <- mapply(toy_model_run, run_num, jur_size, e_nr_sd, e_c0_sd,
                      MoreArgs=list(cc_nr_mean, cc_nr_sd,
                                    c_outc_distr_choice, unif_outc_lb, unif_outc_ub, norm_outc_mean, norm_outc_sd,
                                    e_nr_mean, e_c0_mean,
                                    ppp_payment, ppco2_payment, theta, use_unbiased_bl, use_reverse_auction))

#Create variables for errors, select participating jurisdictions
test <- as.data.frame(output_list[,9])
colnames(test)
test <- test %>%
  dplyr::mutate(e_nr = nr_hat - nr,
                e_c0 = pot_outc_hat - pot_outc)

#Start with action-based
act_part <- subset(test, jur_part_act==1)
hist(act_part$pot_outc_hat)
hist(act_part$e_c0)
hist(act_part$e_nr)
act_part <- act_part %>%
  dplyr::mutate(program = "act") %>%
  dplyr::select(program,
                nr, e_nr, nr_hat,
                pot_outc, e_c0, pot_outc_hat)

#Now do Modeled-results
res_part <- subset(test, jur_part_res==1)
hist(res_part$pot_outc_hat)
hist(res_part$e_c0)
hist(res_part$e_nr)
res_part <- res_part %>%
  dplyr::mutate(program = "res") %>%
  dplyr::select(program,
                nr, e_nr, nr_hat,
                pot_outc, e_c0, pot_outc_hat)

#Combine the two
combined_participants <- rbind(res_part, act_part)
  #Save the results so we don't have to run model later
  saveRDS(combined_participants, file = "C:/Users/Micah/Dropbox/2022 EDF carbon market/applied theory paper/Ecological Economics/data/figure_a1_histograms_10_19.rds")

check <- test %>%
  dplyr::filter(jur_part_res==1 & nr_hat > 0) %>%
  dplyr::mutate(check = ppco2_payment*(jur_pot_est_outc_res-jur_bl_outc_hat)-jur_total_cost_res) %>%
  dplyr::select(nr, nr_hat, pot_outc, pot_outc_hat, pot_part_jur_res, jur_part_res, check, jur_bl_hat, jur_bl, jur_bl_outc, jur_bl_outc_hat, jur_pot_est_outc_res, jur_bl_outc_hat, jur_total_cost_res, everything())


#Try to make a grouped histogram

#Net return plots
  #Set the bin width
  nr_bin_width = 0.1
    #Plot net returns
    nr <- combined_participants %>%
      ggplot(aes(x=nr, fill=program)) +
      geom_histogram(aes(y = nr_bin_width*..density..), boundary = 0,
                     color="white", alpha=0.4, position = 'identity', binwidth = nr_bin_width) +
      scale_fill_manual(values=c("darkorange", "navy"), labels = c("Action-based", "Modeled-results")) +
      labs(fill="") +
      scale_x_continuous(limits = c(-2, 2)) +
      scale_y_continuous(labels = percent_format()) +
      xlab(latex2exp::TeX("$\\r_{ij}$")) +
      ylab(element_blank()) +
      theme_bw() +
      theme(text=element_text(size=16,  family="serif"),
            plot.title = element_text(hjust = 0.5))
    nr

    #Plot error in net returns
    e_nr <- combined_participants %>%
      ggplot(aes(x=e_nr, fill=program)) +
      geom_histogram(aes(y = nr_bin_width*..density..), boundary = 0,
                     color="white", alpha=0.4, position = 'identity', binwidth = nr_bin_width) +
      scale_fill_manual(values=c("darkorange", "navy")) +
      labs(fill="") +
      scale_y_continuous(labels = percent_format()) +
      xlab(latex2exp::TeX("$\\hat{\\r}_{i,j}$")) +
      ylab(element_blank()) +
      theme_bw() +
      theme(text=element_text(size=16,  family="serif"),
            plot.title = element_text(hjust = 0.5))
    e_nr

    #Plot estimated net returns
    nr_hat <- combined_participants %>%
      ggplot(aes(x=nr_hat, fill=program)) +
      geom_histogram(aes(y = nr_bin_width*..density..), boundary = 0,
                     color="white", alpha=0.4, position = 'identity', binwidth = nr_bin_width) +
      scale_fill_manual(values=c("darkorange", "navy")) +
      labs(fill="") +
      scale_x_continuous(limits = c(-4, 0)) +
      scale_y_continuous(labels = percent_format()) +
      xlab(latex2exp::TeX("$\\hat{\\r}_{ij}$")) +
      ylab(element_blank()) +
      theme_bw() +
      theme(text=element_text(size=16,  family="serif"),
            plot.title = element_text(hjust = 0.5))
    nr_hat

#Outcome plots
  #Set the bin width
  outc_bin_width = 0.05
    #Plot true outcomes
    outc <- combined_participants %>%
      ggplot(aes(x=pot_outc, fill=program)) +
      geom_histogram(aes(y = outc_bin_width*..density..), boundary = 0,
                     color="white", alpha=0.4, position = 'identity', binwidth = outc_bin_width) +
      scale_fill_manual(values=c("darkorange", "navy")) +
      labs(fill="") +
      scale_y_continuous(labels = percent_format()) +
      xlab(latex2exp::TeX("$\\c_{ij}$")) +
      ylab(element_blank()) +
      theme_bw() +
      theme(text=element_text(size=16,  family="serif"),
            plot.title = element_text(hjust = 0.5))
    outc

    #Plot estimated outcomes
    outc_hat <- combined_participants %>%
      ggplot(aes(x=pot_outc_hat, fill=program)) +
      geom_histogram(aes(y = outc_bin_width*..density..), boundary = 0,
                     color="white", alpha=0.4, position = 'identity', binwidth = outc_bin_width) +
      scale_fill_manual(values=c("darkorange", "navy")) +
      labs(fill="") +
      scale_y_continuous(labels = percent_format()) +
      xlab(latex2exp::TeX("$\\hat{\\c}_{ij}$")) +
      ylab(element_blank()) +
      theme_bw() +
      theme(text=element_text(size=16,  family="serif"),
            plot.title = element_text(hjust = 0.5))
    outc_hat

    #Plot error in outcomes
    e_outc <- combined_participants %>%
      ggplot(aes(x=e_c0, fill=program)) +
      geom_histogram(aes(y = outc_bin_width*..density..), boundary = 0,
                     color="white", alpha=0.4, position = 'identity', binwidth = outc_bin_width) +
      scale_fill_manual(values=c("darkorange", "navy")) +
      labs(fill="") +
      scale_y_continuous(labels = percent_format()) +
      theme_bw()
    e_outc

#First Figure A.1
    #Grab the legend from one graph
    leg_plot <- nr + theme(legend.position = 'bottom',
                                      legend.key=element_blank(),
                                      legend.key.size = unit(1, "cm"),
                                      legend.text=element_text(size=14)) +
      guides(linetype = guide_legend(ncol = 2,
                                     title.position = "top",
                                     override.aes = list(color = c("darkorange", "navy"))))

    ggdraw(leg_plot)
    legend <- get_plot_component(leg_plot, "guide-box-bottom", return_all = TRUE)
    ggdraw(legend)

    #Combine the outcomes and net returns plots graphs
    figa1 <- plot_grid(
      outc + theme(legend.position="none"),
      nr + theme(legend.position="none"),
      align = 'vh',
      labels = c("(a)", "(b)", "(c)"),
      label_x = .03,
      label_y = 0.99,
      label_size = 16,
      label_fontface = "plain",
      hjust = -2,
      nrow = 1
    )
    ggdraw(figa1)

    #Add titel
    title <- ggdraw() + draw_label("Participants' ES outcomes and returns to adoption", fontfamily='serif', size = 16)

    #Add the legend at the bottom
    final_figa1_plot <- plot_grid(
      title,
      figa1,
      legend,
      nrow = 3,
      rel_heights = c(0.1,1, 0.1))
    ggdraw(final_figa1_plot)

    #Save
    ggsave2(filename = "figurea1.tiff",
            plot = final_figa1_plot,
            device = "tiff",
            path = "C:/Users/Micah/Dropbox/2022 EDF carbon market/applied theory paper/Ecological Economics/figures",
            width = 9.3,
            height = 6.5,
            dpi = 500,
            units = "in")

#########################
    #########################
    #########################
    #########################
    #########################
    #########################
    #########################

    #### Figure A2: Histograms of errors and true vs. estimated outcomes and returns for participants only ####
    #Create variables for errors, select participating jurisdictions
    test <- as.data.frame(output_list[,9])
    colnames(test)
    test <- test %>%
      dplyr::mutate(e_nr = nr_hat - nr,
                    e_c0 = pot_outc_hat - pot_outc)

    #Start with action-based
    act_part <- subset(test, jur_part_act==1)
    hist(act_part$pot_outc_hat)
    hist(act_part$e_c0)
    hist(act_part$e_nr)
    act_part <- act_part %>%
      dplyr::mutate(program = "act") %>%
      dplyr::select(program,
                    nr, e_nr, nr_hat,
                    pot_outc, e_c0, pot_outc_hat)

    #Now do Modeled-results
    res_part <- subset(test, jur_part_res==1)
    hist(res_part$pot_outc_hat)
    hist(res_part$e_c0)
    hist(res_part$e_nr)
    res_part <- res_part %>%
      dplyr::mutate(program = "res") %>%
      dplyr::select(program,
                    nr, e_nr, nr_hat,
                    pot_outc, e_c0, pot_outc_hat)

    #Combine the two
    combined_participants <- rbind(res_part, act_part)
      #Save the results so we don't have to run model later
      saveRDS(combined_participants, file = "C:/Users/Micah/Dropbox/2022 EDF carbon market/applied theory paper/Ecological Economics/data/figure_a2_histograms_10_19.rds")

    #Net return plots
    #Set the bin width
    nr_bin_width = 0.1
    #Plot net returns
    nr <- combined_participants %>%
      ggplot(aes(x=nr, fill=program)) +
      geom_histogram(aes(y = nr_bin_width*..density..), boundary = 0,
                     color="white", alpha=0.4, position = 'identity', binwidth = nr_bin_width) +
      scale_fill_manual(values=c("darkorange", "navy"), labels = c("Action-based", "Modeled-results")) +
      labs(fill="") +
      scale_x_continuous(limits = c(-2, 2)) +
      scale_y_continuous(labels = percent_format()) +
      xlab(latex2exp::TeX("$\\r_{ij}$")) +
      ylab(element_blank()) +
      theme_bw() +
      theme(text=element_text(size=16,  family="serif"),
            plot.title = element_text(hjust = 0.5))
    nr

    #Plot error in net returns
    e_nr <- combined_participants %>%
      ggplot(aes(x=e_nr, fill=program)) +
      geom_histogram(aes(y = nr_bin_width*..density..), boundary = 0,
                     color="white", alpha=0.4, position = 'identity', binwidth = nr_bin_width) +
      scale_fill_manual(values=c("darkorange", "navy")) +
      labs(fill="") +
      scale_y_continuous(labels = percent_format()) +
      xlab(latex2exp::TeX("$\\epsilon_{ij}^{r}$")) +
      ylab(element_blank()) +
      theme_bw() +
      theme(text=element_text(size=16,  family="serif"),
            plot.title = element_text(hjust = 0.5))
    e_nr

    #Plot estimated net returns
    nr_hat <- combined_participants %>%
      ggplot(aes(x=nr_hat, fill=program)) +
      geom_histogram(aes(y = nr_bin_width*..density..), boundary = 0,
                     color="white", alpha=0.4, position = 'identity', binwidth = nr_bin_width) +
      scale_fill_manual(values=c("darkorange", "navy")) +
      labs(fill="") +
      scale_x_continuous(limits = c(-4, 0)) +
      scale_y_continuous(labels = percent_format()) +
      xlab(latex2exp::TeX("$\\hat{\\r}_{ij}$")) +
      ylab(element_blank()) +
      theme_bw() +
      theme(text=element_text(size=16,  family="serif"),
            plot.title = element_text(hjust = 0.5))
    nr_hat

    #Outcome plots
    #Set the bin width
    outc_bin_width = 0.05
    #Plot true outcomes
    outc <- combined_participants %>%
      ggplot(aes(x=pot_outc, fill=program)) +
      geom_histogram(aes(y = outc_bin_width*..density..), boundary = 0,
                     color="white", alpha=0.4, position = 'identity', binwidth = outc_bin_width) +
      scale_fill_manual(values=c("darkorange", "navy")) +
      labs(fill="") +
      scale_y_continuous(labels = percent_format()) +
      xlab(latex2exp::TeX("$\\c_{ij}$")) +
      ylab(element_blank()) +
      theme_bw() +
      theme(text=element_text(size=16,  family="serif"),
            plot.title = element_text(hjust = 0.5))
    outc

    #Plot estimated outcomes
    outc_hat <- combined_participants %>%
      ggplot(aes(x=pot_outc_hat, fill=program)) +
      geom_histogram(aes(y = outc_bin_width*..density..), boundary = 0,
                     color="white", alpha=0.4, position = 'identity', binwidth = outc_bin_width) +
      scale_fill_manual(values=c("darkorange", "navy")) +
      labs(fill="") +
      scale_y_continuous(labels = percent_format()) +
      xlab(latex2exp::TeX("$\\hat{\\c}_{ij}$")) +
      ylab(element_blank()) +
      theme_bw() +
      theme(text=element_text(size=16,  family="serif"),
            plot.title = element_text(hjust = 0.5))
    outc_hat

    #Plot error in outcomes
    e_outc <- combined_participants %>%
      ggplot(aes(x=e_c0, fill=program)) +
      geom_histogram(aes(y = outc_bin_width*..density..), boundary = 0,
                     color="white", alpha=0.4, position = 'identity', binwidth = outc_bin_width) +
      scale_fill_manual(values=c("darkorange", "navy")) +
      labs(fill="") +
      scale_y_continuous(labels = percent_format()) +
      xlab(latex2exp::TeX("$\\epsilon_{ij}^{c}$")) +
      ylab(element_blank()) +
      theme_bw() +
      theme(text=element_text(size=16,  family="serif"),
            plot.title = element_text(hjust = 0.5))
    e_outc

    #Grab the legend from one graph
    leg_plot <- nr + theme(legend.position = 'bottom',
                           legend.key=element_blank(),
                           legend.key.size = unit(1, "cm"),
                           legend.text=element_text(size=14)) +
      guides(linetype = guide_legend(ncol = 2,
                                     title.position = "top",
                                     override.aes = list(color = c("darkorange", "navy"))))

    ggdraw(leg_plot)
    legend <- get_plot_component(leg_plot, "guide-box-bottom", return_all = TRUE)
    ggdraw(legend)

    #Combine the outcomes and net returns plots graphs
    figa2 <- plot_grid(
      nr + theme(legend.position="none"),
      nr_hat + theme(legend.position="none"),
      e_nr + theme(legend.position="none"),
      outc + theme(legend.position="none"),
      outc_hat + theme(legend.position="none"),
      e_outc + theme(legend.position="none"),
      align = 'vh',
      labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),
      label_x = .03,
      label_y = 0.99,
      label_size = 16,
      label_fontface = "plain",
      hjust = -2,
      nrow = 2
    )
    ggdraw(figa2)

    #Add titel
    title <- ggdraw() + draw_label("Participants' ES outcomes and returns", fontfamily='serif', size = 16)

    #Add the legend at the bottom
    final_figa2_plot <- plot_grid(
      title,
      figa2,
      legend,
      nrow = 3,
      rel_heights = c(0.1,1, 0.1))
    ggdraw(final_figa2_plot)

    #Save
    ggsave2(filename = "figurea2.tiff",
            plot = final_figa2_plot,
            device = "tiff",
            path = "C:/Users/Micah/Dropbox/2022 EDF carbon market/applied theory paper/Ecological Economics/figures",
            width = 9.3,
            height = 6.5,
            dpi = 500,
            units = "in")


