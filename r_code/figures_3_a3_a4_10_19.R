#Applied Theory Paper
#Figure 3
#Plotting average cost - action vs. results based as scale changes
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(cowplot)
library(magrittr)
library(ggthemes)
library(data.table)

#List files containing the data to create figure 3
figure3_files <- list.files(path = "C:/Users/Micah/Dropbox/2022 EDF carbon market/applied theory paper/app.theory.sim/data/figure2/",
                            pattern = '-10-14.rds',
                            full.names = TRUE)
figure3_files
output_df <- lapply(figure3_files, readRDS)
output_df <- rbindlist(output_df)

#output_df <- `indmodel_unifdistr_501reps_fig2_2025-02-04`
output_df %>% dplyr::select(e_nr_sd) %>% unique()
output_df %>% dplyr::select(cc_nr_mean) %>% unique()
output_df %>% dplyr::select(cc_nr_sd) %>% unique()

output_df %>% dplyr::select(e_c0_sd) %>% unique()
output_df %>% dplyr::select(norm_outc_mean) %>% unique()
output_df %>% dplyr::select(norm_outc_sd) %>% unique()

#Values that we are plotting
e_nr_sd_vec <- output_df %>% dplyr::select(e_nr_sd) %>% unique()
e_nr_sd_vec <- c(e_nr_sd_vec$e_nr_sd)
e_c0_sd_vec <- output_df %>% dplyr::select(e_c0_sd) %>% unique()
e_c0_sd_vec <- c(e_c0_sd_vec$e_c0_sd)
# e_c0_sd_vec <- c(.1, .2, .3)  #Standard deviation of error for policy makers' estimate of initial soil carbon stock

#######################################
### Additional participation
#######################################
colnames(output_df)
#Summarise to get single values for each point
# outcomes_df <- output_df %>%
#   filter(jur_size<=25) %>%
#   filter(e_nr_sd %in% e_nr_sd_vec & e_c0_sd %in% e_c0_sd_vec) %>%
#   dplyr::group_by(jur_size, e_nr_sd, e_c0_sd) %>%
#   summarise(n = n(),
#             pop_size = mean(num_jur*jur_size),
#             mean_addpart_act = mean((total_plots_enrolled_act-total_spur_adopt_act)/(total_plots_enrolled_act)),
#             sd_addpart_act = sd((total_plots_enrolled_act-total_spur_adopt_act)/(total_plots_enrolled_act)),
#             se_addpart_act = sd_addpart_act / sqrt(n),
#             mean_addpart_res = mean((total_plots_enrolled_res-total_spur_adopt_res)/(total_plots_enrolled_res)),
#             sd_addpart_res = sd((total_plots_enrolled_res-total_spur_adopt_res)/(total_plots_enrolled_res)),
#             se_addpart_res = sd_addpart_res / sqrt(n))

outcomes_df <- output_df %>%
  filter(jur_size<=25) %>%
  filter(e_nr_sd %in% e_nr_sd_vec & e_c0_sd %in% e_c0_sd_vec) %>%
  dplyr::group_by(jur_size, e_nr_sd, e_c0_sd) %>%
  summarise(n = n(),
            pop_size = mean(num_jur*jur_size),
            mean_addpart_act = mean(total_plots_enrolled_act-total_spur_adopt_act),
            sd_addpart_act = sd(total_plots_enrolled_act-total_spur_adopt_act),
            se_addpart_act = sd_addpart_act / sqrt(n),
            mean_addpart_res = mean(total_plots_enrolled_res-total_spur_adopt_res),
            sd_addpart_res = sd(total_plots_enrolled_res-total_spur_adopt_res),
            se_addpart_res = sd_addpart_res / sqrt(n))

#Use pivot longer so we can grph over the effects
outcomes_long <- outcomes_df %>%
  select(jur_size, e_nr_sd, e_c0_sd, starts_with("mean")) %>%
  pivot_longer(cols = starts_with("mean_"), names_to = "dep_var", names_prefix = "mean_", values_to = "mean")
sd_df <- outcomes_df %>%
  select(jur_size, e_nr_sd, e_c0_sd, starts_with("sd")) %>%
  pivot_longer(cols = starts_with("sd_"), names_to = "dep_var", names_prefix = "sd_", values_to = "sd")
se_df <- outcomes_df %>%
  select(jur_size, e_nr_sd, e_c0_sd, starts_with("se")) %>%
  pivot_longer(cols = starts_with("se_"), names_to = "dep_var", names_prefix = "se_", values_to = "se")
outcomes_long <- left_join(outcomes_long, sd_df, by = c("jur_size", "e_nr_sd", "e_c0_sd", "dep_var"))
outcomes_long <- left_join(outcomes_long, se_df, by = c("jur_size", "e_nr_sd", "e_c0_sd", "dep_var"))

#Plot the grid
cols <- c("addpart_act" = "dashed", "addpart_res" = "solid")
label_cols <- c("Action-based", "Modeled-results")

P <- ggplot(outcomes_long, aes(x = jur_size, y = mean/10000, linetype = dep_var, color = dep_var)) + geom_line() +
  facet_grid(rows = vars(e_c0_sd),
             cols = vars(e_nr_sd),
             scales = "free",
             labeller = label_bquote(cols = sigma[epsilon]^r == .(e_nr_sd),
                                     rows = sigma[epsilon]^c == .(e_c0_sd))) +
  scale_linetype_manual(name = element_blank(), values = cols, labels = label_cols) +
  scale_color_manual(values = c("darkorange", "navy"), guide = "none") +
  theme(legend.position = "bottom",
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white'),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        text=element_text(family="serif", size=14),
        axis.title.y = element_text(vjust = 0.5),
        strip.background = element_rect(colour = "black", fill = "white", linewidth = 1, linetype = "solid"),
        strip.placement = "outside",
        strip.switch.pad.grid = unit(0.5, "in"),
        plot.margin = margin(1,1,0,0, "in")) +
  annotate("segment", x=-Inf, xend=25, y=-Inf, yend=-Inf) +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=.225) +
  xlab(bquote(Jurisdiction~size*","*~N)) +
  ylab("Additionality of adoption (per capita)") +
  guides(linetype = guide_legend(override.aes = list(color = c("darkorange", "navy"))))
P

#Save
  ggsave("figure_a4.tiff",
         plot = P,
         device = "tiff",
         path = "C:/Users/Micah/Dropbox/2022 EDF carbon market/applied theory paper/Ecological Economics/figures",
         width = 6.5,
         height = 8,
         dpi = 500,
         units = "in")

#######################################
#Same thing but for additional outcomes
  colnames(output_df)
  #Summarise to get single values for each point
  outcomes_df <- output_df %>%
    filter(jur_size<=25) %>%
    filter(e_nr_sd %in% e_nr_sd_vec & e_c0_sd %in% e_c0_sd_vec) %>%
    dplyr::group_by(jur_size, e_nr_sd, e_c0_sd) %>%
    summarise(n = n(),
              pop_size = mean(num_jur*jur_size),
              mean_outc_act = mean(total_add_outc_act),
              sd_outc_act = sd(total_add_outc_act),
              se_outc_act = sd_outc_act / sqrt(n),
              mean_outc_res = mean(total_add_outc_res),
              sd_outc_res = sd(total_add_outc_res),
              se_outc_res = sd_outc_res / sqrt(n))

  #Use pivot longer so we can grph over the effects
  outcomes_long <- outcomes_df %>%
    select(jur_size, e_nr_sd, e_c0_sd, starts_with("mean")) %>%
    pivot_longer(cols = starts_with("mean_"), names_to = "dep_var", names_prefix = "mean_", values_to = "mean")
  sd_df <- outcomes_df %>%
    select(jur_size, e_nr_sd, e_c0_sd, starts_with("sd")) %>%
    pivot_longer(cols = starts_with("sd_"), names_to = "dep_var", names_prefix = "sd_", values_to = "sd")
  se_df <- outcomes_df %>%
    select(jur_size, e_nr_sd, e_c0_sd, starts_with("se")) %>%
    pivot_longer(cols = starts_with("se_"), names_to = "dep_var", names_prefix = "se_", values_to = "se")
  outcomes_long <- left_join(outcomes_long, sd_df, by = c("jur_size", "e_nr_sd", "e_c0_sd", "dep_var"))
  outcomes_long <- left_join(outcomes_long, se_df, by = c("jur_size", "e_nr_sd", "e_c0_sd", "dep_var"))

  #Plot the grid
  cols <- c("outc_act" = "dashed", "outc_res" = "solid")
  label_cols <- c("Action-based", "Modeled-results")

  P <- ggplot(outcomes_long, aes(x = jur_size, y = mean/10000, linetype = dep_var, color = dep_var)) + geom_line() +
    facet_grid(rows = vars(e_c0_sd),
               cols = vars(e_nr_sd),
               scales = "free",
               labeller = label_bquote(cols = sigma[epsilon]^r == .(e_nr_sd),
                                       rows = sigma[epsilon]^c == .(e_c0_sd))) +
    scale_linetype_manual(name = element_blank(), values = cols, labels = label_cols) +
    scale_color_manual(values = c("darkorange", "navy"), guide = "none") +
    theme(legend.position = "bottom",
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = 'white'),
          legend.background = element_blank(),
          legend.box.background = element_blank(),
          legend.key = element_blank(),
          text=element_text(family="serif", size=14),
          axis.title.y = element_text(vjust = 0.5),
          strip.background = element_rect(colour = "black", fill = "white", linewidth = 1, linetype = "solid"),
          strip.placement = "outside",
          strip.switch.pad.grid = unit(0.5, "in"),
          plot.margin = margin(1,1,0,0, "in")) +
    xlab(bquote(Jurisdiction~size*","*~N)) +
    ylab("Additional outcomes (per capita)") +
    annotate("segment", x=-Inf, xend=25, y=-Inf, yend=-Inf) +
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=.11) +
    guides(linetype = guide_legend(override.aes = list(color = c("darkorange", "navy"))))
  P

  #Save
  ggsave("figure3_10_19.tiff",
         plot = P,
         device = "tiff",
         path = "C:/Users/Micah/Dropbox/2022 EDF carbon market/applied theory paper/Ecological Economics/figures",
         width = 6.5,
         height = 8,
         dpi = 500,
         units = "in")

#######################################
  #Same thing but for inefficient participation
  colnames(output_df)
  #Summarise to get single values for each point
  outcomes_df <- output_df %>%
    filter(jur_size<=25) %>%
    filter(e_nr_sd %in% e_nr_sd_vec & e_c0_sd %in% e_c0_sd_vec) %>%
    dplyr::group_by(jur_size, e_nr_sd, e_c0_sd) %>%
    summarise(n = n(),
              pop_size = mean(num_jur*jur_size),
              mean_ineffinc_act = mean(total_jur_ineff_incl_act/num_jur),
              sd_ineffinc_act = sd(total_jur_ineff_incl_act/num_jur),
              se_ineffinc_act = sd_ineffinc_act / sqrt(n),
              mean_ineffinc_res = mean(total_jur_ineff_incl_res/num_jur),
              sd_ineffinc_res = sd(total_jur_ineff_incl_res/num_jur),
              se_ineffinc_res = sd_ineffinc_res / sqrt(n))

  #Use pivot longer so we can grph over the effects
  outcomes_long <- outcomes_df %>%
    select(jur_size, e_nr_sd, e_c0_sd, starts_with("mean")) %>%
    pivot_longer(cols = starts_with("mean_"), names_to = "dep_var", names_prefix = "mean_", values_to = "mean")
  sd_df <- outcomes_df %>%
    select(jur_size, e_nr_sd, e_c0_sd, starts_with("sd")) %>%
    pivot_longer(cols = starts_with("sd_"), names_to = "dep_var", names_prefix = "sd_", values_to = "sd")
  se_df <- outcomes_df %>%
    select(jur_size, e_nr_sd, e_c0_sd, starts_with("se")) %>%
    pivot_longer(cols = starts_with("se_"), names_to = "dep_var", names_prefix = "se_", values_to = "se")
  outcomes_long <- left_join(outcomes_long, sd_df, by = c("jur_size", "e_nr_sd", "e_c0_sd", "dep_var"))
  outcomes_long <- left_join(outcomes_long, se_df, by = c("jur_size", "e_nr_sd", "e_c0_sd", "dep_var"))

  #Plot the grid
  cols <- c("ineffinc_act" = "dashed", "ineffinc_res" = "solid")
  label_cols <- c("Action-based", "Modeled-results")

  P <- ggplot(outcomes_long, aes(x = jur_size, y = mean, linetype = dep_var, color = dep_var)) + geom_line() +
    facet_grid(rows = vars(e_c0_sd),
               cols = vars(e_nr_sd),
               scales = "free",
               labeller = label_bquote(cols = sigma[epsilon]^r == .(e_nr_sd),
                                       rows = sigma[epsilon]^c == .(e_c0_sd))) +
    scale_linetype_manual(name = element_blank(), values = cols, labels = label_cols) +
    scale_color_manual(values = c("darkorange", "navy"), guide = "none") +
    theme(legend.position = "bottom",
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = 'white'),
          legend.background = element_blank(),
          legend.box.background = element_blank(),
          legend.key = element_blank(),
          text=element_text(family="serif", size=14),
          axis.title.y = element_text(vjust = 0.5),
          strip.background = element_rect(colour = "black", fill = "white", linewidth = 1, linetype = "solid"),
          strip.placement = "outside",
          strip.switch.pad.grid = unit(0.5, "in"),
          plot.margin = margin(1,1,0,0, "in")) +
    xlab(bquote(Jurisdiction~size*","*~N)) +
    ylab("Inefficient participation \n (fraction of jurisdictions)") +
    annotate("segment", x=-Inf, xend=25, y=-Inf, yend=-Inf) +
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=.2) +
    guides(linetype = guide_legend(override.aes = list(color = c("darkorange", "navy"))))
  P

  #Save
  ggsave("figure_a3.tiff",
         plot = P,
         device = "tiff",
         path = "C:/Users/Micah/Dropbox/2022 EDF carbon market/applied theory paper/Ecological Economics/figures",
         width = 6.5,
         height = 8,
         dpi = 500,
         units = "in")

#######################################
#Same thing but for inefficient NON-participation
  colnames(output_df)
  #Summarise to get single values for each point
  outcomes_df <- output_df %>%
    filter(jur_size<=25) %>%
    filter(e_nr_sd %in% e_nr_sd_vec & e_c0_sd %in% e_c0_sd_vec) %>%
    dplyr::group_by(jur_size, e_nr_sd, e_c0_sd) %>%
    summarise(n = n(),
              pop_size = mean(num_jur*jur_size),
              mean_ineffexc_act = mean(total_jur_ineff_excl_act/num_jur),
              sd_ineffexc_act = sd(total_jur_ineff_excl_act/num_jur),
              se_ineffexc_act = sd_ineffexc_act / sqrt(n),
              mean_ineffexc_res = mean(total_jur_ineff_excl_res/num_jur),
              sd_ineffexc_res = sd(total_jur_ineff_excl_res/num_jur),
              se_ineffexc_res = sd_ineffexc_res / sqrt(n))

  #Use pivot longer so we can grph over the effects
  outcomes_long <- outcomes_df %>%
    select(jur_size, e_nr_sd, e_c0_sd, starts_with("mean")) %>%
    pivot_longer(cols = starts_with("mean_"), names_to = "dep_var", names_prefix = "mean_", values_to = "mean")
  sd_df <- outcomes_df %>%
    select(jur_size, e_nr_sd, e_c0_sd, starts_with("sd")) %>%
    pivot_longer(cols = starts_with("sd_"), names_to = "dep_var", names_prefix = "sd_", values_to = "sd")
  se_df <- outcomes_df %>%
    select(jur_size, e_nr_sd, e_c0_sd, starts_with("se")) %>%
    pivot_longer(cols = starts_with("se_"), names_to = "dep_var", names_prefix = "se_", values_to = "se")
  outcomes_long <- left_join(outcomes_long, sd_df, by = c("jur_size", "e_nr_sd", "e_c0_sd", "dep_var"))
  outcomes_long <- left_join(outcomes_long, se_df, by = c("jur_size", "e_nr_sd", "e_c0_sd", "dep_var"))

  #Plot the grid
  cols <- c("ineffexc_act" = "longdash", "ineffexc_res" = "dotted")
  label_cols <- c("Action-based", "Modeled-results")

  P <- ggplot(outcomes_long, aes(x = jur_size, y = mean, linetype = dep_var)) + geom_line() +
    facet_grid(rows = vars(e_c0_sd),
               cols = vars(e_nr_sd),
               scales = "free",
               labeller = label_bquote(cols = {sigma^r}[epsilon] == .(e_nr_sd),
                                       rows = {sigma^c}[epsilon] == .(e_c0_sd))) +
    scale_linetype_manual(name = element_blank(), values = cols, labels = label_cols) +
    theme(legend.position = "bottom",
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = 'white'),
          legend.background = element_blank(),
          legend.box.background = element_blank(),
          legend.key = element_blank(),
          text=element_text(family="serif", size=14),
          axis.title.y = element_text(vjust = 0.5),
          strip.background = element_rect(colour = "black", fill = "white", linewidth = 1, linetype = "solid"),
          strip.placement = "outside",
          strip.switch.pad.grid = unit(0.5, "in"),
          plot.margin = margin(1,1,0,0, "in")) +
    xlab(bquote(Jurisdiction~size*","*~N)) +
    ylab(bquote(Inefficient~nonparticipation)) +
    annotate("segment", x=-Inf, xend=25, y=-Inf, yend=-Inf) +
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=.35)
  P

  #Save
  ggsave("figure2_inefficientnonparticipation.tiff",
         plot = P,
         device = "tiff",
         path = "output_figures",
         width = 6.5,
         height = 8,
         dpi = 500,
         units = "in")

#######################################
  #Same thing but for efficient participation
  colnames(output_df)
  #Summarise to get single values for each point
  outcomes_df <- output_df %>%
    filter(jur_size<=25) %>%
    filter(e_nr_sd %in% e_nr_sd_vec & e_c0_sd %in% e_c0_sd_vec) %>%
    dplyr::group_by(jur_size, e_nr_sd, e_c0_sd) %>%
    summarise(n = n(),
              pop_size = mean(num_jur*jur_size),
              mean_effinc_act = mean(total_jur_eff_incl_act/num_jur),
              sd_effinc_act = sd(total_jur_eff_incl_act/num_jur),
              se_effinc_act = sd_effinc_act / sqrt(n),
              mean_effinc_res = mean(total_jur_eff_incl_res/num_jur),
              sd_effinc_res = sd(total_jur_eff_incl_res/num_jur),
              se_effinc_res = sd_effinc_res / sqrt(n))

  #Use pivot longer so we can grph over the effects
  outcomes_long <- outcomes_df %>%
    select(jur_size, e_nr_sd, e_c0_sd, starts_with("mean")) %>%
    pivot_longer(cols = starts_with("mean_"), names_to = "dep_var", names_prefix = "mean_", values_to = "mean")
  sd_df <- outcomes_df %>%
    select(jur_size, e_nr_sd, e_c0_sd, starts_with("sd")) %>%
    pivot_longer(cols = starts_with("sd_"), names_to = "dep_var", names_prefix = "sd_", values_to = "sd")
  se_df <- outcomes_df %>%
    select(jur_size, e_nr_sd, e_c0_sd, starts_with("se")) %>%
    pivot_longer(cols = starts_with("se_"), names_to = "dep_var", names_prefix = "se_", values_to = "se")
  outcomes_long <- left_join(outcomes_long, sd_df, by = c("jur_size", "e_nr_sd", "e_c0_sd", "dep_var"))
  outcomes_long <- left_join(outcomes_long, se_df, by = c("jur_size", "e_nr_sd", "e_c0_sd", "dep_var"))

  #Plot the grid
  cols <- c("effinc_act" = "longdash", "effinc_res" = "dotted")
  label_cols <- c("Action-based", "Modeled-results")

  P <- ggplot(outcomes_long, aes(x = jur_size, y = mean, linetype = dep_var)) + geom_line() +
    facet_grid(rows = vars(e_c0_sd),
               cols = vars(e_nr_sd),
               scales = "free",
               labeller = label_bquote(cols = {sigma^r}[epsilon] == .(e_nr_sd),
                                       rows = {sigma^c}[epsilon] == .(e_c0_sd))) +
    scale_linetype_manual(name = element_blank(), values = cols, labels = label_cols) +
    theme(legend.position = "bottom",
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = 'white'),
          legend.background = element_blank(),
          legend.box.background = element_blank(),
          legend.key = element_blank(),
          text=element_text(family="serif", size=14),
          axis.title.y = element_text(vjust = 0.5),
          strip.background = element_rect(colour = "black", fill = "white", linewidth = 1, linetype = "solid"),
          strip.placement = "outside",
          strip.switch.pad.grid = unit(0.5, "in"),
          plot.margin = margin(1,1,0,0, "in")) +
    xlab(bquote(Jurisdiction~size*","*~N)) +
    ylab(bquote(Efficient~participation)) +
    annotate("segment", x=-Inf, xend=25, y=-Inf, yend=-Inf) +
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=1)
  P

  #Save
  ggsave("figure2_efficientparticipation.tiff",
         plot = P,
         device = "tiff",
         path = "output_figures",
         width = 6.5,
         height = 8,
         dpi = 500,
         units = "in")

#######################################
  #Same thing but for efficient nonparticipation
  colnames(output_df)
  #Summarise to get single values for each point
  outcomes_df <- output_df %>%
    filter(jur_size<=25) %>%
    filter(e_nr_sd %in% e_nr_sd_vec & e_c0_sd %in% e_c0_sd_vec) %>%
    dplyr::group_by(jur_size, e_nr_sd, e_c0_sd) %>%
    summarise(n = n(),
              pop_size = mean(num_jur*jur_size),
              mean_effexc_act = mean(total_jur_eff_excl_act/num_jur),
              sd_effexc_act = sd(total_jur_eff_excl_act/num_jur),
              se_effexc_act = sd_effexc_act / sqrt(n),
              mean_effexc_res = mean(total_jur_eff_excl_res/num_jur),
              sd_effexc_res = sd(total_jur_eff_excl_res/num_jur),
              se_effexc_res = sd_effexc_res / sqrt(n))

  #Use pivot longer so we can grph over the effects
  outcomes_long <- outcomes_df %>%
    select(jur_size, e_nr_sd, e_c0_sd, starts_with("mean")) %>%
    pivot_longer(cols = starts_with("mean_"), names_to = "dep_var", names_prefix = "mean_", values_to = "mean")
  sd_df <- outcomes_df %>%
    select(jur_size, e_nr_sd, e_c0_sd, starts_with("sd")) %>%
    pivot_longer(cols = starts_with("sd_"), names_to = "dep_var", names_prefix = "sd_", values_to = "sd")
  se_df <- outcomes_df %>%
    select(jur_size, e_nr_sd, e_c0_sd, starts_with("se")) %>%
    pivot_longer(cols = starts_with("se_"), names_to = "dep_var", names_prefix = "se_", values_to = "se")
  outcomes_long <- left_join(outcomes_long, sd_df, by = c("jur_size", "e_nr_sd", "e_c0_sd", "dep_var"))
  outcomes_long <- left_join(outcomes_long, se_df, by = c("jur_size", "e_nr_sd", "e_c0_sd", "dep_var"))

  #Plot the grid
  cols <- c("effexc_act" = "longdash", "effexc_res" = "dotted")
  label_cols <- c("Action-based", "Modeled-results")

  P <- ggplot(outcomes_long, aes(x = jur_size, y = mean, linetype = dep_var)) + geom_line() +
    facet_grid(rows = vars(e_c0_sd),
               cols = vars(e_nr_sd),
               scales = "free",
               labeller = label_bquote(cols = {sigma^r}[epsilon] == .(e_nr_sd),
                                       rows = {sigma^c}[epsilon] == .(e_c0_sd))) +
    scale_linetype_manual(name = element_blank(), values = cols, labels = label_cols) +
    theme(legend.position = "bottom",
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = 'white'),
          legend.background = element_blank(),
          legend.box.background = element_blank(),
          legend.key = element_blank(),
          text=element_text(family="serif", size=14),
          axis.title.y = element_text(vjust = 0.5),
          strip.background = element_rect(colour = "black", fill = "white", linewidth = 1, linetype = "solid"),
          strip.placement = "outside",
          strip.switch.pad.grid = unit(0.5, "in"),
          plot.margin = margin(1,1,0,0, "in")) +
    xlab(bquote(Jurisdiction~size*","*~N)) +
    ylab(bquote(Efficient~nonparticipation)) +
    annotate("segment", x=-Inf, xend=25, y=-Inf, yend=-Inf) +
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=.75)
  P

  #Save
  ggsave("figure2_efficientnonparticipation.tiff",
         plot = P,
         device = "tiff",
         path = "output_figures",
         width = 6.5,
         height = 8,
         dpi = 500,
         units = "in")

  #######################################
  #Same thing but for average cost
  colnames(output_df)
  #Summarise to get single values for each point
  outcomes_df <- output_df %>%
    filter(jur_size<=25) %>%
    filter(e_nr_sd %in% e_nr_sd_vec & e_c0_sd %in% e_c0_sd_vec) %>%
    dplyr::group_by(jur_size, e_nr_sd, e_c0_sd) %>%
    summarise(n = n(),
              pop_size = mean(num_jur*jur_size),
              mean_effexc_act = mean(total_jur_eff_excl_act/num_jur),
              sd_effexc_act = sd(total_jur_eff_excl_act/num_jur),
              se_effexc_act = sd_effexc_act / sqrt(n),
              mean_effexc_res = mean(total_jur_eff_excl_res/num_jur),
              sd_effexc_res = sd(total_jur_eff_excl_res/num_jur),
              se_effexc_res = sd_effexc_res / sqrt(n))

  #Use pivot longer so we can grph over the effects
  outcomes_long <- outcomes_df %>%
    select(jur_size, e_nr_sd, e_c0_sd, starts_with("mean")) %>%
    pivot_longer(cols = starts_with("mean_"), names_to = "dep_var", names_prefix = "mean_", values_to = "mean")
  sd_df <- outcomes_df %>%
    select(jur_size, e_nr_sd, e_c0_sd, starts_with("sd")) %>%
    pivot_longer(cols = starts_with("sd_"), names_to = "dep_var", names_prefix = "sd_", values_to = "sd")
  se_df <- outcomes_df %>%
    select(jur_size, e_nr_sd, e_c0_sd, starts_with("se")) %>%
    pivot_longer(cols = starts_with("se_"), names_to = "dep_var", names_prefix = "se_", values_to = "se")
  outcomes_long <- left_join(outcomes_long, sd_df, by = c("jur_size", "e_nr_sd", "e_c0_sd", "dep_var"))
  outcomes_long <- left_join(outcomes_long, se_df, by = c("jur_size", "e_nr_sd", "e_c0_sd", "dep_var"))

  #Plot the grid
  cols <- c("effexc_act" = "longdash", "effexc_res" = "dotted")
  label_cols <- c("Action-based", "Modeled-results")

  P <- ggplot(outcomes_long, aes(x = jur_size, y = mean, linetype = dep_var)) + geom_line() +
    facet_grid(rows = vars(e_c0_sd),
               cols = vars(e_nr_sd),
               scales = "free",
               labeller = label_bquote(cols = {sigma^r}[epsilon] == .(e_nr_sd),
                                       rows = {sigma^c}[epsilon] == .(e_c0_sd))) +
    scale_linetype_manual(name = element_blank(), values = cols, labels = label_cols) +
    theme(legend.position = "bottom",
          plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = 'white'),
          legend.background = element_blank(),
          legend.box.background = element_blank(),
          legend.key = element_blank(),
          text=element_text(family="serif", size=14),
          axis.title.y = element_text(vjust = 0.5),
          strip.background = element_rect(colour = "black", fill = "white", linewidth = 1, linetype = "solid"),
          strip.placement = "outside",
          strip.switch.pad.grid = unit(0.5, "in"),
          plot.margin = margin(1,1,0,0, "in")) +
    xlab(bquote(Jurisdiction~size*","*~N)) +
    ylab(bquote(Efficient~nonparticipation)) +
    annotate("segment", x=-Inf, xend=25, y=-Inf, yend=-Inf) +
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=.75)
  P

