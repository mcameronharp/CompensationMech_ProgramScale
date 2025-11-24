#Applied Theory Paper - Result 3
#November 2023

library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(cowplot)
library(magrittr)
library(data.table)

#Load in Beocat output
files <- list.files(path = 'C:/Users/Micah/Dropbox/2022 EDF carbon market/applied theory paper/app.theory.sim/data/figure3/', pattern = '.rds', full.names = TRUE)
dat_list <- lapply(files, function (x) data.table(readRDS(x)))
output_df <- rbindlist(dat_list, fill = TRUE) %>% dplyr::select(jur_size, cc_nr_mean, e_nr_sd, e_c0_sd, total_add_outc_res, total_add_outc_act)
rm(dat_list)
gc()
# output_df <- output_df%>% dplyr::filter(cc_nr_mean==0.5)
colnames(output_df)
#Make a list of the jur_sizes
jur_size_list <- output_df %>% dplyr::select(jur_size) %>% arrange(jur_size) %>% unique()
jur_size_list <- jur_size_list %>% dplyr::filter(jur_size <=20)
jur_size_list <- c(jur_size_list$jur_size)
#Make a list of the mean values for the net return to practice adoption
nr_mean_list <- output_df %>% dplyr::select(cc_nr_mean) %>% arrange(cc_nr_mean) %>% unique()
nr_mean_list <- c(nr_mean_list$cc_nr_mean)
#Now make them a grid
jur_sizes <- expand.grid(jur_size_list, nr_mean_list)[,1]
nr_means <- expand.grid(jur_size_list, nr_mean_list)[,2]

#Create column showing if additional outcomes are higher for action-based
working_df <- output_df %>%
  dplyr::mutate(res_addoutc_higher = ifelse(total_add_outc_act < total_add_outc_res, 0, 1)) %>%
  dplyr::filter(jur_size <=20)

#Now summarize to get the median value
working_df <- working_df %>% dplyr::group_by(jur_size, cc_nr_mean, e_nr_sd, e_c0_sd) %>%
  summarise(res_addoutc_higher = mean(res_addoutc_higher)) %>% ungroup()
working_df <- working_df %>%
  dplyr::mutate(factor_res_great = ifelse(res_addoutc_higher > 0.5, 1, 0))

p <- ggplot(working_df, aes(x = e_nr_sd, y = e_c0_sd, color = res_addoutc_higher)) + geom_point()
final_p <- p + facet_grid(jur_size ~ cc_nr_mean,
                          scales = "free",
                          labeller = label_bquote(cols = mu[r] == .(cc_nr_mean),
                                                  rows = N == .(jur_size))) +
  coord_cartesian(ylim = c(0.1, 0.3), xlim = c(0.5, 1.5), expand = FALSE) +
  scale_color_gradient2(low = "navy", mid = "white", high = "darkorange", midpoint = 0.5, limits = c(0,1)) +
  theme(legend.position = "None",
        panel.background = element_rect(fill = 'white'),
        text=element_text(family="serif", size=12),
        axis.title.y = element_text(vjust = 0.5, angle=0),
        strip.background = element_rect(colour = "white", fill = "white", linewidth = 1, linetype = "solid"),
        strip.placement = "outside",
        strip.switch.pad.grid = unit(0.5, "in"),
        plot.margin = margin(1,1,0,0, "in"),
        panel.border=element_blank(),
        axis.line=element_line(),
        strip.text = element_text(family="serif", size=13)) +
  xlab(bquote(sigma[epsilon]^r)) +
  ylab(bquote(sigma[epsilon]^c)) +
  scale_x_continuous(breaks = c(0.5, 1, 1.5),
                     labels = c("0.5" = ".5", "1" = "1", "1.5" ="1.5")) +
  scale_y_continuous(breaks = c(0.1, 0.2, 0.3), labels = c(".1" = ".1", "0.2" =".2", ".3" = ".3"))
  final_p

final_p <- final_p + theme(panel.spacing = unit(1, "lines"))

ggsave("figure4_10_19.tiff",
       plot = final_p,
       device = "tiff",
       path = "C:/Users/Micah/Dropbox/2022 EDF carbon market/applied theory paper/Ecological Economics/figures",
       width = 6.5,
       height = 8.5,
       dpi = 500,
       units = "in")

#Now make another plot to get the legend
p <- ggplot(working_df, aes(x = e_nr_sd, y = e_c0_sd, color = as.factor(factor_res_great))) + geom_point()
final_p <- p + facet_grid(jur_size ~ cc_nr_mean,
                          scales = "free",
                          labeller = label_bquote(cols = mu[r] == .(cc_nr_mean),
                                                  rows = N == .(jur_size))) +
  coord_cartesian(ylim = c(0.1, 0.3), xlim = c(0.5, 1.5), expand = FALSE) +
  scale_color_manual(name = "Program producing greater additional outcomes",
                     values = c(
                       "0" = "darkorange",
                       "1" = "navy"
                     ),
                     labels = c("Action-based",
                                "Modeled-results"),
                     guide = guide_legend(direction = "horizontal",
                                          title.position = "top",
                                          title.hjust = 0.5)) +
  theme(
    panel.background = element_rect(fill = 'white'),
    text=element_text(family="serif", size=12),
    axis.title.y = element_text(vjust = 0.5, angle=0),
    strip.background = element_rect(colour = "white", fill = "white", linewidth = 1, linetype = "solid"),
    strip.placement = "outside",
    strip.switch.pad.grid = unit(0.5, "in"),
    plot.margin = margin(1,1,0,0, "in"),
    panel.border=element_blank(),
    axis.line=element_line(),
    strip.text = element_text(family="serif", size=13)) +
  xlab(bquote({sigma^c}[epsilon])) +
  ylab(bquote({sigma^r}[epsilon])) +
  scale_x_continuous(breaks = c(0.5, 1, 1.5),
                     labels = c("0.5" = ".5", "1" = "1", "1.5" ="1.5")) +
  scale_y_continuous(breaks = c(0.1, 0.2, 0.3), labels = c(".1" = ".1", "0.2" =".2", ".3" = ".3"))

final_p
final_p <- final_p + theme(panel.spacing = unit(1, "lines"))

ggsave("figure4_legend_10_19.tiff",
       plot = final_p,
       device = "tiff",
       path = "C:/Users/Micah/Dropbox/2022 EDF carbon market/applied theory paper/Ecological Economics/figures",
       width = 6.5,
       height = 8.5,
       dpi = 500,
       units = "in")
