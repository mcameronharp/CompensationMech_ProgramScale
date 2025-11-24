library(ggplot2)
library(cowplot)
library(ggrepel)
library(dplyr)
library(tidyr)
library(data.table)

#List files containing the data to create figure 2
figure2_files <- list.files(path = "C:/Users/Micah/Dropbox/2022 EDF carbon market/applied theory paper/app.theory.sim/data/figure1/",
                            pattern = '8_22*',
                            full.names = TRUE)
output_df <- lapply(figure2_files, readRDS)
output_df <- rbindlist(output_df)
colnames(output_df)

e_nr_sd_vec <- output_df %>% dplyr::select(e_nr_sd) %>% unique()
e_nr_sd_vec <- c(e_nr_sd_vec$e_nr_sd)
e_nr_sd_vec
output_df <- output_df %>%
  dplyr::filter(e_nr_sd <= 1)

e_c0_sd_vec <- output_df %>% dplyr::select(e_c0_sd) %>% unique()
e_c0_sd_vec <- c(e_c0_sd_vec$e_c0_sd)
e_c0_sd_vec
e_c0_sd_vec <- c(0, e_c0_sd_vec[3],e_c0_sd_vec[5])

################################
#Plotting results
plot_df <- output_df %>% dplyr::group_by(e_nr_sd, e_c0_sd) %>% summarise(n = n(),
                                                                         pop_size = mean(num_jur*jur_size),
                                                                         #Total cost
                                                                         act_tc = mean(total_payments_rec_act),
                                                                         res_tc = mean(total_payments_rec_res),
                                                                         act_n = n, #Make these so we can keep the n variable after pivot_longer
                                                                         res_n = n,
                                                                         #Additional outcomes
                                                                         act_addoutcomes = mean(total_add_outc_act),
                                                                         res_addoutcomes = mean(total_add_outc_res),
                                                                         #Average Cost
                                                                         act_ac = mean(total_payments_rec_act/total_add_outc_act),
                                                                         res_ac= mean(total_payments_rec_res/total_add_outc_res),
                                                                         #Efficient participation
                                                                         act_effpart = mean(total_jur_eff_incl_act/num_jur),
                                                                         res_effpart = mean(total_jur_eff_incl_res/num_jur),
                                                                         #Efficient opt-out
                                                                         act_effnonpart = mean(total_jur_eff_excl_act/num_jur),
                                                                         res_effnonpart = mean(total_jur_eff_excl_res/num_jur),
                                                                         #Inefficient participants as percent of total participants
                                                                         act_ineffpart = mean(total_jur_ineff_incl_act/num_jur),
                                                                         res_ineffpart = mean(total_jur_ineff_incl_res/num_jur),
                                                                         #Inefficient non-participants as percent of population
                                                                         act_ineffnonpart = mean(total_jur_ineff_excl_act/num_jur),
                                                                         res_ineffnonpart = mean(total_jur_ineff_excl_res/num_jur),
                                                                         #Additional participation
                                                                         res_addpart = mean((total_overest_adopt_res+total_jur_eff_incl_res)/total_jur_enrolled_res),
                                                                         act_addpart = mean(total_jur_eff_incl_act/total_jur_enrolled_act),
                                                                         #Additional participation (in level)
                                                                         res_addpartlev = mean(total_overest_adopt_res+total_jur_eff_incl_res),
                                                                         act_addpartlev = mean(total_jur_eff_incl_act),
                                                                         #Inefficient participants from overestimated outcomes
                                                                         res_overest = mean(total_overest_adopt_res/total_jur_enrolled_res),
                                                                         res_nonadd = mean((total_jur_ineff_incl_res - total_overest_adopt_res)/total_jur_enrolled_res)
) %>%
  dplyr::select(-n, -pop_size) %>%
  pivot_longer(c(-e_nr_sd,-e_c0_sd),
               names_to = c("program", ".value"),
               names_sep = "_")

# #Graphing
# dodge <- position_dodge(width=0.01)

######
# Error for carbon observation now varies
# Plot lines at values  of e_c0_sd:
# e_c0_breaks <- seq(0, 0.2, 0.05)
e_c0_breaks <- e_c0_sd_vec
#Filter data and make new factor variable for plotting
plot_df_filt <- plot_df %>% mutate(check = as.factor(e_c0_sd)) %>%
  dplyr::filter(check %in% e_c0_breaks) %>%
  filter(e_c0_sd==0 | program=="res") %>%
  mutate(e_c_sd_factor = ifelse(check==0, 1, ifelse(check==e_c0_breaks[2], 2, 3)),
         e_c_sd_factor = ifelse(program=="act", 0, e_c_sd_factor),
         e_c_sd_factor = as.factor(e_c_sd_factor))
#Make labels for the carbon observation error
data_label <- plot_df_filt                              # Modify data
data_label$label <- NA
data_label$label[which(data_label$e_nr_sd == max(data_label$e_nr_sd))] <- data_label$e_c0_sd[which(data_label$e_nr_sd == max(data_label$e_nr_sd))]
data_label <- data_label %>% dplyr::mutate(label = ifelse(label==0, paste0(expression(sigma[epsilon]^c), " == ", label), paste0(expression(sigma[epsilon]^c), " == ", label)))
data_label$label[which(data_label$e_nr_sd == max(data_label$e_nr_sd) & data_label$e_c_sd_factor == 0)] <- NA

######
#Define which programs get certain linetypes and colors
ltypes <- c("act" = "dashed", "res" = "solid")
cols <- c("0" = "darkorange", "1" = "darkcyan", "2" = "navy", "3" = "purple4")

#Efficient participation
frac_effpart_plot <- ggplot(data = data_label, aes(x = e_nr_sd, y = effpart, color = e_c_sd_factor)) +
  geom_line(aes(linetype = program), alpha = 0.75) +
  # geom_errorbar(aes(ymin = meanfracspurnt - 2*sefracspurnt, ymax = meanfracspurnt + 2*sefracspurnt),  position = dodge) +
  scale_x_continuous(limits = c(-0.1, 1.5),, breaks = c(0, 0.5, 1.0)) +
  geom_label_repel(aes(label = label),
                   direction = 'y',
                   nudge_x = 0.5,
                   na.rm = TRUE,
                   parse = TRUE,
                   segment.alpha = 0,
                   size = 4) +
  ggtitle("Efficient participation") +
  xlab(expression('Net return error st. dev., '*sigma[italic(epsilon)]^r)) +
  ylab(NULL) +
  scale_y_continuous(breaks = c(0.1, .12, .14, .16, .18, .2),
                     limits = c(0.09, .21),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_linetype_manual(values = ltypes, labels = c('Action-based', 'Modeled-results')) +
  scale_color_manual(values = cols, guide = 'none') +
  theme(text=element_text(size=16,  family="serif"),
        plot.title = element_text(hjust = 0.5)) +
  theme(panel.background = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(legend.key=element_rect(fill="white")) +
  theme(legend.title=element_blank()) +
  theme(legend.title=element_blank(),
        legend.position = c(0.8, 0.5),
        legend.key=element_blank(),
        legend.key.size = unit(1.5, "cm"),
        legend.text=element_text(size=16),
        legend.text.align = 0)
ggdraw(frac_effpart_plot)

#Efficient opt-out
frac_effnonpart_plot <- ggplot(data = data_label, aes(x = e_nr_sd, y = effnonpart, color = e_c_sd_factor)) +
  geom_line(aes(linetype = program), alpha = 0.75) +
  # geom_errorbar(aes(ymin = meanfracspurnt - 2*sefracspurnt, ymax = meanfracspurnt + 2*sefracspurnt),  position = dodge) +
  scale_x_continuous(limits = c(-0.1, 1.5),, breaks = c(0, 0.5, 1.0)) +
  geom_label_repel(aes(label = label),
                   direction = 'y',
                   nudge_x = 0.5,
                   na.rm = TRUE,
                   parse = TRUE,
                   segment.alpha = 0,
                   size = 4) +
  ggtitle("Efficient opt-out") +
  xlab(expression('Net return error st. dev., '*sigma[italic(epsilon)]^r)) +
  ylab(NULL) +
  scale_y_continuous(breaks = c(0.7, .72, .74, .76, .78, .8),
                     limits = c(0.69, .81),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_linetype_manual(values = ltypes, labels = c('Action-based', 'Modeled-results')) +
  scale_color_manual(values = cols, guide = 'none') +
  theme(text=element_text(size=16,  family="serif"),
        plot.title = element_text(hjust = 0.5)) +
  theme(panel.background = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(legend.key=element_rect(fill="white")) +
  theme(legend.title=element_blank()) +
  theme(legend.title=element_blank(),
        legend.position = c(0.8, 0.5),
        legend.key=element_blank(),
        legend.key.size = unit(1.5, "cm"),
        legend.text=element_text(size=16),
        legend.text.align = 0)
ggdraw(frac_effnonpart_plot)

#Fraction of participants that are inefficiently participating.
frac_ineffpart_plot <- ggplot(data = data_label, aes(x = e_nr_sd, y = ineffpart, color = e_c_sd_factor)) +
  geom_line(aes(linetype = program), alpha = 0.75) +
  # geom_errorbar(aes(ymin = meanfracspurnt - 2*sefracspurnt, ymax = meanfracspurnt + 2*sefracspurnt),  position = dodge) +
  scale_x_continuous(limits = c(-0.1, 1.5),, breaks = c(0, 0.5, 1.0)) +
  geom_label_repel(aes(label = label),
                   direction = 'y',
                   nudge_x = 0.5,
                   na.rm = TRUE,
                   parse = TRUE,
                   segment.alpha = 0,
                   size = 4) +
  ggtitle("Inefficient participation") +
  xlab(expression('Net return error st. dev., '*sigma[italic(epsilon)]^r)) +
  ylab(NULL) +
  scale_y_continuous(breaks = c(0, .02, .04, .06, .08, .1, .12),
                     limits = c(0, .12),
                     labels = scales::percent_format(accuracy = 1)) +
  scale_linetype_manual(values = ltypes, labels = c('Action-based', 'Modeled-results')) +
  scale_color_manual(values = cols, guide = 'none') +
  theme(text=element_text(size=16,  family="serif"),
        plot.title = element_text(hjust = 0.5)) +
  theme(panel.background = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(legend.key=element_rect(fill="white")) +
  theme(legend.title=element_blank()) +
  theme(legend.title=element_blank(),
        legend.position = c(0.8, 0.5),
        legend.key=element_blank(),
        legend.key.size = unit(1.5, "cm"),
        legend.text=element_text(size=16),
        legend.text.align = 0)
ggdraw(frac_ineffpart_plot)

#Fraction of jurisdictions that are inefficiently not participating
frac_ineffnonpart_plot <- ggplot(data = data_label, aes(x = e_nr_sd, y = ineffnonpart, color = e_c_sd_factor)) +
  geom_line(aes(linetype = program), alpha = 0.75) +
  # geom_errorbar(aes(ymin = meanfracspurnt - 2*sefracspurnt, ymax = meanfracspurnt + 2*sefracspurnt),  position = dodge) +
  scale_x_continuous(limits = c(-0.1, 1.5),, breaks = c(0, 0.5, 1.0)) +
  geom_label_repel(aes(label = label),
                   direction = 'y',
                   nudge_x = 0.5,
                   na.rm = TRUE,
                   parse = TRUE,
                   segment.alpha = 0,
                   size = 4) +
  ggtitle("Inefficient opt-out") +
  xlab(expression('Net return error st. dev., '*sigma[italic(epsilon)]^r)) +
  ylab(NULL) +
  scale_y_continuous(breaks = c(0, .02, .04, .06, .08, .1, .12),
                     limits = c(0, .12),
                     labels = scales::percent_format(accuracy = 1)) +  scale_linetype_manual(values = ltypes, labels = c('Action-based', 'Modeled-results')) +
  scale_color_manual(values = cols, guide = 'none') +
  theme(text=element_text(size=16,  family="serif"),
        plot.title = element_text(hjust = 0.5)) +
  theme(panel.background = element_blank()) +
  theme(axis.line = element_line(colour = "black")) +
  theme(legend.key=element_rect(fill="white")) +
  theme(legend.title=element_blank()) +
  theme(legend.title=element_blank(),
        legend.position = c(0.8, 0.5),
        legend.key=element_blank(),
        legend.key.size = unit(1.5, "cm"),
        legend.text=element_text(size=16),
        legend.text.align = 0)
ggdraw(frac_ineffnonpart_plot)

#Grab the legend from one graph
leg_plot <- frac_ineffnonpart_plot + theme(legend.position = 'bottom',
                                  legend.key=element_blank(),
                                  legend.key.size = unit(1, "cm"),
                                  legend.text=element_text(size=14)) +
  guides(linetype = guide_legend(ncol = 2,
                                 title.position = "top",
                                 override.aes = list(color = c("darkorange", "navy"))))

ggdraw(leg_plot)
legend <- get_plot_component(leg_plot, "guide-box-bottom", return_all = TRUE)
ggdraw(legend)

#Combine the 4 graphs
prow_fig2 <- plot_grid(
  frac_effpart_plot + theme(legend.position="none"),
  frac_effnonpart_plot + theme(legend.position="none"),
  frac_ineffpart_plot + theme(legend.position="none"),
  frac_ineffnonpart_plot + theme(legend.position="none"),
  align = 'vh',
  labels = c("(a)", "(b)", "(c)", "(d)"),
  label_size = 18,
  label_fontface = "plain",
  hjust = -2,
  nrow = 2
)
ggdraw(prow_fig2)

#Add the legend at the bottom
final_fig2_plot <- plot_grid(
  prow_fig2,
  legend,
  nrow = 2,
  rel_heights = c(1, 0.1))
ggdraw(final_fig2_plot)

#Save
ggsave2(filename = "figure2_4plot_efficiency_10_18.tiff",
        plot = final_fig2_plot,
        device = "tiff",
        path = "C:/Users/Micah/Dropbox/2022 EDF carbon market/applied theory paper/Ecological Economics/figures",
        width = 9.3,
        height = 9.3,
        dpi = 500,
        units = "in")

