library(tidyverse)
from_new_data <- F
if(file.exists("temp/recruitment_vs_light.csv") == FALSE | from_new_data == T){
  source('runs/ED2_run_light_demo.R')
}else{
  
  run_type <- "ED2" # keep this as ED2
  emulate_ED2 <- T
  patch_run_type <- "many" #"many" #one or "many"
  synthetic_patches <- T  # T or F
  no_real_patch_light <- T
  run_name <- "recruitment_vs_light"
  start_date <- "2001-01-01"
  end_date <- '2024-12-31'#"2034-12-31"
  n_PFTs <- 4
  soil_layer <- 15 # 15 is 6 cm, 16 is 2 cm deep
  
  #set path to driver data
  driver_data_path <- "~/cloud/gdrive/rec_submodel/data/ED2_output/"
  path_to_output <- "~/cloud/gdrive/rec_submodel/output/"
  
  #dbh.x <- 500 #dbh in mm
  #N_co.x <- 800  #the number of individuals in a cohort
  model_area <- 10000 #area in square meters
  
  summary_data <- read_csv("temp/recruitment_vs_light.csv")
}
print("making recruitment versus light figure...")

source("create_output/figure_formatting.R")

#import benchmarking data
bench <- read_csv("benchmarking/bci_rec_benchmarks_long.csv")
bench4graph <- bench %>%
  filter(date > start_date,
         date < end_date) %>%
  group_by(pft) %>%
  summarise(R_bench = mean(rec_rate)) %>%
  mutate(light = 2) %>%
  mutate(model = "BCI obs.")


psize <- 5
axis_size <- 20
title_size <- 25
pd <- position_dodge(0.003)


se_df <-summary_data %>%
  dplyr::select(pft, R_avg, R_avg_ED2, R_sd, R_sd_ED2, mean_pct_light) %>%
  rename(submodel = R_sd, ED2 = R_sd_ED2) %>%  ###############################
  gather(submodel:ED2, key = "model", value = "sd") 


yrs_in_analysis <- (as.numeric(substr(end_date,start = 1,stop = 4)) - as.numeric(substr(start_date,start = 1,stop = 4))) - 4


Rvsl <- summary_data %>%
  dplyr::select(pft, R_avg, R_avg_ED2, R_sd, R_sd_ED2, mean_pct_light) %>%
  rename(submodel = R_avg, ED2 = R_avg_ED2) %>%  ###############################
  gather(submodel:ED2, key = "model", value = "R") %>% 
  left_join(se_df, by = c("model","mean_pct_light","pft")) %>%
  mutate_at(.vars = "sd", .funs = function(x){x / sqrt(yrs_in_analysis)}) %>%
  #filter(model == "submodel") %>%
  ggplot(aes(x = mean_pct_light * 100, y = R * 365, color = pft, shape = model)) +
  #geom_point() +
  geom_point(size = psize, stroke = 1) +
  geom_line() +
  geom_point(data = bench4graph, mapping = aes(x = light, y = R_bench, color = pft, shape = model), size = psize, stroke = 2) +
  geom_errorbar(aes(ymin= (R * 365) - (sd * 365), ymax = (R * 365) + (sd * 365)), width=0, position = pd, show.legend = F) +
  scale_color_manual(values = pft.cols) +
  #scale_shape_manual(values = c(10,1,2)) +
  scale_shape_manual(values = c(10,21,24)) +
  scale_x_log10(breaks = round(lseq(from = 1, to = 100,length.out = 5)),
                labels = round(lseq(from = 1, to = 100,length.out = 5))) +
  #scale_y_log10() +
  rec.y.axis +
  #ylab(expression(paste('recruitment rate ',"(# indv. ha"^"-1", "yr"^"-1", ")"))) +
  xlab("light at seedling layer (% TOC)") +
  #geom_vline(xintercept = 46, linetype = "dotted") +
  #annotate(geom = "text", x = 46, y = 50, label = "46% TOC", size = 4) +
  #labs(title = "Light-sensitive recruitment \n among patches") +
  geom_segment(aes(x = 1.5, y = 80, xend = 1.3, yend = 5),
               arrow = arrow(length = unit(0.5, "cm")), color = "black") +
  #annotate(geom = "text", x = 1.5, y = 110, label = "LD recruitment\nfails", size = 4) +
  #annotate(geom = "text", x = 7, y = 350, label = "LD_DT recruitment\ndominates", size = 4) +
  geom_segment(aes(x = 7, y = 350, xend = 25, yend = 350),
               arrow = arrow(length = unit(0.5, "cm")), color = "black") +
  #plot_title +
  #facet_wrap(~model) +
  adams_theme #+
  #guides(fill = guide_legend(override.aes = list(shape = 21)))
  #annotate("segment", x = 2, xend = 0.1, y = 15, yend = 25, colour = "pink", size=3, alpha=0.6, arrow=arrow())


#makePNG(fig = Rvsl, path_to_output.x = paste0(path_to_output,"forMS/"), file_name = "rec_vs_light")









Rvsl_submodel <- summary_data %>%
  dplyr::select(pft, R_avg, R_avg_ED2, R_sd, R_sd_ED2, mean_pct_light) %>%
  rename(submodel = R_avg, ED2 = R_avg_ED2) %>%  ###############################
gather(submodel:ED2, key = "model", value = "R") %>% 
  left_join(se_df, by = c("model","mean_pct_light","pft")) %>%
  mutate_at(.vars = "sd", .funs = function(x){x / sqrt(yrs_in_analysis)}) %>%
  mutate(model = case_when(
    model == "submodel" ~ "TRS",
    model == "ED2" ~ "ED2" 
  )) %>%
  filter(model == "TRS") %>%
  ggplot(aes(x = mean_pct_light * 100, y = R * 365, color = pft, shape = model)) +
  #geom_point() +
  geom_point(size = psize, stroke = 1) +
  geom_line(show.legend = F) +
  geom_point(data = bench4graph, mapping = aes(x = light, y = R_bench, color = pft, shape = model), size = psize, stroke = 2, position = pd) +
  
  geom_errorbar(aes(ymin= (R * 365) - (sd * 365), ymax = (R * 365) + (sd * 365)), width=0, position = pd, show.legend = F) +
  scale_color_manual(values = pft.cols) +
  #scale_shape_manual(values = c(10,1,2)) +
  scale_shape_manual(values = c(10,24,21)) +
  scale_x_log10(breaks = round(lseq(from = 1, to = 100,length.out = 5)),
                labels = round(lseq(from = 1, to = 100,length.out = 5)),
                limits = c(1,100)) +
  scale_y_continuous(limits = c(0,300), breaks = seq(0,300,100)) +
  #scale_y_log10() +
  rec.y.axis +
  #ylab(expression(paste('recruitment rate ',"(# indv. ha"^"-1", "yr"^"-1", ")"))) +
  xlab("light at seedling layer [% TOC]") +
  #geom_vline(xintercept = 46, linetype = "dotted") +
  #annotate(geom = "text", x = 46, y = 50, label = "46% TOC", size = 4) +
  labs(title = "TRS") +
  #geom_segment(aes(x = 1.5, y = 80, xend = 1.3, yend = 5),
  #             arrow = arrow(length = unit(0.5, "cm")), color = "black") +
  #annotate(geom = "text", x = 2, y = 115, label = "LD recruitment\nfails", size = 4) +
  #annotate(geom = "text", x = 3, y = 350, label = "LD_DT recruitment\ndominates", size = 4) +
  geom_segment(aes(x = 7, y = 350, xend = 25, yend = 350),
               arrow = arrow(length = unit(0.5, "cm")), color = "black") +
  #plot_title +
  #facet_wrap(~model) +
  adams_theme +
  #geom_hline(yintercept=46,color = "red") +
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  guides(color = guide_legend(override.aes = list(shape = 15)))
  # theme(#axis.text.x=element_blank()),
  #       axis.text.y=element_blank(),axis.ticks=element_blank(),
  #       axis.title.y=element_blank())
#guides(fill = guide_legend(override.aes = list(shape = 21)))
#annotate("segment", x = 2, xend = 0.1, y = 15, yend = 25, colour = "pink", size=3, alpha=0.6, arrow=arrow())




Rvsl_ED2 <- summary_data %>%
  dplyr::select(pft, R_avg, R_avg_ED2, R_sd, R_sd_ED2, mean_pct_light) %>%
  rename(submodel = R_avg, ED2 = R_avg_ED2) %>%  ###############################
gather(submodel:ED2, key = "model", value = "R") %>% 
  left_join(se_df, by = c("model","mean_pct_light","pft")) %>%
  mutate_at(.vars = "sd", .funs = function(x){x / sqrt(yrs_in_analysis)}) %>%
  mutate(model = case_when(
    model == "submodel" ~ "TRS",
    model == "ED2" ~ "ED2" 
  )) %>%
  filter(model == "ED2") %>%
  ggplot(aes(x = mean_pct_light * 100, y = R * 365, color = pft, shape = model)) +
  #geom_point() +
  geom_point(size = psize, stroke = 1) +
  geom_point(data = summary_data %>%
               dplyr::select(pft, R_avg, R_avg_ED2, R_sd, R_sd_ED2, mean_pct_light) %>%
               rename(submodel = R_avg, ED2 = R_avg_ED2) %>%  ###############################
               gather(submodel:ED2, key = "model", value = "R") %>% 
               left_join(se_df, by = c("model","mean_pct_light","pft")) %>%
               mutate_at(.vars = "sd", .funs = function(x){x / sqrt(yrs_in_analysis)}) %>%
               mutate(model = case_when(
                 model == "submodel" ~ "TRS",
                 model == "ED2" ~ "ED2" 
               )) %>%
               filter(model == "TRS"), mapping = aes(x = mean_pct_light * 2e6, y = R*2e6, color = pft, shape = model), size = psize) +
  geom_line(show.legend = F) +
  geom_point(data = bench4graph, mapping = aes(x = light, y = R_bench, color = pft, shape = model), size = psize, stroke = 2) +
  geom_errorbar(aes(ymin= (R * 365) - (sd * 365), ymax = (R * 365) + (sd * 365)), width=0, position = pd, show.legend = F) +
  scale_color_manual(values = pft.cols) +
  #scale_shape_manual(values = c(10,1,2)) +
  scale_shape_manual(values = c(10,21,24)) +
  scale_x_log10(breaks = round(lseq(from = 1, to = 100,length.out = 5)),
                labels = round(lseq(from = 1, to = 100,length.out = 5)),
                limits = c(1,100)) +
  scale_y_continuous(limits = c(0,300), breaks = seq(0,300,100)) +
  #scale_y_log10() +
  rec.y.axis +
  #ylab(expression(paste('recruitment rate ',"(# indv. ha"^"-1", "yr"^"-1", ")"))) +
  xlab("light at seedling layer [% TOC]") +
  labs(title = "ED2") +
  #geom_vline(xintercept = 46, linetype = "dotted") +
  #annotate(geom = "text", x = 46, y = 50, label = "46% TOC", size = 4) +
  #labs(title = "Light-sensitive recruitment \n among patches") +
  #plot_title +
  #facet_wrap(~model) +
  adams_theme +
  theme(#axis.text.x=element_blank()),
    axis.text.y=element_blank(),axis.ticks=element_blank(),
    axis.title.y=element_blank(),
    axis.title.x = element_blank()) +
  guides(color = guide_legend(override.aes = list(shape = 15)))
    #legend.position = c(0.5,0.8))
  

# require(gridExtra)
# library(gtable)
# library(grid)
# Extracxt the legend from p1
# legend = gtable_filter(ggplotGrob(Rvsl_ED2), "guide-box") 
# grid.draw(legend)    # Make sure the legend has been extracted

# Arrange the elements to be plotted. 
# The inner arrangeGrob() function arranges the four plots, the main title, 
#   and the global y-axis title.
# The outer grid.arrange() function arranges and draws the arrangeGrob object and the legend.



plot1 <- Rvsl_submodel
plot2 <- Rvsl_ED2

test_plot.x <- plot_grid(plot1, plot2,
                       rel_widths = c(2,2.5))

test_plot <- ggdraw(add_sub(test_plot.x, "light at seedling layer [% TOC]", vpadding=grid::unit(1,"lines"), size = axis_size ))
test_plot

PNGwidth <- 9
makePNG(fig = test_plot, path_to_output.x = paste0(path_to_output,"forMS/"), file_name = "rec_vs_light")




# grid.arrange(arrangeGrob(plot1 + theme(legend.position="none"), 
#                          plot2 + theme(legend.position="none"),
#                          ncol = 2,
#                          bottom = textGrob("X Title", vjust = 1, gp = gpar(fontface = "bold", cex = 1.5)),
#                          #left = textGrob("Global Y-axis Label", rot = 90, vjust = 1)), 
#              legend),
#              widths= unit.c(unit(1, "in") - 5, 5), 
#              nrow=1)
# 
# 
# 
# 
# 
# grid.arrange(plot1, plot2, ncol=2)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 






# Rvsl <- summary_data %>%
#   dplyr::select(pft, R_avg, R_avg_ED2, R_sd, R_sd_ED2, mean_pct_light) %>%
#   rename(submodel = R_avg, ED2 = R_avg_ED2) %>%  ###############################
# gather(submodel:ED2, key = "model", value = "R") %>% 
#   left_join(se_df, by = c("model","mean_pct_light","pft")) %>%
#   mutate_at(.vars = "sd", .funs = function(x){x / sqrt(yrs_in_analysis)}) %>%
#   #filter(model == "submodel") %>%
#   ggplot(aes(x = mean_pct_light * 100, y = R * 365, color = pft, shape = model)) +
#   #geom_point() +
#   geom_point(size = psize, stroke = 1) +
#   geom_line() +
#   #geom_point(data = bench4graph, mapping = aes(x = light, y = R_bench, color = pft, shape = model), size = psize, stroke = 2) +
#   geom_errorbar(aes(ymin= (R * 365) - (sd * 365), ymax = (R * 365) + (sd * 365)), width=0, position = pd, show.legend = F) +
#   scale_color_manual(values = pft.cols) +
#   #scale_shape_manual(values = c(10,1,2)) +
#   scale_shape_manual(values = c(10,21,24)) +
#   scale_x_log10(breaks = round(lseq(from = 1, to = 100,length.out = 5)),
#                 labels = round(lseq(from = 1, to = 100,length.out = 5))) +
#   #scale_y_log10() +
#   rec.y.axis +
#   #ylab(expression(paste('recruitment rate ',"(# indv. ha"^"-1", "yr"^"-1", ")"))) +
#   xlab("light at seedling layer (% TOC)") +
#   geom_vline(xintercept = 46, linetype = "dotted") +
#   annotate(geom = "text", x = 46, y = 50, label = "46% TOC", size = 4) +
#   #labs(title = "Light-sensitive recruitment \n among patches") +
#   geom_segment(aes(x = 1.5, y = 80, xend = 1.3, yend = 5),
#                arrow = arrow(length = unit(0.5, "cm")), color = "black") +
#   annotate(geom = "text", x = 1.5, y = 110, label = "LD recruitment\nfails", size = 4) +
#   annotate(geom = "text", x = 7, y = 350, label = "LD_DT recruitment\ndominates", size = 4) +
#   geom_segment(aes(x = 7, y = 350, xend = 25, yend = 350),
#                arrow = arrow(length = unit(0.5, "cm")), color = "black") +
#   #plot_title +
#   facet_wrap(~model) +
#   adams_theme #+
# 
























# Rvsl_just_submodel <- summary_data %>%
#   dplyr::select(pft, R_avg, R_avg_ED2, R_sd, R_sd_ED2, mean_pct_light) %>%
#   rename(submodel = R_avg, ED2 = R_avg_ED2) %>%  ###############################
#   gather(submodel:ED2, key = "model", value = "R") %>% 
#   left_join(se_df, by = c("model","mean_pct_light","pft")) %>%
#   mutate_at(.vars = "sd", .funs = function(x){x / sqrt(yrs_in_analysis)}) %>%
#   filter(model == "submodel") %>%
#   ggplot(aes(x = mean_pct_light * 100, y = R * 365, color = pft, shape = model)) +
#   #geom_point() +
#   geom_point(size = 2.5, stroke = 1, alpha = 1) +
#   geom_point(data = bench4graph, mapping = aes(x = light, y = R_bench, color = pft, shape = model), size = 5) +
#   geom_errorbar(aes(ymin= (R * 365) - (sd * 365), ymax = (R * 365) + (sd * 365)), width=0, position = pd) +
#   scale_color_manual(values = pft.cols) +
#   scale_shape_manual(values = rep(c(10,24),2)) +
#   scale_x_log10() +
#   ylab(expression(paste('recruitment rate ',"(# indv. ha"^"-1", "yr"^"-1", ")"))) +
#   xlab("percent light at seedling layer (% TOC)") +
#   geom_vline(xintercept = 46, linetype = "dotted") +
#   #plot_title +
#   adams_theme
# 
# png(paste0(path_to_output,"forMS/","R_variation_with_light_just_submodel",model_run_time_stamp,".png"), height=PNGheight, width=PNGwidth, units=PNGunits, res = PNGres)
# print(Rvsl_just_submodel)
# dev.off()


print("successfully made recruitment versus light figure")