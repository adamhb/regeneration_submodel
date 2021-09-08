#This script creates the benchmarking figure used in
#Hanbury-Brown et al., in prep Fig. 4

source('runs/ED2_BASE.R')
source('utils/supporting_funcs.R')
print("making benchmarking figure...")

psize <- 5
axis_size <- 20
title_size <- 25

adams_theme_benchFig <- theme_minimal() + theme(plot.title = element_text(hjust = 0.5, size = title_size),
                     strip.text.x = element_text(size = axis_size),
                     legend.title = element_blank (),
                     axis.title.x = element_text (size = axis_size), # change the axis title
                     axis.title.y = element_text (size = axis_size),
                     axis.title.y.right = element_text (size = axis_size, color = pft.cols[2]),
                     axis.text.x = element_text (size = axis_size, colour = "black"),
                     axis.text.y = element_text (size = axis_size, colour = "black"),
                     legend.text = element_text (size = axis_size),
                     legend.spacing.x = unit(0.3, 'cm'),
                     legend.spacing.y = unit(0.3, 'cm'), #this changes the spacing between groups of legend symbols
                     legend.key.size = unit(0.9, "cm")) #this changes the spacing between legend symbols


bench_data <- bench4graph %>%
  filter(date > as.Date(as.numeric(as.Date(start_date)) + 365*3, origin = "1970-01-01")) %>%
  group_by(pft) %>% summarise(R = mean(BCI_obs), se_R = sd(BCI_obs)/sqrt(length(BCI_obs))) %>%
  mutate(simYr = 10, model = "BCI obs.") %>%
  dplyr::select(model, pft, R, se_R)

#write_csv(N_recs_per_year_pfts, path = "temp/outofbox_bench.csv")


##################################
#making figure with default params
##################################
addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

N_recs_per_year_BCI_params <- read_csv("temp/N_recs_per_yr_bci_params.csv") %>% mutate(t_vs_u = "tuned")
N_recs_per_year_default_params <- read_csv("temp/N_recs_per_yr_default_params.csv") %>% mutate(t_vs_u = "untuned")

benchmark_fig_data <- N_recs_per_year_default_params %>%
  rbind(N_recs_per_year_BCI_params) %>% 
  gather(submodel:ED2, key = "model", value = "R") %>%
  mutate(model = case_when(
    model == "submodel" ~ "TRS",
    model == "ED2" ~ "ED2" 
  )) %>%
  mutate(model = paste(model,t_vs_u)) %>%
  group_by(model,pft) %>%
  summarise(Rmean = mean(R), se_R = sd(R)/sqrt(length(R))) %>%
  rename(R = Rmean) %>% 
  ungroup() %>%
  rbind(bench_data) %>%
  mutate(model = factor(model,levels = c("ED2 untuned","TRS untuned","BCI obs.","ED2 tuned","TRS tuned"))) 
  #filter(model %in% c("ED2 untuned","TRS untuned","BCI obs."))
  #filter(model %in% c("BCI obs.","ED2 tuned","TRS tuned"))


benchmark_fig <- benchmark_fig_data %>%
  ggplot(mapping = aes(x = model, y = R, color = pft)) +
  geom_point(size = psize, stroke = 1, alpha = 1, shape = 1, position = position_dodge(width = 0.1)) +
  scale_color_manual(values = pft.cols) +
  scale_y_log10(limits = c(1,300), breaks = round(lseq(from = 1, to = 300,length.out = 7)),labels = round(lseq(from = 2, to = 300,length.out = 7))) +
  scale_x_discrete(breaks=unique(levels(benchmark_fig_data$model)), 
                   labels=addline_format(unique(levels(benchmark_fig_data$model)))) +
  geom_errorbar(aes(ymin= (R - se_R), ymax = (R + se_R)), width=0, show.legend = F,
                position = position_dodge(width = 0.1)) +
  ylab(expression(paste("N recruits"," [ha"^"-1"," yr"^"-1","]")))+
  xlab(bquote('model'))+
  adams_theme_benchFig +
  theme(legend.position = c(0.15,0.2),
        legend.direction = "vertical",
        legend.background = element_rect(fill = "white"),
        legend.margin = margin(0.1,0.1,0.1,0.1, unit="cm"),
        legend.key.size = unit(0.1, "cm"),
        panel.spacing = unit(0.1, "cm"),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(angle = 90, vjust = 0, hjust = 0, size = 15),
        axis.title.x = element_blank()) +
  guides(color = guide_legend(override.aes = list(shape = 15)),
         fill=guide_legend(title="PFT"))
benchmark_fig



makePNG(fig = benchmark_fig, path_to_output.x = paste0(path_to_output,"forMS/"), 
        file_name = "benchmark_fig_tuned",width = 6)


print("FINISHED making benchmarking figure!")











