rm(list = ls())
gc()

high_light <- F
source('runs/ED2_WET.R')
source("create_output/figure_formatting.R")


N_recs_per_year_pfts_low_light <- N_recs_per_year_pfts %>%
  gather(submodel:ED2, key = "model", value = "R") %>%
  filter(year > as.Date(as.numeric(as.Date(start_date)) + 365*3, origin = "1970-01-01")) %>%
  filter(as.numeric(stringr::str_sub(year, start = 1, end = 4)) %% 2 == 0) %>%
  mutate(facet_var = paste0(model," ",percent_light * 100, "% TOC"))
  #filter(model != "ED2") %>%
  #mutate(facet_var = factor(model,levels =  c("submodel","ED2"))) %>%

write_csv(N_recs_per_year_pfts_low_light,"temp/WET_sim_low_light.csv")  


rm(list = ls())
gc()
high_light <- T

source('runs/ED2_WET.R')

N_recs_per_year_pfts_low_light <- read_csv("temp/WET_sim_low_light.csv") %>%
  mutate(facet_var = factor(facet_var, levels = c("submodel 2% TOC","ED2 2% TOC"))) %>%
  mutate(facet_var = case_when(
    facet_var == "submodel 2% TOC" ~ "TRS 2% TOC",
    facet_var == "ED2 2% TOC" ~ "ED2 2% TOC" 
  ))



N_recs_per_year_pfts_high_light <- N_recs_per_year_pfts %>%
  gather(submodel:ED2, key = "model", value = "R") %>%
  filter(year > as.Date(as.numeric(as.Date(start_date)) + 365*3, origin = "1970-01-01")) %>%
  filter(as.numeric(stringr::str_sub(year, start = 1, end = 4)) %% 2 == 0) %>%
  mutate(facet_var = paste0(model," ",percent_light * 100, "% TOC")) %>%
  ungroup() %>%
  mutate(facet_var = case_when(
    facet_var == "submodel 20% TOC" ~ "TRS 20% TOC",
    facet_var == "ED2 20% TOC" ~ "ED2 20% TOC" 
  )) %>%
  mutate(facet_var = factor(facet_var, levels = c("TRS 20% TOC","ED2 20% TOC")))
                                                  

#log axis
WET_20_pct <- N_recs_per_year_pfts_high_light %>%
  mutate(model = case_when(
    model == "submodel" ~ "TRS",
    model == "ED2" ~ "ED2" 
  )) %>%
  ggplot(mapping = aes(x = year, y = R, color = pft, shape = model)) +
  geom_line() +
  point +
  # geom_point(data = bench4graph %>%
  #              filter(date > as.Date(as.numeric(as.Date(start_date)) + 365*3, origin = "1970-01-01")),
  #            mapping = aes(x = date, y = BCI_obs, color = pft), size = 4) +
  #geom_segment(data = bench4graph, mapping = aes(x = start_dateB, xend = end_dateB, y = BCI_obs, yend = BCI_obs, color = pft)) +
  scale_color_manual(values = pft.cols) +
  #scale_x_date(limits = c(as.Date("2007-10-01"),as.Date("2014-01-01")), date_breaks = "1 year", labels = date_format("%Y")) +
  # scale_x_date(limits = c(as.Date("2005-01-01"),as.Date("2044-12-31")), 
  #              breaks = c(as.Date("2005-01-01"), as.Date("2010-01-01"), as.Date("2015-01-01"),
  #                         as.Date("2020-01-01"), as.Date("2025-01-01"), as.Date("2030-01-01"), as.Date("2035-01-01"),
  #                         as.Date("2040-01-01"), as.Date("2045-01-01")),
  #              #date_breaks = "5 year", 
  #              labels = date_format("%y")) +
  x_axis +
  scale_shape_manual(values = c(21,24)) +
  #scale_y_log10(labels = c(0,10,100)) +
  rec.y.axis +
  facet_wrap(~facet_var) +
  #scale_y_continuous(limits = c(0,350), breaks = c(0,50,100,50,100,150,200,250,300,350)) +
  scale_y_log10(limits = c(10,260),breaks = lseq(from = 10,to = 260,length.out = 7)) +
  xlab(bquote('Simulation year'))+
  #labs(title = paste("WET",percent_light * 100,"% light")) +
  adams_theme +
  long_term_sim_theme +
  adams_guides +
  color_guide +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none", 
        legend.text = element_text (size = 12),
        legend.spacing.x = unit(0.2, 'cm'),
        legend.spacing.y = unit(0.2, 'cm'), #this changes the spacing between groups of legend symbols
        legend.key.size = unit(0.1, "cm"),
        panel.spacing = unit(0.01, "lines"),
        legend.box.background = element_rect(colour = "black"),
        legend.direction = "horizontal",
        legend.margin = margin(0.1,0.1,0.1,0.1, unit="cm")) +
  guides(color = guide_legend(override.aes = list(size=3)),
         shape = guide_legend(override.aes = list(size=3)),
         fill=guide_legend(title="PFT"))



WET_2pct <- N_recs_per_year_pfts_low_light %>%
  mutate(model = case_when(
    model == "submodel" ~ "TRS",
    model == "ED2" ~ "ED2" 
  )) %>%
  mutate(facet_var = factor(x = facet_var, levels = c("TRS 2% TOC","ED2 2% TOC"))) %>%
  ggplot(mapping = aes(x = year, y = R, color = pft, shape = model)) +
  geom_line() +
  point +
  # geom_point(data = bench4graph %>%
  #              filter(date > as.Date(as.numeric(as.Date(start_date)) + 365*3, origin = "1970-01-01")),
  #            mapping = aes(x = date, y = BCI_obs, color = pft), size = 4) +
  #geom_segment(data = bench4graph, mapping = aes(x = start_dateB, xend = end_dateB, y = BCI_obs, yend = BCI_obs, color = pft)) +
  scale_color_manual(values = pft.cols) +
  #scale_x_date(limits = c(as.Date("2007-10-01"),as.Date("2014-01-01")), date_breaks = "1 year", labels = date_format("%Y")) +
  # scale_x_date(limits = c(as.Date("2005-01-01"),as.Date("2044-12-31")), 
  #              breaks = c(as.Date("2005-01-01"), as.Date("2010-01-01"), as.Date("2015-01-01"),
  #                         as.Date("2020-01-01"), as.Date("2025-01-01"), as.Date("2030-01-01"), as.Date("2035-01-01"),
  #                         as.Date("2040-01-01"), as.Date("2045-01-01")),
  #              #date_breaks = "5 year", 
  #              labels = date_format("%y")) +
  x_axis +
  scale_shape_manual(values = c(21,24)) +
  #scale_y_log10(breaks = labels = )) +
  rec.y.axis +
  facet_wrap(~facet_var) +
  #scale_y_continuous(limits = c(0,350), breaks = c(0,50,100,150,200,250,300,350)) +
  #scale_y_log10(limits = c(1,350), breaks = lseq(from = 1,to = 350,length.out = 8)) +
  xlab(bquote('Simulation year'))+
  #labs(title = paste("WET",percent_light * 100,"% light")) +
  adams_theme +
  long_term_sim_theme +
  adams_guides +
  theme(legend.title = element_blank()) +
  theme(legend.position = c(.75,.8), 
        legend.text = element_text (size = 12),
        legend.spacing.x = unit(0.2, 'cm'),
        legend.spacing.y = unit(0.2, 'cm'), #this changes the spacing between groups of legend symbols
        legend.key.size = unit(0.1, "cm"),
        panel.spacing = unit(0.01, "lines"),
        legend.box.background = element_rect(colour = "black"),
        legend.direction = "horizontal",
        legend.margin = margin(0.1,0.1,0.1,0.1, unit="cm")) +
  guides(color = guide_legend(override.aes = list(size=3, shape = 15)),
         shape = guide_legend(override.aes = list(size=3)),
           fill=guide_legend(title="PFT"))


WET <- plot_grid(WET_2pct, WET_20_pct, nrow = 2)


makePNG(fig = WET,path_to_output.x = paste0(path_to_output,"forMS/"),file_name = "WET_long_term", height = 7, width = 8, units = 'in', res = 100)







print("FINISHED making WET figure")

