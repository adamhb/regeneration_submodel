
source('runs/ED2_BASE.R')
source("create_output/figure_formatting.R")
BASE_long_term <- N_recs_per_year_pfts %>%
  gather(submodel:ED2, key = "model", value = "R") %>%
  filter(year > as.Date(as.numeric(as.Date(start_date)) + 365*3, origin = "1970-01-01")) %>%
  filter(as.numeric(stringr::str_sub(year, start = 1, end = 4)) %% 2 == 0) %>%
  #filter(model != "ED2") %>%
  ggplot(mapping = aes(x = year, y = R, color = pft, shape = model)) +
  geom_line() +
  point +
  # geom_point(data = bench4graph %>%
  #              filter(date > as.Date(as.numeric(as.Date(start_date)) + 365*3, origin = "1970-01-01")),
  #            mapping = aes(x = date, y = BCI_obs, color = pft), size = 4) +
  #geom_segment(data = bench4graph, mapping = aes(x = start_dateB, xend = end_dateB, y = BCI_obs, yend = BCI_obs, color = pft)) +
  scale_color_manual(values = pft.cols) +
  #scale_x_date(limits = c(as.Date("2007-10-01"),as.Date("2014-01-01")), date_breaks = "1 year", labels = date_format("%Y")) +
  x_axis +
  scale_shape_manual(values = c(21,24)) +
  #scale_y_log10(labels = c(0,10,100)) +
  scale_y_log10() + 
  rec.y.axis +
  facet_wrap(~model) +
  scale_y_continuous(limits = c(50,350), breaks = c(50,100,150,200,250,300,350)) +
  scale_y_log10() +
  xlab(bquote('year of simulation'))+
  labs(title = paste("BASE",percent_light * 100,"% light")) +
  adams_theme +
  long_term_sim_theme +
  adams_guides +
  theme(legend.title = element_text(size = 18))

png(paste0(path_to_output,"forMS/","BASE_long_term",percent_light*100,"pctLight",".png"), height=5, width=8, units="in", res = 100)
print(BASE_long_term)
dev.off()

print("FINISHED making BASE figure")

