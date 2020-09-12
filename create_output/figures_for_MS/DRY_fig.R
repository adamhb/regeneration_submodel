
source('runs/ED2_DRY1.R')
source("create_output/figure_formatting.R")


x_axis <- scale_x_date(limits = c(as.Date("2205-01-01"),as.Date("2234-12-31")), 
                       breaks = c(as.Date("2205-01-01"), as.Date("2210-01-01"), as.Date("2215-01-01"),
                                  as.Date("2220-01-01"), as.Date("2225-01-01"), as.Date("2230-01-01"), as.Date("2235-01-01")),
                       #date_breaks = "5 year", 
                       labels = date_format("%Y"))

DRY_long_term <- N_recs_per_year_pfts %>%
  gather(submodel:ED2, key = "model", value = "R") %>%
  filter(year > as.Date(as.numeric(as.Date(start_date)) + 365*3, origin = "1970-01-01")) %>%
  filter(as.numeric(stringr::str_sub(year, start = 1, end = 4)) %% 2 == 0) %>%
  mutate(simYr = as.numeric(stringr::str_sub(year, start = 1, end = 4)) - 2000) %>%
  #filter(model != "ED2") %>%
  ggplot(mapping = aes(x = simYr, y = R, color = pft, shape = model)) +
  geom_line() +
  point +
  # geom_point(data = bench4graph %>%
  #              filter(date > as.Date(as.numeric(as.Date(start_date)) + 365*3, origin = "1970-01-01")),
  #            mapping = aes(x = date, y = BCI_obs, color = pft), size = 4) +
  #geom_segment(data = bench4graph, mapping = aes(x = start_dateB, xend = end_dateB, y = BCI_obs, yend = BCI_obs, color = pft)) +
  scale_color_manual(values = pft.cols) +
  #scale_x_date(limits = c(as.Date("2007-10-01"),as.Date("2014-01-01")), date_breaks = "1 year", labels = date_format("%Y")) +
  scale_x_continuous(breaks = seq(5,35,5)) +
  scale_shape_manual(values = c(21,24)) +
  #scale_y_log10(labels = c(0,10,100)) +
  scale_y_log10(limits = c(1,450), breaks = c(1,10,100,400)) +
  rec.y.axis +
  #facet_wrap(~model) +
  #scale_y_continuous() +
  xlab(bquote('simulation year'))+
  labs(title = paste("DRY1 (",percent_light * 100,"% light)",sep = "")) +
  adams_theme +
  long_term_sim_theme +
  adams_guides +
  theme(legend.title = element_text(size = 18))



png(paste0(path_to_output,"forMS/","DRY_long_term",percent_light*100,"pctLight",".png"), height=5, width=8, units="in", res = 100)
print(DRY_long_term)
dev.off()






# DRY_long_term <- N_recs_per_year_pfts %>%
#   gather(submodel:ED2, key = "model", value = "R") %>%
#   filter(year > as.Date(as.numeric(as.Date(start_date)) + 365*3, origin = "1970-01-01")) %>%
#   filter(as.numeric(stringr::str_sub(year, start = 1, end = 4)) %% 2 == 0) %>%
#   mutate(simYr = as.numeric(stringr::str_sub(year, start = 1, end = 4)) - 2000) %>%
#   #filter(model != "ED2") %>%
#   ggplot(mapping = aes(x = simYr, y = R, color = pft, shape = model)) +
#   geom_line() +
#   point +
#   # geom_point(data = bench4graph %>%
#   #              filter(date > as.Date(as.numeric(as.Date(start_date)) + 365*3, origin = "1970-01-01")),
#   #            mapping = aes(x = date, y = BCI_obs, color = pft), size = 4) +
#   #geom_segment(data = bench4graph, mapping = aes(x = start_dateB, xend = end_dateB, y = BCI_obs, yend = BCI_obs, color = pft)) +
#   scale_color_manual(values = pft.cols) +
#   #scale_x_date(limits = c(as.Date("2007-10-01"),as.Date("2014-01-01")), date_breaks = "1 year", labels = date_format("%Y")) +
#   #x_axis +
#   scale_x_continuous(breaks = seq(5,35,5)) +
#   scale_shape_manual(values = c(21,24)) +
#   #scale_y_log10(labels = c(0,10,100)) +
#   #scale_y_log10(limits = c(1,450), breaks = c(1,10,100,400)) + 
#   rec.y.axis +
#   facet_wrap(~model) +
#   #scale_y_continuous(limits = c(50,350), breaks = c(50,100,150,200,250,300,350)) +
#   #scale_y_continuous() +
#   xlab(bquote('simulation year'))+
#   labs(title = paste("DRY1 (",percent_light * 100,"% light)",sep = "")) +
#   adams_theme +
#   long_term_sim_theme +
#   adams_guides +
#   theme(legend.title = element_text(size = 18))
# 
# 
# png(paste0(path_to_output,"forMS/","DRY_long_term",percent_light*100,"pctLight",".png"), height=5, width=8, units="in", res = 100)
#  print(DRY_long_term)
#  dev.off()





print("FINISHED making DRY1 figure")

