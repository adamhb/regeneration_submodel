source('runs/ED2_BASE.R')
print("making benchmarking figure...")

ED2_data_for_fig <- N_recs_per_year_pfts %>%
  gather(submodel:ED2, key = "model", value = "R") %>%
  filter(model == "ED2")


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



benchmark_fig_log <- N_recs_per_year_pfts %>%
  gather(submodel:ED2, key = "model", value = "R") %>%
  filter(year > as.Date(as.numeric(as.Date(start_date)) + 365*3, origin = "1970-01-01")) %>%
  #rename(pftold = pft) %>%
  # mutate(pft = case_when(
  #   pftold == "earlydi" ~ "LD_DI",
  #   pftold == "earlydt" ~ "LD_DT",
  #   pftold == "latedi" ~  "ST_DI",
  #   pftold == "latedt" ~  "ST_DT"
  # )) %>% 
  #filter(model != "ED2") %>%
  ggplot(mapping = aes(x = year, y = R, color = pft, shape = model)) +
  geom_point(size = psize, stroke = 1, alpha = 1, position = position_jitter(height = 0, width = 10)) +
  geom_point(data = bench4graph %>%
               #rename(pftold = pft) %>%
               # mutate(pft = case_when(
               #   pftold == "earlydi" ~ "LD_DI",
               #   pftold == "earlydt" ~ "LD_DT",
               #   pftold == "latedi" ~  "ST_DI",
               #   pftold == "latedt" ~  "ST_DT"
               # )) %>%
               filter(date > as.Date(as.numeric(as.Date(start_date)) + 365*3, origin = "1970-01-01")),
             mapping = aes(x = date - 80, y = BCI_obs, color = pft), size = psize+2,
             position = position_jitter(width = 1)) +
  #geom_segment(data = bench4graph, mapping = aes(x = start_dateB, xend = end_dateB, y = BCI_obs, yend = BCI_obs, color = pft)) +
  scale_color_manual(values = pft.cols) +
  scale_x_date(limits = c(as.Date("2007-08-01"),as.Date("2014-01-21")), date_breaks = "1 year", labels = date_format("%Y")) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels = c(0,1,10,100,200,300), breaks = c(0,1,10,100,200,300)) + 
  scale_shape_manual(values = c(10,21,24)) +
  #scale_y_log10(labels = c(0,10,100)) +
  #scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels = c(0,1,10,100,200,300), breaks = c(0,1,10,100,200,300)) + 
  ylab(expression(paste("N recruits"," ha"^"-1"," yr"^"-1")))+
  xlab(bquote('year'))+
  labs(title = "Predicting rank order \n of PFT-specific recruitment") +
  adams_theme_benchFig 

benchmark_fig_log

png(paste0(path_to_output,"forMS/","benchmark_fig_log.png"), height=5, width=8, units="in", res = 100)
print(benchmark_fig_log)
dev.off()


# benchmark_fig_sec_axis <- N_recs_per_year_pfts %>%
#   gather(submodel:ED2, key = "model", value = "R") %>%
#   filter(year > as.Date(as.numeric(as.Date(start_date)) + 365*3, origin = "1970-01-01")) %>%
#   filter(model == "submodel") %>%
#   #filter(model != "ED2") %>%
#   ggplot(mapping = aes(x = year, y = R, color = pft, shape = model)) +
#   geom_point(size = psize) +
#   #geom_point(position=position_jitter(h=8, w=0), alpha = 0.99, stroke = 1, size = psize) +
#   geom_line(linetype = "solid", size = 1) +
#   geom_point(data = ED2_data_for_fig, mapping = aes(x = year, y = R/5, color = pft, shape = model),
#              alpha = 0.99, stroke = 1, size = psize) +
#   geom_line(data = ED2_data_for_fig, mapping = aes(x = year, y = R/5, color = pft, shape = model),
#              linetype = "dotted", size = 1) +
#   #geom_point(size = psize, stroke = 1, alpha = 1) +
#   geom_point(data = bench4graph %>%
#                filter(date > as.Date(as.numeric(as.Date(start_date)) + 365*3, origin = "1970-01-01")),
#              mapping = aes(x = date, y = BCI_obs, color = pft), size = psize+3, position = position_jitter(width = 5)) +
#   #geom_segment(data = bench4graph, mapping = aes(x = start_dateB, xend = end_dateB, y = BCI_obs, yend = BCI_obs, color = pft)) +
#   scale_color_manual(values = pft.cols) +
#   scale_x_date(limits = c(as.Date("2007-10-01"),as.Date("2014-01-01")), date_breaks = "1 year", labels = date_format("%Y")) +
#   scale_shape_manual(values = c(10,21,24)) +
#   scale_y_continuous(
#     "submodel", 
#     sec.axis = sec_axis(~ . * 5, name = "ED2")
#   ) +
#   #scale_y_log10(labels = c(0,10,100)) +
#   #scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels = c(0,1,10,100,200,300), breaks = c(0,1,10,100,200,300)) + 
#   ylab(expression(paste("N recruits"," ha"^"-1"," yr"^"-1")))+
#   xlab(bquote('year'))+
#   labs(title = "BASE 2% light") +
#   adams_theme_benchFig 
# 
# png(paste0(path_to_output,"forMS/","benchmark_fig_sec_axis.png"), height=5, width=8, units="in", res = 100)
# print(benchmark_fig_sec_axis)
# dev.off()




# benchmark_fig <- N_recs_per_year_pfts %>%
#   gather(submodel:ED2, key = "model", value = "R") %>%
#   filter(year > as.Date(as.numeric(as.Date(start_date)) + 365*3, origin = "1970-01-01")) %>%
#   #filter(model != "ED2") %>%
#   ggplot(mapping = aes(x = year, y = R, color = pft, shape = model)) +
#   geom_point(size = psize, stroke = 1, alpha = 1, position = position_jitter(height = 0, width = 10)) +
#   geom_point(data = bench4graph %>%
#                filter(date > as.Date(as.numeric(as.Date(start_date)) + 365*3, origin = "1970-01-01")),
#              mapping = aes(x = date - 80, y = BCI_obs, color = pft), size = psize+2,
#              position = position_jitter(width = 1)) +
#   #geom_segment(data = bench4graph, mapping = aes(x = start_dateB, xend = end_dateB, y = BCI_obs, yend = BCI_obs, color = pft)) +
#   scale_color_manual(values = pft.cols) +
#   scale_x_date(limits = c(as.Date("2007-08-01"),as.Date("2014-01-21")), date_breaks = "1 year", labels = date_format("%Y")) +
#   scale_shape_manual(values = c(10,21,24)) +
#   #scale_y_log10(labels = c(0,10,100)) +
#   #scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels = c(0,1,10,100,200,300), breaks = c(0,1,10,100,200,300)) + 
#   ylab(expression(paste("N recruits"," ha"^"-1"," yr"^"-1")))+
#   xlab(bquote('year'))+
#   labs(title = "Predicting rank order \n of PFT-specific recruitment") +
#   adams_theme_benchFig 
# 
# benchmark_fig
# 
# png(paste0(path_to_output,"forMS/","benchmark_fig.png"), height=5, width=8, units="in", res = 100)
# print(benchmark_fig)
# dev.off()
# 
# 
# benchmark_fig_sub_only <- N_recs_per_year_pfts %>%
#   gather(submodel:ED2, key = "model", value = "R") %>%
#   filter(year > as.Date(as.numeric(as.Date(start_date)) + 365*3, origin = "1970-01-01")) %>%
#   filter(model != "ED2") %>%
#   ggplot(mapping = aes(x = year, y = R, color = pft, shape = model)) +
#   geom_point(size = psize, stroke = 1, alpha = 1, position = position_jitter(height = 0, width = 10)) +
#   geom_point(data = bench4graph %>%
#                filter(date > as.Date(as.numeric(as.Date(start_date)) + 365*3, origin = "1970-01-01")),
#              mapping = aes(x = date - 80, y = BCI_obs, color = pft), size = psize+2,
#              position = position_jitter(width = 1)) +
#   #geom_segment(data = bench4graph, mapping = aes(x = start_dateB, xend = end_dateB, y = BCI_obs, yend = BCI_obs, color = pft)) +
#   scale_color_manual(values = pft.cols) +
#   scale_x_date(limits = c(as.Date("2007-08-01"),as.Date("2014-01-21")), date_breaks = "1 year", labels = date_format("%Y")) +
#   scale_shape_manual(values = c(10,24)) +
#   #scale_y_log10(labels = c(0,10,100)) +
#   #scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels = c(0,1,10,100,200,300), breaks = c(0,1,10,100,200,300)) + 
#   ylab(expression(paste("N recruits"," ha"^"-1"," yr"^"-1")))+
#   xlab(bquote('year'))+
#   labs(title = "") +
#   adams_theme_benchFig +
#   theme(legend.position = "none")
# 
# benchmark_fig_sub_only
# 
# png(paste0(path_to_output,"forMS/","benchmark_fig_sub_only.png"), height=5, width=6, units="in", res = 100)
# print(benchmark_fig_sub_only)
# dev.off()
# 


print("FINISHED making benchmarking figure!")











