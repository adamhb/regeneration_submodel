

source('runs/ED2_WET.R')

WET_long_term <- N_recs_per_year_pfts %>%
  gather(submodel:ED2, key = "model", value = "R") %>%
  filter(year > as.Date(as.numeric(as.Date(start_date)) + 365*3, origin = "1970-01-01")) %>%
  #filter(model != "ED2") %>%
  ggplot(mapping = aes(x = year, y = R, color = pft, shape = model)) +
  geom_point(size = 2.5, stroke = 1, alpha = 1) +
  # geom_point(data = bench4graph %>%
  #              filter(date > as.Date(as.numeric(as.Date(start_date)) + 365*3, origin = "1970-01-01")),
  #            mapping = aes(x = date, y = BCI_obs, color = pft), size = 4) +
  #geom_segment(data = bench4graph, mapping = aes(x = start_dateB, xend = end_dateB, y = BCI_obs, yend = BCI_obs, color = pft)) +
  scale_color_manual(values = pft.cols) +
  #scale_x_date(limits = c(as.Date("2007-10-01"),as.Date("2014-01-01")), date_breaks = "1 year", labels = date_format("%Y")) +
  scale_shape_manual(values = c(21,24)) +
  #scale_y_log10(labels = c(0,10,100)) +
  scale_y_log10() + 
  ylab(expression(paste("N recruits"," ha"^"-1"," yr"^"-1")))+
  xlab(bquote('year'))+
  labs(title = "WET 2% light") +
  adams_theme +
  guides(shape = guide_legend(override.aes = list(size = legend_symbol_size))) +
  guides(color = guide_legend(override.aes = list(size = legend_symbol_size)),
         fill=guide_legend(title="PFT"))
  #theme(legend.key.size = unit(6,"point"))

png(paste0(path_to_output,"forMS/","WET_long_term.png"), height=5, width=8, units="in", res = 100)
print(WET_long_term)
dev.off()

print("FINISHED making WET figure")


source('runs/ED2_ENSO.R')

ENSO_long_term <- N_recs_per_year_pfts %>%
  gather(submodel:ED2, key = "model", value = "R") %>%
  filter(year > as.Date(as.numeric(as.Date(start_date)) + 365*3, origin = "1970-01-01")) %>%
  #filter(model != "ED2") %>%
  ggplot(mapping = aes(x = year, y = R, color = pft, shape = model)) +
  geom_point(size = 2.5, stroke = 1, alpha = 1) +
  # geom_point(data = bench4graph %>%
  #              filter(date > as.Date(as.numeric(as.Date(start_date)) + 365*3, origin = "1970-01-01")),
  #            mapping = aes(x = date, y = BCI_obs, color = pft), size = 4) +
  #geom_segment(data = bench4graph, mapping = aes(x = start_dateB, xend = end_dateB, y = BCI_obs, yend = BCI_obs, color = pft)) +
  scale_color_manual(values = pft.cols) +
  #scale_x_date(limits = c(as.Date("2007-10-01"),as.Date("2014-01-01")), date_breaks = "1 year", labels = date_format("%Y")) +
  scale_shape_manual(values = c(21,24)) +
  #scale_y_log10(labels = c(0,10,100)) +
  scale_y_continuous(limits = c(0,300)) + 
  ylab(expression(paste("N recruits"," ha"^"-1"," yr"^"-1")))+
  xlab(bquote('year'))+
  labs(title = "ENSO 45% light") +
  adams_theme 

png(paste0(path_to_output,"forMS/","ENSO_long_term.png"), height=5, width=8, units="in", res = 100)
print(ENSO_long_term)
dev.off()




ENSO <- full_output %>%
  rename(submodel = R, ED2 = ED2_R) %>%
  gather(c(submodel,ED2), key = "model", value = "R") %>%
  filter(date > as.Date(as.numeric(as.Date(start_date)) + 365*3, origin = "1970-01-01")) %>%
  arrange(desc(pft)) %>% 
  ggplot(aes(x = as.Date(date), y = R*365, color = pft, linetype = model)) +
  #custom_line +
  geom_line(position=position_dodge(width = 20)) +
  #scale_y_log10() +
  scale_linetype_manual(values = c("dashed","solid")) +
  year_axis +
  ylab(expression(paste('N recruits'," ha"^"-1"," year"^"-1")))+
  scale_x_date(limits = c(as.Date("2028-10-01"),as.Date("2029-10-01")), 
               date_breaks = "2 months", 
               labels = date_format("%b")) +
  xlab(bquote(''))+
  labs(title = paste('Recruitment across \n an ENSO event at',percent_light * 100,"% light")) +
  theme_classic() +
  #geom_line(mapping = aes(x = as.Date(date), y = SMP)) +
  adams_theme +
  scale_color_manual(values = pft.cols)


png(paste0(path_to_output,"forMS/","ENSO.png"), height=5, width=8, units="in", res = 100)
print(ENSO)
dev.off()

print("FINISHED making ENSO figure")
