source('runs/ED2_ENSO.R')
source("create_output/figure_formatting.R")
source('utils/supporting_funcs.R')


#png options
PNGheight = 5
PNGwidth = 8 #usually 8
PNGunits = "in"
PNGres = 100

point <- geom_point(size = 2.5, stroke = 1, alpha = 1)

ENSO_long_term <- N_recs_per_year_pfts %>%
  gather(submodel:ED2, key = "model", value = "R") %>%
  mutate(model = case_when(
    model == "submodel" ~ "TRS",
    model == "ED2" ~ "ED2" 
  )) %>%
  filter(year > as.Date(as.numeric(as.Date(start_date)) + 365*3, origin = "1970-01-01")) %>%
  filter(as.numeric(stringr::str_sub(year, start = 1, end = 4)) %% 2 == 0) %>%
  mutate(model = factor(model,levels =  c("TRS","ED2"))) %>%
  #filter(model != "ED2") %>%
  ggplot(mapping = aes(x = year, y = R, color = pft, shape = model)) +
  geom_line() +
  point +
  # geom_point(data = bench4graph %>%
  #              filter(date > as.Date(as.numeric(as.Date(start_date)) + 365*3, origin = "1970-01-01")),
  #            mapping = aes(x = date, y = BCI_obs, color = pft), size = 4) +
  #geom_segment(data = bench4graph, mapping = aes(x = start_dateB, xend = end_dateB, y = BCI_obs, yend = BCI_obs, color = pft)) +
  scale_color_manual(values = pft.cols) +
  scale_x_date(limits = c(as.Date("2005-01-01"),as.Date("2034-12-31")), 
               breaks = c(as.Date("2005-01-01"), as.Date("2010-01-01"), as.Date("2015-01-01"),
                          as.Date("2020-01-01"), as.Date("2025-01-01"), as.Date("2030-01-01"),
                          as.Date("2035-01-01")),
               #date_breaks = "5 year", 
               labels = date_format("%y")) +
  scale_shape_manual(values = c(24,21)) +
  scale_y_continuous(limits = c(0,350), breaks = c(0,50,100,150,200,250,300,350)) +
  #scale_y_log10(limits = c(0,360), breaks = lseq(from = )) +
  geom_vline(xintercept = as.Date("2009-01-01"), linetype = "dotted", alpha = 0.4, color = "black") +
  #annotate(geom = "text", x = as.Date("2005-01-01"), y = 350, label = "a", size = 8) +
  geom_vline(xintercept = as.Date("2029-01-01"), linetype = "dotted", alpha = 0.4, color = "black") +
  #scale_linetype_manual(values = c("dotted","dashed")) +
  #scale_y_log10(labels = c(0,10,100)) +
  #scale_y_continuous(limits = c(0,300)) + 
  ylab(expression(paste("N recruits"," [ha"^"-1"," yr"^"-1","]")))+
  xlab(bquote('simulation year'))+
  #labs(title = paste("ENSO",percent_light * 100,"% light")) +
  facet_wrap(~model) +
  long_term_sim_theme +
  adams_guides +
  #guides(shape=FALSE) +
  theme(legend.position = c(.7,.9), 
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
         fill=guide_legend(title="PFT")) +
  color_guide
  



png(paste0(path_to_output,"forMS/","ENSO_longterm",percent_light,"pctLight",".png"), height=5, width=8, units="in", res = 100)
print(ENSO_long_term)
dev.off()


ENSO <- full_output %>%
  rename(submodel = R, ED2 = ED2_R) %>%
  gather(c(submodel,ED2), key = "model", value = "R") %>%
  mutate(model = case_when(
    model == "submodel" ~ "TRS",
    model == "ED2" ~ "ED2" 
  )) %>%
  mutate(model = factor(model,levels = c("TRS","ED2"))) %>%
  filter(date > as.Date(as.numeric(as.Date(start_date)) + 365*3, origin = "1970-01-01")) %>%
  arrange(desc(pft)) %>% 
  ggplot(aes(x = as.Date(date), y = R*365, color = pft)) +
  #custom_line +
  geom_line(position=position_dodge(width = 1)) +
  #scale_y_log10() +
  #scale_linetype_manual(values = c("dashed","solid")) +
  #year_axis +
  ylab(expression(paste('N recruits'," [ha"^"-1"," year"^"-1","]")))+
  scale_y_continuous(limits = c(0,355), breaks = c(0,50,100,150,200,250,300,350)) +
  scale_x_date(limits = c(as.Date("2028-10-01"),as.Date("2029-10-01")), 
               date_breaks = "2 months", 
               labels = date_format("%b")) +
  xlab("month of simulation yrs 29/30")+
  #labs(title = paste('Recruitment across \n an ENSO event at',percent_light * 100,"% light")) +
  scale_color_manual(values = pft.cols) +
  #geom_line(mapping = aes(x = as.Date(date), y = SMP)) +
  facet_wrap(~model) +
  long_term_sim_theme +
  adams_guides +
  theme(legend.position = "none",
        #legend.direction = "horizontal",
        legend.box.background = element_rect(colour = "black"),
        legend.text = element_text(size = 12),
        legend.margin = margin(0.1,0.1,0.1,0.1, unit="cm"),
        legend.spacing.y = unit(0.005,"cm"),
        legend.spacing.x = unit(0.01,"cm"),
        legend.key.size = unit(0.5, "cm"),
        panel.spacing = unit(0.01, "lines"))+
  guides(color = guide_legend(override.aes = list(size=3)),
         shape = guide_legend(override.aes = list(size=3)),
         fill=guide_legend(title="PFT"))
  
makePNG(fig = ENSO, path_to_output.x = paste0(path_to_output,"forMS/"), file_name = "ENSO_shorterm")
# muiltipanel.ENSO <- plot_grid(ENSO_long_term, ENSO, align = 'v', axis = 'l')
# 
# muiltipanel.ENSO.anno  <- ggdraw(add_sub(test_plot.x, "light at seedling layer (% TOC)", vpadding=grid::unit(1,"lines"), size = axis_size ))
# test_plot
# 
# PNGwidth <- 9
# makePNG(fig = test_plot, path_to_output.x = paste0(path_to_output,"forMS/"), file_name = "rec_vs_light")
# 

# png(paste0(path_to_output,"forMS/","ENSO",percent_light,"pctLight",".png"), height=5, width=8, units="in", res = 100)
# print(ENSO)
# dev.off()







# require(gridExtra)
# library(gtable)
# library(grid)


#calculating recruitment drop over ENSO years
EnsoData <- full_output %>%
  rename(submodel = R, ED2 = ED2_R) %>%
  gather(c(submodel,ED2), key = "model", value = "R") %>%
  mutate(model = case_when(
    model == "submodel" ~ "TRS",
    model == "ED2" ~ "ED2" 
  )) %>%
  mutate(model = factor(model,levels = c("TRS","ED2"))) %>%
  filter(date > as.Date(as.numeric(as.Date(start_date)) + 365*3, origin = "1970-01-01")) %>%
  arrange(desc(pft))

names(EnsoData)

EnsoData1 <- EnsoData %>% dplyr::select(yr,date,pft,R,model) %>% arrange(yr,date,model,pft,R) %>%
  group_by(yr,pft,model) %>%
  summarise(r = sum(R)) %>%
  filter(yr %in% c(2008,2009,2028,2029)) %>%
  mutate(dtvsdi = case_when(
    pft %in% c("LD_DI","ST_DI") ~ "DI",
    pft %in% c("LD_DT","ST_DT") ~ "DT"
  )) %>%
  mutate(ENSOyr = case_when(
    yr %in% c("2009","2029") ~ TRUE,
    TRUE ~ FALSE
  )) %>% 
  group_by(model,ENSOyr) %>%
  summarise(r = mean(r))


trs <- EnsoData1 %>% filter(model == "TRS") %>%
  pull(r)
trs_pct_reduction <- (trs[1] - trs[2]) / trs[1]


ED2 <- EnsoData1 %>% filter(model == "ED2") %>%
  pull(r)
ED2_pct_reduction <- (ED2[1] - ED2[2]) / ED2[1]











print("FINISHED making ENSO figure")

