#This script uses the output of the Tree Regeneration Scheme to create figures
#that are primarily diagnostic.
#This script is required to generate figures for Hanbury-Brown et al., in prep

source("create_output/figure_formatting.R")
print(paste("generating output figures...",Sys.time()))

#use new benchmarking data
new_bench <- F

if(new_bench == T){
  source("benchmarking/create_recruitment_benchmarks.R")
}

bench <- read_csv("benchmarking/bci_rec_benchmarks_long.csv")

bench4graph <- bench %>%
  mutate(year = substring(text = as.character(date), first = 1, last = 4)) %>%
  filter(date > start_date,
         date < end_date) %>%
  group_by(pft,int) %>%
  summarise(BCI_obs = mean(rec_rate),
            start_dateB = min(date),
            end_dateB = max(date),
            date = mean(date)) %>%
  mutate(model = "BCI obs.")



#time stamp
model_run_time_stamp <- Sys.time() %>% 
  sub(pattern = ":", replacement = "-") %>%
  sub(pattern = ":", replacement = "-") %>%
  sub(pattern = " ", replacement = "-")

#create folder to store output
path_to_this_run_output <- paste0(path_to_output,run_name,"_",sub(pattern = " ", replacement = "",x = model_run_time_stamp))

if(patch_run_type == "many"){
  path_to_this_run_output <- paste0(path_to_this_run_output,"bin_num-",bin_num)
}

dir.create(path = path_to_this_run_output)


if(run_type == "ED2"){
  dbh.x <- full_output %>%
    group_by(pft) %>%
    summarise(mean_dbh_pft = mean(dbh)) %>%
    pull(mean_dbh_pft) %>%
    round() %>%
    paste0(collapse = ",")
  
  N_co.x <- full_output %>%
    group_by(pft) %>%
    summarise(mean_N_co = mean(N_co)) %>%
    pull(mean_N_co) %>%
    round() %>%
    paste0(collapse = ",")
}


if(patch_run_type == "many"){
  percent_light <- mean(tmp_patch_data$lightZ0)
}


#put the param used for run in the output folder
write.csv(paramsOFrun, file = paste0(path_to_this_run_output,"/params.csv"))





start_yr <- substr(lubridate::ymd(as.Date(min(full_output$date))), start = 1, stop = 4)
end_yr <- substr(lubridate::ymd(as.Date(max(full_output$date))), start = 1, stop = 4)
yrs <- as.numeric(end_yr) - as.numeric(start_yr)


if(yrs > 40){
  date_breaks_custom <- "10 years"
  custom_line <- smoother_line
} else{
  date_breaks_custom <- "2 years"
  custom_line <- geom_line()
}


year_axis <- scale_x_date(breaks = date_breaks(date_breaks_custom), labels = date_format("%y"))



# NPP comes out in g C per day per PFT
# NPP for each PFT 

#str(full_output)

ggplot(data = full_output, mapping = aes(x = ))

NPP_g <- ggplot(data = full_output, aes(x = as.Date(date), y = NPP*10000, color = pft)) +
  custom_line +
  labs(title = "NPP") +
  theme_classic()+
  #smoother_line +
  year_axis +
  ylab(expression(paste("NPP ", "(g C ha"^"-2","day"^"-1",")")))+
  xlab(bquote('year'))+
  scale_color_manual(values = pft.cols) +
  adams_theme 
  #theme(legend.position = "none")

png(paste0(path_to_this_run_output,"/01_NPP.png"), height=5, width=8, units="in", res = 100)
print(NPP_g)
dev.off()




#graphing the fraction of NPP going to reproduction
p1 <- ggplot(data = full_output, aes(x = as.Date(date), y = e_frac, color = pft)) +
  custom_line +
  year_axis +
  ylab(bquote('fraction of NPP going to reproduction'))+
  xlab(bquote('year'))+
#   geom_text(mapping = aes(x = median(as.Date(date)), y = 0.2), data = full_output, label = paste("early",full_output$e_frac %>% head(.,n=1)), color = "black") +
#   geom_text(mapping = aes(x = median(as.Date(date)), y = 0.17), data = full_output, label = paste("late",full_output$e_frac %>% tail(.,n=1)), color = "black") +
  labs(title = "The frac. of NPP allocated to repro.") +
  theme_classic()+
  adams_theme+
  scale_color_manual(values = pft.cols)

#xlab("Production Type")+
#ylab(expression(paste("Kg ", "CO" ["2(eq)"], " per Kg fresh tomato" )))

png(paste0(path_to_this_run_output,"/02_e_frac.png"), height=5, width=8, units="in", res = 100)
print(p1)
dev.off()



#graphing the carbon allocated to reproduction
p2 <- ggplot(data = full_output, aes(x = as.Date(date), y = c_repro, color = pft)) +
  custom_line +
  #smoother_line +
  year_axis +
  ylab(expression(paste("carbon for repro.", "(g C day"^"-1","ha"^"-2",")")))+
  xlab(bquote('year'))+
  labs(title = "C allocated to reproduction per day") +
  theme_classic() +
  adams_theme +
  theme(legend.position = "none") +
  scale_color_manual(values = pft.cols)

png(paste0(path_to_this_run_output,"/03_C_for_repro.png"), height=5, width=8, units="in", res = 100)
print(p2)
dev.off()




p2b <- ggplot(data = full_output, aes(x = as.Date(date), y = c_repro, color = pft)) +
  geom_line() +
  #smoother_line +
  year_axis +
  ylab(expression(paste("carbon for repro.", "(g C day"^"-1","ha"^"-2",")")))+
  xlab(bquote('year'))+
  labs(title = "C allocated to reproduction per day") +
  theme_classic() +
  adams_theme +
  theme(legend.position = "none")+
  scale_color_manual(values = pft.cols)

png(paste0(path_to_this_run_output,"/03_C_for_repro_no_smoothing.png"), height=5, width=8, units="in", res = 100)
print(p2b)
dev.off()



#graphing seedbank size


p3 <- ggplot(data = full_output, aes(x = as.Date(date), y = seedbank, color = pft)) +
  #smoother_line +
  custom_line +
  year_axis +
  ylab(expression(paste("seedbank size ", " (g C ","ha"^"-1",")")))+
  xlab(bquote('year'))+
  labs(title = "Seed bank size (g C)") +
  theme_classic() +
  adams_theme +
  theme(legend.position = "none")+
  scale_color_manual(values = pft.cols)

png(paste0(path_to_this_run_output,"/04_SeedBank.png"), height=5, width=8, units="in", res = 100)
print(p3)
dev.off()

# full_output %>%
#   filter(date > "2020-04-08" & date < "2020-04-14") %>%
#   ggplot(aes(x = as.Date(date), y = seedbank, color = pft)) +
#   #smoother_line +
#   custom_line +
#   scale_x_date() +
#   ylab(expression(paste("seedbank size ", " (g C ","ha"^"-1",")")))+
#   xlab(bquote('year'))+
#   labs(title = "Seed bank size (g C)") +
#   theme_classic() +
#   adams_theme +
#   theme(legend.position = "none")+
#   scale_color_manual(values = pft.cols)
#   




#add precip to this on a second axis
#graphing the fraction of the seedbank emerging each day
p4 <- ggplot(data = full_output, aes(x = as.Date(date), y = frac_emerging, color = pft)) +
  custom_line +
  #smoother_line +
  #geom_smooth(size = 1.8, method = "loess", span = .01, se = F, lty = 1)+
  year_axis +
  ylab(expression(paste("frac. of seedbank emerging"," (day)"^"-1")))+
  xlab(bquote('year'))+
  labs(title = expression(paste("Seedling Emergence"," (day)"^"-1"))) +
  #theme(legend.position = "none")+
  scale_color_manual(values = pft.cols)+
  scale_linetype_manual(values = c(1,2,1,2))+
  theme_classic() +
  adams_theme

png(paste0(path_to_this_run_output,"/05_frac_emerging.png"), height=5, width=8, units="in", res = 100)
print(p4)
dev.off()


p4b <- ggplot(data = full_output, aes(x = as.Date(date), y = frac_emerging, color = pft)) +
  custom_line +
  #smoother_line +
  #geom_smooth(size = 1.8, method = "loess", span = .01, se = F, lty = 1)+
  scale_x_date(breaks = date_breaks("1 month"), 
               labels = date_format("%b"),
               limits = as.Date(c("2004-01-01","2005-01-01")))+
  #year_axis +
  ylab(expression(paste("frac. of seedbank emerging"," (day)"^"-1")))+
  xlab(bquote('year'))+
  labs(title = paste0("b = ",b_emerg[1])) +
  #theme(legend.position = "none")+
  scale_color_manual(values = pft.cols)+
  scale_linetype_manual(values = c(1,2,1,2))+
  theme_classic() +
  adams_theme +
  theme(axis.text.x = element_text(size = 12))



png(paste0(path_to_this_run_output,"/05_frac_emerging_seasonal_cycle.png"), height=5, width=8, units="in", res = 100)
print(p4b)
dev.off()




p4b_smp <- ggplot(data = full_output, aes(x = as.Date(date), y = SMP, color = pft)) +
  custom_line +
  #smoother_line +
  #geom_smooth(size = 1.8, method = "loess", span = .01, se = F, lty = 1)+
  scale_x_date(breaks = date_breaks("1 month"), 
               labels = date_format("%b"),
               limits = as.Date(c("2004-01-01","2005-01-01")))+
  #year_axis +
  ylab(expression(paste("frac. of seedbank emerging"," (day)"^"-1")))+
  xlab(bquote('year'))+
  labs(title = paste0("b_LD = ",b_emerg[1], "a = ", a_emerg[1])) +
  #theme(legend.position = "none")+
  scale_color_manual(values = pft.cols)+
  scale_linetype_manual(values = c(1,2,1,2))+
  theme_classic() +
  adams_theme +
  theme(axis.text.x = element_text(size = 12))




#graphing the seedling pool
p5 <- ggplot(data = full_output, aes(x = as.Date(date), y = seedpool, color = pft)) +
  custom_line +
  #geom_line(size = 1.8)+
  year_axis +
  ylab(bquote('seedling pool size (gC)'))+
  xlab(bquote('year'))+
  labs(title = 'seedling pool size (gC)') +
  scale_color_manual(values = pft.cols)+
  theme_classic() +
  adams_theme



png(paste0(path_to_this_run_output,"/06_seedling_pool.png"), height=5, width=8, units="in", res = 100)
print(p5)
dev.off()


#graphing the light mortality rate
p6 <- ggplot(data = full_output, aes(x = as.Date(date), y = light_mort_rate, color = pft)) +
  custom_line +
  year_axis +
  ylab(bquote('daily mort rate (% of seedling pool)'))+
  xlab(bquote('year'))+
  #scale_color_manual(values = c("darkolivegreen4", "midnightblue"))+
  labs(title = paste('light-dep. seedling mortality',paramsOFrun$param_vals[12],"TOC")) +
  theme_classic() +
  adams_theme +
  scale_color_manual(values = pft.cols)


png(paste0(path_to_this_run_output,"/07_light_mort_3pct.png"), height=5, width=8, units="in", res = 100)
print(p6)
dev.off()



#graphing H20 mortality rate
p7 <- ggplot(data = full_output, aes(x = as.Date(date), y = H20_mort_rate, color = pft)) +
  custom_line +
  year_axis +
  ylab(expression(paste('H20 Mort. Rate'," (day)"^"-1"))) +
  #ylab(bquote('H20 mort rate'))+
  xlab(bquote('year'))+
  labs(title = 'H20 mort rate') +
  #scale_color_manual(values = c("darkolivegreen2", "darkolivegreen4"))+
  theme_classic() +
  adams_theme+
  scale_color_manual(values = pft.cols)


png(paste0(path_to_this_run_output,"/08_H20_mort_rate.png"), height=5, width=8, units="in", res = 100)
print(p7)
dev.off()


# precip_g <- ggplot(data = full_output, aes(x = as.Date(date), y = precip, color = pft)) +
#   labs(title = "02_precip.") +
#   theme_classic()+
#   #geom_point()+
#   smooth_line +
#   year_axis +
#   ylab(expression(paste("daily precip (mm)")))+
#   xlab(bquote('year'))+
#   adams_theme +
#   theme(legend.position = "none")+
#   scale_color_manual(values = (rep("blue",4)))

# png(paste0(path_to_this_run_output,"/09_precip.png"), height=5, width=8, units="in", res = 100)
# print(precip_g)
# dev.off()



#graphing the soil matric potential over time

SMP_MPa_g <- ggplot(data = full_output, aes(x = as.Date(date), y = SMP/1e5)) +
  labs(title = "Soil Matric Potential (MPa)") +
  theme_classic()+
  geom_line(size = 1.8, color = "black")+
  #smooth_line +
  year_axis +
  ylab(expression(paste("SMP (MPa)")))+
  xlab(bquote('year'))+
  adams_theme +
  theme(legend.position = "none") +
  geom_hline(yintercept = psi_crit[1]/1e5, size = 1.8, color = "darkolivegreen2")+
  geom_hline(yintercept = psi_crit[2]/1e5, size = 1.8, color = "darkolivegreen4")#+
#geom_text(mapping = aes(x = median(as.Date(date)), y = thresh.xx[1]/1e5), data = full_output, label = "DI threshold", color = "black") +
#geom_text(mapping = aes(x = median(as.Date(date)), y = thresh.xx[2]/1e5), data = full_output, label = "DT threshold", color = "black")

png(paste0(path_to_this_run_output,"/10_SMP.png"), height=5, width=8, units="in", res = 100)
print(SMP_MPa_g)
dev.off()



water_def_g <- ggplot(data = full_output, aes(x = as.Date(date), y = water_def, color = pft)) +
  labs(title = "Water Deficit Days") +
  theme_classic()+
  geom_line(size = 1.8)+
  custom_line +
  year_axis +
  ylab(expression(paste("Deficit Days (cum. sum of SMP deficit)")))+
  xlab(bquote('year'))+
  adams_theme +
  #theme(legend.position = "none") +
  geom_hline(yintercept = psi_crit[1]/1e5)+
  geom_hline(yintercept = psi_crit[2]/1e5)#+
#geom_text(mapping = aes(x = median(as.Date(date)), y = thresh.xx[1]/1e5), data = full_output, label = "DI threshold", color = "black") +
#geom_text(mapping = aes(x = median(as.Date(date)), y = thresh.xx[2]/1e5), data = full_output, label = "DT threshold", color = "black")

png(paste0(path_to_this_run_output,"/11_water_def.png"), height=5, width=8, units="in", res = 100)
print(water_def_g)
dev.off()


#graphing the fraction recruiting from the seedling pool to the adult size class
p8 <- ggplot(data = full_output, aes(x = as.Date(date), y = frac_rec.t, color = pft)) +
  custom_line +
  #smooth_line +
  year_axis +
  ylab(expression(paste('rec. rate (% of seedling pool'," day"^"-1",")")))+
  xlab(bquote('year'))+
  labs(title = 'seedling recruitment rate (%)') +
  theme_classic() +
  adams_theme +
  scale_color_manual(values = pft.cols)

png(paste0(path_to_this_run_output,"/12_fraction_recruiting.png"), height=5, width=8, units="in", res = 100)
print(p8)
dev.off()



#graphing the daily recruitment rate without the total
p9 <- full_output %>%
  rename(submodel = R, ED2 = ED2_R) %>%
  gather(c(submodel,ED2), key = "model", value = "R") %>%
  filter(date > as.Date(as.numeric(as.Date(start_date)) + 365*3, origin = "1970-01-01")) %>%
  arrange(desc(pft)) %>% 
  ggplot(aes(x = as.Date(date), y = R*365, color = pft, linetype = model)) +
  #custom_line +
  geom_line() +
  scale_y_log10() +
  scale_linetype_manual(values = c("dashed","solid")) +
  year_axis +
  ylab(expression(paste('N recruits'," ha"^"-1"," year"^"-1")))+
  xlab(bquote('year'))+
  labs(title = 'annual number of recruits') +
  theme_classic() +
  #geom_line(mapping = aes(x = as.Date(date), y = SMP)) +
  adams_theme +
  scale_color_manual(values = pft.cols)

#+
#geom_smooth(data = full_output %>% group_by(date) %>% summarise(total_R = sum(R), pft = "total"), 
#mapping = aes(x = as.Date(date), y = total_R, color = pft),
#size = 1.8, method = "loess", span = .01, se = F) 

png(paste0(path_to_this_run_output,"/13_annual_N_recruits.png"), height=5, width=8, units="in", res = 100)
print(p9)
dev.off()


p9 <- full_output %>%
  rename(submodel = R, ED2 = ED2_R) %>%
  gather(c(submodel,ED2), key = "model", value = "R") %>%
  filter(date > as.Date(as.numeric(as.Date(start_date)) + 365*3, origin = "1970-01-01")) %>%
  filter(model == "submodel") %>%
  arrange(desc(pft)) %>% 
  ggplot(aes(x = as.Date(date), y = R*365, color = pft, linetype = model)) +
  #custom_line +
  geom_line() +
  #scale_y_log10() +
  scale_linetype_manual(values = c("dashed","solid")) +
  year_axis +
  ylab(expression(paste('N recruits'," ha"^"-1"," year"^"-1")))+
  xlab(bquote('year'))+
  labs(title = 'recruitment rate') +
  theme_classic() +
  #geom_line(mapping = aes(x = as.Date(date), y = SMP)) +
  adams_theme +
  scale_color_manual(values = pft.cols)

#+
#geom_smooth(data = full_output %>% group_by(date) %>% summarise(total_R = sum(R), pft = "total"), 
#mapping = aes(x = as.Date(date), y = total_R, color = pft),
#size = 1.8, method = "loess", span = .01, se = F) 

png(paste0(path_to_this_run_output,"/13_annual_N_recruits_just_submodel.png"), height=5, width=8, units="in", res = 100)
print(p9)
dev.off()




#graphing the annual recruitment rate with the total
#rec_bench_totals.df <- data.frame(bench = rec_bench_totals[-1], date = as.Date(c("2007-06-01", "2012-06-01")))

p10 <- full_output  %>% arrange(desc(pft)) %>% ggplot( aes(x = as.Date(date), y = R*365, color = pft)) +
  geom_line() +
  #smooth_line +
  year_axis +
  ylab(expression(paste('N recruits'," ha"^"-1"," yr"^"-1")))+
  xlab(bquote('year'))+
  labs(title = 'Recruitment Rate') +
  theme_classic() +
  adams_theme +
  geom_smooth(data = full_output %>% group_by(date) %>% summarise(total_R = sum(R), pft = "total"), 
              mapping = aes(x = as.Date(date), y = total_R*365, color = pft),
              size = 1.8, method = "loess", span = .01, se = F)+
  geom_point(aes(x  = as.Date("2007-06-01"), y = 77), color = "black", size = 5, pch = 2) +
  geom_point(aes(x  = as.Date("2012-06-01"), y = 125), color = "black", size = 5, pch = 2) +
  scale_color_manual(values = append(pft.cols, "black"))

png(paste0(path_to_this_run_output,"/recruitment_rate_w_total.png"), height=5, width=8, units="in", res = 100)
print(p10)
dev.off()


#creating table of the annual number of recruits per year per PFT
N_recs_per_year_pfts <- full_output %>% 
  mutate(year = substring(text = as.character(date), first = 1, last = 4)) %>%
  mutate_at(.vars = "date", .funs = as.Date) %>%
  left_join(bench4graph, by = c("pft","date")) %>%
  group_by(year, pft) %>% 
  summarise(submodel = sum(R),
            ED2 = sum(ED2_R))

N_recs_per_year_pfts$year <- as.Date(paste0((as.numeric(N_recs_per_year_pfts$year)+1), "-01-01"))


############################################
############################################
#############################################
################NEW GRAPHS#################



Submodel_annual_rec <- N_recs_per_year_pfts %>%
  gather(submodel:ED2, key = "model", value = "R") %>%
  filter(year > as.Date(as.numeric(as.Date(start_date)) + 365*3, origin = "1970-01-01")) %>%
  #filter(model != "ED2") %>%
  ggplot(mapping = aes(x = year, y = R, color = pft, shape = model)) +
  geom_point(size = 2.5, stroke = 1, alpha = 1) +
  geom_point(data = bench4graph %>%
               filter(date > as.Date(as.numeric(as.Date(start_date)) + 365*3, origin = "1970-01-01")),
             mapping = aes(x = date, y = BCI_obs, color = pft), size = 4) +
  #geom_segment(data = bench4graph, mapping = aes(x = start_dateB, xend = end_dateB, y = BCI_obs, yend = BCI_obs, color = pft)) +
  scale_color_manual(values = pft.cols) +
  scale_shape_manual(values = c(10,21,24)) +
  scale_y_log10() +
  ylab(expression(paste('N recruits'," ha"^"-1"," yr"^"-1")))+
  xlab(bquote('year'))+
  labs(title = run_name) +
  adams_theme 

png(paste0(path_to_this_run_output,"/16_Recruitment_Annual_Sums.png"), height=5, width=8, units="in", res = 100)
print(Submodel_annual_rec)
dev.off()



# Submodel_annual_rec <- ggplot() +
#   custom_line +
#   ylab(expression(paste('N recruits'," ha"^"-1"," yr"^"-1")))+
#   xlab(bquote('year'))+
#   labs(title = 'Trends in Annual PFT-specific Recruitment') +
#   #geom_smooth(data = full_output %>% group_by(date) %>% summarise(total_R = sum(R), pft = "total"), mapping = aes(x = as.Date(date), y = total_R*365, color = pft), size = 1.8, method = "loess", span = .1, se = F) +
#   geom_point(data = N_recs_per_year_pfts, mapping = aes(x = year, y = N_rec, color = pft), size = 8) +
#   #geom_point(data = graphable_bench_data %>% filter(date >= as.Date("2005-01-01") & date <= as.Date("2015-01-01")), mapping = aes(x = date, y = rec_rate, color = pft), size = 1) +
#   #geom_segment(data = graphable_bench_data %>% filter(date <= as.Date("2015-01-01")), mapping = aes(x = date, y = rec_rate, color = pft), xend = as.Date("2010-01-01"), yend = 20)+
#   #scale_linetype_manual(values = "yellow2")+
#   theme_classic() +
#   adams_theme +
#   year_axis +
#   scale_color_manual(values = pft.cols)

# png(paste0(path_to_this_run_output,"/16_Recruitment_Annual_Sums.png"), height=5, width=8, units="in", res = 100)
# print(Submodel_annual_rec)
# dev.off()

print(paste("Finished generating output",Sys.time(), "Figures are in", path_to_this_run_output))


