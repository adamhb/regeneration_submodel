
#create folder to store output
path_to_this_run_output <- paste0(path_to_output,"/",run_name,"_",sub(pattern = " ", replacement = "",x = Sys.time()))
dir.create(path = path_to_this_run_output)


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}






#set theme for the plots
adams_theme <- theme(plot.title = element_text(hjust = 0.5, size = 20),
                     strip.text.x = element_text(size = 18),
                     legend.title = element_blank (),
                     axis.title.x = element_text (size = 15), # change the axis title
                     axis.title.y = element_text (size = 15),
                     axis.title.y.right = element_text (size = 15, color = pft.cols[2]),
                     axis.text.x = element_text (size = 14, colour = "black"),
                     axis.text.y = element_text (size = 14, colour = "black"),
                     legend.text = element_text (size = 15))
year_axis <-  scale_x_date(breaks = date_breaks("2 years"), labels = date_format("%Y"))
smooth_line <- geom_smooth(size = 1.2, method = "loess", span = .01, se = F)
smoother_line <- geom_smooth(size = 1.8, method = "loess", span = .1, se = F)
smooth_line_black <- geom_smooth(size = 1.8, method = "loess", span = .01, se = F, color = "black")








#create a folder for the output
dir.create(path = paste0("C:/Users/ahanb/OneDrive/Documents/rec_submodel/Submodel_Output/",run_name), showWarnings = T)

#record the params
paramsOFrun <- data.frame(param_names = c("model_area", "dbh.x", "N_co.x", "Dmax", "frac_repro", "seed_frac","decay_rate", "a_emerg", "b_emerg", "a_rec", "b_rec", "percent_light", "thresh", "window.x", "seedbank_0", "seedpool_0", "litter_0"), param_vals = c(model_area, dbh.x, N_co.x, paste0(Dmax, collapse = ","),paste0(frac_repro, collapse = ","), seed_frac, decay_rate, paste0(a_emerg, collapse = ","), paste0(b_emerg, collapse = ","), paste0(a_rec, collapse = ","), paste0(b_rec, collapse = ","), percent_light, paste0(thresh.xx, collapse = ","), window.x, seedbank_0, seedpool_0, litter_0))

#put the params file in the output folder
write.csv(paramsOFrun, file = paste0("C:/Users/ahanb/OneDrive/Documents/rec_submodel/Submodel_Output/",run_name,"/params.csv"))


setwd(paste0("C:/Users/ahanb/OneDrive/Documents/rec_submodel/Submodel_Output/",run_name))



# NPP for each PFT 


NPP_g <- ggplot(data = full_output, aes(x = as.Date(date), y = NPP*10000, color = pft)) +
  labs(title = "NPP") +
  theme_classic()+
  smooth_line +
  year_axis +
  ylab(expression(paste("NPP ", "(g C ha"^"-1","day"^"-1",")")))+
  xlab(bquote('year'))+
  adams_theme +
  theme(legend.position = "none")

png(paste0(getwd(),"/01_NPP.png"), height=5, width=8, units="in", res = 100)
NPP_g
dev.off()




#graphing the fraction of NPP going to reproduction
p1 <- ggplot(data = full_output, aes(x = as.Date(date), y = e_frac, color = pft)) +
  geom_line(size = 1.8)+
  year_axis +
  ylab(bquote('fraction of NPP going to reproduction'))+
  xlab(bquote('year'))+
  geom_text(mapping = aes(x = median(as.Date(date)), y = 0.2), data = full_output, label = paste("early",full_output$e_frac %>% head(.,n=1)), color = "black") +
  geom_text(mapping = aes(x = median(as.Date(date)), y = 0.17), data = full_output, label = paste("late",full_output$e_frac %>% tail(.,n=1)), color = "black") +
  labs(title = "The frac. of NPP allocated to repro.") +
  theme_classic()+
  adams_theme+
  scale_color_manual(values = pft.cols)

#xlab("Production Type")+
#ylab(expression(paste("Kg ", "CO" ["2(eq)"], " per Kg fresh tomato" )))

png(paste0(getwd(),"/02_e_frac.png"), height=5, width=8, units="in", res = 100)
p1
dev.off()



#graphing the carbon allocated to reproduction
p2 <- ggplot(data = full_output, aes(x = as.Date(date), y = c_repro, color = pft)) +
  smooth_line +
  year_axis +
  ylab(expression(paste("carbon for repro.", "(g C day"^"-1","ha"^"-1",")")))+
  xlab(bquote('year'))+
  labs(title = "C allocated to reproduction per day") +
  theme_classic() +
  adams_theme +
  theme(legend.position = "none")+
  scale_color_manual(values = pft.cols)

png(paste0(getwd(),"/03_C_for_repro.png"), height=5, width=8, units="in", res = 100)
p2
dev.off()



#graphing seedbank size
p3 <- ggplot(data = full_output, aes(x = as.Date(date), y = seedbank, color = pft)) +
  geom_line(size = 1.8)+
  year_axis +
  ylab(expression(paste("seedbank size ", " (g C ","ha"^"-1",")")))+
  xlab(bquote('year'))+
  labs(title = "Seed bank size (g C)") +
  theme_classic() +
  adams_theme +
  theme(legend.position = "none")+
  scale_color_manual(values = pft.cols)

png(paste0(getwd(),"/04_SeedBank.png"), height=5, width=8, units="in", res = 100)
p3
dev.off()


#add precip to this on a second axis
#graphing the fraction of the seedbank emerging each day
p4 <- ggplot(data = full_output, aes(x = as.Date(date), y = frac_emerging, color = pft)) +
  #geom_line()
  geom_smooth(size = 1.8, method = "loess", span = .01, se = F, lty = 1)+
  year_axis +
  ylab(expression(paste("frac. of seedbank emerging"," (day)"^"-1")))+
  xlab(bquote('year'))+
  labs(title = expression(paste("Seedling Emergence"," (day)"^"-1"))) +
  #theme(legend.position = "none")+
  scale_color_manual(values = pft.cols)+
  scale_linetype_manual(values = c(1,2,1,2))+
  theme_classic() +
  adams_theme

png(paste0(getwd(),"/05_frac_emerging.png"), height=5, width=8, units="in", res = 100)
p4
dev.off()


#graphing the seedling pool
p5 <- ggplot(data = full_output %>% filter(date >= as.POSIXct("2005-01-01")), aes(x = as.Date(date), y = seedpool, color = pft)) +
  geom_line(size = 1.8)+
  year_axis +
  ylab(bquote('seedling pool size (gC)'))+
  xlab(bquote('year'))+
  labs(title = 'seedling pool size (gC)') +
  scale_color_manual(values = pft.cols)+
  theme_classic() +
  adams_theme



png(paste0(getwd(),"/06_seedling_pool.png"), height=5, width=8, units="in", res = 100)
p5
dev.off()


#graphing the light mortality rate
p6 <- ggplot(data = full_output %>% filter(date >= as.POSIXct("2005-01-01")), aes(x = as.Date(date), y = light_mort_rate, color = pft)) +
  geom_line(size = 1.8)+
  year_axis +
  ylab(bquote('daily mort rate (% of seedling pool)'))+
  xlab(bquote('year'))+
  #scale_color_manual(values = c("darkolivegreen4", "midnightblue"))+
  labs(title = 'light-dep. seedling mortality (3% light)') +
  theme_classic() +
  adams_theme +
  scale_color_manual(values = pft.cols)


png(paste0(getwd(),"/07_light_mort_3pct.png"), height=5, width=8, units="in", res = 100)
p6
dev.off()



#graphing H20 mortality rate
p7 <- ggplot(data = full_output %>% filter(date >= as.POSIXct("2005-01-01")), aes(x = as.Date(date), y = H20_mort_rate, color = pft)) +
  geom_line(size = 1.8)+
  year_axis +
  ylab(expression(paste('H20 Mort. Rate'," (day)"^"-1"))) +
  #ylab(bquote('H20 mort rate'))+
  xlab(bquote('year'))+
  labs(title = 'H20 mort rate') +
  #scale_color_manual(values = c("darkolivegreen2", "darkolivegreen4"))+
  theme_classic() +
  adams_theme+
  scale_color_manual(values = pft.cols)


png(paste0(getwd(),"/08_H20_mort_rate.png"), height=5, width=8, units="in", res = 100)
p7
dev.off()


precip_g <- ggplot(data = full_output, aes(x = as.Date(date), y = precip, color = pft)) +
  labs(title = "02_precip.") +
  theme_classic()+
  #geom_point()+
  smooth_line +
  year_axis +
  ylab(expression(paste("daily precip (mm)")))+
  xlab(bquote('year'))+
  adams_theme +
  theme(legend.position = "none")+
  scale_color_manual(values = (rep("blue",4)))

png(paste0(getwd(),"/09_precip.png"), height=5, width=8, units="in", res = 100)
precip_g
dev.off()



#graphing the soil matric potential over time

SMP_MPa_g <- ggplot(data = full_output %>% filter(date >= "2005-01-01"), aes(x = as.Date(date), y = SMP/1e5)) +
  labs(title = "Soil Matric Potential (MPa)") +
  theme_classic()+
  geom_line(size = 1.8, color = "black")+
  #smooth_line +
  year_axis +
  ylab(expression(paste("SMP (MPa)")))+
  xlab(bquote('year'))+
  adams_theme +
  theme(legend.position = "none") +
  geom_hline(yintercept = thresh.xx[1]/1e5, size = 1.8, color = "darkolivegreen2")+
  geom_hline(yintercept = thresh.xx[2]/1e5, size = 1.8, color = "darkolivegreen4")#+
#geom_text(mapping = aes(x = median(as.Date(date)), y = thresh.xx[1]/1e5), data = full_output, label = "DI threshold", color = "black") +
#geom_text(mapping = aes(x = median(as.Date(date)), y = thresh.xx[2]/1e5), data = full_output, label = "DT threshold", color = "black")



png(paste0(getwd(),"/10_SMP.png"), height=5, width=8, units="in", res = 100)
SMP_MPa_g
dev.off()



water_def_g <- ggplot(data = full_output, aes(x = as.Date(date), y = water_def, color = pft)) +
  labs(title = "Water Deficit Days") +
  theme_classic()+
  geom_line(size = 1.8)+
  smooth_line +
  year_axis +
  ylab(expression(paste("Deficit Days (cum. sum of SMP deficit)")))+
  xlab(bquote('year'))+
  adams_theme +
  #theme(legend.position = "none") +
  geom_hline(yintercept = thresh.xx[1]/1e5)+
  geom_hline(yintercept = thresh.xx[2]/1e5)#+
#geom_text(mapping = aes(x = median(as.Date(date)), y = thresh.xx[1]/1e5), data = full_output, label = "DI threshold", color = "black") +
#geom_text(mapping = aes(x = median(as.Date(date)), y = thresh.xx[2]/1e5), data = full_output, label = "DT threshold", color = "black")


png(paste0(getwd(),"/11_water_def.png"), height=5, width=8, units="in", res = 100)
water_def_g
dev.off()





#graphing the fraction recruiting from the seedling pool to the adult size class
p8 <- ggplot(data = full_output, aes(x = as.Date(date), y = frac_rec.t, color = pft)) +
  smooth_line +
  year_axis +
  ylab(expression(paste('rec. rate (% of seedling pool'," day"^"-1",")")))+
  xlab(bquote('year'))+
  labs(title = 'seedling recruitment rate (%)') +
  theme_classic() +
  adams_theme +
  scale_color_manual(values = pft.cols)

png(paste0(getwd(),"/12_fraction_recruiting.png"), height=5, width=8, units="in", res = 100)
p8
dev.off()



#graphing the daily recruitment rate without the total
p9 <- full_output %>% arrange(desc(pft)) %>% ggplot( aes(x = as.Date(date), y = R*365, color = pft)) +
  smoother_line +
  year_axis +
  ylab(expression(paste('N recruits'," ha"^"-1"," year"^"-1")))+
  xlab(bquote('year'))+
  labs(title = 'annual number of recruits') +
  theme_classic() +
  adams_theme +
  scale_color_manual(values = pft.cols)

#+
#geom_smooth(data = full_output %>% group_by(date) %>% summarise(total_R = sum(R), pft = "total"), 
#mapping = aes(x = as.Date(date), y = total_R, color = pft),
#size = 1.8, method = "loess", span = .01, se = F) 



png(paste0(getwd(),"/13_annual_N_recruits.png"), height=5, width=8, units="in", res = 100)
p9
dev.off()





#graphing the annual recruitment rate with the total

rec_bench_totals.df <- data.frame(bench = rec_bench_totals[-1], date = as.Date(c("2007-06-01", "2012-06-01")))

p10 <- full_output %>% filter(date >= as.POSIXct("2005-01-01")) %>% arrange(desc(pft)) %>% ggplot( aes(x = as.Date(date), y = R*365, color = pft)) +
  smooth_line +
  year_axis +
  ylab(expression(paste('N recruits'," ha"^"-1"," yr"^"-1")))+
  xlab(bquote('year'))+
  labs(title = 'Recruitment Rate') +
  theme_classic() +
  adams_theme +
  geom_smooth(data = full_output %>% filter(date >= as.POSIXct("2005-01-01")) %>% group_by(date) %>% summarise(total_R = sum(R), pft = "total"), 
              mapping = aes(x = as.Date(date), y = total_R*365, color = pft),
              size = 1.8, method = "loess", span = .01, se = F)+
  geom_point(aes(x  = as.Date("2007-06-01"), y = 77), color = "black", size = 5, pch = 2) +
  geom_point(aes(x  = as.Date("2012-06-01"), y = 125), color = "black", size = 5, pch = 2) +
  scale_color_manual(values = append(pft.cols, "black"))


png(paste0(getwd(),"/recruitment_rate_w_total.png"), height=5, width=8, units="in", res = 100)
p10
dev.off()







##################################################
#submodel annual sum of recruitment with benchmark
####################################################
#creating data of the number of recruits per year


#creating table of the annual number of recruits per year per PFT
N_recs_per_year_pfts <- full_output %>% mutate(year = substring(text = as.character(date), first = 1, last = 4)) %>% group_by(year, pft) %>% summarise(N_rec = sum(R)) 

N_recs_per_year_pfts$year <- as.Date(paste0((as.numeric(N_recs_per_year_pfts$year)+1), "-01-01"))



Submodel_annual_rec <- ggplot() +
  smoother_line +
  ylab(expression(paste('N recruits'," ha"^"-1"," yr"^"-1")))+
  xlab(bquote('year'))+
  labs(title = 'Trends in Annual PFT-specific Recruitment') +
  #geom_smooth(data = full_output %>% group_by(date) %>% summarise(total_R = sum(R), pft = "total"), mapping = aes(x = as.Date(date), y = total_R*365, color = pft), size = 1.8, method = "loess", span = .1, se = F) +
  geom_point(data = N_recs_per_year_pfts %>% filter(year >= as.Date("2005-01-01")), mapping = aes(x = year, y = N_rec, color = pft), size = 8) +
  geom_point(data = graphable_bench_data %>% filter(date >= as.Date("2005-01-01") & date <= as.Date("2015-01-01")), mapping = aes(x = date, y = rec_rate, color = pft), size = 1) +
  #geom_segment(data = graphable_bench_data %>% filter(date <= as.Date("2015-01-01")), mapping = aes(x = date, y = rec_rate, color = pft), xend = as.Date("2010-01-01"), yend = 20)+
  #scale_linetype_manual(values = "yellow2")+
  theme_classic() +
  adams_theme +
  year_axis +
  scale_color_manual(values = pft.cols)

png(paste0(getwd(),"/16_Recruitment_Annual_Sums.png"), height=5, width=8, units="in", res = 100)
Submodel_annual_rec
dev.off()




