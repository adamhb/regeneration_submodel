source("utils/supporting_funcs.R")

###########################
#######Load Data###########
###########################
germ_data_Pearson <- read_csv(paste0(path_to_observations,"Pearson_et_al_2002_Fig2_data.csv"))  #data from Pearson et al., 2002
#source2(file = "runs/ED2_BASE.R",start = 5,end = 45)
#source(file = 'runs/ED2_BASE.R')
#input_vars <- read_csv("temp/input_vars.csv")
#full_output <- read_csv("temp/full_output.csv")
source("model_dev/photoblastic_germination.R")


############################
###The emergence function###
############################
#This function was created by looking at seasonal seedling emergence observations (Garwood, 1983)


#old params
# a_emerg <- rep(0.0006,4)
# names(a_emerg) <- pft_names
# b_emerg <- c(1.6,1.6,1.2,1.2)
# names(b_emerg) <- pft_names


#new params

# emerg_func_old <- function(a = a_emerg[PFT], b = b_emerg[PFT], SMP.2.to.0.wks.ago, SMP.4.to.2.wks.ago, seedbank.x, light.xx){
#   
#   if(input_vars$SMP[i] < emerg_thresh){
#     frac_emerg <- 0
#   } else {
#     
#     log10_frac_emerg <- log10(a) + b*log10(abs(SMP.4.to.2.wks.ago)/abs(SMP.2.to.0.wks.ago)) 
#     
#     frac_emerg <- (10^log10_frac_emerg) *  photoblastic_germ_rate_modifier(l_crit.x = l_crit, light.x = light.xx * 1e6)
#     
#     #if(frac_emerg > 0.07){frac_emerg <- 0.07}
#     
#   }
#   
#   C_emerg <- frac_emerg * seedbank.x
#   
#   out <- list(frac_emerg, C_emerg)
#   names(out) <- c("frac_emerg", "C_emerg")
#   return(out)
# }


emerg_func <- function(a = a_emerg[PFT], b = b_emerg[PFT], SMP.2.to.0.wks.ago, seedbank.x, light.xx){
  
  wet_index <- 1 / (SMP.2.to.0.wks.ago * -1 / 1e5)
  
  if(SMP.2.to.0.wks.ago < emerg_thresh){
    frac_emerg <- 0
  } else {
    
    frac_emerg <- (a * wet_index^b)  *  photoblastic_germ_rate_modifier(l_crit.x = l_crit, light.x = light.xx)
    
    #if(frac_emerg > 0.07){frac_emerg <- 0.07}
    
  }
  
  C_emerg <- frac_emerg * seedbank.x
  
  out <- list(frac_emerg, C_emerg)
  names(out) <- c("frac_emerg", "C_emerg")
  return(out)
}


#emerg_func(a = 0.0006, b = 1.2, SMP.2.to.0.wks.ago = -1471, light.xx = 0.334, seedbank.x = 1)

#create the data with the emerg func
emerg_data <- tibble()
SMPrange <- read_csv(file = 'temp/SMPrange.csv')
SMPrange <- SMPrange$SMPrange

for(p in pft_names){
  
    PFT <- p
    tmp <- tibble(SMP = SMPrange) %>%
      mutate(pft = p)
    
    emergs <- c()
    for(i in 1:length(SMPrange)){
    
      emerg_rate_i <- emerg_func(a = a_emerg[PFT], 
                               b = b_emerg[PFT], 
                               SMP.2.to.0.wks.ago = tmp$SMP[i],
                               #SMP.4.to.2.wks.ago = tmp$SMP2,
                               seedbank.x = 1,
                               light.xx = 16.7)$frac_emerg
    emergs <- append(emergs,emerg_rate_i)
  }
  tmp <- tmp %>% mutate(emerg = emergs)
  emerg_data <- rbind(emerg_data,tmp)
}


#create figure
emerg_vs_SMP_fig <- emerg_data %>%
  drop_na() %>%
  #mutate(delta = (abs(SMP2) - abs(SMP1)) / abs(SMP2) ) %>%
  ggplot(aes(x = SMP/1e5, y = emerg, color = pft, linetype = pft)) +
  geom_line(size = 2) +
  #annotate(geom = "text", x = -0.5, y = 0.05, label = "b", size = subplot_heading_size) +
  scale_x_continuous(limits = c(-0.25,0)) +
  scale_y_continuous(limits = c(0,0.25)) +
  scale_color_manual(values = pft.cols) +
  scale_linetype_manual(values=c("solid", "dashed","solid","dashed"))+
  ylab(label = "daily fraction of \n seedbank emerging") +
  xlab(label = "mean soil matric potential \n [MPa] in prior two weeks") +
  labs(title = "Moisture & \n seedling emergence") +
  theme_minimal() +
  multipanel_theme
emerg_vs_SMP_fig

print("made emerg_vs_SMP_fig")














########################
#derivation of a_emerg##
########################
##DEPRECATED
##the mean emergence when light and moisture are not limiting
# a_emerg <- germ_data_Pearson %>% pull(germ) %>% mean() %>% `/` (100*60) %>% rep(.,4) #data from Pearson et al., 2002
# names(a_emerg) <- pft_names
# print(paste("a_emerge =",a_emerg,"for all PFTs"))





###############################################################
#derivation of psi_emerge (soil moisture emergence threshold)##
###############################################################
SMP_frac_emerg <- full_output %>%
  select(yr:date,SMP,frac_emerging,seedbank) %>%
  mutate_at(.vars = "SMP",.funs = function(x){x/1e5}) %>%
  mutate(doy = strftime(date, format = "%j")) %>%
  group_by(doy) %>%
  summarise(n = length(SMP),
            SMP_med = median(SMP),
            fe_med = median(frac_emerging),
            seedbank_med = median(seedbank),
            SMP_sd = sd(SMP,na.rm = T)) %>%
  mutate(wetness = 1/-SMP_med) %>%
  mutate_at(.vars = "doy",.funs = as.numeric) %>%
  mutate(seeds_emerging = seedbank_med * fe_med)

#calculate SMP threshold for emergence. 
#This is the minimum daily median soil moisture in the wet season at BCI (day 135 to 365)
#The wet season was determined visually by looking at inflection points in soil mositure throughout the year.
wetness <- SMP_frac_emerg %>%
  mutate(moisture_period = case_when(
    doy < 135 ~ "dry",
    doy %in% 135:165 ~ "trans",
    doy > 165 ~ "wet"
  )) %>%
  group_by(moisture_period) %>%
  summarise(med_wetness_per_period = min(wetness)) 

emerg_thresh <- 1/-(wetness$med_wetness_per_period[3]) * 1e5 #mm H20 suction

print(paste("moisture emergence threshold is",emerg_thresh,"mm H20 suction (which equals MPa * 10^5)")) #mm H20 suction
print("b_emerg = ")
print(b_emerg)

############################################################################
#visualize seasonal soil moisture and emergence as implemented in the model#
############################################################################
#This is used to check that the parameters (a_emerg, b_emerg, emerg_thresh) used replicate observations by Garwood,1983

seasonal_emergence_graph <- ggplot(data = SMP_frac_emerg,
                          mapping = aes(doy,wetness)) +
  smooth_line +
  geom_line(data = SMP_frac_emerg,mapping = aes(doy,seeds_emerging/5.37)) +
  ylab(expression(paste("soil mosture [1/-SMP (MPa)]"))) +
  xlab(bquote('day of year')) +
  scale_y_continuous(limits = c(0,30))+
  #labs(title = paste("b_emerg:",b_emerg[c(1,3)])) +
  scale_y_continuous(sec.axis = sec_axis(~. *5.37, name = "emergence [g per day ha-1]")) +
  adams_theme +
  theme(axis.title.y = element_text(colour = "blue"),
        axis.text.y = element_text(colour = "blue"),
        axis.text.y.right = element_text(colour = "black"),
        axis.title.y.right = element_text(colour = "black"))

print("made seasonal_emergence_graph")

makePNG(fig = seasonal_emergence_graph, path_to_output.x = paste0(path_to_output,"model_dev_figs/"),
        file_name = "seasonal_emergence_graph")


#####################################
######visualize emergence function####
#####################################

# 
# SMP_output <- full_output %>%
#   filter(yr %in% c(2002:2005))
# 
# SMP_start <- c()
# SMP_end <- c()
# frac_emerg_from_output <-c()
# pft_emerg <- c()
# for (i in 29:nrow(SMP_output)){
#   SMP_start[i] <- mean(SMP_output$SMP[(i-W_emerg):(i-round(W_emerg/2))])
#   SMP_end[i] <- mean(SMP_output$SMP[(i-round(W_emerg/2)):i])
#   frac_emerg_from_output[i] <- SMP_output$frac_emerging[i]
#   pft_emerg[i] <- SMP_output$pft[i]
# }
# 
# emerg_data <- tibble(SMP_start = SMP_start, SMP_end = SMP_end, emerg = frac_emerg_from_output, pft = pft_emerg) %>%
#   mutate(delta = (abs(SMP_start) - abs(SMP_end)) / abs(SMP_start) ) 
# 
# emerg_data %>%
# ggplot(aes(x = delta * 100, y = emerg, color = pft, linetype = pft)) +
#   geom_line(size = 2) +
#   #annotate(geom = "text", x = -0.5, y = 0.05, label = "b", size = subplot_heading_size) +
#   scale_x_continuous(limits = c(0,100)) +
#   scale_color_manual(values = pft.cols) +
#   scale_linetype_manual(values=c("solid", "dashed","solid","dashed"))+
#   ylab(label = "fraction of \n seedbank emerging") +
#   xlab(label = "% increase in SMP \n in prior month") +
#   labs(title = "Seedling emergence") +
#   theme_minimal() +
#   multipanel_theme
# 
# 
# tmp <- tibble(SMP1 = full_output$SMP[365:(365*5)], SMP2 = lag(full_output$SMP[365:(365*5)], 14))







# 
# 
# 
# SMPv <- full_output %>%
#   filter(pft == "ST_DT") %>%
#   pull(SMP)
# 
# smp4.2 <- c()
# smp2.0 <- c()
# for(i in 28:length(SMPv)){
#   smp4.2 <- append(smp4.2,mean(SMPv[(i-27):(i-14)]))
#   smp2.0 <- append(smp2.0,mean(SMPv[(i-13):i]))
# }
# 
# smpDF <- tibble(SMP = SMPv[28:7065],
#                 smp4.2 = smp4.2,
#                 smp2.0 = smp2.0,
#                 day = 28:7065) %>%
#   mutate(delta = abs(smp4.2)/abs(smp2.0))
# 
# 
# day <- c()
# SMPf <- c()
# smp4.2 <- c()
# smp2.0 <- c()
# deltaf <- c()
# emerg <- c()
# pft <- c()
# 
# for(p in pft_names){
#   for(i in 1:nrow(smpDF)){
#     day <- append(day,smpDF$day[i])
#     SMPf <- append(SMPf,smpDF$SMP[i])
#     smp4.2 <- append(smp4.2,smpDF$smp4.2[i])
#     smp2.0 <- append(smp2.0,smpDF$smp2.0[i])
#     deltaf <- append(deltaf,smpDF$delta[i])
#     PFT <- p
#     tmp <- emerg_func(SMP.2.to.0.wks.ago = smpDF$smp2.0[i],
#                       SMP.4.to.2.wks.ago = smpDF$smp4.2[i],
#                       seedbank.x = 1,
#                       light.xx = 1e5)$frac_emerg
#     emerg <- append(emerg, tmp) 
#     pft <- append(pft, p) 
#     #print(paste(i/nrow(smpDF) * 100,"% done"))
#   }
# }
# 
# viz_emerg <- tibble(day = day,
#                     SMP = SMPf,
#                     delta = deltaf,
#                     smp4.2 = smp4.2,
#                     smp2.0 = smp2.0,
#                     pft = pft,
#                     emerg = emerg) %>%
#   mutate(pct_increase = (abs(smp4.2) - abs(smp2.0)) / abs(smp4.2)) %>%
#   #filter(pft == "LD_DI") %>%
#   ggplot(aes(x = pct_increase, y = emerg, color = pft, linetype = pft)) +
#   geom_line(size = 2) +
#   #annotate(geom = "text", x = -0.5, y = 0.05, label = "b", size = subplot_heading_size) +
#   scale_x_continuous(limits = c(-0.5,1)) +
#   scale_color_manual(values = pft.cols) +
#   scale_linetype_manual(values=c("solid", "dashed","solid","dashed"))+
#   ylab(label = "fraction of \n seedbank emerging") +
#   xlab(label = "% change in SMP \n in prior month") +
#   labs(title = "Seedling emergence") +
#   theme_minimal() +
#   multipanel_theme
# 
# #path_to_output <- "~/cloud/gdrive/rec_submodel/output/forMS/"
# #png options
# PNGheight = 11/3
# PNGwidth = 4.5 #usually 8
# PNGunits = "in"
# PNGres = 100
# #makePNG(fig = viz_emerg, path_to_output.x = path_to_output, file_name = "visualize_emergence")
# 
# 
# 
