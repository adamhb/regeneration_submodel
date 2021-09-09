#This script is used to derive parameters for the light-based seedling mortality function
#for PFTs at BCI

source('utils/supporting_funcs.R')
source('model/process_funcs.R')

#WML is the rolling time window over which incident light is calculated for the seedling mortality response. 
#We used a default value of 64 days for WML because this was the approximate mean number of days required to see tropical
#seedling mortality responses to different light levels (see Fig. 1 in Augspurger (1984) and Fig. 1 in Myers & Kitajima
#(2007). 

#Augspurger CK. 1984. Light Requirements of Neotropical Tree Seedlings: A Comparative Study of Growth and Survival. 
#Journal of Ecology 72: 777–795.

#Myers JA, Kitajima K. 2007. Carbohydrate storage enhances seedling shade and stress tolerance in a neotropical forest. 
#Journal of Ecology 95: 383–395.

#The time at which mortality rates in the light deviate from mortality rates in the shade from 
#Carrol Augspurger's (1984) seedling light-based mortality experiment
W_ML <- round(mean(c(5,10,10,1,5,18,5,2,15,5,10,10,1,10,30)) * 7)

print(paste("the value for W_ML is", W_ML, "days"))
#This value corresponds reasonably well to other studies (Myers and Kitajima, 2007)


#Setting some intermediary parameters
#Calculating mean TOC solar radiation at Kobe's site (La Selva) during the period of time he did his analysis

#load solar radiation data from Clark, 2017
la.sel.sol.rad <- read_csv(file = paste0(path_to_observations,"la_selva_solar_rad.csv"))

#calculate the mean daily solar radiation at La Selva during the year Kobe (1999)
#did his study

meanTOC_solarRad <- la.sel.sol.rad %>%
  filter(Year == 1994) %>%
  rename(rad = `DailyLSPyr MJ/d`) %>%
  pull(rad) %>% mean()  #14.11947 MJ of solar radiation per day; (Clark, 2017)


mean_bci_TOC_par <- meanTOC_solarRad*par_per_solar_rad #megajoules of par per day; see supporting funcs for citation
mean_bci_understory <- mean_bci_TOC_par * 0.02 #mean understory light at BCI MJ m-2 day-1
mean_bci_20pct_gap <- mean_bci_TOC_par * 0.2

#Clark DA. 2017. La Selva daily solar radiation (pyranometer) March 1982 through 2016. 
#https://bixa.tropicalstudies.org/meteoro/data/lsdata/LS%20daily%20solar%20radiation%20(pyranometer)%20March1992%20through%202016%20(Jan%202017,%20DAC).xlsx.


#Here we replicate Kobe (1990)'s statistical model for light-based seedling mortality
#We use it to model absolute light as the input instead of relative irradiance
#This step generates data used to model the relationship between absolute light and seedling mortality directly

#Kobe RK. 1999. Light Gradient Partitioning among Tropical Tree Species through Differential #Seedling Mortality and Growth. Ecology 80: 187–201.
light_mort2 <- function(light, seedpool.x = 1){
  
  #this temporary function receives absolute light as an input, but then
  #converts it % relative irradiance at Kobe's site in Costa Rica
  #and the applied Kobe's (1999) statistical model
  
  #NOTE: this is not the function used by the TRS. This is just an intermediary function
  #to derive a relationship between absolute light and seedling mortality.
  #The function used by the TRS is in 'model/process_funcs.R'
  pct_light <- (light / (mean_bci_TOC_par * W_ML)) * 100 
  
  A <- P1light_mort[PFT]
  B <- P2light_mort[PFT]
  
  Ml <- A * exp(-B*pct_light) #Kobe's (1999) statistical model
  
  Pm_yr <- 1 - exp(-Ml*(W_ML/30.4)) #probability of mortality per year
  
  Pm_day <- Pm_yr / W_ML #probability of mortality per day
  
  return(Pm_day)
}


#Parameter values for Trophis racemosa (ST) and Cecropia obusifolia (LD) for Kobe's statistical model
P1light_mort <- c(0.752, 0.752, 0.0241, 0.0241)
names(P1light_mort) <- pft_names
P2light_mort <- c(0.1368, 0.1368, 0.0404, 0.0404)
names(P2light_mort) <- pft_names


#Here we used Kobe's to statistical model to generate data on probability of mortality from
#variation in understory light levels.

#generating cumulative light levels (par in MJ m2-1 W_ML_days-1) which range from intensities reflecting 
#understory conditions to TOC solar radiation. This is used to drive Kobe's statistical model.
light_mort_rates <- c()
lights <- 7.65:480 

pft <- c()
for(p in pft_names){
  for(i in 1:length(lights)){
    PFT <- p
    light.xxx <- lights[i]
    tmp <- light_mort2(light = light.xxx, seedpool.x = 1)
    light_mort_rates <- append(light_mort_rates, tmp) 
    pft <- append(pft, p) 
  }
}

light_mort_data <- tibble(pft = pft,
       lights = rep(lights,4),
       light_mort_rates = light_mort_rates) 


#generating parameters that relate absolute light (as opposed to relative irradiance as done in Kobe (1999))
#to seedling mortality. These parameters are used by the Tree Recruitment Scheme's light mortality function.
LDmod <- lm(data = light_mort_data %>% filter(pft == "LD_DI"), formula = log(light_mort_rates) ~ lights)
STmod <- lm(data = light_mort_data %>% filter(pft == "ST_DI"), formula = log(light_mort_rates) ~ lights)

#summary(LDmod)
#exp(predict(object = LDmod, newdata = light_mort_data %>% filter(pft == "LD_DI")))

a.ML <- c(rep(coef(LDmod)[2],2),rep(coef(STmod)[2],2))
names(a.ML ) <- pft_names 
b.ML <- c(rep(coef(LDmod)[1],2),rep(coef(STmod)[1],2))
names(b.ML ) <- pft_names

print("a_ML =")
print(a.ML)

print("b_ML =")
print(b.ML)


#using these parameters and the Tree Recruitment Scheme's
#light based mortality function to visualize the relationship 
#between light and seedling mortality for PFTs at BCI.

light_mort_rates <- c()
lights <- seq(from = mean_bci_understory * 0.6 * W_ML, to = 1.1 * mean_bci_TOC_par * W_ML) 
pft <- c()
for(p in pft_names){
  for(i in 1:length(lights)){
    PFT <- p
    light.xxx <- lights[i]
    tmp <- light_mort3(light = light.xxx, seedpool.x = 1)
    light_mort_rates <- append(light_mort_rates, tmp) 
    pft <- append(pft, p) 
  }
}

light_mort_data <- tibble(pft = pft,
                          lights = rep(lights,4),
                          light_mort_rates = light_mort_rates) 

viz_light_mort <- light_mort_data %>%
  ggplot(aes(x = lights, y = light_mort_rates, color = pft, linetype = pft)) +
  geom_line(size = 2) +
  scale_color_manual(values = pft.cols) +
  scale_linetype_manual(values=c("solid", "dashed","solid","dashed"))+
  ylab(label = "daily light stress \n seedling mortality rate") +
  xlab(label = expression(atop(paste("forest floor light [MJ PAR m"^"-2","]"),"cum. sum prior 2 months"))) +
  #xlab(expression(atop("solar rad. at seedling layer", paste("[MJ m"^"-2"," day"^"-1","]")))) +
  labs(title = "Light & \n seedling mortality") +
  geom_vline(xintercept = mean_bci_understory * W_ML, linetype = "dotted") +
  geom_vline(xintercept = mean_bci_20pct_gap * W_ML, linetype = "dashed") +
  scale_x_continuous(limits = c(mean_bci_understory * W_ML * 0.6, mean_bci_20pct_gap * W_ML * 1.1 )) +
  #annotate(geom = "text", x = 700, y = 0.15, label = "BCI mean \n at smallest cohort", size = 5) +
  #geom_segment(aes(x = 350, y = 0.12, xend = 70, yend = 0.05),
              # arrow = arrow(length = unit(0.5, "cm")), color = "black") +
  #annotate(geom = "text", x = mean_bci_TOC, y = 0.09, label = "BCI mean \n TOC", size = 5) +
  theme_minimal() +
  multipanel_theme

print("made viz_light_mort")

#makePNG(fig = viz_light_mort, path_to_output.x = path_to_output, file_name = "viz_light")


