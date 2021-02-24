#light-based seedling mortality
# light_mort <- function(light = 5000000*60, seedpool.x = 750000){
#   
#   pct_light <- (light / (15750113 * 90 / 1e6)) * 100 #the percent RI equivalent at Kobe's site in Costa Rica
#   
#   #seedlings_N <- seedpool.x / Z0_seedling[PFT]
#   
#   A <- P1light_mort[PFT]
#   B <- P2light_mort[PFT]
#   
#   ifelse((test = PFT == "ST_DI" | PFT == "ST_DT" | (PFT == "LD_DI" & pct_light <= LD_light_thresh) | (PFT == "LD_DT" & pct_light <= LD_light_thresh)), 
#          yes = Ml <- A * exp(-B*pct_light),
#          no = Ml <- A * exp(-B*LD_light_thresh))
#   
#   Pm_yr <- 1 - exp(-Ml*3)
#   
#   Pm_day <- Pm_yr / 90 # why did I divide by 90 here instead of 365, check Kobe paper on this
#   
#   #N_mort <- Pm_day * seedlings_N
#   
#   #frac_mort <- (N_mort * Z0_seedling[PFT]) / seedpool.x
#   
#   return(Pm_day)
# }

#deriving W_ML
#The week at which mortality rates in the light deviate from mortality rates in the shade from 
#Carrol Augspurger's (1984) seedling light-based mortality experiment
source("utils/supporting_funcs.R")
print(paste("the value for W_L is", mean(c(5,10,10,1,5,18,5,2,15,5,10,10,1,10,30)) * 7, "days"))

#replicating Kobe (1990)'s statistical model, but using absolute light

light_mort2 <- function(light, seedpool.x = 1){
  
  #browser()
  pct_light <- (light / (14.11947 * W_ML)) * 100 #the percent RI equivalent at Kobe's site in Costa Rica
  
  #seedlings_N <- seedpool.x / Z0_seedling[PFT]
  
  A <- P1light_mort[PFT]
  B <- P2light_mort[PFT]
  
  Ml <- A * exp(-B*pct_light)
  
  Pm_yr <- 1 - exp(-Ml*(W_ML/30.4))
  
  Pm_day <- Pm_yr / W_ML # divide by 90 here because 3 (for 3 months) is in the numerator
  
  #N_mort <- Pm_day * seedlings_N
  
  #frac_mort <- (N_mort * Z0_seedling[PFT]) / seedpool.x
  
  return(Pm_day)
}



#seedling light mort
P1light_mort <- c(0.752, 0.752, 0.0241, 0.0241)
names(P1light_mort) <- pft_names
P2light_mort <- c(0.1368, 0.1368, 0.0404, 0.0404)
names(P2light_mort) <- pft_names
#LD_light_thresh <- 18.98 # from Kobe


light_mort_rates <- c()
lights <- 30:2081 
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


mean_bci <- (52.2) * 64/90 # MJ of light in prior 3 months
mean_bci_TOC <- 1512 * 64/90


light_mort_data <- tibble(pft = pft,
       lights = rep(lights,4),
       light_mort_rates = light_mort_rates) 

# viz_light_mort <- light_mort_data %>%
#   ggplot(aes(x = lights, y = light_mort_rates * 30, color = pft, linetype = pft)) +
#   geom_line(size = 2) +
#   scale_color_manual(values = pft.cols) +
#   scale_linetype_manual(values=c("solid", "dashed","solid","dashed"))+
#   ylab(label = "monthly light-based \n seedling mortality rate") +
#   xlab(label = expression(paste("cum. light in prior 2 months ","(MJ m"^"-2",")"))) +
#   labs(title = "Light-based seedling mortality") +
#   geom_vline(xintercept = mean_bci, linetype = "dotted") +
#   geom_vline(xintercept = mean_bci_TOC, linetype = "dotted") +
#   annotate(geom = "text", x = 700, y = 0.15, label = "BCI mean \n at smallest cohort", size = 5) +
#   geom_segment(aes(x = 350, y = 0.12, xend = 70, yend = 0.05),
#                arrow = arrow(length = unit(0.5, "cm")), color = "black") +
#   annotate(geom = "text", x = mean_bci_TOC, y = 0.09, label = "BCI mean \n TOC", size = 5) +
#   theme_minimal() +
#   multipanel_theme



LDmod <- lm(data = light_mort_data %>% filter(pft == "LD_DI"), formula = log(light_mort_rates) ~ lights)
STmod <- lm(data = light_mort_data %>% filter(pft == "ST_DI"), formula = log(light_mort_rates) ~ lights)
summary(LDmod)

exp(predict(object = LDmod, newdata = light_mort_data %>% filter(pft == "LD_DI")))

a.ML <- c(rep(coef(LDmod)[2],2),rep(coef(STmod)[2],2))
names(a.ML ) <- pft_names
b.ML <- c(rep(coef(LDmod)[1],2),rep(coef(STmod)[1],2))
names(b.ML ) <- pft_names

print("a_ML =")
print(a.ML)

print("b_ML =")
print(b.ML)


#simplified light mort function that is used in the regeneration submodel
#seedling light mort
light_mort3 <- function(light = 90, seedpool.x = 1){
  
  #browser()
  a.ML.x <- a.ML[PFT]
  b.ML.x <- b.ML[PFT]
  
  Pm_day <- exp(a.ML.x * light +  b.ML.x)
  
  #N_mort <- Pm_day * seedlings_N
  
  #frac_mort <- (N_mort * Z0_seedling[PFT]) / seedpool.x
  
  return(Pm_day)
}

#makePNG(fig = viz_light_mort, path_to_output.x = path_to_output, file_name = "viz_light_mort")


# P1light_mort <- c(-0.010673455, -0.010673455, -0.003168996, -0.003168996)
# P2light_mort <- c(-3.817788, -3.817788, -7.142556, -7.142556)

# 
# P1light_mort <- c(-0.010673455, -0.010673455, -0.003168996, -0.003168996)
# P2light_mort <- c(-4.217788, -4.217788, -7.142556, -7.142556)


light_mort_rates <- c()
lights <- 30:2081 
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
  ggplot(aes(x = lights, y = light_mort_rates * 30, color = pft, linetype = pft)) +
  geom_line(size = 2) +
  scale_color_manual(values = pft.cols) +
  scale_linetype_manual(values=c("solid", "dashed","solid","dashed"))+
  ylab(label = "monthly light-based \n seedling mortality rate") +
  xlab(label = expression(paste("cum. light in prior 2 months ","(MJ m"^"-2",")"))) +
  labs(title = "Light-based seedling mortality") +
  geom_vline(xintercept = mean_bci, linetype = "dotted") +
  geom_vline(xintercept = mean_bci_TOC, linetype = "dotted") +
  annotate(geom = "text", x = 700, y = 0.15, label = "BCI mean \n at smallest cohort", size = 5) +
  geom_segment(aes(x = 350, y = 0.12, xend = 70, yend = 0.05),
               arrow = arrow(length = unit(0.5, "cm")), color = "black") +
  annotate(geom = "text", x = mean_bci_TOC, y = 0.09, label = "BCI mean \n TOC", size = 5) +
  theme_minimal() +
  multipanel_theme

print("made viz_light_mort")

#makePNG(fig = viz_light_mort, path_to_output.x = path_to_output, file_name = "viz_light")


