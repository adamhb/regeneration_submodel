#playing with sub functions
rm(list = ls())
gc()

#load libraries
library(ncdf4)
library(ncdf.tools)
library(magrittr)
library(tidyverse)
library(lubridate)
library(scales)
library(plotrix)
library(ggplot2)
library(reshape2)

source("utils/supporting_funcs.R")

#name the run
run_type <- "ED2" # keep this as ED2
emulate_ED2 <- F
patch_run_type <- "one" #"many" #one or "many"
synthetic_patches <- F  # T or F
run_name <- "vanilla_run"
start_date <- "2001-01-01"
end_date <- "2015-01-01"
n_PFTs <- 4
soil_layer <- 15 # 15 is 6 cm, 16 is 2 cm deep

#set path to driver data
driver_data_path <- "~/cloud/gdrive/rec_submodel/data/ED2_output/"
path_to_output <- "~/cloud/gdrive/rec_submodel/output/"

#site and scenario params
avg_precip <- 71 #precipitation in mm over two weeks (the annual average)
avg_SMP <- -60326 #
avg_l <- 61 #the average total solar radiation load (MJ per m2) at the forest floor over 6 months (annual average)

if(patch_run_type != "many"){
  percent_light <- 0.03
}


#dbh.x <- 500 #dbh in mm
#N_co.x <- 800  #the number of individuals in a cohort
model_area <- 10000 #area in square meters

#source parameter values
source("parameter_files/parameters.R")

source("clean_input/prep_driver_data_ED2_bci.R")




############functions###############
#the probability that an individual is of reproductive status as a function of dbh (mm)
prob_repro <- function(k = 0.0125, L = 1, size_mm, Dmax){
  y <- L / (1 + exp(-k*(size_mm - 0.5*Dmax)))
  return(y)
}


efrac <- function(N, co_dbh_ind, PFT){
  N_repro <- prob_repro(size_mm = co_dbh_ind, Dmax = Dmax[PFT]) * N
  fraction_reproductive <- N_repro / N
  e_frac <- fraction_reproductive * frac_repro[PFT] #frac repro for now is just a fixed percent of NPP (10%), need better data to get better numbers for this
  return(e_frac)
}


#seedling emergence
emerg_func <- function(a = a_emerg[PFT], b = b_emerg[PFT], SMP.x = avg_SMP, avg_SMP.x = avg_SMP, seedbank.x){
  
  log10_frac_emerg <- log10(a) + b*log10(abs(avg_SMP.x)/abs(SMP.x)) 
  
  frac_emerg <- 10^log10_frac_emerg 
  if(frac_emerg > 0.07){frac_emerg <- 0.07}
  
  C_emerg <- frac_emerg * seedbank.x
  
  out <- list(frac_emerg, C_emerg)
  names(out) <- c("frac_emerg", "C_emerg")
  return(out)
}


def_func <- function(soil_moist, thresh.x = thresh.xx[PFT], window){
  thresh <- thresh.x
  def <- (abs(thresh) - abs(soil_moist))*-1
  no_def <- def < 0 
  def[no_def] <- 0
  deficit_days <- c()
  for(i in 1:length(def)){
    deficit_days[i] <- ifelse(i < window, sum(def[1:i]), sum(def[(i-window):i]))
  }
  return(deficit_days)
}



H20_mort <- function(deficit_days, pft.x){
  PFT <- pft.x
  mort_rate <- deficit_days * P1H20[PFT] + P2H20[PFT]
  return(mort_rate/(window.x))
}


#light-based seedling mortality
light_mort <- function(light = 5000000*60, seedpool.x = 750000){
  
  pct_light <- (light / (15750113 * 90)) * 100 #the percent RI equivalent at Kobe's site in Costa Rica
  
  seedlings_N <- seedpool.x / Z0_seedling[PFT]
  
  A <- P1light_mort[PFT]
  B <- P2light_mort[PFT]
  
  ifelse((test = PFT == "latedi" | PFT == "latedt" | (PFT == "earlydi" & pct_light <= 18.98) | (PFT == "earlydt" & pct_light <= 18.98)), 
         yes = Ml <- A * exp(-B*pct_light),
         no = Ml <- A * exp(-B*18.98))
  
  Pm_yr <- 1 - exp(-Ml*3)
  
  Pm_day <- Pm_yr / 90
  
  #N_mort <- Pm_day * seedlings_N
  
  #frac_mort <- (N_mort * Z0_seedling[PFT]) / seedpool.x
  
  return(Pm_day)
}



#recruitment subroutine function
#inputs: l = light in MJ per square meter at the forest floor over the prior 6 months
##average light at the forest floor over any 6 month period


#output
#provides the number of recruits per day

rec_func <- function(a_rec.x = a_rec[PFT], b_rec.x = b_rec[PFT], l, avg_l.x = avg_l, seedpool.x){
  
  log10_frac_rec <- log10(a_rec.x) + b_rec.x*log10(l/avg_l) 
  
  frac_rec <- (10^log10_frac_rec) #the fraction of the seedling pool recruiting per day
  
  C_rec <- frac_rec * seedpool.x
  
  N_rec <- C_rec / Z0
  
  out <- list(frac_rec,C_rec, N_rec)
  
  names(out) <- c("frac_rec", "C_rec", "N_rec")
  return(out)
}


#generating water deficit values for the time series
water_def <- c()
for(PFT in pft_names){
  water_def <- append(water_def, def_func(soil_moist = input_data[input_data$pft == PFT,]$SMP, thresh.x = thresh.xx[PFT], window = window.x))
}



#adding water deficit to the input data
input_data$water_def <- water_def

#applying the H20 mortality function to the time series
input_data <- input_data %>%
  mutate(H20_mort_rate = base::mapply(FUN = H20_mort, deficit_days = input_data$water_def, pft.x = input_data$pft))


input_data <- input_data %>%
  mutate(e_frac = base::mapply(FUN = efrac, N = (input_data$N_co), co_dbh_ind = (input_data$dbh), PFT = input_data$pft)) %>% #adding the "effective fraction" of NPP that gets allocated to reproduction in each time step
  mutate(c_repro = e_frac * NPP * model_area) %>%  #calculating the carbon allocated to reproduction in each daily timestep for the whole model area (1 hectare). Because NPP is input in units of per m2
  mutate_at(.tbl = .,.vars = vars(c_repro), .funs = function(x){ifelse(x < 0, 0, x)}) %>% 
  arrange(., day,pft) 

if(emulate_ED2 == T){
  input_data <- input_data %>%
    mutate(ED2_R = ED2_recruitment(NPPseed = nppseed_pft_day * model_area))
}


if(patch_run_type != "many"){
  input_data <- input_data %>%
    mutate(light = FSDS * percent_light / 1e6) #appears to be units of MJ at the forest canopy
}

if(patch_run_type == "many"){
  input_data <- input_data %>%
    mutate(light = FSDS * lightZ0 / 1e6)
}





#emergence vs. SMP
tibble(x = summary(input_data$SMP), y = emerg_func(seedbank.x = 20000, SMP.x = summary(input_data$SMP))$frac_emerg) %>%
ggplot(aes(x = x, y = y)) +
  geom_point() +
  labs(title = "emergence vs. SMP")



#light based mortality
light_mort_data <- tibble()
light_input <- seq(from = 0, to = tail(summary(input_data$FSDS),1) * 90, length.out = 100)

for (PFT in pft_names){
  temp <- tibble(x = light_input, y = light_mort(light = light_input, seedpool.x = 20000))
  temp$PFT <- PFT
  light_mort_data <- rbind(light_mort_data,temp)
} 

light_mort_data %>%
  ggplot(aes(x = x, y = y, color = PFT)) +
  geom_point() +
  labs(title = "light vs. mortality") +
  xlab(label = "light (J) over prior 6 months") +
  ylab(label = "percent of seedling pool dying per day") 



#light based transition rate into the adult population
light_rec_data <- tibble()

for (PFT in pft_names){
  temp <- tibble(x = light_input, y = light_mort(light = light_input, seedpool.x = 20000))
  temp$PFT <- PFT
  light_mort_data <- rbind(light_mort_data,temp)
} 

light_mort_data %>%
  ggplot(aes(x = x, y = y, color = PFT)) +
  geom_point() +
  labs(title = "light vs. mortality") +
  xlab(label = "light (J) over prior 6 months") +
  ylab(label = "percent of seedling pool dying per day") 


