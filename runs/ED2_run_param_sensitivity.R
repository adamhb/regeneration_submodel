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
run_name <- "parameter_sensitivity"
start_date <- "2005-01-01"
end_date <- "2014-01-01"
n_PFTs <- 4
soil_layer <- 15 # 15 is 6 cm, 16 is 2 cm deep

#set path to driver data
driver_data_path <- "~/cloud/gdrive/rec_submodel/data/ED2_output/"
path_to_output <- "~/cloud/gdrive/rec_submodel/output/"


#initialize dataframe
dummy <- 1
params_in_sens <- c("dummy", "Dmax", "frac_repro", 
                    "seed_frac","decay_rate", "a_emerg", "b_emerg", 
                    "background_seedling_mort", 
                    "P1H20", "P2H20","thresh.xx", "window.x",
                    "P1light_mort","P2light_mort",
                    "Z0_seedling",
                    "a_rec", "b_rec","Z0")

param_sens_data <- tibble()

for(param in params_in_sens){
  
  source("parameter_files/parameters_ED2_run_Aug_4.R")
  
  assign(x = param,value = eval(as.name(param)) * 1.1)
  
  source("clean_input/prep_driver_data_ED2_bci.R")
  
  source("model/regeneration_submodel.R")
  
  tmp <- full_output %>%
    filter(as.numeric(yr) > as.numeric(substr(start_date,1,4)) + 3) %>% #doesn't include 3 yr spin up in calculations
    group_by(pft) %>%
    summarise(
      start_date = as.Date(min(date)),
      end_date = as.Date(max(date)),
      R_avg = mean(R),
      param_changed = param
    )
  
  param_sens_data <- rbind(param_sens_data,tmp)
  
  print(paste("finished param",match(param,params_in_sens),"of",length(params_in_sens)))
  
}

write_csv(param_sens_data, "temp/param_sens_data.csv")














