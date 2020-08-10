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
emulate_ED2 <- T
patch_run_type <- "one" #"many" #one or "many"
synthetic_patches <- F  # T or F
run_name <- "SMP_DEMO_2.0pct_light"
start_date <- "2001-01-01"
end_date <- "2023-12-31"
n_PFTs <- 4
soil_layer <- 15 # 15 is 6 cm, 16 is 2 cm deep

#set path to driver data
driver_data_path <- "~/cloud/gdrive/rec_submodel/data/ED2_output/"
path_to_output <- "~/cloud/gdrive/rec_submodel/output/"

#site and scenario params
#avg_precip <- 71 #precipitation in mm over two weeks (the annual average)
#avg_SMP <- -60326 #
#avg_l <- 61 #the average total solar radiation load (MJ per m2) at the forest floor over 6 months (annual average)
if(patch_run_type != "many"){
  percent_light <- 0.02
}


#dbh.x <- 500 #dbh in mm
#N_co.x <- 800  #the number of individuals in a cohort
model_area <- 10000 #area in square meters

#source parameter values
source("parameter_files/parameters_ED2_run_Aug_4.R")
source("clean_input/prep_driver_data_ED2_bci.R")

SMP_orig <- input_data$SMP

if(emulate_ED2 == T){
  source('model/ED2_emulation.R')
}

summary_data <- tibble()

for(q in 1:20){
  
  input_data$SMP <- SMP_orig * q
  source("model/regeneration_submodel.R")
  
  
  temp_summary <- full_output %>%
    filter(as.numeric(yr) > as.numeric(substr(start_date,1,4)) + 2) %>% #doesn't include 3 yr spin up in calculations
    group_by(pft,yr) %>%
    summarise(
      start_date = as.Date(min(date)),
      end_date = as.Date(max(date)),
      SMP_avg_per_yr = mean(SMP),
      R_avg_per_yr = mean(R),
      R_sd_per_yr = sd(R),
      R_avg_ED2_per_yr = mean(ED2_R), # recruits per day per ha
      R_sd_ED2_per_yr = sd(ED2_R)
    ) %>%
    group_by(pft) %>%
    summarise(
      R_avg = mean(R_avg_per_yr),
      R_sd = sd(R_avg_per_yr),
      R_avg_ED2 = mean(R_avg_ED2_per_yr), # recruits per day per ha
      R_sd_ED2 = sd(R_avg_ED2_per_yr),
      SMP_avg = mean(SMP_avg_per_yr),
      patch = q
    )
  
  summary_data <- rbind(summary_data,temp_summary)
  print(paste("done with patch",q))
}


write.csv(summary_data, file = "temp/SMP_summary_data.csv")


  #source("create_output/create_output.R")



# soil_moisture_period <- 120
# 
# total_length <- nrow(full_output)
# n_segments <- floor(total_length / (soil_moisture_period * 4))
# remainder <- total_length - ((soil_moisture_period * 4) * n_segments)
# 
# segment <- c()
# for(i in 1:n_segments){
#   segment <- append(segment,rep(i,(soil_moisture_period * 4)))
# }
# segment <- append(segment,rep(tail(segment,1),remainder))
# 
# soil_moisture_data <- full_output %>%
#   arrange(date) %>% 
#   add_column(segment = segment) %>% 
#   as.tibble() %>% 
#   group_by(segment,pft) %>%
#   summarise(
#     start_date = as.Date(min(date)),
#     end_date = as.Date(max(date)),
#     soil_moisture = mean(SMP),
#     submodel = mean(R),
#     ED2 = mean(ED2_R)) %>%
#   gather(submodel:ED2,key = "model",value = "R")
# 
source("create_output/figure_recruitment_versus_SMP_v2.R")
  






