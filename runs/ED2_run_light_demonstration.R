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
patch_run_type <- "many" #"many" #one or "many"
synthetic_patches <- T  # T or F
run_name <- "light_based_rec_demo"
start_date <- "2000-01-01"
end_date <- "2015-12-01"
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

if(emulate_ED2 == T){
  source('model/ED2_emulation.R')
}

if(patch_run_type != "many"){
  source("model/regeneration_submodel.R")
  source("create_output/create_output.R")
  }


if(patch_run_type == "many"){

source("clean_input/patch_level_simulations.R")

summary_data <- tibble()
  
for(bin_num in c(1:20)){
  
  tmp_patch_data <- patch_level_light %>% filter(bin == bin_num)
  
  input_data <- input_data1 %>%
    left_join(tmp_patch_data, by = c("yr","month")) %>% 
    dplyr::select(-dateChar, -DateLubr)
  
 source("model/regeneration_submodel.R")
  
 temp_summary <- full_output %>%
    group_by(pft) %>%
    summarise(
      start_date = as.Date(min(date)),
      end_date = as.Date(max(date)),
      bin = mean(bin),
      mean_pct_light = mean(lightZ0),
      sd_pct_light = sd(lightZ0),
      patch_age = mean(patch_age),
      R_avg = mean(R),
      R_avg_ED2 = mean(ED2_R) # recruits per day per ha
    )
  
 summary_data <- rbind(summary_data,temp_summary)
  
#source("create_output/create_output.R") # just run this if you want to see full details of output within age bins
  
}
summary_data <- summary_data %>%
  mutate(eVl = case_when(
    pft %in% c("earlydi","earlydt") ~ "early",
    pft %in% c("latedi","latedt") ~ "late"
  )) %>%
  mutate(dtVdi = case_when(
    pft %in% c("earlydi","latedi") ~ "drought_intol",
    pft %in% c("earlydt","latedt") ~ "drought_tol"
  ))
source("create_output/figure_recruitment_versus_light.R")
}









