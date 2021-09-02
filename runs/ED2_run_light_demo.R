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
no_real_patch_light <- T
run_name <- "recruitment_vs_light"
start_date <- "2001-01-01"
end_date <- '2024-12-31'#"2034-12-31"
n_PFTs <- 4
soil_layer <- 15 # 15 is 6 cm, 16 is 2 cm deep

#set path to driver data
driver_data_path <- "~/cloud/gdrive/rec_submodel/data/ED2_output/"
path_to_output <- "~/cloud/gdrive/rec_submodel/output/"

#dbh.x <- 500 #dbh in mm
#N_co.x <- 800  #the number of individuals in a cohort
model_area <- 10000 #area in square meters

#source parameter values
source("parameter_files/bci_parameters.R") #changed this to BCI params; was bci_params_ed2_default

source("clean_input/prep_driver_data_ED2_bci.R")

source('model/ED2_emulation.R')

num_bins <- 20

source("clean_input/patch_level_simulations.R")

summary_data <- tibble()
for(bin_num in 1:num_bins){
  
  tmp_patch_data <- patch_level_light %>% filter(bin == bin_num)
  
  input_data <- input_data1 %>%
    left_join(tmp_patch_data, by = c("yr","month")) %>% 
    dplyr::select(-dateChar, -DateLubr)
  
 source("model/regeneration_submodel.R")


 temp_summary <- full_output %>%
    filter(as.numeric(yr) > as.numeric(substr(start_date,1,4)) + 4) %>% #doesn't include 4 yr spin up in calculations
    group_by(pft,yr) %>%
    summarise(
      start_date = as.Date(min(date)),
      end_date = as.Date(max(date)),
      bin = mean(bin),
      mean_pct_light_yr = mean(lightZ0),
      sd_pct_light = sd(lightZ0),
      patch_age = mean(patch_age),
      R_avg_per_yr = mean(R),
      R_sd_per_yr = sd(R),
      R_avg_ED2_per_yr = mean(ED2_R), # recruits per day per ha
      R_sd_ED2_per_yr = sd(ED2_R),
      NPP = mean(NPP),
      N_co = mean(N_co),
      seedpool = mean(seedpool)
    ) %>%
   group_by(pft) %>%
   summarise(R_avg = mean(R_avg_per_yr),
             R_sd = sd(R_avg_per_yr),
             R_avg_ED2 = mean(R_avg_ED2_per_yr),
             R_sd_ED2 = sd(R_avg_ED2_per_yr),
             mean_pct_light = mean(mean_pct_light_yr)
             )
 
   
 summary_data <- rbind(summary_data,temp_summary)
  print(paste("done with patch",bin_num,"of",num_bins))
 if(bin_num < 2){
   source("create_output/create_output.R")
 }
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


write_csv(summary_data,"temp/recruitment_vs_light.csv")

#source("create_output/figure_recruitment_versus_light.R")











