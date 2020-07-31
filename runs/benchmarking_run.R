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

#tuning param
avg_l <- 105 #baesd on the average total solar radiation load (MJ per m2) at the forest floor over 6 months (annual average); can be 61

#name the run
run_type <- "ED2" # keep this as ED2
emulate_ED2 <- T
patch_run_type <- "many" #"many" #one or "many"
synthetic_patches <- T  # T or F
no_real_patch_light <- T
run_name <- "benchmarking"
start_date <- "2002-01-01"
end_date <- "2005-01"
n_PFTs <- 4
soil_layer <- 15 # 15 is 6 cm, 16 is 2 cm deep

#set path to driver data
driver_data_path <- "~/cloud/gdrive/rec_submodel/data/ED2_output/"
path_to_output <- "~/cloud/gdrive/rec_submodel/output/"

#site and scenario params
#avg_precip <- 71 #precipitation in mm over two weeks (the annual average)
avg_SMP <- -60326 #


#dbh.x <- 500 #dbh in mm
#N_co.x <- 800  #the number of individuals in a cohort


#source parameter values
source("parameter_files/parameters_ED2_run_benchmarking.R")
source("clean_input/prep_driver_data_ED2_bci.R")
source('model/ED2_emulation.R')


#running the model inside patches
num_patches <- 3
patch_light <- c(0.2,0.1,0.03)
patch_fractional_areas <- c(0.03,0.03,0.94)



output_all_patches <- tibble()
for (i in 1:num_patches){

model_area <- 10000 * patch_fractional_areas[i] #area in square meters
  
bin_num <- i
   
tmp_patch_data <- input_data %>%
  distinct(yr,month) %>%
  mutate(bin = i,
         lightZ0 = patch_light[i])

input_data <- input_data1 %>%
  left_join(tmp_patch_data, by = c("yr","month"))

source("model/regeneration_submodel.R")
source("create_output/create_output.R")

output_all_patches <- rbind(output_all_patches,full_output)
}


output_all_patches %>%
  group_by(yr, pft) %>%
  summarise(R_sum = sum(R))


#sum the patch level output to view on graphs



