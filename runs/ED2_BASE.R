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
run_type <- "ED2" # keep this as ED2, do not change
emulate_ED2 <- T #do you want to predict ED2's recruitment rates as well? Keep this as TRUE
patch_run_type <- "one" #is this a single patch simulation or a multi-patch simulation?
synthetic_patches <- F  #are you prescribing light in each patch or taking the distribution of patch-level light from the host VDM?
run_name <- "SMP_BASE" #user can change the name of the run (i.e. "testing new parameters")
start_date <- "2001-01-01" #this has to be in the range of the driver data
end_date <- "2020-12-31"
n_PFTs <- 4 #the number of PFTs in the run
soil_layer <- 15 #The is the soil layer of the host VDM that you want to use for soil moisutre experienced by the seedlings. Soil layer 15 is 6 cm below the surface. 16 is 2 cm deep

#set path to driver data
driver_data_path <- "~/cloud/gdrive/rec_submodel/data/ED2_output/"
path_to_output <- "~/cloud/gdrive/rec_submodel/output/"

#source parameter values
source("parameter_files/default_parameters.R") #source the default parameters
#changes from default parameter values
percent_light <- 0.02 #set the understory light level

source("clean_input/prep_driver_data_ED2_bci.R") #prepare the driver data for the submodel

if(emulate_ED2 == T){
  source('model/ED2_emulation.R') #source ED2's functions
}

source("model/regeneration_submodel.R") #run the submodel
source("create_output/create_output.R") #create submodel output


#pick up here
write_csv(N_recs_per_year_pfts,"temp/N_recs_per_yr_default_params.csv")


source("parameter_files/bci_parameters.R")
#changes from default parameter values
percent_light <- 0.02

source("clean_input/prep_driver_data_ED2_bci.R")

if(emulate_ED2 == T){
  source('model/ED2_emulation.R')
}

source("model/regeneration_submodel.R")
source("create_output/create_output.R")

write_csv(N_recs_per_year_pfts,"temp/N_recs_per_yr_bci_params.csv")



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
# source("create_output/figure_recruitment_versus_SMP.R")
  






