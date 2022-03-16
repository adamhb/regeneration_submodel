#This script runs the Tree Recruitment Scheme under BASE meteorology
#using data from ED2 for a simulation at Barro Colorado Island.
#It runs the TRS with default parameter values and values tuned to BCI. 

#Load libraries
library(ncdf4)
#library(ncdf.tools)
library(magrittr)
library(tidyverse)
library(lubridate)
library(scales)
#library(plotrix)
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
percent_light <- 0.02 #set the understory light level

#source parameter values
source("parameter_files/default_parameters.R") #source the default parameters
#changes from default parameter values

source("clean_input/prep_driver_data_ED2_bci.R") #prepare the driver data for the submodel

if(emulate_ED2 == T){
  source('model/ED2_emulation.R') #source ED2's functions
}

source("model/regeneration_submodel.R") #run the submodel

write_csv(full_output,'temp/full_output.csv')
write_csv(input_vars,"temp/input_vars.csv")

source("create_output/create_output.R") #create submodel output

write_csv(N_recs_per_year_pfts,"temp/N_recs_per_yr_default_params.csv")



#run the same simulation as above, but with parameters tuned to BCI
source("parameter_files/bci_parameters.R")

percent_light <- 0.02

source("clean_input/prep_driver_data_ED2_bci.R")

if(emulate_ED2 == T){
  source('model/ED2_emulation.R')
}

source("model/regeneration_submodel.R")

write_csv(full_output,'model_data_output/model_output_BASE_scenario.csv')

source("create_output/create_output.R")

write_csv(N_recs_per_year_pfts,"temp/N_recs_per_yr_bci_params.csv")









