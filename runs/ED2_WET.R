#This script runs the Tree Recruitment Scheme under WET meteorology (wetter than 
#observed) using data from ED2 for a simulation at Barro Colorado Island.


#load libraries
library(ncdf4)
library(magrittr)
library(tidyverse)
library(lubridate)
library(scales)
library(ggplot2)
library(reshape2)

source("utils/supporting_funcs.R")

#name the run
run_type <- "ED2" # keep this as ED2
emulate_ED2 <- T
patch_run_type <- "one" #"many" #one or "many"
synthetic_patches <- F  # T or F
run_name <- "WET_no_emerg_max"
start_date <- "2001-01-01"
end_date <- "2044-12-31" #was 2034
n_PFTs <- 4
soil_layer <- 15 # 15 is 6 cm, 16 is 2 cm deep

#set path to driver data
driver_data_path <- "~/cloud/gdrive/rec_submodel/data/ED2_wet/"
path_to_output <- "~/cloud/gdrive/rec_submodel/output/"

#changes from default parameter values

if(high_light == T){
  percent_light <- 0.2
}else{
  percent_light <- 0.02
}

#source parameter values
source("parameter_files/bci_parameters.R")




source("clean_input/prep_driver_data_ED2_bci.R")

if(emulate_ED2 == T){
  source('model/ED2_emulation.R')
}

source("model/regeneration_submodel.R")

write_csv(full_output,'model_data_output/model_output_WET_scenario.csv')

source("create_output/create_output.R")















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
  






