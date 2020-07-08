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


#name the run
run_type <- "single"
run_name <- "bci_ED2_test_after_fixing_date_issues"
start_date <- "2003-01-01"
end_date <- "2015-12-29"
n_PFTs <- 4
soil_layer <- 15 # 15 is 6 cm, 16 is 2 cm deep

#set path to driver data
driver_data_path <- "~/cloud/gdrive/rec_submodel/data/ED2_output"
path_to_output <- "~/cloud/gdrive/rec_submodel/output"

#site and scenario params
avg_precip <- 71 #precipitation in mm over two weeks (the annual average)
avg_SMP <- -60326 #
avg_l <- 61 #the average total solar radiation load (MJ per m2) at the forest floor over 6 months (annual average)

if(run_type != "multiPatch"){
  percent_light <- 0.03
}



#dbh.x <- 500 #dbh in mm
#N_co.x <- 800  #the number of individuals in a cohort
model_area <- 10000 #area in square meters

#source parameter values
source("parameter_files/parameters.R")

source("clean_input/prep_driver_data_ED2_bci.R")

if(run_type == "multiPatch"){
  source("patch_level_simulations.R")
}

source("model/regeneration_submodel.R")
#run model
#source("model/regeneration_submodel.R")
#generate output
source("create_output/create_output.R")






