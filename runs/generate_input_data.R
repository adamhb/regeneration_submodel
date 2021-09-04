#generate input data for a BASE simulation under observed meteorology at BCI
source("utils/supporting_funcs.R")

run_name <- "SMP_BASE" #user can change the name of the run (i.e. "testing new parameters")
emulate_ED2 <- T #do you want to predict ED2's recruitment rates as well? Keep this as TRUE
start_date <- "2001-01-01" #this has to be in the range of the driver data
end_date <- "2020-12-31"
n_PFTs <- 4 #the number of PFTs in the run
soil_layer <- 15 #The is the soil layer of the host VDM that you want to use for soil moisutre experienced by the seedlings. Soil layer '15' is 6 cm below the surface. 16 is 2 cm deep
run_type <- "ED2" # keep this as ED2, do not change
emulate_ED2 <- T #do you want to predict ED2's recruitment rates as well? Keep this as TRUE
patch_run_type <- "one" #is this a single patch simulation or a multi-patch simulation?
synthetic_patches <- F 


#set path to driver data
driver_data_path <- "~/cloud/gdrive/rec_submodel/data/ED2_output/"
path_to_output <- "~/cloud/gdrive/rec_submodel/output/"
percent_light <- 0.02 #set the understory light level

#source parameter values
source("parameter_files/default_parameters.R") #source the default parameters
#process the driver data
source("clean_input/prep_driver_data_ED2_bci.R") #prepare the driver data for the submodel