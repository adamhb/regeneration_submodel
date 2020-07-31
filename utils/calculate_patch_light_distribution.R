rm(list = ls())
gc()


###----------------------------------------------------------####
###  LIBRARIES                                               ####
###----------------------------------------------------------####
library(hdf5r)    #package used to open hdf5 files
#library(chron) # package used for time series data
library(tidyverse)
library(lubridate)
#print(paste("Preparing Input Data...",Sys.time()))


driver_data_path <- "~/cloud/gdrive/rec_submodel/data/ED2_output/"
path_to_output <- "~/cloud/gdrive/rec_submodel/output/"
#name the run
run_type <- "ED2" # keep this as ED2
emulate_ED2 <- T
patch_run_type <- "many" #"many" #one or "many"
synthetic_patches <- T  # T or F
no_real_patch_light <- T
run_name <- "benchmarking"
start_date <- "2000-01-01"
end_date <- "2016-01-01"
n_PFTs <- 4
soil_layer <- 15 # 15 is 6 cm, 16 is 2 cm deep


source("utils/supporting_funcs.R")
source("clean_input/prep_driver_data_ED2_bci.R")

inputref  = paste0(driver_data_path,"BCI_Xu")
outpath   = path_to_output
patch_level_light <- tibble()
num_ntiles <- 30

for (fl in files){
  
  
  myfile         = paste0(driver_data_path,fl) #build the names of each input file
  mydata1        = h5file(myfile)
  AGE            = mydata1[["AGE"]][]
  AREA           = mydata1[["AREA"]][]
  
  ageDF = tibble(AGE) %>% rownames_to_column(var = "patch") %>% 
    mutate_at(.vars = "patch",.funs = as.numeric) %>%
    mutate(frac_patch_area = AREA)
  
  pft            = mydata1[["PFT"]][]
  paco.n         = mydata1[["PACO_N"]][]
  
  tmp1           = rep(1:length(paco.n), times = paco.n)
  MMEAN_LIGHT_LEVEL = mydata1[["MMEAN_LIGHT_LEVEL"]][]
  
  
  temp <- tibble(patch = tmp1, light = MMEAN_LIGHT_LEVEL) %>%
    group_by(patch) %>%
    summarise(light_at_smallest_cohort = min(light)) %>%
    left_join(ageDF, by = "patch") %>% #print(n= 100)
    mutate(bin = ntile(x = AGE, n =  num_ntiles)) %>% 
    group_by(bin) %>%
    summarise(lightZ0 = mean(light_at_smallest_cohort),
              patch_age = mean(AGE),
              bin_area = sum(frac_patch_area)) %>%
    mutate(yr = unlist(str_extract_all(myfile, "(?<=-)[:digit:]{4}(?=-)")), 
           month =  unlist(str_extract(myfile, "(?<=-)[:digit:]{2}(?=-)"))) %>%
    mutate(dateChar = paste(yr, month, "01" ,sep = "-")) %>%
    mutate_at(.vars = "dateChar",.funs = as.character) %>%
    mutate(DateLubr = ymd(dateChar)) 
  #ggplot(mapping = aes(x = AGE, y = light_at_smallest_cohort)) +
  #geom_point()
  
 
  
  patch_level_light <- rbind(temp,patch_level_light)
  
}

print(
patch_level_light %>%
  group_by(bin) %>%
  summarise(mean_area = mean(bin_area),
            mean_light = mean(lightZ0)) 
)
#about 3 percent isat 20% light,
#another 3 percent is at 8.5% light,
#and the remainder is around 0.05% light




