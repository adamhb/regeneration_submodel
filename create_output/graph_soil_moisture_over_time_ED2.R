#graphing soil moisture over time from the ED2 data
source("create_output/figure_formatting.R")

library(hdf5r)
library(tidyverse)
library(stringr)



soil_layer <- 15

driver_data_path <- "~/cloud/gdrive/rec_submodel/data/ED2_output/"
path_to_output <- "~/cloud/gdrive/rec_submodel/output/"

files <- list.files(driver_data_path)

MMEAN_SOIL_MSTPOT <- c()
date <- c()
j <- 0


for (fl in files[-c(1:2)]){
  
  j <- j + 1
  myfile         = paste0(driver_data_path,fl) #build the names of each input file
  date_tmp       = dateFromFile(myfile)
  mydata1        = h5file(myfile)
  
  tmp <- mydata1[["MMEAN_SOIL_MSTPOT"]][soil_layer,] #shallowest layer (as evidenced by SLZ)
  MMEAN_SOIL_MSTPOT <- append(MMEAN_SOIL_MSTPOT,tmp)
  date <- append(date,date_tmp)
  #lyr[j] <- unlist(str_extract_all(myfile, "(?<=-)[:digit:]{4}(?=-)")) 
  #lmo[j] <- unlist(str_extract(myfile, "(?<=-)[:digit:]{2}(?=-)"))
}

out <- tibble(date = date, soil_moisture = MMEAN_SOIL_MSTPOT)

out %>%
  ggplot(mapping = aes(date,soil_moisture)) +
  geom_line() +
  adams_theme


# 
# ED2_data %>%
#   mutate() %>%
#   mutate_at(.vars = "date",.funs = as.character) %>%
#   #arrange(pft) %>% 
#   mutate(Date = ymd(date)) 