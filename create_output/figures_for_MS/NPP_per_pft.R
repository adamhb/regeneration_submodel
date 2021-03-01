#show NPP per pft per scenario

#load libraries
library(ncdf4)
library(ncdf.tools)
from_new_data <- T

library(magrittr)
library(tidyverse)
library(lubridate)
library(scales)
library(plotrix)
library(ggplot2)
library(reshape2)

source("utils/supporting_funcs.R")
source("create_output/figure_formatting.R")
source("parameter_files/bci_parameters.R")



if(from_new_data == T){
  #name the run
  run_type <- "ED2" # keep this as ED2
  emulate_ED2 <- T
  patch_run_type <- "one" #"many" #one or "many"
  synthetic_patches <- F  # T or F
  run_name <- "SMP_BASE"
  start_date <- "2001-01-01"
  end_date <- "2030-12-31"
  n_PFTs <- 4
  soil_layer <- 15 # 15 is 6 cm, 16 is 2 cm deep
  
  #set path to driver data
  
  runs <- c("ED2_output/", "ED2_ENSO/", "ED2_wet/","ED2_dryDS/analy_dry75/")
  
  for(r in runs){
    driver_data_path <- paste0("~/cloud/gdrive/rec_submodel/data/",r)
    source("clean_input/prep_driver_data_ED2_bci.R")
    write_csv(input_data %>% select(yr,pft,NPP) %>% mutate(run = basename(driver_data_path)),path = paste0("temp/",basename(driver_data_path),".csv"))
    print(paste("wrote csv for",r))
  }
}



NPPdata <- read_csv("temp/ED2_output.csv") %>%
  rbind(read_csv("temp/ED2_ENSO.csv")) %>%
  rbind(read_csv("temp/ED2_wet.csv")) %>%
  rbind(read_csv("temp/analy_dry75.csv")) 
  
NPP_fig <- NPPdata %>%
  rownames_to_column(var = "day") %>%
  filter(as.numeric(day) %% 200 == 0) %>%
  mutate(simYr = as.numeric(stringr::str_sub(yr, start = 1, end = 4)) - 2000) %>%
  mutate(run = case_when(
    run == "analy_dry75" ~ "DRY-DS",
    run == "ED2_ENSO" ~ "SYN-ENSO",
    run == "ED2_output" ~ "BASE",
    run == "ED2_wet" ~ "WET"
  )) %>%
  mutate(run = factor(run,levels = c("BASE","SYN-ENSO","WET","DRY-DS"))) %>%
  ggplot(aes(simYr,NPP,color = pft)) +
  geom_smooth(se = F) +
  scale_y_continuous(limits = c(0.5,1.3), breaks = c(0.5,0.75,1,1.25)) +
  ylab(expression(paste("NPP ", "(g C m"^"-2","day"^"-1",")")))+
  xlab(label = "simulation year") +
  facet_wrap(~run) +
  scale_color_manual(values = pft.cols) +
  adams_theme +
  theme(panel.spacing = unit(2, "lines")) +
  guides(colour = guide_legend(override.aes = list(size = 5)))
  
makePNG(NPP_fig,path_to_output.x = path_to_MS_figures,file_name = "NPP_figs")






