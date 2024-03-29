run400 <- F
source('create_output/figures_for_MS/benchmark_figure_BCI_2008to2014.R')
#show NPP per pft per scenario

#load libraries
library(ncdf4)
#library(ncdf.tools)
from_new_data <- F

library(magrittr)
library(tidyverse)
library(lubridate)
library(scales)
#library(plotrix)
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
  end_date <- "2399-12-31"
  n_PFTs <- 4
  soil_layer <- 15 # 15 is 6 cm, 16 is 2 cm deep
  
  #set path to driver data
  
  runs <- c("ED2_output/", "ED2_ENSO/", "ED2_wet/","ED2_dryDS/analy_dry75/")
  
  for(r in runs){
    driver_data_path <- paste0("~/cloud/gdrive/rec_submodel/data/",r)
    source("clean_input/prep_driver_data_ED2_bci.R")
    
    #get precip data
    precip <- ED2_data1 %>% select(yr, month) %>% distinct() %>%
      arrange(yr,month) %>%
      add_column(rain) %>%
      mutate(run = basename(driver_data_path))
    
    write_csv(precip,path = paste0("temp/","precip_",basename(driver_data_path),".csv"))
    
    write_csv(input_data %>% select(yr,month,pft,NPP) %>% mutate(run = basename(driver_data_path)),path = paste0("temp/",basename(driver_data_path),".csv"))
    print(paste("wrote csv for",r))
  }
}

precip_data <- read_csv("temp/precip_ED2_output.csv") %>%
  rbind(read_csv("temp/precip_ED2_ENSO.csv")) %>%
  rbind(read_csv("temp/precip_ED2_wet.csv")) %>%
  rbind(read_csv("temp/precip_analy_dry75.csv")) 

NPPdata <- read_csv("temp/ED2_output.csv") %>%
  rbind(read_csv("temp/ED2_ENSO.csv")) %>%
  rbind(read_csv("temp/ED2_wet.csv")) %>%
  rbind(read_csv("temp/analy_dry75.csv")) %>%
  left_join(precip_data, by = c("yr","month","run"))

#write_csv(NPPdata,"misc/NPPdata400Yrs.csv")
if(run400 == T){
  NPPdata <- read_csv(paste0(path_to_observations,"NPPdata400Yrs.csv"))
}

#create a dataframe with the vertical line information
vh_line <- data.frame(
  xintercept = c(9, 29),
  run = rep("SYN-ENSO", 2)
)

if(run400 == F){
  NPP_fig <- NPPdata %>%
    rownames_to_column(var = "day") %>%
    filter(as.numeric(day) %% 200 == 0) %>%
    mutate(simYr = as.numeric(stringr::str_sub(yr, start = 1, end = 4)) - 2000) %>%
    filter(simYr < 36) %>%
    mutate(run = case_when(
      run == "analy_dry75" ~ "DRY-DS",
      run == "ED2_ENSO" ~ "SYN-ENSO",
      run == "ED2_output" ~ "BASE",
      run == "ED2_wet" ~ "WET"
    )) %>%
    mutate(run = factor(run,levels = c("BASE","SYN-ENSO","WET","DRY-DS"))) %>%
    ggplot(aes(simYr,NPP,color = pft)) +
    #geom_line() +
    #geom_smooth(se = F, span = 0.1) +
    geom_smooth(se = F) +
    geom_vline(data = vh_line, aes(xintercept = xintercept), linetype='dashed', size=1) +
    scale_y_continuous(limits = c(0.5,1.3), breaks = c(0.5,0.75,1,1.25)) +
    ylab(expression(paste("NPP ", "(g C m"^"-2","day"^"-1",")")))+
    xlab(label = "simulation year") +
    facet_wrap(~run) +
    scale_color_manual(values = pft.cols) +
    adams_theme +
    theme(panel.spacing = unit(2, "lines")) +
    guides(colour = guide_legend(override.aes = list(size = 5)))
  
  makePNG(NPP_fig,path_to_output.x = path_to_MS_figures,file_name = "NPP_figs", res = 600)
  
  pdf(file = "figures/Fig3.pdf", width = 7, height = 5)
  NPP_fig
  dev.off()
  
}



if(run400 == T){
  
NPP_fig_400_yrs <- NPPdata %>%
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
  #geom_line() +
  geom_smooth(se = F, span = 0.1) +
  #geom_smooth(size = 1, method = "loess", span = .01, se = F) +
  #geom_vline(data = vh_line, aes(xintercept = xintercept), linetype='dashed', size=1) +
  scale_y_continuous(limits = c(0,1.3), breaks = c(0,0.25,0.5,0.75,1,1.25)) +
  ylab(expression(paste("NPP ", "(g C m"^"-2","day"^"-1",")")))+
  xlab(label = "simulation year") +
  facet_wrap(~run) +
  scale_color_manual(values = pft.cols) +
  adams_theme +
  theme(panel.spacing = unit(2, "lines")) +
  guides(colour = guide_legend(override.aes = list(size = 5)))
  
makePNG(NPP_fig_400_yrs,path_to_output.x = path_to_MS_figures,file_name = "NPP_figs_400_yr")

}



seasonal_NPP_fig <- NPPdata %>%
  rownames_to_column(var = "day") %>%
  #filter(as.numeric(day) %% 200 == 0) %>%
  mutate(simYr = as.numeric(stringr::str_sub(yr, start = 1, end = 4)) - 2000) %>%
  mutate(run = case_when(
    run == "analy_dry75" ~ "DRY-DS",
    run == "ED2_ENSO" ~ "SYN-ENSO",
    run == "ED2_output" ~ "BASE",
    run == "ED2_wet" ~ "WET"
  )) %>%
  mutate(run = factor(run,levels = c("BASE","SYN-ENSO","WET","DRY-DS"))) %>%
  filter(run == "SYN-ENSO") %>%
  mutate_at(.vars = "month",.funs = as.numeric) %>%
  filter(simYr < 31 & simYr > 28) %>%
  ggplot(aes(day,NPP,color = pft)) +
  #geom_line() +
  #geom_smooth(se = F, span = 0.1) +
  #geom_smooth(se = F) +
  geom_line() +
  #geom_vline(data = vh_line, aes(xintercept = xintercept), linetype='dashed', size=1) +
  scale_y_continuous(limits = c(0.5,1.3), breaks = c(0.5,0.75,1,1.25)) +
  ylab(expression(paste("NPP ", "(g C m"^"-2","day"^"-1",")")))+
  xlab(label = "simulation year") +
  scale_color_manual(values = pft.cols) +
  adams_theme +
  #theme(panel.spacing = unit(2, "lines")) +
  guides(colour = guide_legend(override.aes = list(size = 5)))


NPPdata %>%
  rownames_to_column(var = "day") %>%
  filter(as.numeric(day) %% 200 == 0) %>%
  mutate(simYr = as.numeric(stringr::str_sub(yr, start = 1, end = 4)) - 2000) %>%
  filter(simYr < 31 & simYr > 28) %>%














#Figure for the SI that shows variation in precipitation for each of the scenarios
precipData <- NPPdata %>%
  mutate_at(.vars = "month", .funs = as.numeric) %>%
  mutate(season = case_when(
    month %in% 1:4 ~ "dry",
    month %in% 5:12 ~ "wet"
  )) %>%
  filter(pft == "LD_DI") %>% 
  distinct() %>%
  mutate(run = case_when(
    run == "analy_dry75" ~ "DRY-DS",
    run == "ED2_ENSO" ~ "SYN-ENSO",
    run == "ED2_output" ~ "BASE",
    run == "ED2_wet" ~ "WET"
  )) %>% 
  mutate(simYr = as.numeric(stringr::str_sub(yr, start = 1, end = 4)) - 2000) %>%
  group_by(run, simYr, season) %>%
  summarise(precip = sum(rain)) 
  

precipFig <- precipData %>%
  ggplot(aes(simYr,precip,color = season)) +
  geom_line() +
  scale_color_manual(values = c("brown","blue")) +
  geom_vline(data = vh_line, aes(xintercept = xintercept), linetype='dashed', size=1) +
  facet_wrap(~run) +
  scale_y_log10() +
  ylab(label = "total precip. per season (mm)") +
  xlab(label = "simulation year") +
  adams_theme +
  theme(legend.title = element_text()) +
  guides(fill=guide_legend(title="Season"))

makePNG(precipFig,path_to_output.x = path_to_MS_figures,file_name = "precipFig")






