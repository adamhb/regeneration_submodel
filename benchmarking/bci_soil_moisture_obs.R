library(tidyverse)
source('utils/supporting_funcs.R')

PTF_func <- function(sgwc){
  
  sgwc_T  <- c(0.78, 0.47, 0.34)
  matric_T <- c(0, -0.5, -3.5)
  d <- data.frame(sgwc_T = sgwc_T, matric_T = matric_T)
  #plot(sgwc_T,matric_T)
  
  sgwc_T2 <- sgwc_T^2
  PTF <- lm(matric_T ~ sgwc_T + sgwc_T2)
  
  
  matric <- coef(PTF)[3]*sgwc^2 + coef(PTF)[2]*sgwc + coef(PTF)[1]
  return(matric) #returns matric potential
}

path_to_benchmarking_data <- "~/cloud/gdrive/rec_submodel/data/observations/"

sm.obs <- read_csv(paste0(path_to_benchmarking_data,"bci_lutz_soil_man.csv")) # https://smithsonian.figshare.com/articles/dataset/Barro_Colorado_Island_Lutz_catchment_Soil_moisture_manual/10042517/3

Lutz.smp <- sm.obs %>%
  filter(site == "LUTZ *",
         date > as.Date("2008-01-01"),
         date < as.Date("2014-12-31"),
         depth == "0-10") %>% pull(h2o.by.wet) %>% mean() %>% PTF_func()

