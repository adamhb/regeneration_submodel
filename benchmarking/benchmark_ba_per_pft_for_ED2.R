library(tidyverse)
library(ncdf4)
library(ncdf.tools)
source("utils/system_settings.R")
source('create_output/figure_formatting.R')

ba.sp <- read_csv(paste0(path_to_observational_data,"bci_tree_basal_area_12_1_2020.csv"),skip = 1) %>%
  select(Species,`2015`) %>%
  rename(Latin = Species, ba = `2015`)

pfts <- read_csv("benchmarking/pft_assignments.csv")

ba.sp2 <- pfts %>%
  left_join(ba.sp, by = "Latin") %>%
  group_by(pft) %>%
  summarise(ba = sum(ba, na.rm = T))


#ED2 predictions of ba
start_date <- "2001-01-01" #this has to be in the range of the driver data
end_date <- "2020-12-31"
inDateRange <- dateFromFile(list.files(path = driver_data_path)) %in% seq(ymd(start_date), ymd(end_date), by = "months")
files <- list.files(driver_data_path)[inDateRange]
inputref  = paste0(driver_data_path,"BCI_Xu")
driver_data_path <- "~/cloud/gdrive/rec_submodel/data/ED2_output/"


#ED2_ba <- tibble()
Ba_pft <- tibble()
j = 0
for (fl in files){
  j              = j + 1
  myfile         = paste0(driver_data_path,fl) #build the names of each input file
  mydata1        = h5file(myfile)
  
  BA_PFT2         = sum(mydata1[["BA_PFT"]][2,1])
  BA_PFT4         = sum(mydata1[["BA_PFT"]][4,1])
  BA_PFT25        = sum(mydata1[["BA_PFT"]][25,1])
  BA_PFT26        = sum(mydata1[["BA_PFT"]][26,1])
  
  tmp2 <- tibble(LD_DT = BA_PFT2,
                 ST_DT = BA_PFT4,
                 LD_DI = BA_PFT25,
                 ST_DI = BA_PFT26) %>%
    mutate(yr = unlist(str_extract_all(myfile, "(?<=-)[:digit:]{4}(?=-)")), 
           month =  unlist(str_extract(myfile, "(?<=-)[:digit:]{2}(?=-)")))
  Ba_pft <- rbind(Ba_pft,tmp2)
  print(paste(j,"of",length(files)))
}

#converting to long form
Ba_pft2 <- Ba_pft %>%
  gather(LD_DT:ST_DI, key = "pft",value = "ba") %>%
  group_by(pft) %>%
  summarise(ba = mean(ba)) %>%
  mutate(type = "ED2")


#bci obs
ba_per_pft_fig <- ba.sp2 %>%
  mutate(type = "bci obs.") %>%
  ggplot(aes(pft,ba, fill = pft)) +
  geom_bar(stat="identity") +
  ylab(expression(paste("basal area [","m"^"-2"," ha"^"-1","]"))) +
  scale_fill_manual(values = pft.cols) +
  geom_point(data = Ba_pft2, mapping = aes(pft,ba), size = 4, shape = 10) +
  adams_theme +
  theme(legend.position = "none") 


makePNG(fig = ba_per_pft_fig, path_to_output.x = paste0(path_to_output,"forMS/"),file_name = "benchmarking_ba_for_ED2")



