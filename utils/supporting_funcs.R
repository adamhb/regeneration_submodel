library(stringr)
library(tidyverse)

dateFromFile <- function(filename){
  yr <- unlist(str_extract_all(filename, "(?<=-)[:digit:]{4}(?=-)"))
  month <- unlist(str_extract(filename, "(?<=-)[:digit:]{2}(?=-)"))
  date <- lubridate::ymd(paste(yr, month, "01" ,sep = "-"))
  return(date)
}

makePNG <- function(fig, path_to_output.x = path_to_output, file_name = "unamed_graph"){
  
#fig = SMP_fig
#  file_name = "sMP_fig"
  model_run_time_stamp <- Sys.time() %>% 
    sub(pattern = ":", replacement = "-") %>%
    sub(pattern = ":", replacement = "-") %>%
    sub(pattern = " ", replacement = "-")
  
  png(paste0(path_to_output.x,file_name,"_",model_run_time_stamp,".png"), height=PNGheight, width=PNGwidth, units=PNGunits, res = PNGres)
  print(fig)
  dev.off()
}
