library(stringr)
library(tidyverse)
library(lubridate)
library(broom)

path_to_observational_data <- "~/cloud/gdrive/rec_submodel/data/observations/"
path_to_observations <- path_to_observational_data
path_to_output <- "~/cloud/gdrive/rec_submodel/output/"

options(dplyr.print_max = 1e5)
options(max.print=1e4)
options(tibble.print_max = 1e4)





#constants
megajoules_per_joule <- 1e-6
par_per_solar_rad <- 0.45 #Garcia-Rodriguez et al., 2020


draw_key_polygon3 <- function(data, params, size) {
  lwd <- min(data$size, min(size) / 4)
  
  grid::rectGrob(
    width = grid::unit(0.3, "npc"),
    height = grid::unit(0.3, "npc"),
    gp = grid::gpar(
      col = data$colour,
      fill = alpha(data$fill, data$alpha),
      lty = data$linetype,
      lwd = lwd * .pt,
      linejoin = "mitre"
    ))
}


dateFromFile <- function(filename){
  yr <- unlist(str_extract_all(filename, "(?<=-)[:digit:]{4}(?=-)"))
  month <- unlist(str_extract(filename, "(?<=-)[:digit:]{2}(?=-)"))
  date <- lubridate::ymd(paste(yr, month, "01" ,sep = "-"))
  return(date)
}





makePNG <- function(fig, path_to_output.x = path_to_output, file_name = "unamed_graph",
                    height=PNGheight,  width=PNGwidth, units=PNGunits, res = PNGres){
  
#fig = SMP_fig
#  file_name = "sMP_fig"
  model_run_time_stamp <- Sys.time() %>% 
    sub(pattern = ":", replacement = "-") %>%
    sub(pattern = ":", replacement = "-") %>%
    sub(pattern = " ", replacement = "-")
  
  png(paste0(path_to_output.x,file_name,"_",model_run_time_stamp,".png"), height=height, width=width, units=units, res = res)
  print(fig)
  dev.off()
}



#source only parts of a file
source2 <- function(file, start, end, ...) {
  file.lines <- scan(file, what=character(), skip=start-1, nlines=end-start+1, sep='\n')
  file.lines.collapsed <- paste(file.lines, collapse='\n')
  source(textConnection(file.lines.collapsed), ...)
}


load_basic_inputData <- function(){
  source2(file = 'runs/ED2_BASE.R',start = 3,end = 38)
}


#generate a log sequence
lseq <- function(from=1, to=100000, length.out=6) {
  # logarithmic spaced sequence
  # blatantly stolen from library("emdbook"), because need only this
  round(exp(seq(log(from), log(to), length.out = length.out)))
}

source2(file = "parameter_files/bci_parameters.R", start = 1, end = 98)
source("create_output/figure_formatting.R")
source("parameter_files/param_names.R")


