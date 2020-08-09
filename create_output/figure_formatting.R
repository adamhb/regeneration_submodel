source('utils/supporting_funcs.R')

path_to_output <- "~/cloud/gdrive/rec_submodel/output/"
path_to_MS_figures <- paste0(path_to_output,"forMS/")


pft.cols <- c("darkolivegreen2","darkolivegreen4","lightskyblue", "midnightblue")

#set theme for the plots
adams_theme <- theme(plot.title = element_text(hjust = 0.5, size = 20),
                     strip.text.x = element_text(size = 18),
                     legend.title = element_blank (),
                     axis.title.x = element_text (size = 15), # change the axis title
                     axis.title.y = element_text (size = 15),
                     axis.title.y.right = element_text (size = 15, color = pft.cols[2]),
                     axis.text.x = element_text (size = 14, colour = "black"),
                     axis.text.y = element_text (size = 14, colour = "black"),
                     legend.text = element_text (size = 15))

adams_theme <- theme_minimal() + adams_theme


multipanel_theme <- theme(plot.title = element_text(hjust = 0.5, size = 25),
                     strip.text.x = element_text(size = 20),
                     legend.title = element_blank (),
                     axis.title.x = element_text (size = 22), # change the axis title
                     axis.title.y = element_text (size = 22),
                     axis.title.y.right = element_text (size = 25, color = pft.cols[2]),
                     axis.text.x = element_text (size = 20, colour = "black"),
                     axis.text.y = element_text (size = 20, colour = "black"),
                     legend.position = "none")


if(exists("start_date")){
  plot_title <- labs(title = paste0(start_date,"--",end_date,"\n",
                                    "driver: ",basename(driver_data_path)))
}



#png options
PNGheight=5
PNGwidth=6.5 #usually 8
PNGunits="in"
PNGres = 100
