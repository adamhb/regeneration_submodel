source('utils/supporting_funcs.R')

path_to_output <- "~/cloud/gdrive/rec_submodel/output/"
path_to_MS_figures <- paste0(path_to_output,"forMS/")


pft.cols <- c("darkolivegreen2","darkolivegreen4","lightskyblue", "midnightblue")

psize <- 7
axis_size <- 20
title_size <- 25

#set theme for the plots
adams_theme <- theme(plot.title = element_text(hjust = 0.5, size = 22),
                     strip.text.x = element_text(size = 20),
                     legend.title = element_blank (),
                     axis.title.x = element_text (size = 18), # change the axis title
                     axis.title.y = element_text (size = 18),
                     axis.title.y.right = element_text (size = 18, color = pft.cols[2]),
                     axis.text.x = element_text (size = 16, colour = "black"),
                     axis.text.y = element_text (size = 16, colour = "black"),
                     legend.text = element_text (size = 18),
                     legend.spacing.x = unit(0.3, 'cm'),
                     legend.spacing.y = unit(0.3, 'cm'), #this changes the spacing between groups of legend symbols
                     legend.key.size = unit(0.9, "cm"))

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
PNGheight = 5
PNGwidth = 8 #usually 8
PNGunits = "in"
PNGres = 100
