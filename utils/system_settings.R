library(tidyverse)
library(broom)
path_to_observational_data <- "~/cloud/gdrive/rec_submodel/data/observations/"
path_to_observations <- path_to_observational_data
path_to_output <- "~/cloud/gdrive/rec_submodel/output/"

options(dplyr.print_max = 1e5)
options(max.print=1e4)
options(tibble.print_max = 1e4)


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

