library(ggplot2)
library(magrittr)
library(tidyverse)

# figures for submodel simulations
rm(list = ls())
gc()



pft.cols <- c("darkolivegreen4","midnightblue")

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


# generating simulation data to demonstrate figures ideas

patch_age <- 1:300
avg_light_2m <- runif(min = 10e4, max = 27e6, n = 300)
pct_light <- seq(from = 1, to = 100, length.out = 300)

rec_sub_early <- avg_light_2m * 0.00004 + avg_light_2m^-2  + 40 + rnorm(mean = 100, sd = 50, n = 300)    
rec_sub_late <- avg_light_2m * 0.00002 + avg_light_2m^-2  + 40 + rnorm(mean = 100, sd = 50, n = 300)
ed2_early <- rnorm(mean = 800, sd = 30, n = 300)
ed2_late <- rnorm(mean = 800, sd = 30, n = 300)
rec <- c(rec_sub_early, rec_sub_late, ed2_early, ed2_late)

pft <- c(rep("earlyPFT", 300), rep("latePFT", 300), rep("earlyPFT", 300), rep("latePFT", 300))
model <- c(rep("submodel", 600), rep("ED2", 600))


tibble(patch_age = rep(patch_age,4), avg_light_2m = rep(avg_light_2m,4), pct_light = rep(pct_light,4), rec = rec, pft = pft, model = model) %>%
  sample_n(size = 200) %>%
  ggplot(mapping = aes(x = avg_light_2m, y = rec, color = pft, shape = model)) +
  geom_point(size = 2.5, stroke = 1, alpha = 0.5) +
  adams_theme +
  scale_shape_manual(values = rep(c(21,24),2)) +
  scale_color_manual(values = rep(c(pft.cols[1], pft.cols[2]),2)) +
  ylab(expression(paste('avg. rec. rate'," (# indv. ha"^"-1", "yr"^"-1", ")"))) +
  xlab("light (MJ in prior 6 months)")

# MJ m-2









soil_moisture <- seq(from = 10e4, to =  10e6, length.out = 100)

rec_sub_di <- soil_moisture * -0.00008 + soil_moisture^-2  + 40 + rnorm(mean = 100, sd = 50, n = 100) + 500    
rec_sub_dt <- soil_moisture * -0.00002 + soil_moisture^-2  + 40 + rnorm(mean = 100, sd = 50, n = 100) + 500   
ed2_di <- rnorm(mean = 800, sd = 30, n = 100)
ed2_dt <- rnorm(mean = 800, sd = 30, n = 100)
rec <- c(rec_sub_di, rec_sub_dt, ed2_di, ed2_dt)

pft <- c(rep("diPFT", 100), rep("dtPFT", 100), rep("diPFT", 100), rep("dtPFT", 100))
model <- c(rep("submodel", 200), rep("ED2", 200))


tibble(soil_moisture = rep(soil_moisture,4), rec = rec, pft = pft, model = model) %>%
  sample_n(size = 200) %>%
  ggplot(mapping = aes(x = soil_moisture, y = rec, color = pft, shape = model)) +
  geom_point(size = 2.5, stroke = 1, alpha = 0.5) +
  adams_theme +
  scale_shape_manual(values = rep(c(21,24),2)) +
  scale_color_manual(values = rep(c(pft.cols[1], pft.cols[2]),2)) +
  ylab(expression(paste('avg. rec. rate'," (# indv. ha"^"-1", "yr"^"-1", ")"))) +
  xlab("soil matric potential (avg. over prior 4 months)") +
  scale_y_continuous(limits = c(0,900))




