source("create_output/figure_formatting.R")

agb <- c(c(5.5,5.3,5.0,5.1,5,5,5.3,5.3),c(4,4.2,5,5,5,4,5.5,5.3),c(2.2,1.8,1.9,2.1,2,1.8,2.2,2.2),c(1.9,2.1,2.1,1.8,2,2.2,1.8,1.8))
pft <- c(rep("LD_DT",8),rep("LD_DI",8),rep("ST_DT",8),rep("ST_DI",8))
simYr <- rep(seq(from = 0,to = 400, length.out = 8),4)

tibble(simYr = simYr, agb = agb, pft = pft, scenario = "BASE") %>%
  ggplot(aes(simYr,agb,color = pft)) +
  geom_line(size = 4) +
  scale_color_manual(values = pft.cols) +
  labs(title = "BASE") +
  adams_theme 
  

#ENSO

agb <- c(c(5.5,5.3,5.0,5.1,5,5,5.3,5.3),c(4,4.2,5,5,5,4,5.5,5.3),c(2.2,1.8,1.9,2.1,2,1.8,2.2,2.2),c(1.9,2.1,2.1,1.8,2,2.2,1.8,1.8))
pft <- c(rep("LD_DT",8),rep("LD_DI",8),rep("ST_DT",8),rep("ST_DI",8))
simYr <- rep(seq(from = 0,to = 400, length.out = 8),4)