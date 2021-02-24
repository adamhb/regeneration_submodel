#png options
PNGheight = 11/3
PNGwidth = 4.5 #usually 8
PNGunits = "in"
PNGres = 100

multipanel_theme <- theme_minimal() + theme(plot.title = element_text(hjust = 0.5, size = 18),
                                            strip.text.x = element_text(size = 20),
                                            legend.title = element_blank (),
                                            axis.title.x = element_text (size = 16), # change the axis title
                                            axis.title.y = element_text (size = 16),
                                            axis.title.y.right = element_text (size = 16, color = pft.cols[2]),
                                            axis.text.x = element_text (size = 14, colour = "black"),
                                            axis.text.y = element_text (size = 14, colour = "black"),
                                            legend.position = "none")


#seedling emergence
emerg_func <- function(a = a_emerg[PFT], b = b_emerg[PFT], SMP.x = avg_SMP, avg_SMP.x = avg_SMP, seedbank.x){
  
  log10_frac_emerg <- log10(a) + b*log10(abs(avg_SMP.x)/abs(SMP.x)) 
  
  frac_emerg <- 10^log10_frac_emerg 
  if(frac_emerg > 0.07){frac_emerg <- 0.07}
  
  C_emerg <- frac_emerg * seedbank.x
  
  out <- list(frac_emerg, C_emerg)
  names(out) <- c("frac_emerg", "C_emerg")
  return(out)
}


emerg <- c()
SMP_windows <- seq(from = -50000, to = 0, length.out = 100)
pft <- c()
for(p in pft_names){
  for(i in 1:length(SMP_windows)){
    PFT <- p
    SMP_window <- SMP_windows[i]
    tmp <- emerg_func(SMP.x = SMP_window, seedbank.x = 1)$frac_emerg
    emerg <- append(emerg, tmp) 
    pft <- append(pft, p) 
  }
}


viz_emerg <- tibble(pft = pft,
                    SMP = rep(SMP_windows,4),
                    emerg = emerg) %>%
  ggplot(aes(x = SMP / 1e5, y = emerg, color = pft, linetype = pft)) +
  geom_line(size = 2) +
  scale_color_manual(values = pft.cols) +
  scale_linetype_manual(values=c("solid", "dashed","solid","dashed"))+
  ylab(label = "fraction of \n seedbank emerging") +
  xlab(label = "mean SMP (Mpa) \n in prior two weeks") +
  labs(title = "Seedling emergence") +
  theme_minimal() +
  multipanel_theme




#model this data with a simple quadratic function

em_dt <- tibble(pft = pft,
       SMP = rep(SMP_windows,4),
       emerg = emerg) %>%
  filter(emerg < 0.07) 
  
  
DT <- em_dt %>%
  rename(tr_data = emerg) %>%
  filter(pft == "LD_DT")
DTm2 <- DT %>%
  lm(formula = tr_data ~ SMP + I(SMP^2) + I(SMP^3))

DT$new <- predict(DTm2,newdata = DT)

DT <- DT %>%
  gather(c(tr_data,new),key = model,value = tr)
LDm3 <- DTm2


DI <- em_dt %>%
  rename(tr_data = emerg) %>%
  filter(pft == "ST_DI")
DIm2 <- DI %>%
  lm(formula = tr_data ~ SMP + I(SMP^2) + I(SMP^3))

DI$new <- predict(DIm2,newdata = DI)

DI <- DI %>%
  gather(c(tr_data,new),key = model,value = tr)
summary(DIm2)
STm3 <- DIm2


em_data2 <- rbind(DT,DI)


em_data2 %>%
  ggplot(aes(x = SMP / 1e5, y = tr, color = pft, linetype = model)) +
  geom_line(size = 2) +
  scale_color_manual(values = pft.cols) +
  scale_linetype_manual(values=c("solid", "dashed","solid","dashed"))+
  ylab(label = "fraction of \n seedbank emerging") +
  xlab(label = "mean SMP (Mpa) \n in prior two weeks") +
  labs(title = "Seedling emergence") +
  theme_minimal() +
  multipanel_theme

em_data2$pft %>% unique()




a_emerg <- c(rep(coef(LDm3)[2],2),rep(coef(STm3)[2],2))
names(a_emerg) <- pft_names
b_emerg <- c(rep(coef(LDm3)[3],2),rep(coef(STm3)[3],2))
names(b_emerg) <- pft_names
c_emerg <- c(rep(coef(LDm3)[4],2),rep(coef(STm3)[4],2))
names(c_emerg) <- pft_names
d_emerg <- c(rep(coef(LDm3)[1],2),rep(coef(STm3)[1],2))
names(d_emerg) <- pft_names



emerg_func2 <- function(a = a_emerg[PFT], b = b_emerg[PFT], c = c_emerg[PFT], d = d_emerg[PFT], SMP.x = avg_SMP, seedbank.x){
  
  
  frac_emerg <- d + a * SMP.x + b * SMP.x^2 + c * SMP.x^3
    
  if(frac_emerg > 0.07){frac_emerg <- 0.07}
  
  C_emerg <- frac_emerg * seedbank.x
  
  out <- list(frac_emerg, C_emerg)
  names(out) <- c("frac_emerg", "C_emerg")
  return(out)
}



emerg <- c()
SMP_windows <- seq(from = -5000, to = 0, length.out = 100)
pft <- c()
for(p in pft_names){
  for(i in 1:length(SMP_windows)){
    PFT <- p
    SMP_window <- SMP_windows[i]
    tmp <- emerg_func2(SMP.x = SMP_window, seedbank.x = 1)$frac_emerg
    emerg <- append(emerg, tmp) 
    pft <- append(pft, p) 
  }
}


viz_emerg <- tibble(pft = pft,
                    SMP = rep(SMP_windows,4),
                    emerg = emerg) %>%
  ggplot(aes(x = SMP / 1e5, y = emerg, color = pft, linetype = pft)) +
  geom_line(size = 2) +
  scale_color_manual(values = pft.cols) +
  scale_linetype_manual(values=c("solid", "dashed","solid","dashed"))+
  ylab(label = "fraction of \n seedbank emerging") +
  xlab(label = "mean SMP (Mpa) \n in prior two weeks") +
  labs(title = "Seedling emergence") +
  theme_minimal() +
  multipanel_theme

path_to_output <- "~/cloud/gdrive/rec_submodel/output/forMS/"

makePNG(fig = viz_emerg, path_to_output.x = path_to_output, file_name = "visualize_emergence")
