library(tidyverse)
source("parameter_files/parameters_ED2_run_Aug_4.R")
source("create_output/figure_formatting.R")



a_rec <- rep(0.0004,4)
names(a_rec) <- pft_names
avg_l <- 105
 
rec_func <- function(a_rec.x = a_rec[PFT], b_rec.x = b_rec[PFT], l, avg_l.x = avg_l, seedpool.x, SMP.x = avg_SMP){
  
  
  log10_frac_rec <- log10(a_rec.x) + b_rec.x*log10(l/avg_l) 
  
  frac_rec <- (10^log10_frac_rec) #the fraction of the seedling pool recruiting per day
  
  if(SMP.x < thresh.xx[PFT]){
    frac_rec <- 0
  }
  
  C_rec <- frac_rec * seedpool.x
  
  N_rec <- C_rec / Z0
  
  out <- list(frac_rec,C_rec, N_rec)
  
  names(out) <- c("frac_rec", "C_rec", "N_rec")
  return(out)
}



rec_func(l = 20, seedpool.x = 1)

#transition rate
light_regimes <- 20:1000
light_rec_data <- tibble()

#for(i in pft_names[c(1,3)]){
for(i in pft_names){
  PFT <- i
  temp <- tibble(
    annual_transition_rate =
      rec_func(l = light_regimes,
               a_rec.x = a_rec[PFT], 
               b_rec.x = b_rec[PFT], 
               seedpool.x = 1, 
               SMP.x = avg_SMP)$frac_rec,
    pft = rep(i,length(light_regimes)),
    light = light_regimes
  )
  
  light_rec_data <- rbind(light_rec_data,temp)  
}





l_rec_LD <- light_rec_data %>% filter(pft == "LD_DI")
LDmod <- lm(formula = log10(annual_transition_rate) ~ log10(light), data = l_rec_LD)
l_rec_ST <- light_rec_data %>% filter(pft == "ST_DI")
STmod <- lm(formula = log10(annual_transition_rate) ~ log10(light), data = l_rec_ST)

a_rec <- c(rep(coef(LDmod)[1],2),rep(coef(STmod)[1],2))

#a_rec < - c(-5.016452,-5.016452,-4.604534,-4.604534) 
names(a_rec) <- pft_names



# frac_rec3 <- function(a_rec.x = a_rec[PFT], b_rec.x = b_rec[PFT], l){
#   log10tr <- a_rec.x + b_rec.x*log10(l)
#   tr = 10^log10tr
#   return(tr)
# }




light_rec_fig <- light_rec_data %>%
  #mutate(pft = case_when(
  #  pft == "earlydi" ~ "early",
  #  pft == "latedi" ~ "late"
  #)) %>%
  ggplot(aes(x = light, y = annual_transition_rate * 30.4, color = pft, linetype = pft)) +
  geom_line(size = 2) +
  scale_color_manual(values = pft.cols) +
  scale_linetype_manual(values = c("solid","dashed","solid","dashed")) +
  ylab(label = "monthly seedling to \n sapling transition prob.") +
  scale_x_continuous(limits = c(0,1000)) +
  #xlab(label = "cumulative solar radiation at seedling layer \n (MJ accumulated in prior 6 months)") +
  xlab(expression(atop("cum. solar rad. at seedling layer", paste("(MJ m"^"-2"," in prior 6 months)")))) +
  labs(title = "Seedling to sapling transition") +
  #geom_vline(xintercept = 92, linetype = "dashed") +
  #annotate("text", x = 170, y = 0.15, label = paste0("avg_l = ", avg_l), size = 25) +
  #annotate("text", x = 250, y = 0.1, label = "mean light \n at BCI seedling layer", size = 5) +
  #geom_vline(xintercept = 920, linetype = "dashed") +
  #annotate("text", x = 800, y = 0.032, label = "mean light \n in small gap \n (20% TOC)", size = 5) +
  #geom_vline(xintercept = 61) +
  #annotate("text", x = 61, y = 0.06, label = "original avg_l value") +
  theme_minimal() +
  multipanel_theme 



LD <- light_rec_data %>%
  rename(tr_data = annual_transition_rate) %>%
  filter(pft == "LD_DI")
LDm2 <- LD %>%
  lm(formula = tr_data ~ light + I(light^2))

LD$new <- predict(LDm2,newdata = LD)
LD <- LD %>%
  gather(c(tr_data,new),key = model,value = tr)
summary(LDm2)

ST <- light_rec_data %>%
  rename(tr_data = annual_transition_rate) %>%
  filter(pft == "ST_DI")
STm2 <- ST %>%
  lm(formula = tr_data ~ light + I(light^2))
ST$new <- predict(STm2,newdata = ST)
ST <- ST %>%
  gather(c(tr_data,new),key = model,value = tr)
summary(STm2)


light_rec_data2 <- rbind(ST,LD)


light_rec_data2 %>%
  filter(model == "new")%>%
  ggplot(aes(x = light, y = tr, color = pft, linetype = model)) +
  geom_line(size = 2) +
  scale_color_manual(values = pft.cols) +
  scale_linetype_manual(values = c("dashed","solid")) +
  ylab(label = "monthly seedling to \n sapling transition prob.") +
  scale_x_continuous(limits = c(0,150)) +
  scale_y_continuous(limits = c(0,0.0005)) +
  #xlab(label = "cumulative solar radiation at seedling layer \n (MJ accumulated in prior 6 months)") +
  xlab(expression(atop("cum. solar rad. at seedling layer", paste("(MJ m"^"-2"," in prior 6 months)")))) +
  labs(title = "Seedling to sapling transition") +
  geom_vline(xintercept = 92, linetype = "dashed") +
  #annotate("text", x = 170, y = 0.15, label = paste0("avg_l = ", avg_l), size = 25) +
  annotate("text", x = 250, y = 0.1, label = "mean light \n at BCI seedling layer", size = 5) +
  geom_vline(xintercept = 920, linetype = "dashed") +
  annotate("text", x = 800, y = 0.032, label = "mean light \n in small gap \n (20% TOC)", size = 5) +
  #geom_vline(xintercept = 61) +
  #annotate("text", x = 61, y = 0.06, label = "original avg_l value") +
  theme_minimal() +
  multipanel_theme 



a_rec <- c(rep(coef(LDm2)[2],2),rep(coef(STm2)[2],2))
names(a_rec) <- pft_names
b_rec <- c(rep(coef(LDm2)[3],2),rep(coef(STm2)[3],2))
names(b_rec) <- pft_names
c_rec <- c(rep(coef(LDm2)[1],2),rep(coef(STm2)[1],2))
names(c_rec) <- pft_names


rec_func2 <- function(a_rec.x = a_rec[PFT],
                     b_rec.x = b_rec[PFT],
                     l,
                     seedpool.x,
                     SMP.x = avg_SMP,
                     c_rec.x = c_rec[PFT]){
  
  #frac_rec <- predict(object = STm2, newdata = tibble(light = l))
  
  frac_rec <- a_rec.x * l + b_rec.x * l^2 + c_rec.x
  
  if(SMP.x < thresh.xx[PFT]){
    frac_rec <- 0
  }
  
  C_rec <- frac_rec * seedpool.x
  
  N_rec <- C_rec / Z0
  
  out <- list(frac_rec,C_rec, N_rec)
  
  names(out) <- c("frac_rec", "C_rec", "N_rec")
  
  return(out) 
}





#transition rate
light_regimes <- 20:1000
light_rec_data3 <- tibble()

#for(i in pft_names[c(1,3)]){
for(i in pft_names){
  PFT <- i
  temp <- tibble(
    annual_transition_rate =
      rec_func2(l = light_regimes,
               a_rec.x = a_rec[PFT], 
               b_rec.x = b_rec[PFT], 
               seedpool.x = 1, 
               SMP.x = avg_SMP)$frac_rec,
    pft = rep(i,length(light_regimes)),
    light = light_regimes
  )
  
  light_rec_data3 <- rbind(light_rec_data,temp)  
}

light_rec_fig <- light_rec_data3 %>%
  #mutate(pft = case_when(
  #  pft == "earlydi" ~ "early",
  #  pft == "latedi" ~ "late"
  #)) %>%
  ggplot(aes(x = light, y = annual_transition_rate * 30.4, color = pft, linetype = pft)) +
  geom_line(size = 2) +
  scale_color_manual(values = pft.cols) +
  scale_linetype_manual(values = c("solid","dashed","solid","dashed")) +
  ylab(label = "monthly seedling to \n sapling transition prob.") +
  scale_x_continuous(limits = c(0,1000)) +
  #xlab(label = "cumulative solar radiation at seedling layer \n (MJ accumulated in prior 6 months)") +
  xlab(expression(atop("cum. solar rad. at seedling layer", paste("(MJ m"^"-2"," in prior 6 months)")))) +
  labs(title = "Seedling to sapling transition") +
  geom_vline(xintercept = 92, linetype = "dashed") +
  annotate("text", x = 270, y = 0.1, label = "mean light at \n BCI seedling layer", size = 5) +
  geom_vline(xintercept = 920, linetype = "dashed") +
  annotate("text", x = 800, y = 0.032, label = "mean light \n in small gap \n (20% TOC)", size = 5) +
  theme_minimal() +
  multipanel_theme 



rec_func2(l = 20,
          a_rec.x = a_rec[PFT], 
          b_rec.x = b_rec[PFT], 
          seedpool.x = 1, 
          SMP.x = avg_SMP)$frac_rec










































rec_func3 <- function(a_rec.x = a_rec[PFT], b_rec.x = b_rec[PFT], l, SMP.x, seedpool.x){
  log10tr <- a_rec.x + b_rec.x*log10(l)
  frac_rec <- 10^log10tr
  
  if(SMP.x < thresh.xx[PFT]){
    frac_rec <- 0
  }
  
  C_rec <- frac_rec * seedpool.x
  
  N_rec <- C_rec / Z0
  
  out <- list(frac_rec,C_rec, N_rec)
  
  names(out) <- c("frac_rec", "C_rec", "N_rec")
  
  return(out) 

}

#a_rec < - c(-5.016452,-5.016452,-4.604534,-4.604534) 
names(a_rec) <- pft_names

b_rec


#transition rate
light_regimes <- 20:1000
light_rec_data3 <- tibble()

#for(i in pft_names[c(1,3)]){
for(i in pft_names){
  PFT <- i
  temp <- tibble(
    annual_transition_rate =
      rec_func3(l = light_regimes,
                a_rec.x = a_rec[PFT], 
                b_rec.x = b_rec[PFT], 
                seedpool.x = 1, 
                SMP.x = avg_SMP)$frac_rec,
    pft = rep(i,length(light_regimes)),
    light = light_regimes
  )
  
  light_rec_data3 <- rbind(light_rec_data,temp)  
}

light_rec_fig <- light_rec_data3 %>%
  #mutate(pft = case_when(
  #  pft == "earlydi" ~ "early",
  #  pft == "latedi" ~ "late"
  #)) %>%
  ggplot(aes(x = light, y = annual_transition_rate * 30.4, color = pft, linetype = pft)) +
  geom_line(size = 2) +
  scale_color_manual(values = pft.cols) +
  scale_linetype_manual(values = c("solid","dashed","solid","dashed")) +
  ylab(label = "monthly seedling to \n sapling transition prob.") +
  scale_x_continuous(limits = c(0,1000)) +
  #xlab(label = "cumulative solar radiation at seedling layer \n (MJ accumulated in prior 6 months)") +
  xlab(expression(atop("cum. solar rad. at seedling layer", paste("(MJ m"^"-2"," in prior 6 months)")))) +
  labs(title = "Seedling to sapling transition") +
  geom_vline(xintercept = 92, linetype = "dashed") +
  annotate("text", x = 270, y = 0.1, label = "mean light at \n BCI seedling layer", size = 5) +
  geom_vline(xintercept = 920, linetype = "dashed") +
  annotate("text", x = 800, y = 0.032, label = "mean light \n in small gap \n (20% TOC)", size = 5) +
  theme_minimal() +
  multipanel_theme 
