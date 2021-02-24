source('utils/system_settings.R')
source('create_output/figure_formatting.R')
lg <- read_csv(paste0(path_to_observations,"light_based_germination_requirement.csv"))

#light in ED is given in terms of Wm2, so I need to convert ppfd to Wm2
ppfd2watts <- function(ppfd){ #1 W/m2 ≈ 4.6 μmole.m2/s (https://www.researchgate.net/post/Can-I-convert-PAR-photo-active-radiation-value-of-micro-mole-M2-S-to-Solar-radiation-in-Watt-m2#:~:text=Pyranometers%20measure%20TOTAL%20solar%20radiation,but%20a%20pretty%20good%20one.)
  wm2 <- ppfd / 4.6 #https://www.controlledenvironments.org/wp-content/uploads/sites/6/2017/06/Ch01.pdf
  return(wm2)
}

lg <- lg %>%
  mutate(mean_MJ_day = ppfd2watts(mean_ppdf)*3600*12/1e6)

#relationship between ratio of red to far-red light and ppfd
#this step is necessary because 
lg %>%
  ggplot(aes(R_FR,mean_MJ_day)) +
  geom_point() +
  xlab("Red:FarRed") +
  adams_theme

mod <- lm(data = lg, formula = mean_MJ_day ~ R_FR + I(R_FR^2))
lg$pred_mean_MJ_day <- predict(object = mod, newdata = lg)


#this study observed germination as a function of different red:far-red ratios, but they also showed that there was a strong relationship between ppdf and those ratios in the forest
#therefore, I have modeled germination as a function of ppfd
mod1 <- lm(data = lg, formula = germ ~ log(mean_MJ_day))
summary(mod1)

mod2 <- lm(data = lg, formula = germ ~ mean_MJ_day + I(mean_MJ_day^2))
summary(mod2)

pred_germ0 <- tibble(pred_germ = predict(object = mod2, newdata = tibble(mean_MJ_day = seq(0,17,length.out = 200))), mean_MJ_day = seq(0,17,length.out = 200))
pred_germ <- tibble(pred_germ = predict(object = mod1, newdata = tibble(mean_MJ_day = seq(0,17,length.out = 200))), mean_MJ_day = seq(0,17,length.out = 200))

#relationship between predicted ppfd (from red:far-red ratios) and germination observatinos
#this is a logarithmic relationship
lg %>%
  ggplot(aes(mean_MJ_day,germ)) +
  geom_point() +
  geom_line(data = pred_germ,mapping = aes(mean_MJ_day,pred_germ)) +
  #geom_line(data = pred_germ0,mapping = aes(mean_MJ_day,pred_germ), color = "red") +
  xlab("MJ per day at forest floor") +
  #xlab("ppfd [umol m2 s]") +
  adams_theme


b0_light_germ <- c(rep(as.numeric(coef(mod1)[1]),2),NA,NA)
b1_light_germ <- c(rep(as.numeric(coef(mod1)[2]),2),NA,NA)

print("b0_light_germ:")
print(b0_light_germ)

print("b1_light_germ:")
print(b1_light_germ)



#function that goes in the model
light_germ <- function(light.x){
  if(PFT %in% c("LD_DT","LD_DI")){
    germ_rate <- b1_light_germ[PFT]*log(light.x) + b0_light_germ[PFT]
    return(ifelse(test = germ_rate < 0, yes = 0, no = germ_rate))
  } else {
    return(1)
  }
}









