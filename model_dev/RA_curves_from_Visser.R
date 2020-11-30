
#run scripts to set up environment and create pft assignments
source('utils/system_settings.R')
source('create_output/figure_formatting.R')
source("benchmarking/assigning_pfts.R")

#load data
RAobs <- read_csv(paste0(path_to_observational_data,"Dataset3_BCIreproduction.csv")) %>%
  mutate_at(.vars = "sp",.funs = tolower)
pfts <- read_csv("benchmarking/pft_assignments.csv")
g.forms <- read_csv(paste0(path_to_observational_data,"bci50splistwrepthresh.csv")) %>%
  rename(sp = sp6)


#cleaning RA data
RA <- pfts %>%
  left_join(RAobs,by = "sp") %>%
  left_join(g.forms) %>%
  dplyr::select(Latin,sp,pft,grform,rep,dbh,repdbh_mm,repmindbhmm) %>%
  mutate_at(.vars = "rep", as.logical) %>%
  mutate_at(.vars = c("pft","grform"), as.factor) %>%
  drop_na(rep)

#figures showing mean size of reproductive and non-reproductive trees
fig.allsp <- RA %>%
  ggplot(aes(rep,dbh,fill = pft)) +
  geom_boxplot() +
  ylab("dbh (mm)") +
  xlab("reproductive status") +
  scale_fill_manual(values = pft.cols) +
  adams_theme 

#faceted by growth form
fig.facet.grform <- RA %>%
  ggplot(aes(rep,dbh,fill = pft)) +
  geom_boxplot() +
  facet_grid(~grform) +
  ylab("dbh (mm)") +
  xlab("reproductive status") +
  scale_fill_manual(values = pft.cols) +
  adams_theme 

makePNG(fig = fig.allsp,path_to_output.x = paste0(path_to_output,"model_dev_figs/"),file_name = "boxplot_allsp_mean_dbh_repro")
makePNG(fig = fig.facet.grform,path_to_output.x = paste0(path_to_output,"model_dev_figs/"),file_name = "boxplot_mean_dbh_repro_by_grform")

#defining a function to add the predictions to data frame
adams_augment <- function(d){
  augment(d,type.predict = "response")
}

#logistic regression
#pooling by PFT
RA2 <- RA %>%
  group_by(pft) %>%
  nest() %>%
  mutate(model = purrr::map(data, ~glm(rep ~ dbh, data = .,family = "binomial"))) %>%
  ungroup() %>%
  mutate(augs = map(.x = model,.f = adams_augment)) %>%
  mutate(coefs = map(.x = model,.f = coef)) %>%
  unnest(cols = data,augs) %>%
  unnest(cols = coefs) %>%
  select(pft,Latin,sp,grform,rep,dbh,.fitted,.se.fit,repdbh_mm,repmindbhmm,coefs) %>%
  rename(rep_fitted = .fitted, se = .se.fit)

#plotting the reproductive allocation curves
curves_allsp <- RA2 %>%
  ggplot(aes(dbh,rep_fitted,color = pft)) +
  geom_line(size = 2) +
  scale_color_manual(values = pft.cols) +
  adams_theme
makePNG(fig = curves_allsp,path_to_output.x = paste0(path_to_output,"model_dev_figs/"),file_name = "curves_allsp.png")

#NEED TO ADD COEFS TO TABLE BELOW
#logistic regression
#pooling by PFT and growth form
RA3 <- RA %>%
  group_by(pft,grform) %>%
  nest() %>%
  mutate(model = map(data, ~glm(rep ~ dbh, data = .,family = "binomial"))) %>%
  ungroup() %>%
  mutate(augs = map(.x = model,.f = adams_augment)) %>%
  unnest(cols = data,augs) %>%
  select(pft,Latin,sp,grform,rep,dbh,.fitted,.se.fit,repdbh_mm,repmindbhmm) %>%
  rename(rep_fitted = .fitted, se = .se.fit)


#plotting the reproductive allocation curves
curves_by_grform <- RA3 %>%
  ggplot(aes(dbh,rep_fitted,color = pft)) +
  geom_line(size = 2) +
  facet_grid(~grform,scales = "fixed") +
  scale_color_manual(values = pft.cols) +
  adams_theme
makePNG(fig = curves_by_grform,path_to_output.x = paste0(path_to_output,"model_dev_figs/"),file_name = "curves_by_grform_fixed_axes.png")



