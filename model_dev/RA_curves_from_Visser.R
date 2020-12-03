
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

#number of species in the RAobs dataset
N_sp_RAobs <- length(unique(RAobs$sp))

#number of species comprising submodel's PFTs
length(unique(pfts$sp))

#basal area per species in most recent census (2015)
if(file.exists("temp/ba_per_sp_2015.R") == F){
  source('model_dev/Dmax_BCI.R')
}
ba_per_sp <- read_csv(file = "temp/ba_per_sp_2015.R")
total_ba <- sum(ba_per_sp$ba)

#calculating fraction of total ba for each species
ba_per_sp <- ba_per_sp %>%
  mutate(ba_frac = ba / total_ba)

#fraction of total ba covered by Visser obs is 63%
tmp <- unique(RAobs$sp) %>%
  tibble()
names(tmp) <- "sp"
frac_Visser_total <- tmp %>%
  left_join(ba_per_sp,by = "sp") %>% pull(ba_frac) %>% sum()
print(paste("fraction of total ba covered by Visser obs is",frac_Visser_total))


#fraction of total ba within each growth form / pft class
ba.per.pft.grf.all.bci <- pfts %>%
  left_join(g.forms) %>%
  left_join(ba_per_sp) %>%
  drop_na() %>%
  group_by(pft,grform) %>%
  summarise(ba.gr.pft.all.bci = sum(ba))


#total ba per growth form / pft in the Visser data
total_ba_gr_pft <- tmp %>%
  left_join(ba_per_sp,by = "sp") %>%
  left_join(g.forms, by = "sp") %>%
  left_join(pfts, by = "sp") %>%
  drop_na(pft) %>%
  group_by(pft,grform) %>%
  summarise(ba.gr.pft = sum(ba)) %>%
  left_join(ba.per.pft.grf.all.bci, by = c("pft","grform")) %>%
  mutate(frac_total_at_bci = ba.gr.pft/ba.gr.pft.all.bci)

#total ba per pft for the 196 species we cover in our PFTs at BCI 
total_ba_per_pft_at_bci <- pfts %>% 
  left_join(ba_per_sp,by = "sp") %>%
  left_join(g.forms, by = "sp") %>%
  drop_na(pft) %>%
  group_by(pft) %>%
  summarise(ba.gr.pft = sum(ba,na.rm = T)) %>%
  add_column(grform = "T") #this is just here to make the next figure work


#fig of total ba per growth form / pft for the 196 species we cover in our PFTs at BCI 
total_ba_per_pft_grform_at_bci <- pfts %>% 
  left_join(ba_per_sp,by = "sp") %>%
  left_join(g.forms, by = "sp") %>%
  drop_na(pft) %>%
  group_by(pft,grform) %>%
  summarise(ba.gr.pft = sum(ba,na.rm = T)) %>%
  drop_na()

fig_total_ba_bci <- total_ba_per_pft_grform_at_bci %>%
  ggplot(aes(pft,ba.gr.pft,fill=grform)) +
  geom_bar(stat="identity") +
  ylab("basal area") +
  adams_theme

makePNG(fig = fig_total_ba_bci,path_to_output.x = paste0(path_to_output,"model_dev_figs/"),file_name = "fig_total_ba_bci.png")

#graph showing the amount of basal area covered by the species sampled in the Visser data
ba_coverage <- total_ba_gr_pft %>%
  ggplot(aes(pft,ba.gr.pft,fill=grform)) +
  geom_bar(stat="identity") +
  geom_point(data = total_ba_per_pft_at_bci, mapping = aes(pft,ba.gr.pft)) +
  ylab("basal area") +
  adams_theme
makePNG(fig = ba_coverage,path_to_output.x = paste0(path_to_output,"model_dev_figs/"),file_name = "ba_coverage.png")



#cleaning RA obs data
RA <- pfts %>%
  left_join(RAobs,by = "sp") %>%
  left_join(g.forms) %>%
  dplyr::select(Latin,sp,pft,grform,rep,dbh,repdbh_mm,repmindbhmm) %>%
  mutate_at(.vars = "rep", as.logical) %>%
  mutate_at(.vars = c("pft","grform"), as.factor) %>%
  drop_na(rep) 


#defining a function to add the predictions to data frame
adams_augment <- function(d){
  augment(d,type.predict = "response")
}

#logistic regression
#pooling by PFT
RA2 <- RA %>%
  left_join(ba_per_sp,by = "sp") %>%
  group_by(pft) %>%
  nest() %>%
  mutate(model = purrr::map(data, ~glm(rep ~ dbh, data = .,family = "binomial",weights = ba))) %>%
  ungroup() %>%
  mutate(augs = purrr::map(.x = model,.f = adams_augment)) %>%
  mutate(coefs = purrr::map(.x = model,.f = coef)) %>%
  unnest(cols = data,augs) %>%
  unnest(cols = coefs) %>%
  select(pft,Latin,sp,grform,rep,dbh,.fitted,.se.fit,repdbh_mm,repmindbhmm,coefs) %>%
  rename(rep_fitted = .fitted, se = .se.fit)

#plotting the reproductive allocation curves
curves_allsp <- RA2 %>%
  ggplot(aes(dbh,rep_fitted,color = pft)) +
  geom_line(size = 1) +
  scale_color_manual(values = pft.cols) +
  ylab("probability reproductive") +
  xlab("dbh (mm)") +
  adams_theme
makePNG(fig = curves_allsp,path_to_output.x = paste0(path_to_output,"model_dev_figs/"),file_name = "curves_allsp.png")

#getting parameter values per pft
pft_names_df <- tibble(pft = c("LD_DI", "LD_DT", "ST_DI", "ST_DT"))

intercept <- pft_names_df %>%
  left_join(RA2,by = "pft") %>%
  distinct(pft,coefs) %>% filter(coefs < 0) %>% pull(coefs)

b1 <- pft_names_df %>%
  left_join(RA2,by = "pft") %>%
  distinct(pft,coefs) %>% filter(coefs > 0) %>% pull(coefs)

print(paste('param vals are',b1))



#AS DEMONSTRATION I DID THE SAME AS ABOVE BUT BROKE OUT BY GROWTH FORM
#logistic regression
#pooling by PFT and growth form
RA3 <- RA %>%
  left_join(ba_per_sp,by = "sp") %>%
  group_by(pft,grform) %>%
  nest() %>%
  mutate(model = purrr::map(data, ~glm(rep ~ dbh, data = .,family = "binomial", weights = ba))) %>%
  ungroup() %>%
  mutate(augs = purrr::map(.x = model,.f = adams_augment)) %>%
  mutate(coefs = purrr::map(.x = model,.f = coef)) %>%
  unnest(cols = data,augs) %>%
  unnest(cols = coefs) %>%
  select(pft,Latin,sp,grform,rep,dbh,.fitted,.se.fit,repdbh_mm,repmindbhmm,coefs) %>%
  rename(rep_fitted = .fitted, se = .se.fit)


#plotting the reproductive allocation curves
curves_by_grform <- RA3 %>%
  ggplot(aes(dbh,rep_fitted,color = pft,linetype = pft)) +
  geom_line(size = 1) +
  facet_wrap(~grform,scales = "fixed",nrow = 3) +
  scale_color_manual(values = pft.cols) +
  scale_linetype_manual(values = c(rep("solid",3),"dashed")) +
  ylab("probability reproductive") +
  adams_theme
  
makePNG(fig = curves_by_grform,path_to_output.x = paste0(path_to_output,"model_dev_figs/"),file_name = "curves_by_grform_fixed_axes.png")
































#extra
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



