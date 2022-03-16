#This script calculates the parameters of the reproductive allocation function
#These parameters are used Eqn 1 (main text) which relate dbh to the probability of 
#being reproductive, which is subsequently used to calculate the effective fraction of
#carbon for growth and reproduction that is allocated to reproduction at the cohort-level.

#load supporting functions
source('utils/supporting_funcs.R')
source('runs/generate_input_data.R')
source('model/process_funcs.R')

#sizes <- 1:1500

#load the BCI Tree reproduction dataset
#available here: 
#The Barro Colorado Island Tree Reproduction Dataset is openly available through the 
#Smithsonian Tropical Research Institute at https://doi.org/10.5479/si.data.201511251100 

RAobs <- read_csv(paste0(path_to_observational_data,"Dataset3_BCIreproduction.csv")) %>% 
  mutate_at(.vars = "sp",.funs = tolower)

#load the species assignments to each PFT
pfts <- read_csv("benchmarking/pft_assignments.csv")

#load the expert identification of species growth forms
#This is used to remove understory specialists.
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

ba_per_sp <- read_csv(file = "temp/ba_per_sp_2015.R") #basal area calculated from Knox's benchmarking driver

total_ba <- sum(ba_per_sp$ba)

#calculating fraction of total ba for each species
ba_per_sp <- ba_per_sp %>%
  mutate(ba_frac = ba / total_ba)


###############################################
##############Analysis of Basal Area###########
###############################################
#This analysis of basal area is a preamble to the derivation of
#reproductive allocation parameters. It is used to check how much
#the species in the tree reproduction data set account for total basal
#at BCI, to know how representative these parameter would be for BCI.

#This is also used to generate a basal-area weighted mean
#contribution of each species to the repoductive allocation curves

#fraction of total ba covered by Visser obs is 63%
tmp <- unique(RAobs$sp) %>%
  tibble()
names(tmp) <- "sp"
frac_Visser_total <- tmp %>%
  left_join(ba_per_sp,by = "sp") %>% pull(ba_frac) %>% sum()
print(paste("fraction of total ba covered by Visser obs is",frac_Visser_total))


#observed ba in FDP within each growth form / pft class
ba.per.pft.grf.all.bci <- pfts %>%
  left_join(g.forms) %>%
  left_join(ba_per_sp) %>%
  drop_na() %>%
  group_by(pft,grform) %>%
  summarise(ba.gr.pft.all.bci = sum(ba))


#total ba per growth form / pft in the FDP, just including species in the Visser data
total_ba_gr_pft <- tmp %>%
  left_join(ba_per_sp,by = "sp") %>%
  left_join(g.forms, by = "sp") %>%
  left_join(pfts, by = "sp") %>%
  drop_na(pft) %>%
  group_by(pft,grform) %>%
  summarise(ba.gr.pft = sum(ba)) %>%
  left_join(ba.per.pft.grf.all.bci, by = c("pft","grform")) %>%
  mutate(frac_total_at_bci = ba.gr.pft/ba.gr.pft.all.bci)


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
#makePNG(fig = fig_total_ba_bci,path_to_output.x = paste0(path_to_output,"model_dev_figs/"),file_name = "fig_total_ba_bci.png")

#graph showing the amount of basal area covered by the species sampled in the Visser data
ba_coverage <- total_ba_gr_pft %>% 
  filter(grform == "T") %>%
  ggplot(aes(pft,ba.gr.pft)) +
  geom_bar(stat="identity") +
  geom_point(data = total_ba_per_pft_at_bci,
             mapping = aes(pft,ba.gr.pft)) +
  ylab("basal area") +
  adams_theme

makePNG(fig = ba_coverage,path_to_output.x = paste0(path_to_output,"model_dev_figs/"),file_name = "ba_coverage.png")


###################################################
##############END Analysis of Basal Area###########
###################################################

#Joining data and cleaning RA observation data
RA <- pfts %>%
  left_join(RAobs,by = "sp") %>%
  left_join(g.forms) %>%
  dplyr::select(Latin,sp,pft,grform,rep,dbh,repdbh_mm,repmindbhmm) %>%
  mutate_at(.vars = "rep", as.logical) %>%
  mutate_at(.vars = c("pft","grform"), as.factor) %>%
  drop_na(rep) 

#Defining a function to add the model predictions to data frame
adams_augment <- function(d){
  augment(d,type.predict = "response", se.fit = T)
}

#Logistic regression to find reproductive allocation parameters
#pooling by PFT
RA2 <- RA %>%
  left_join(ba_per_sp,by = "sp") %>%
  filter(grform == "T") %>% #CANOPY TREES ONLY
  group_by(pft) %>%
  nest() %>%
  mutate(model = purrr::map(data, ~glm(rep ~ dbh, data = .,family = "binomial",weights = ba))) %>%
  ungroup() %>%
  mutate(augs = purrr::map(.x = model,.f = adams_augment)) %>%
  mutate(coefs = purrr::map(.x = model,.f = coef)) %>%
  tidyr::unnest(cols = data,augs) %>%
  tidyr::unnest(cols = coefs) %>% 
  dplyr::select(pft,Latin,sp,grform,rep,dbh,.fitted,.sigma,repdbh_mm,repmindbhmm,coefs) %>%
  rename(rep_fitted = .fitted, se = .sigma) #was se.fit

#Plotting the reproductive allocation curves
curves_allsp <- RA2 %>%
  ggplot(aes(dbh,rep_fitted,color = pft)) +
  geom_line(size = 1) +
  scale_color_manual(values = pft.cols) +
  ylab("probability reproductive") +
  xlab("dbh [mm]") +
  adams_theme
makePNG(fig = curves_allsp,path_to_output.x = paste0(path_to_output,"model_dev_figs/"),file_name = "curves_allsp.png")

#getting parameter values per pft
pft_names_df <- tibble(pft = c("LD_DI", "LD_DT", "ST_DI", "ST_DT"))

intercept <- pft_names_df %>%
  left_join(RA2,by = "pft") %>%
  distinct(pft,coefs) %>% filter(coefs < 0) %>% pull(coefs)

a_RA <- pft_names_df %>%
  left_join(RA2,by = "pft") %>%
  distinct(pft,coefs) %>% filter(coefs > 0) %>% pull(coefs)

b_RA <- pft_names_df %>%
  left_join(RA2,by = "pft") %>%
  distinct(pft,coefs) %>% filter(coefs < 0) %>% pull(coefs)

names(a_RA) <- pft_names
names(b_RA) <- pft_names

print('a_RA:')
print(round(a_RA,4))

print('b_RA:')
print(round(b_RA,4))




###################################################
#visualize the reproductive allocation function####
###################################################
#generate a range of dbh sizes
sizes <- 1:1500 #dbh in mm
pft <- c()

for(i in pft_names){
  pft <- append(pft,rep(i,length(sizes)))
}


F_repro_fig <- tibble(F_alloc = c(efrac(N = 1000, co_dbh_ind = sizes, PFT = pft_names[1]),
                                  efrac(N = 1000, co_dbh_ind = sizes, PFT = pft_names[2]),
                                  efrac(N = 1000, co_dbh_ind = sizes, PFT = pft_names[3]),
                                  efrac(N = 1000, co_dbh_ind = sizes, PFT = pft_names[4])),
                      PFT = pft,
                      dbh = rep(sizes,4)) %>%
  ggplot(aes(x = dbh, y = F_alloc, color = PFT, linetype = PFT)) +
  geom_line(size = 2) +
  scale_color_manual(values = pft.cols) +
  scale_linetype_manual(values=c("solid", "solid","solid","solid")) +
  ylab(expression(atop(paste("Fraction of ",C[g+r]),"allocated to reproduction"))) +
  xlab("Cohort dbh [mm]") +
  #annotate(geom = "text", x = 0, y = 0.1, label = "a", size = subplot_heading_size) +
  labs(title = "Size & \n reproductive allocation") +
  theme_minimal() +
  multipanel_theme +
  #legend.key.size = unit(2,"line")) +
  theme(legend.position = c(0.75,0.25), legend.text = element_text(size = 20), legend.title = element_text(size = 20)) +
  guides(color = guide_legend(override.aes = list(size=8)))

makePNG(fig = F_repro_fig, path_to_output.x = paste0(path_to_output,"model_dev_figs/"), file_name = "reproductive_allocation", res = 600)
print("made F_repro_fig")


