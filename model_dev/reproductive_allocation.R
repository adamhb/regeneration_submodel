source('utils/supporting_funcs.R')

#load data
RAobs <- read_csv(paste0(path_to_observational_data,"Dataset3_BCIreproduction.csv")) %>% #BCI Tree reproduction dataset
  mutate_at(.vars = "sp",.funs = tolower)
pfts <- read_csv("benchmarking/pft_assignments.csv")
g.forms <- read_csv(paste0(path_to_observational_data,"bci50splistwrepthresh.csv")) %>% #expert knowledge of growth form dataset
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


###############################################
###############################################
###############################################


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
  augment(d,type.predict = "response", se.fit = T)
}

#logistic regression
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
  select(pft,Latin,sp,grform,rep,dbh,.fitted,.se.fit,repdbh_mm,repmindbhmm,coefs) %>%
  rename(rep_fitted = .fitted, se = .se.fit)

#plotting the reproductive allocation curves
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


#model function
prob_repro <- function(size_mm,PFT.x){
  a_RA.x <- a_RA[PFT.x]
  b_RA.x <- b_RA[PFT.x]
  y <- (exp(b_RA.x+a_RA.x*size_mm) / (1 + exp(b_RA.x+a_RA.x*size_mm)))
  return(y)
}

efrac <- function(N, co_dbh_ind, PFT){
  N_repro <- prob_repro(size_mm = co_dbh_ind, PFT.x = PFT) * N
  fraction_reproductive <- N_repro / N
  e_frac <- fraction_reproductive * F_repro[PFT] #frac repro for now is just a fixed percent of NPP (10%), need better data to get better numbers for this
  return(e_frac)
}


sizes <- 1:1500 
pft <- c()

for(i in pft_names){
  pft <- append(pft,rep(i,length(sizes)))
}

efrac(N = 1000, co_dbh_ind = 500, PFT = "ST_DI")


#added as experiment
#b_RA <- rep(0,4)
#names(b_RA) <- pft_names

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
  ylab(expression(atop(paste("fraction of ",C[g+r]),"allocated to reproduction"))) +
  xlab("cohort dbh [mm]") +
  #annotate(geom = "text", x = 0, y = 0.1, label = "a", size = subplot_heading_size) +
  labs(title = "Allocation to reproduction") +
  theme_minimal() +
  multipanel_theme +
  #legend.key.size = unit(2,"line")) +
  theme(legend.position = c(0.75,0.25), legend.text = element_text(size = 20), legend.title = element_text(size = 20)) +
  guides(color = guide_legend(override.aes = list(size=8)))

makePNG(fig = F_repro_fig, path_to_output.x = paste0(path_to_output,"model_dev_figs/"), file_name = "reproductive_allocation")
print("made F_repro_fig")



#figure out what to do with the below code (i.e. supporting analyses below)

#AS DEMONSTRATION I DID THE SAME AS ABOVE BUT BROKE OUT BY GROWTH FORM
#logistic regression
#pooling by PFT and growth form
# RA3 <- RA %>%
#   left_join(ba_per_sp,by = "sp") %>%
#   group_by(pft,grform) %>%
#   nest() %>%
#   mutate(model = purrr::map(data, ~glm(rep ~ dbh, data = .,family = "binomial", weights = ba))) %>%
#   ungroup() %>%
#   mutate(augs = purrr::map(.x = model,.f = adams_augment)) %>%
#   mutate(coefs = purrr::map(.x = model,.f = coef)) %>%
#   unnest(cols = data,augs) %>%
#   unnest(cols = coefs) %>%
#   select(pft,Latin,sp,grform,rep,dbh,.fitted,.se.fit,repdbh_mm,repmindbhmm,coefs) %>%
#   rename(rep_fitted = .fitted, se = .se.fit)
# 
# 
# #plotting the reproductive allocation curves
# curves_by_grform <- RA3 %>%
#   ggplot(aes(dbh,rep_fitted,color = pft,linetype = pft)) +
#   geom_line(size = 1) +
#   facet_wrap(~grform,scales = "fixed",nrow = 3) +
#   scale_color_manual(values = pft.cols) +
#   scale_linetype_manual(values = c(rep("solid",3),"dashed")) +
#   ylab("probability reproductive") +
#   adams_theme
#   
# makePNG(fig = curves_by_grform,path_to_output.x = paste0(path_to_output,"model_dev_figs/"),file_name = "curves_by_grform_fixed_axes.png")
# 
# 
# #extra
# #figures showing mean size of reproductive and non-reproductive trees
# fig.allsp <- RA %>%
#   ggplot(aes(rep,dbh,fill = pft)) +
#   geom_boxplot() +
#   ylab("dbh (mm)") +
#   xlab("reproductive status") +
#   scale_fill_manual(values = pft.cols) +
#   adams_theme 
# 
# #faceted by growth form
# fig.facet.grform <- RA %>%
#   ggplot(aes(rep,dbh,fill = pft)) +
#   geom_boxplot() +
#   facet_grid(~grform) +
#   ylab("dbh (mm)") +
#   xlab("reproductive status") +
#   scale_fill_manual(values = pft.cols) +
#   adams_theme 
# makePNG(fig = fig.allsp,path_to_output.x = paste0(path_to_output,"model_dev_figs/"),file_name = "boxplot_allsp_mean_dbh_repro")
# makePNG(fig = fig.facet.grform,path_to_output.x = paste0(path_to_output,"model_dev_figs/"),file_name = "boxplot_mean_dbh_repro_by_grform")
# 


