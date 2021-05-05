from_new_data <- FALSE
print("creating parameter sensitivity figure...")

#generate param sensitivity data
if(file.exists('temp/param_sens_data.csv') == F | from_new_data == T){
  print("need to run parameter sensitivity analysis.R")
  source("runs/ED2_run_param_sensitivity.R")
}else(print("making figure with prior run's data"))


param_sens_data <- read_csv("temp/param_sens_data.csv")
#param_sens_data <- read_csv("temp/param_sens_data_ENSO.csv")

base_avg_rec <- param_sens_data %>%
  filter(param_changed == "dummy") %>%
  pull(R_avg) %>% sum()

param_sens_data1 <- param_sens_data %>%
  group_by(param_changed) %>%
  summarise(pct_change = (abs(sum(R_avg) - base_avg_rec) / base_avg_rec * 100)) %>%
  filter(param_changed != "dummy")

#creating an ordered list of figure labels (one for each parameter)
param_name_code <- read_csv("temp/param_name_code.csv")
param_order <- param_sens_data1 %>%
  arrange(abs(pct_change)) %>% pull(param_changed)
param_orderdf <- tibble(param_order)
names(param_orderdf) <- "param_name"
fig_labels <- param_orderdf %>%
  left_join(param_name_code, by = "param_name") %>%
  pull(fig_label)


param_sens_fig <- param_sens_data1 %>%
  mutate(param_changed = factor(param_changed, levels = 
                                  param_order)) %>%
  ggplot(aes(x=param_changed, y=pct_change)) +
  geom_bar(stat="identity", size = 0.1) +
  coord_flip() +
  scale_x_discrete(labels = parse(text = fig_labels)) +
  theme_minimal() +
  adams_theme +
  xlab("parameter") +
  ylab("% change in recruitment rate")

makePNG(fig = param_sens_fig, path_to_output.x = path_to_MS_figures, file_name = "param_sens_BASE", height = 7)
print("FINISHED making parameter sensitivity figure")







#################parameter sensitivity under DRY-DS###########


# param_sens_data <- read_csv("temp/param_sens_data_DRY_DS.csv")
# #param_sens_data <- read_csv("temp/param_sens_data_ENSO.csv")
# 
# source("create_output/figure_formatting.R")
# 
# base_avg_rec <- param_sens_data %>%
#   filter(param_changed == "dummy") %>%
#   pull(R_avg) %>% sum()
# 
# param_sens_data1 <- param_sens_data %>%
#   group_by(param_changed) %>%
#   summarise(pct_change = (abs(sum(R_avg) - base_avg_rec) / base_avg_rec * 100)) %>%
#   filter(param_changed != "dummy")
# 
# param_order <- param_sens_data1 %>%
#   arrange(abs(pct_change)) %>% pull(param_changed)
# 
# param_sens_fig <- param_sens_data1 %>%
#   mutate(param_changed = factor(param_changed, levels = 
#                                   param_order)) %>%
#   ggplot(aes(x=param_changed, y=pct_change)) +
#   geom_bar(stat="identity", size = 0.1) +
#   coord_flip() +
#   scale_x_discrete(labels = parse(text=c(
#     "a.MH20" = "a[MH20]"      ,
#     "b.MH20" = "a[MH20]"      ,
#     "c.MH20" = "a[MH20]"      ,
#     "MDDs_crit" = "MDDs[crit]",
#     "W_psi" = expression(W[psi]),
#     "psi_crit" = expression(psi[crit]) ,
#     "b_emerg" = "b[emerg]"     ,
#     "k" = "k",
#     "a.ML" = "a[ML]"       ,
#     "W_ML" = "W[ML]"     ,
#     "M_background" = "M_[background]",
#     "a_emerg" = "a[emerg]"     ,
#     "S_decay" = "S[mort]"    ,
#     "W_TR"  = "W[TR]"      ,
#     "a_TR"  = "a[TR]"      ,
#     "Dmax"  = "Dmax"      ,
#     "F_repro" = "F[repro]"    ,
#     "F_seed"  = "F[seed]"    ,
#     "b.ML"  = "b[ML]"        ,
#     "b_TR" = "b[TR]"))) +
#   theme_minimal() +
#   adams_theme +
#   xlab("parameter") +
#   ylab("% change in recruitment rate")
# 
# makePNG(fig = param_sens_fig, path_to_output.x = path_to_MS_figures, file_name = "param_sens_DRY_DS", height = 7)
# print("FINISHED making parameter sensitivity figure")





# scale_x_discrete(labels = parse(text=c(
#   "a.MH20" = expression(a[MH20])      ,
#   "b.MH20" = "a[MH20]"      ,
#   "c.MH20" = "a[MH20]"      ,
#   "MDDs_crit" = "MDDs[crit]",
#   "W_psi" = "W[psi]",
#   "psi_crit" = expression(psi[crit]) ,
#   "b_emerg" = "b[emerg]"     ,
#   "a.ML" = "a[ML]"       ,
#   "W_ML" = "W[ML]"     ,
#   "M_background" = "M_[background]",
#   "a_emerg" = "a[emerg]"     ,
#   "S_decay" = "S[mort]"    ,
#   "W_TR"  = "W[TR]"      ,
#   "a_TR"  = "a[TR]"      ,
#   "Dmax"  = "Dmax"      ,
#   "F_repro" = "F[repro]"    ,
#   "F_seed"  = "F[seed]"    ,
#   "b.ML"  = "b[ML]"        ,
#   "W_emerg" = "W[emerg]"   ,
#   "emerg_thresh" = expression(psi[emerg]) ,
#   "l_crit" = "PAR[crit]" ,
#   "b_TR" = "b[TR]"))) +








#############################################################################3



# 
