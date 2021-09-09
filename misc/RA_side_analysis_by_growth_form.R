

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

