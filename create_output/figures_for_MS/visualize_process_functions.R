#source scripts that create figures displaying the functions behavior
source("model_dev/reproductive_allocation.R")
source("model_dev/seedling_emergence.R")
source('model_dev/seedling_mortality_H20.R')
source("model_dev/seedling_to_sapling_transition.R")
source("model_dev/seedling_mortality_light.R")


visualize_process_functions_fig <- plot_grid(F_repro_fig,
                                             emerg_vs_SMP_fig,
                                             photoblastic_germ_fig,
                                             Fig_seedling_mort_H20,
                                             viz_light_mort,
                                             light_rec_fig, 
                                                 nrow = 3,labels = paste0("(",letters[1:6],")"), label_size = 20)

makePNG(fig = visualize_process_functions_fig, 
        path_to_output.x = paste0(path_to_output,"forMS/"), 
        file_name = "process_functions",
        height = 14,width = 12,units = "in",res = 70)

print(paste("made visualize process functions figure in",paste0(path_to_output,"forMS/")))
