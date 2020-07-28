
params <- list(b_rec = b_rec, a_rec = a_rec)

map(.x = params, .f = function(x){x*4})

paramsOFrun <- data.frame(param_names = c("model_area", "dbh.x", "N_co.x", "Dmax", "frac_repro", "seed_frac","decay_rate", 
                                          "a_emerg", "b_emerg", "a_rec", "b_rec", "percent_light", "thresh", "window.x", 
                                          "seedbank_0", "seedpool_0", "litter_0", "gitCommit", "start_date", "end_date", "driver_data"), 
                          param_vals = c(model_area, dbh.x, N_co.x, paste0(Dmax, collapse = ","),paste0(frac_repro, collapse = ","), 
                                         seed_frac, decay_rate, paste0(a_emerg, collapse = ","), paste0(b_emerg, collapse = ","), 
                                         paste0(a_rec, collapse = ","), paste0(b_rec, collapse = ","), 
                                         percent_light, paste0(thresh.xx, collapse = ","), 
                                         window.x, seedbank_0, seedpool_0, litter_0, system("git rev-parse HEAD", intern=TRUE),
                                         start_date, end_date, basename(driver_data_path)))