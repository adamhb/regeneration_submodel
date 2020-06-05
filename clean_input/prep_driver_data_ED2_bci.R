print(paste("Preparing Input Data...",Sys.time()))

#soil matric potential (mm) = MPa * 1e5
SMP <- c()
for(i in paste0(driver_data_path,dir(path = driver_data_path))){
  open_nc_file <- nc_open(i)
  tmp <- ncvar_get(open_nc_file,varid = "SMP")[3,]
  SMP <- append(SMP,tmp)
  nc_close(open_nc_file)
}

#SMP_dry <- SMP * 1.6
NPP <- c()
for(i in paste0(driver_data_path,dir(path = driver_data_path))){
  open_nc_file <- nc_open(i)
  tmp <- ncvar_get(open_nc_file,varid = "NPP")
  NPP <- append(NPP,tmp)
  nc_close(open_nc_file)
}

FSDS <- c()
for(i in paste0(driver_data_path,dir(path = driver_data_path))){
  open_nc_file <- nc_open(i)
  tmp <- ncvar_get(open_nc_file,varid = "FSDS")
  FSDS <- append(FSDS,tmp)
  nc_close(open_nc_file)
}

#importing driver data
FATES_vars <- data.frame(SMP = SMP, FSDS = FSDS, NPP = NPP)
hour <- seq(from = 0, to = length(FATES_vars$SMP) -1)


FATES_vars <- FATES_vars %>% 
  mutate(date = as_datetime(hour*3600, origin = start_date)) %>% #adding the date and time
  mutate(day = floor(as.numeric(date)/86400)-floor(as.numeric(date[1])/86400)) %>% # adding the day
  mutate_at(.,.vars = vars(NPP), .funs = function(x){x*3600}) %>% #converting NPP from a flux every second to an hourly flux. 
  mutate_at(.,.vars = vars(FSDS), .funs = function(x){x*3600}) #convert FSDS from a flux every second to an hourly flux (J hour -1 m -1)

nTstep <- nrow(FATES_vars)
pft_col <- c(rep(pft_names[1], nTstep), rep(pft_names[2], nTstep), rep(pft_names[3], nTstep), rep(pft_names[4], nTstep))


#this section of code can be improved. It duplicates the data to turn a 1 pft stream into 4 pfts, but it does this this makes assumptions:
#1) NPP is distributed evenly
#2) PFTs all have the same cohort size


FATES_vars <- FATES_vars %>% 
  rbind(.,.) %>% #duplicating the data to account for multiple PFTs
  rbind(.,.) %>%
  add_column(pft = pft_col) %>% #adding pfts
  add_column(dbh = rep(dbh.x, nTstep * n_PFTs)) %>% #adding the dbh
  add_column(N_co = rep(N_co.x, nTstep * n_PFTs)) #adding the number in the cohort


# these state variables currently don't change
FATES_state_vars <- FATES_vars %>%
  dplyr::select(day, date, pft, dbh, N_co, SMP) %>%
  group_by(day,pft) %>%
  summarise_all(.,mean) %>% ungroup(.) %>% arrange(pft, day) #converting to daily time step


FATES_flux_vars <- FATES_vars %>%
  dplyr::select(day, pft, NPP, FSDS) %>%
  group_by(day,pft) %>%
  summarise_all(.,sum) %>% ungroup(.) %>% arrange(pft, day) #summing hourly fluxes. FSDS = Joules per day, NPP = gC per day


FATES_vars <- cbind(FATES_state_vars, FATES_flux_vars[,-c(1,2)]) #combining the state vars and the flux vars
#add_column(water_def = append(def_func(soil_moist = FATES_state_vars$SMP, thresh.x = thresh.xx, window = window.x, dPFT = "DI"), def_func(soil_moist = FATES_state_vars$SMP, thresh.x = thresh.xx, window = window.x, dPFT = "DT"))) %>% #adding water deficit

print(paste("Finished preparing input data.",Sys.time()))













