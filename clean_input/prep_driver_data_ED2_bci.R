###----------------------------------------------------------####
###  LIBRARIES                                               ####
###----------------------------------------------------------####
library(hdf5r)    #package used to open hdf5 files
library(chron) # package used for time series data
library(tidyverse)
library(stringr)
print(paste("Preparing Input Data...",Sys.time()))

###The top part of this script is based off of a script written by Tom Powell


#########################################################################################
###### START CODE THAT COMPILES MULTIPLE HDF5 FILES INTO ONE DATAFRAME  #################
#########################################################################################
onscreen    = FALSE
yearstart   = 2000  #starting year of the simulation
yearend     = 2100
YOI         = 2008:2014  #year of interest
MOI         = "12"  # month of interest
PAOI        = 5     # patch age of interest
run         = "dr00"  #dr00, avg, enso,
site        ='BCI'     ## BCI, M34, TNF, CAX
test        = 68
PFTs        = c(2,4,25,26)

cexscal.1   = 1.5
cexscal.2   = 1.3
cexscal.3   = 2
labscal     = 1.8
legscal     = 3
legcol      = 1

###specify the location of the raw data.  
###the first 3 characters of each input file.

driver_data_path <- "~/cloud/gdrive/rec_submodel/data/ED2_output/"
path_to_output <- "~/cloud/gdrive/rec_submodel/output/"
inputref  = paste0(driver_data_path,"BCI_Xu")
outpath   = path_to_output



####---------------------------------------------------------####

# Initialize variables
totyrs         = yearend-yearstart+1
YEAR           = NULL
Month          = NULL
rain           = NULL
patchage       = NULL
AGB.PFT.PAOI   = matrix(NA,nrow=totyrs,ncol=4)

AGB.PFT.fraction       = list()
AGB.PFT.tmp1           = matrix(NA,nrow=12,ncol=(length(PFTs)))
for(gx in 1:length(totyrs)){
  AGB.PFT.fraction[[gx]]     = AGB.PFT.tmp1}


is.leap <- function(year){
  leaptf <- year %% 400 == 0 || (year %% 4 == 0 && year %% 100 != 0)
  return(leaptf)
} #end function

daymax <- function(month,year){
  mmm  <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  mday <- mmm[month]
  if (month == 2 && is.leap(year)) mday <- mday + 1
  return(mday)
} #end function


ED2_data <- data.frame()

#The following code extracts the data from each hdf5 file and then compiles the data into a dataframe.
j = 2
files <- head(list.files(driver_data_path),1000)
for (fl in files[-c(1:2)]){
  #yr = 2000
  j              = j + 1
  myfile         = paste0(driver_data_path,fl) #build the names of each input file
  mydata1        = h5file(myfile)
  ddd            = daymax(as.numeric(MOI),year)
  #YEAR[j]        = yr
  Month[j]       = as.numeric(MOI)
  rain[j]        = mydata1[["MMEAN_PCPG"]][]*86400*ddd     #call up precipitation, convert units from mm/s to mm/month
  thismonth      = chron(dates=paste(MOI,1,YOI,sep="/"),times="00:00:00")
  
  ### Adam: The section below is where AGB is calculated for each patch.
  ### tmp1 creates a column vector of the patch each cohort belongs to.
  ### PACO_N is the number of cohorts in each patch
  ### If you are ever in doubt about where the patch divisions are in a 
  ###      column vector of cohorts, you can look at the DBH variable, 
  ###      it is organized from largest to smallest tree, so when it steps 
  ###      back up to a large tree, then that is the start of a new patch 
  ###      within the column vector.
  ### AREA is the area of the patch.
  ### For NPP you need to multiple the NPP of each cohort by the number of 
  ###      trees in the cohort (multiply by NPLANT, units: 1/m2), then sum over 
  ###      all the trees in each patch, then multiply by the AREA fraction of the patch.
  pft            = mydata1[["PFT"]][]
  paco.n         = mydata1[["PACO_N"]][]
  tmp1           = rep(1:length(paco.n), times = paco.n)
  AREA           = mydata1[["AREA"]][]
  AGE            = mydata1[["AGE"]][]
  AGE.round      = round(AGE, digits=1)
  area.tmp       = rep(AREA, times = paco.n)
  nplant.tmp0    = mydata1[["NPLANT"]][]
  nplant.tmp1    = ifelse(pft==1,0,nplant.tmp0) #This removes the grasses which are PFT1
  
  
  
  AGB.CO.tmp0    = mydata1[["AGB_CO"]][]
  MMEAN_NPPDAILY_CO = mydata1[["MMEAN_NPPDAILY_CO"]][]
  MMEAN_NPPSEEDS_CO = mydata1[["MMEAN_NPPSEEDS_CO"]][]
  DBH_CO = mydata1[["DBH"]][]
  BSEEDS_CO = mydata1[["BSEEDS_CO"]][]
  AGB.CO.tmp1    = AGB.CO.tmp0*nplant.tmp1
  AGB.CO.tmp3    = tapply(AGB.CO.tmp1,tmp1,sum)
  AGB.PFT        = sum(AGB.CO.tmp3*AREA)     #This is a sanity check to make sure the "for" loop calculation is correct.
  
  
  tmp_ED2_data <- data.frame(pft = pft, patchID = tmp1, cohort_area = area.tmp, nplant_per_co_m2 = nplant.tmp1,
             npp_co_per_ind = MMEAN_NPPDAILY_CO, npp_seed_co_per_ind = MMEAN_NPPSEEDS_CO, dbh_co = DBH_CO, bseeds_co_m2 = BSEEDS_CO) %>%
    mutate(nplant_per_co = nplant_per_co_m2 * cohort_area * 10000) %>%
    mutate(npp_co_mo = npp_co_per_ind * nplant_per_co / 12) %>% #(NPP per cohort, per month, Kg C month per cohort) divide by 12 because units of MMEAN_NPPDAILY_CO are KgC yr-1
    mutate(nppseed_co_mo = npp_seed_co_per_ind * nplant_per_co / 12) %>%
    mutate(bseeds_co_total = bseeds_co_m2 * cohort_area * 10000) %>% #total Kg of seed per cohort on a 1 ha simulation plot
    group_by(pft) %>%
    summarise(npp_pft_mo = sum(npp_co_mo), #Kg C per pft per month
              nppseed_pft_mo = sum(nppseed_co_mo),
              bseeds_pft_mo = sum(bseeds_co_total), # Kg C per pft
              dbh_pft = mean(dbh_co),
              n_per_pft = sum(nplant_per_co)) %>%
    mutate(yr = unlist(str_extract_all(myfile, "(?<=-)[:digit:]{4}(?=-)")), 
           month =  unlist(str_extract(myfile, "(?<=-)[:digit:]{2}(?=-)")))
 
  ED2_data <- rbind(ED2_data, tmp_ED2_data)
  
  
  # 
  # AGB.CO.tmp2   <- matrix(NA,nrow=length((paco.n)),ncol=4)  # initialize AGB.CO.tmp2 since it is in a "for" loop with an index [m]
  # for (m in 1:length(paco.n))
  # {
  #   flag1            = tmp1%in%tmp1 & tmp1==m  
  #   AGB.CO.tmp2[m,]  = (tapply(AGB.CO.tmp1[flag1],pft[flag1],sum))*AREA[m]
  # }
  # 
  # #flag2                = which(AGE>4.25 & AGE<5.55 | AGE>5.5 & AGE<6.25)
  # 
  # flag2                = which(AGE.round==PAOI | AGE.round==PAOI+1 | AGE.round==PAOI+2)
  # flag3                = rev(flag2)
  # # flag3                = which(AGE.round==6)
  # # flag4                = which(AGE.round==7)
  # #if(flag2==0)
  # AGB.PFT.PAOI[j,1:4]  = AGB.CO.tmp2[flag3[1],]    
  # patchage[j]          = AGE.round[flag3[1]]
}


#convert to daily timestep
ED2_data1 <- ED2_data %>%
  mutate(date = paste(yr, month, "01" ,sep = "-")) %>%
  mutate_at(.vars = "date",.funs = as.character) %>%
  arrange(pft) %>% 
  mutate(Date = ymd(date)) 

#create a vector of days
day_vector <- c() 
date_vector <- c()
month_vector <- c()

for(i in 1:nrow(ED2_data1)){
  daysInMonth <- days_in_month(ED2_data1$Date[i])
  day_vector <- append(day_vector, 
                  seq(from = 1, to = daysInMonth, by = 1))
  month_vector <- append(month_vector,
                         rep(substr(ED2_data1$Date[i],start = 6,stop = 7),daysInMonth))
  date_vector <- append(date_vector, paste(substr(ED2_data1$Date[i],start = 1,stop = 7),str_pad(string = seq(1:daysInMonth),width = 2,side = "left", pad ="0"),sep = "-"))
}

#ED2 data (excludes environmental vars (H20 and light))
ED2_data_daily <- tibble(day = day_vector, month = month_vector, date = date_vector) %>%
  mutate(yr = substr(date,start = 1,stop = 4)) %>%
  left_join(ED2_data1,by = c("month","yr")) %>%
  dplyr::select(-date.x,-date.y) %>%
  mutate(npp_pft_day = npp_pft_mo / lubridate::days_in_month(Date),
         nppseed_pft_day = nppseed_pft_mo / lubridate::days_in_month(Date)) %>%
  arrange(pft,Date) %>%
  mutate_at(.vars = "dbh_pft", .funs = function(x){x*10}) %>%
  rename(date = Date, NPP = npp_pft_day, N_co = n_per_pft, dbh = dbh_pft)



#ED2 environmental data

#FSDS should be in Joules per day at TOC
#NPP should be in g C m2 per day per PFT


#gets light at TOC
lyr <- c()
lmo <- c()
MMEAN_RSHORT <- c()
files <- head(list.files(driver_data_path),1000)
j <- 0
for (fl in files[-c(1:2)]){
  j <- j + 1
  myfile         = paste0(driver_data_path,fl) #build the names of each input file
  mydata1        = h5file(myfile)
  tmp <- mydata1[["MMEAN_RSHORT"]][]
  MMEAN_RSHORT <- append(MMEAN_RSHORT,tmp)
  lyr[j] <- unlist(str_extract_all(myfile, "(?<=-)[:digit:]{4}(?=-)")) 
  lmo[j] <- unlist(str_extract(myfile, "(?<=-)[:digit:]{2}(?=-)"))
}

MMEAN_SOIL_MSTPOT <- c()
j <- 0
for (fl in files[-c(1:2)]){
  j <- j + 1
  myfile         = paste0(driver_data_path,fl) #build the names of each input file
  mydata1        = h5file(myfile)
  tmp <- mydata1[["MMEAN_SOIL_MSTPOT"]][15,] #second shallowest layer?
  MMEAN_SOIL_MSTPOT <- append(MMEAN_SOIL_MSTPOT,tmp)
  #lyr[j] <- unlist(str_extract_all(myfile, "(?<=-)[:digit:]{4}(?=-)")) 
  #lmo[j] <- unlist(str_extract(myfile, "(?<=-)[:digit:]{2}(?=-)"))
}

envData <- data.frame(yr = lyr, month = lmo, FSDS = MMEAN_RSHORT * 3600 * 24, SMP = MMEAN_SOIL_MSTPOT * 1000)

ED2_data_daily1 <- ED2_data_daily %>% left_join(envData, by = c("yr","month"))


#get soil moisture

# for(i in paste0(driver_data_path,dir(path = driver_data_path))){
#   open_nc_file <- nc_open(i)
#   
#   nc_close(open_nc_file)
# }
# 
# MMEAN_RSHORT = mydata1[["MMEAN_RSHORT"]][]
# MMEAN_RLONG = mydata1[["MMEAN_RLONG"]][]
# 
# mydata1[["MMEAN_PAR_L"]][]
# 
# mydata1[["MMEAN_RSHORT_GND"]][] #at ground
# mydata1[["MMEAN_RLONG_GND"]][] #at ground


input_data <- ED2_data_daily1 %>%
  select(day, pft, date, dbh, N_co, SMP, NPP, FSDS, nppseed_pft_day) %>%
  mutate_at(.vars = c("NPP","nppseed_pft_day"), .funs = function(x){x/10}) %>% #this converts NPP from KgC / ha / day / pft to gC / m2 / day / pft (which the submodel takes)
  mutate_at(.vars = "date",.funs = as.POSIXct) %>%
  rename(day_of_month = day) %>%
  rowid_to_column(var = "day") %>%
  dplyr::select(-day_of_month) 


FATES_vars <- input_data





#j
#YEAR
#AGE

# outdat   = data.frame("sim_year"   = YEAR,
#                       "sim_month"  = Month,
#                       "patch_age"  = patchage,
#                       "AGB.PFT"    = AGB.PFT.PAOI)



#soil matric potential (mm) = MPa * 1e5
# SMP <- c()
# for(i in paste0(driver_data_path,dir(path = driver_data_path))){
#   open_nc_file <- nc_open(i)
#   tmp <- ncvar_get(open_nc_file,varid = "SMP")[3,]
#   SMP <- append(SMP,tmp)
#   nc_close(open_nc_file)
# }
# 
# #SMP_dry <- SMP * 1.6
# NPP <- c()
# for(i in paste0(driver_data_path,dir(path = driver_data_path))){
#   open_nc_file <- nc_open(i)
#   tmp <- ncvar_get(open_nc_file,varid = "NPP")
#   NPP <- append(NPP,tmp)
#   nc_close(open_nc_file)
# }
# 
# FSDS <- c()
# for(i in paste0(driver_data_path,dir(path = driver_data_path))){
#   open_nc_file <- nc_open(i)
#   tmp <- ncvar_get(open_nc_file,varid = "FSDS")
#   FSDS <- append(FSDS,tmp)
#   nc_close(open_nc_file)
# }
# 
# #importing driver data
# FATES_vars <- data.frame(SMP = SMP, FSDS = FSDS, NPP = NPP)
# hour <- seq(from = 0, to = length(FATES_vars$SMP) -1)
# 
# 
# FATES_vars <- FATES_vars %>% 
#   mutate(date = as_datetime(hour*3600, origin = start_date)) %>% #adding the date and time
#   mutate(day = floor(as.numeric(date)/86400)-floor(as.numeric(date[1])/86400)) %>% # adding the day
#   mutate_at(.,.vars = vars(NPP), .funs = function(x){x*3600}) %>% #converting NPP from a flux every second to an hourly flux. 
#   mutate_at(.,.vars = vars(FSDS), .funs = function(x){x*3600}) #convert FSDS from a flux every second to an hourly flux (J hour -1 m -1)
# 
# nTstep <- nrow(FATES_vars)
# pft_col <- c(rep(pft_names[1], nTstep), rep(pft_names[2], nTstep), rep(pft_names[3], nTstep), rep(pft_names[4], nTstep))
# 
# 
# #this section of code can be improved. It duplicates the data to turn a 1 pft stream into 4 pfts, but it does this this makes assumptions:
# #1) NPP is distributed evenly
# #2) PFTs all have the same cohort size
# 
# 
# FATES_vars <- FATES_vars %>% 
#   rbind(.,.) %>% #duplicating the data to account for multiple PFTs
#   rbind(.,.) %>%
#   add_column(pft = pft_col) %>% #adding pfts
#   add_column(dbh = rep(dbh.x, nTstep * n_PFTs)) %>% #adding the dbh
#   add_column(N_co = rep(N_co.x, nTstep * n_PFTs)) #adding the number in the cohort
# 
# 
# # these state variables currently don't change
# FATES_state_vars <- FATES_vars %>%
#   dplyr::select(day, date, pft, dbh, N_co, SMP) %>%
#   group_by(day,pft) %>%
#   summarise_all(.,mean) %>% ungroup(.) %>% arrange(pft, day) #converting to daily time step
#   
# 
# FATES_flux_vars <- FATES_vars %>%
#   dplyr::select(day, pft, NPP, FSDS) %>%
#   group_by(day,pft) %>%
#   summarise_all(.,sum) %>% ungroup(.) %>% arrange(pft, day) #summing hourly fluxes. FSDS = Joules per day, NPP = gC per day
# 
# 
# FATES_vars <- cbind(FATES_state_vars, FATES_flux_vars[,-c(1,2)]) #combining the state vars and the flux vars
# #add_column(water_def = append(def_func(soil_moist = FATES_state_vars$SMP, thresh.x = thresh.xx, window = window.x, dPFT = "DI"), def_func(soil_moist = FATES_state_vars$SMP, thresh.x = thresh.xx, window = window.x, dPFT = "DT"))) %>% #adding water deficit
# 
 print(paste("Finished preparing input data.",Sys.time()))













