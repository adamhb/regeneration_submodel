#This script prepares the driver data to be usable by the tree recruitment scheme.
#It receives as input a folder of driver data files in hdf5 format, and outputs a 
#data frame of driver variables that is used by the tree recruitment scheme. Note that
#this script also calculates reproductive allocation in each timestep, using NPP.


print(paste("Preparing Input Data...",Sys.time()))
source("utils/supporting_funcs.R")
source("model/process_funcs.R") #use the process functions script

#Adapated from script written by T.L. Powell

########################################################################################
##### START CODE THAT COMPILES MULTIPLE HDF5 FILES INTO ONE DATAFRAME  #################
########################################################################################
onscreen    = FALSE
yearstart   = 2000  #starting year of the simulation
yearend     = 2100
YOI         = 2008:2014  #year of interest
MOI         = "12"  # month of interest
#PAOI        = 5     # patch age of interest
#run         = "dr00"  #dr00, avg, enso,
#site        ='BCI'     ## BCI, M34, TNF, CAX
#test        = 68
PFTs        = c(2,4,25,26)

#cexscal.1   = 1.5
#cexscal.2   = 1.3
#cexscal.3   = 2
#labscal     = 1.8
#legscal     = 3
#legcol      = 1

###specify the location of the raw data.
###the first 3 characters of each input file.


#inputref  = paste0(driver_data_path,"BCI_Xu")
outpath   = path_to_output


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

#functions to detect leap years and days within a month
############################################################
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
#############################################################


#The following code extracts the driver data from each hdf5 file and then compiles it
#into a dataframe that is used by the tree recruitment scheme.

ED2_data <- data.frame()

#Step 1. Determining which driver data files are in the date range of the simulation
####################################################################################
inDateRange <- dateFromFile(list.files(path = driver_data_path)) %in% seq(ymd(start_date), ymd(end_date), by = "months")
files <- list.files(driver_data_path)[inDateRange]
####################################################################################

#Step 2. Extracting the driver data from the hdf5 files
####################################################################################
j = 0
for (fl in files){
  #yr = 2000
  j              = j + 1
  myfile         = paste0(driver_data_path,fl) #build the names of each input file
  mydata1        = h5file(myfile)
  ddd            = daymax(as.numeric(MOI),year)
  #YEAR[j]        = yr
  Month[j]       = as.numeric(MOI)
  rain[j]        = mydata1[["MMEAN_PCPG"]][]*86400*ddd     #call up precipitation, convert units from mm/s to mm/month
  thismonth      = chron(dates=paste(MOI,1,YOI,sep="/"),times="00:00:00")
  ### tmp1 creates a column vector of the patch each ED2 adult cohort belongs to.
  ### PACO_N is the number of cohorts in each patch
  ### If you are ever in doubt about where the patch divisions are in a 
  ###      column vector of cohorts, you can look at the DBH variable, 
  ###      it is organized from largest to smallest tree, so when it steps 
  ###      back up to a large tree, then that is the start of a new patch 
  ###      within the column vector.
  ### AREA is the area of the patch.
  ### To calculate NPP per pft we multiple the NPP of each cohort 
  ###      (which is actually on a per individual basis) 
  ###      by the number of trees in the cohort (multiply by NPLANT, units: 1/m2), then sum over 
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
  MMEAN_NPPDAILY_CO = mydata1[["MMEAN_NPPDAILY_CO"]][] #KgC per individual per year
  MMEAN_NPPSEEDS_CO = mydata1[["MMEAN_NPPSEEDS_CO"]][]
  DBH_CO = mydata1[["DBH"]][] * 10 #convert to mm of dbh
  BSEEDS_CO = mydata1[["BSEEDS_CO"]][]
  AGB.CO.tmp1    = AGB.CO.tmp0*nplant.tmp1
  AGB.CO.tmp3    = tapply(AGB.CO.tmp1,tmp1,sum)
  AGB.PFT        = sum(AGB.CO.tmp3*AREA)
  
  tmp_ED2_data <- data.frame(pft_tmp = pft, 
                             patchID = tmp1, 
                             cohort_area = area.tmp, 
                             nplant_per_co_m2 = nplant.tmp1,
                             npp_co_per_ind = MMEAN_NPPDAILY_CO, 
                             npp_seed_co_per_ind = MMEAN_NPPSEEDS_CO, 
                             dbh_co = DBH_CO, 
                             bseeds_co_m2 = BSEEDS_CO,
                             agb_co_ind = AGB.CO.tmp0) %>%
    mutate(pft = case_when(
      pft_tmp == 2 ~ "LD_DT",
      pft_tmp == 4 ~ "ST_DT",
      pft_tmp == 25 ~ "LD_DI",
      pft_tmp == 26 ~ "ST_DI"
    )) %>%
    mutate(e_frac = base::mapply(FUN = efrac, 
                                 N = (nplant_per_co_m2 * cohort_area * 10000),
                                 co_dbh_ind = dbh_co, 
                                 PFT = pft)) 
  
  tmp_ED2_data1 <- tmp_ED2_data %>%
    mutate(nplant_per_co = nplant_per_co_m2 * cohort_area * 10000) %>%
    mutate(agb_co = agb_co_ind * nplant_per_co) %>%
    mutate(npp_co_mo = npp_co_per_ind * 1000 * nplant_per_co / (12 * 10000)) %>% #(NPP, gC month per m2 per cohort)
    mutate(nppseed_co_mo = npp_seed_co_per_ind * 1000 * nplant_per_co / (12 * 10000)) %>% # same units as above
    mutate(CgANDr_co_per_mo = nppseed_co_mo / 0.3) %>%
    mutate(c_repro_co_per_mo = e_frac * CgANDr_co_per_mo) %>%
    mutate(bseeds_co_total = bseeds_co_m2 * cohort_area * 10000)  #total Kg of seed per cohort on a 1 ha simulation plot
  
  tmp_ED2_data2 <- tmp_ED2_data1 %>%
    group_by(pft) %>%
    summarise(npp_pft_mo = sum(npp_co_mo), #g C per pft per month
              nppseed_pft_mo = sum(nppseed_co_mo),
              bseeds_pft_mo = sum(bseeds_co_total), # Kg C per pft
              dbh_pft = mean(dbh_co),
              n_per_pft = sum(nplant_per_co),
              agb_pft = sum(agb_co),
              c_repro_per_pft_per_mo = sum(c_repro_co_per_mo)) %>%
    mutate(yr = unlist(str_extract_all(myfile, "(?<=-)[:digit:]{4}(?=-)")), 
           month =  unlist(str_extract(myfile, "(?<=-)[:digit:]{2}(?=-)")))
 
  ED2_data <- rbind(ED2_data, tmp_ED2_data2)

}

#extracting environmental variables from ED2 
#(i.e. light and soil moisture from the hdf5 driver data files)

lyr <- c()
lmo <- c()
MMEAN_RSHORT <- c()
j <- 0
for (fl in files){
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
for (fl in files){
  j <- j + 1
  myfile         = paste0(driver_data_path,fl) #build the names of each input file
  mydata1        = h5file(myfile)
  tmp <- mydata1[["MMEAN_SOIL_MSTPOT"]][soil_layer,] #shallowest layer (as evidenced by SLZ)
  MMEAN_SOIL_MSTPOT <- append(MMEAN_SOIL_MSTPOT,tmp)
  #lyr[j] <- unlist(str_extract_all(myfile, "(?<=-)[:digit:]{4}(?=-)")) 
  #lmo[j] <- unlist(str_extract(myfile, "(?<=-)[:digit:]{2}(?=-)"))
}

#END extracting light and soil moisture from the hdf5 driver data files

##########################################################################################
#END extracting the driver data from the hdf5 files


#Step 3. Clean the extracted driver data
##########################################################################################
#Adding a date field to the driver data
ED2_data1 <- ED2_data %>%
  mutate(date = paste(yr, month, "01" ,sep = "-")) %>%
  mutate_at(.vars = "date",.funs = as.character) %>%
  mutate(Date = ymd(date)) 

#converting ED2 data to daily timestep (ED2 history files were on a monthly timestep)
ED2_data_daily <- tibble()
PFTs <- pft_names
for(pft.x in PFTs){
  
  df <- ED2_data1 %>% filter(pft == pft.x) 
  
  daily_temp <- df %>% mutate(Date = ymd(Date)) %>%
    group_by(Date) %>%
    tidyr::expand(Date = seq(floor_date(Date, unit = "month"),
                      ceiling_date(Date, unit="month")-days(1), 
                      by="day"),
           pft,
           npp_pft_mo, 
           nppseed_pft_mo,
           bseeds_pft_mo,
           dbh_pft,
           n_per_pft,
           c_repro_per_pft_per_mo,
           yr,
           month) %>%
    as.data.frame() %>%
    rownames_to_column(var = "day")
  
  ED2_data_daily <- rbind(ED2_data_daily,daily_temp)
  
}

ED2_data_daily1 <- ED2_data_daily %>% 
  mutate(npp_pft_day = npp_pft_mo / lubridate::days_in_month(Date),
         nppseed_pft_day = nppseed_pft_mo / lubridate::days_in_month(Date), #npp per pft per m2 per day (gC)
         c_repro_per_pft_per_day = c_repro_per_pft_per_mo / lubridate::days_in_month(Date)) %>% 
  arrange(pft,Date) %>%
  #mutate_at(.vars = "dbh_pft", .funs = function(x){x*10}) %>% 
  rename(date = Date, NPP = npp_pft_day, N_co = n_per_pft, dbh = dbh_pft, c_repro = c_repro_per_pft_per_day)


#clean the environmental data drivers (light and soil moisture) and add to the productivity data
envData <- data.frame(yr = lyr, month = lmo, FSDS = MMEAN_RSHORT * 3600 * 24, SMP = MMEAN_SOIL_MSTPOT * 1000)
ED2_data_daily2 <- ED2_data_daily1 %>% left_join(envData, by = c("yr","month"))


#more data cleaning
input_data <- ED2_data_daily2 %>%
  dplyr::select(yr, month, day, pft, date, dbh, N_co, SMP, NPP, c_repro, FSDS, nppseed_pft_day) %>%
  mutate_at(.vars = "date",.funs = as.POSIXct) %>%
  mutate_at(.vars = "day", .funs = as.numeric) %>%
  mutate_at(.vars = "c_repro", .funs = function(x){x * model_area}) %>%
  mutate_at(.vars = "FSDS", .funs = function(x){x * par_per_solar_rad}) 
 

if(patch_run_type == "many"){
  input_data1 <- input_data
}

#calculate the average soil moisture for the simulation
avg_SMP <- mean(input_data$SMP)


#END prep driver data
print(paste("Finished preparing input data.",Sys.time()))













