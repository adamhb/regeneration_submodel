### This version begins from 1/13/20
### 
### Code to calculate AGB for a specific patch age over several years.
### Generates a cummulative distribution plots for the AGB proportions of the select patch.
### USED FOR Manuscript figures to illustrate how changes in precip change the frequency distributions.

rm(list=ls()) #clear workspace
gc()
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

###----------------------------------------------------------####
###  LIBRARIES                                               ####
###----------------------------------------------------------####
library(hdf5r)    #package used to open hdf5 files
library(chron) # package used for time series data

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

#The following code extracts the data from each hdf5 file and then compiles the data into a dataframe.
j = 0
for (yr in yearstart:yearend){
  
    j              = j + 1
    myfile         = paste(inputref,"-E-",yr,"-",MOI,"-00-000000-g01.h5",sep="") #build the names of each input file
    mydata1        = h5file(myfile)
    ddd            = daymax(as.numeric(MOI),year)
    YEAR[j]        = yr
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
    AGB.CO.tmp1    = AGB.CO.tmp0*nplant.tmp1
    AGB.CO.tmp3    = tapply(AGB.CO.tmp1,tmp1,sum)
    AGB.PFT        = sum(AGB.CO.tmp3*AREA)     #This is a sanity check to make sure the "for" loop calculation is correct.

    AGB.CO.tmp2   <- matrix(NA,nrow=length((paco.n)),ncol=4)  # initialize AGB.CO.tmp2 since it is in a "for" loop with an index [m]
for (m in 1:length(paco.n))
    {
    flag1            = tmp1%in%tmp1 & tmp1==m  
    AGB.CO.tmp2[m,]  = (tapply(AGB.CO.tmp1[flag1],pft[flag1],sum))*AREA[m]
    }

    #flag2                = which(AGE>4.25 & AGE<5.55 | AGE>5.5 & AGE<6.25)

    flag2                = which(AGE.round==PAOI | AGE.round==PAOI+1 | AGE.round==PAOI+2)
    flag3                = rev(flag2)
    # flag3                = which(AGE.round==6)
    # flag4                = which(AGE.round==7)
    #if(flag2==0)
    AGB.PFT.PAOI[j,1:4]  = AGB.CO.tmp2[flag3[1],]    
    patchage[j]          = AGE.round[flag3[1]]
    }

#j
#YEAR
#AGE

outdat   = data.frame("sim_year"   = YEAR,
                      "sim_month"  = Month,
                      "patch_age"  = patchage,
                      "AGB.PFT"    = AGB.PFT.PAOI)
  
write.csv(outdat, file=paste(outpath,"AGB_youngpatch_",run,".csv",sep=""))
###--------------------------------------------------------------------------------####
### Note: 
### After writing outdat, then need to add the corresponding observation year of 
### the meteorological drivers.
### File location: C:\Users\TLPowell\Desktop\LBNL_work\NGEE-T\BCI\simulations\met_sim_years.xlsx
### Cut and paste simyear and obsyear for the simulations years in outdat.
### Sort simyear and obsyear by simyear, smallest to largest, then put obsyear in
### column next to Year in AGB_youngpatch_<run>.csv.
### INMPORTANT: now shift the obsyear by the patch_age to align the AGB with the 
### year the patch was intitialized.  Remove the NA. Save, then read updated
### AGB_youngpatch_<run>.csv back into R with instruction below.
###--------------------------------------------------------------------------------####
mydata2       = read.csv(paste(outpath,"AGB_youngpatch_",run,".csv", sep=""), header=TRUE)
head(mydata2)

obsyear.PAOI5 = mydata2$obsyear.init.PAOI5
obsyear.PAOI6 = mydata2$obsyear.init.PAOI6
obsyear.PAOI7 = mydata2$obsyear.init.PAOI7
simyear       = mydata2$simyear
patch_age     = mydata2$patch_age
AGB.edt.temp1 = mydata2$AGB.PFT.1
AGB.ldt.temp1 = mydata2$AGB.PFT.2
AGB.edi.temp1 = mydata2$AGB.PFT.3
AGB.ldi.temp1 = mydata2$AGB.PFT.4

    flag4 = which(patch_age==PAOI)
      
    AGB.edt = AGB.edt.temp1[flag4]
    AGB.edi = AGB.edi.temp1[flag4] 
    AGB.ldt = AGB.ldt.temp1[flag4]
    AGB.ldi = AGB.ldi.temp1[flag4]
    
    AGB.edt.frac = AGB.edt/(AGB.edt+AGB.edi)
    AGB.ldt.frac = AGB.ldt/(AGB.ldt+AGB.ldi)    
    AGB.DT.frac  = (AGB.edt+AGB.ldt)/(AGB.edt+AGB.edi+AGB.ldt+AGB.ldi)
    obsyear.temp1 = obsyear.PAOI5[flag4]
    
    
flag5  = which(obsyear.PAOI5==2008)
flag6  = which(obsyear.PAOI5==2009)
flag7  = which(obsyear.PAOI5==2010)
flag8  = which(obsyear.PAOI5==2011)
flag9  = which(obsyear.PAOI5==2012)
flag10 = which(obsyear.PAOI5==2013)
flag11 = which(obsyear.PAOI5==2014)


AGB.edt.frac1 = AGB.edt.frac[flag5]
AGB.edt.frac2 = AGB.edt.frac[flag6]
AGB.edt.frac3 = AGB.edt.frac[flag7]
AGB.edt.frac4 = AGB.edt.frac[flag8]
AGB.edt.frac5 = AGB.edt.frac[flag9]
AGB.edt.frac6 = AGB.edt.frac[flag10]
AGB.edt.frac7 = AGB.edt.frac[flag11]

AGB.ldt.frac1 = AGB.ldt.frac[flag5]
AGB.ldt.frac2 = AGB.ldt.frac[flag6]
AGB.ldt.frac3 = AGB.ldt.frac[flag7]
AGB.ldt.frac4 = AGB.ldt.frac[flag8]
AGB.ldt.frac5 = AGB.ldt.frac[flag9]
AGB.ldt.frac6 = AGB.ldt.frac[flag10]
AGB.ldt.frac7 = AGB.ldt.frac[flag11]

AGB.DT.frac1 = AGB.DT.frac[flag5]
AGB.DT.frac2 = AGB.DT.frac[flag6]
AGB.DT.frac3 = AGB.DT.frac[flag7]
AGB.DT.frac4 = AGB.DT.frac[flag8]
AGB.DT.frac5 = AGB.DT.frac[flag9]
AGB.DT.frac6 = AGB.DT.frac[flag10]
AGB.DT.frac7 = AGB.DT.frac[flag11]

####-----------
####  PLOTS  
####-----------
legendid   = c("2008","2013","2009","2014","2011","2012","2010") 
legendcol  = c("#d73027", "#fc8d59", "#fee090", "#ffffbf","#e0f3f8","#91bfdb","#4575b4")

## color gradient, colorblind safe. 
#d73027 --> 2008
#fc8d59 --> 2013
#fee090 --> 2009
#ffffbf --> 2014
#e0f3f8 --> 2011
#91bfdb --> 2012
#4575b4 --> 2010


##EARLY
par(mar=c(4.5, 6, 3, 1))
plot(ecdf(AGB.edt.frac1), ylab="", xlab="AGB fraction of safe-inefficient PFT", 
     xlim=c(0.2,0.9), cex=1.5, cex.lab=labscal, cex.axis=labscal, col="#d73027",
     las=1, main=paste("EDT fraction Patch AGE = ", PAOI, " test", test, sep=""))  #2008
lines(ecdf(AGB.edt.frac2), cex=1.2, col="#fee090")  #2009
lines(ecdf(AGB.edt.frac3), cex=1.2, col="#4575b4")  #2010
lines(ecdf(AGB.edt.frac4), cex=1.2, col="#e0f3f8")  #2011
lines(ecdf(AGB.edt.frac5), cex=1.2, col="#91bfdb")  #2012
lines(ecdf(AGB.edt.frac6), cex=1.2, col="#fc8d59")  #2013
lines(ecdf(AGB.edt.frac7), cex=1.2, col="#ffffbf")  #2014
mtext("Cumulative distribution", line=4.3, side=2, cex=1.8, las=3)

###LATE
par(mar=c(4.5, 6, 3, 1))
plot(ecdf(AGB.ldt.frac1), ylab="", xlab="AGB fraction of safe-inefficient PFT", 
     xlim=c(0.3,0.7), cex=1.5, cex.lab=labscal, cex.axis=labscal, col="#d73027",
     las=1, main=paste("LDT fraction Patch AGE = ", PAOI, " test", test, sep=""))  #2008
lines(ecdf(AGB.ldt.frac2), cex=1.2, col="#fee090")  #2009
lines(ecdf(AGB.ldt.frac3), cex=1.2, col="#4575b4")  #2010
lines(ecdf(AGB.ldt.frac4), cex=1.2, col="#e0f3f8")  #2011
lines(ecdf(AGB.ldt.frac5), cex=1.2, col="#91bfdb")  #2012
lines(ecdf(AGB.ldt.frac6), cex=1.2, col="#fc8d59")  #2013
lines(ecdf(AGB.ldt.frac7), cex=1.2, col="#ffffbf")  #2014
mtext("Cumulative distribution", line=4.5, side=2, cex=1.8, las=3)

### ALL
par(mar=c(4.5, 6, 3, 1))
plot(ecdf(AGB.DT.frac1), ylab="", xlab="AGB fraction of safe-inefficient PFT", 
     xlim=c(0.3,0.7), cex=1.2, cex.lab=labscal, cex.axis=labscal, col="#d73027",
     las=1, main=paste("DT-all fraction Patch AGE = ", PAOI, " test", test, sep=""))  #2008
lines(ecdf(AGB.DT.frac2), cex=1.2, col="#fee090")  #2009
lines(ecdf(AGB.DT.frac3), cex=1.2, col="#4575b4")  #2010
lines(ecdf(AGB.DT.frac4), cex=1.2, col="#e0f3f8")  #2011
lines(ecdf(AGB.DT.frac5), cex=1.2, col="#91bfdb")  #2012
lines(ecdf(AGB.DT.frac6), cex=1.2, col="#fc8d59")  #2013
lines(ecdf(AGB.DT.frac7), cex=1.2, col="#ffffbf")  #2014
mtext("Cumulative distribution", line=4.5, side=2, cex=1.8, las=3)

#create legend
plot(1,1)
legend(0.9,1.2,legend=legendid, pch=16, col=legendcol, cex=1.5)
text(locator(1),"Year of met data when 5 year-old patch initialized")
