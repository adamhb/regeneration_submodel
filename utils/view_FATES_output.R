### Script to read FATES output in ncdf files
### developed 5/18/20

### AHB modified this script 6/1/2020

#########################################################################################
##### LIBRARIES                                         #################################
#########################################################################################
library(ncdf4)    #package used to open ncdf files
library(chron)    # package used for time series data

rm(list=ls()) #clear workspace
gc()

#########################################################################################
######     USER INPUT VARIABLES          ################################################
#########################################################################################
onscreen    = FALSE
yearstart   = 1900   ## starting year of the simulation
yearend     = 1902   ## end year of the simulation
endyrmos    = 8     ## number of months in the final year of the simulation
#run        = "X"    ## place holder for manipulations
#site        = "bci"  ## BCI, M34, TNF, CAX
#run_num     = "2"
# met         = "PNM"  ## BCI, SL, PNM
# simlen      = '300'  ## length of simulations
# test        = "01"
# commit      = "3fa9452_c99c5bd" #test16:9c12e40_3f6749d, test18:3fa9452_c99c5bd
# 
# prefix1      = paste(site,"t",test,"_",commit,".clm2.h0.",yearend,"-12.nc", sep="")
# prefix2      = paste(site,"t",test,"_",commit,".clm2.h0.", sep="")

prefix2     = "ahb-bci-test-run.Ccb5c90210-Faddc742.2020-05-28.clm2.h0."

inpath      = paste0("~/cloud/gdrive/rec_submodel/data/FATES_data/")

outpath     = "~/cloud/gdrive/rec_submodel/data/FATES_data/"

#prefix1      = paste(site,"t",test,"_BCIsoil_",commit,".clm2.h0.",yearend,"-12.nc", sep="")
prefix1      = "testing.Cfe16302fc-F7c065e21.2020-06-15.clm2.h0.1902-08.nc"

# prefix2      = paste(site,"t",test,"_BCIsoil_",commit,".clm2.h0.", sep="")
# inpath      = paste("C:/Users/TLPowell/Desktop/LBNL_work/NGEE-T/Panama_gradient/simulations/FATES/",site,
#                    "/",site,"t",test,"_BCIsoil_",commit,"/lnd/hist/", sep="")
# outpath     = paste("C:/Users/TLPowell/Desktop/LBNL_work/NGEE-T/Panama_gradient/simulations/FATES/",site,"/", sep="")



#nc_open(filename = "~/simple-brazil-setup-test-nodebug.fates.lobata.Cd909f573-Faddc742.clm2.r.0001-01-06-00000.nc")


#########################################################################################
#####  Extract ncdf data of individual file             #################################
#########################################################################################
 myfile       = paste(inpath,prefix1, sep="")
 

 mydata       = nc_open(myfile)
 mydata          #lists the variables in the netCDF file.
# 
 date         = ncvar_get(mydata, "mcdate")  # 
 time0        = ncvar_get(mydata, "mcsec")   # 
 time1        = time0/3600
# 
 biomass      = ncvar_get(mydata, "PFTbiomass")  
 recruitment      = ncvar_get(mydata, "RECRUITMENT")   
 GPP_m2       = ncvar_get(mydata, "GPP")*86400   # converts units to g C m-2 d-1
 GPP_ha       = ncvar_get(mydata, "GPP")*864     # 864 converts units to t C ha-1 d-1  (1/100 = 1t/1000000g * 10000m2/1ha)
# ### NOTE: a ball park GPP value for tropical forests is 30 t-c/ha/yr (3000 g-C/m2/yr)
# 
# #variable-X  <- ncvar_get(mydata, "[other variables]")         # by default, reads ALL the data

 
 ncvar_get(mydata, "GPP")

#########################################################################################
#####  Extract ncdf data of multiple files              #################################
#########################################################################################

# Initialize variables, variables that are in a for-loop need to be initialized first.
thismonth   = NULL
GPP      = NULL
NPP      = NULL
TLAI      = NULL      


## NOTE: There are many variable that can be extracted. You can read the vaiable list 
##       by reading mydata.  The units will be supplied with the variable.
##       Among the other variables that may by of interest: NPP, SMP, HR, AR, ED_biomass,
##       which are net primary production, soil matric potential, heterotrophic respiration,
##       autotrophic respiration, and total biomass.



#The following code extracts the data from each netcdf file and then compiles the data into a dataframe.
m = 0
for (year in yearstart:yearend){
  for (month in 1:12){ ## comment this and uncomment next line to read only the same month from each year. Will speed-up reading very long time series.
    # month = 1  # specify which individual month to represent each year.
    m              = m + 1
    cmonth         = substring(100+month,2,3)    #extract month information from file name
    myfile         = paste(inpath,prefix2,year,"-",cmonth,".nc",sep="") #build the names of each input file
    mydata         = nc_open(myfile)
    
    thismonth[m] = chron(dates=paste(month,1,year,sep="/"),times="00:00:00")

    GPP          = ncvar_get(mydata, "GPP")         # units g C m-2 s-1 
    GPP_m2[m]    = ncvar_get(mydata, "GPP")*86400   # converts units to g C m-2 d-1
    GPP_ha[m]    = ncvar_get(mydata, "GPP")*864     # 864 converts units to t C ha-1 d-1  (1/100 = 1t/1000000g * 10000m2/1ha)
      # If you want to have the entire time series as an output, you must initialize the variable and use the counting variable (m in this case).

    # LAISHA[m]    = ncvar_get(mydata, "LAISHA")      # units: m2 m-2
    # LAISUN[m]    = ncvar_get(mydata, "LAISUN")      # units: m2 m-2
    # BTRAN[m]     = ncvar_get(mydata, "BTRAN")
    # AGB[m]       = ncvar_get(mydata, "AGB")
    nc_close(mydata)  #WARNING! Need to close the file or the DATA COULD BE LOST IN THE FILE, and too many files remain open and the for loop crashes.
  } #month for loop 
  } #year for loop

#LAITOT   = LAISUN + LAISHA

#Construct and format time variables 
thismonth       = chron(thismonth)
year.tmp1      = (format(as.Date(thismonth, format="%m/%d/%Y"), format="%Y"))
month.tmp1     = (format(as.Date(thismonth, format="%m/%d/%Y"), format="%m"))
day.tmp1       = (format(as.Date(thismonth, format="%m/%d/%Y"), format="%d"))

#ahb added this line to get the full year to show up
#thismonth <- paste(year.tmp1, month.tmp1, day.tmp1, sep="-")

# BTRAN.tmp2             = aggregate(BTRAN~month.tmp1, FUN="mean")
# names(BTRAN.tmp2)      = c("month","BTRAN")
# BTRAN.sd.tmp2          = aggregate(BTRAN~month.tmp1, FUN="sd") #Caution, need to do this only over one met cycle
# names(BTRAN.sd.tmp2)   = c("month","BTRAN")
# BTRAN.length.tmp2      = aggregate(BTRAN~month.tmp1, FUN="length")
# names(BTRAN.tmp2)      = c("month","BTRAN")
# BTRAN.CI.tmp2          = (BTRAN.sd.tmp2$BTRAN/(sqrt(BTRAN.length.tmp2$BTRAN)))*1.97

outdat  = data.frame("date"   = thismonth,
                     "GPP"    = GPP_ha)#,
                     # "LAItot" = LAITOT,
                     # "LAIsha" = LAISHA,
                     # "LAIsun" = LAISUN,
                     # "AGB"    = AGB,
                     # "BTRAN"  = BTRAN)


write.csv(outdat, file=paste(outpath,"testTomsScript1",".csv",sep=""))
#######################################################################################
####  Plots                                                                       #####
#######################################################################################
date.number          = as.numeric(thismonth)

xaxis_10yr         = c(date.number[1],date.number[12],date.number[24],date.number[36],date.number[48],date.number[60],date.number[72],date.number[84],date.number[96],date.number[108],date.number[120])  
xaxis_10yrlabels   = c("0","1","2","3","4","5","6","7","8","9","10") #these labels work only when simulation begins at 2290.

xaxis_100yr         = c(date.number[1],date.number[120],date.number[240],date.number[360],date.number[480],date.number[600],date.number[720],date.number[840],date.number[960],date.number[1080],date.number[1200])
xaxis_100yrlabels   = c("0","10","20","30","40","50","60","70","80","90","100") #these labels work only when simulation begins at 1900.

xaxis_300yr         = c(date.number[1],date.number[600],date.number[1200],date.number[1800],date.number[2400],date.number[3000],date.number[3600])  
xaxis_300yrlabels   = c("0","50","100","150","200","250","300")                #these labels work only when simulation begins at 1900.
xaxis_500yr         = c(date.number[1],date.number[600],date.number[1200],date.number[1800],date.number[2400],date.number[3000],date.number[3600],date.number[4200],date.number[4800],date.number[5400],date.number[6000])  
xaxis_500yrlabels   = c("0","50","100","150","200","250","300","350","400","450","500")


legendid   = c("GPP")          # labels for the legend
legendcol  = c("forestgreen")  # colors for the legend

legendLAI     = c("total","sun","shade")          # labels for the legend
legendcolLAI  = c("darkolivegreen", "darkolivegreen1","darkolivegreen3")  # colors for the legend


### GPP
par(mar=c(5,6,5,2))
plot(thismonth, GPP_m2, type="l", main=paste("GPP ",site," ",simlen," years test",test, sep=""), 
     xlab="Simulation year",xaxt="n",  
     ylab="GPP [g-C m"^{-2}~" d"^{-1}~"]", ylim=c(0,10),
     col="forestgreen", lwd=3, cex=2, cex.lab=2, cex.axis=2, las=1)
  #axis(1, at=xaxis_10yr, labels=xaxis_10yrlabels, cex.axis=2)  #use this to see how seasonality differs.
  #axis(1, at=xaxis_100yr, labels=xaxis_100yrlabels, cex.axis=2)
  #axis(1, at=xaxis_300yr, labels=xaxis_300yrlabels, cex.axis=2)
  axis(1, at=xaxis_500yr, labels=xaxis_500yrlabels, cex.axis=2)

  legend("topleft",legend=legendid, col=legendcol, lwd=4, cex=2)



### LAI  
  par(mar=c(5,6,5,2))
  plot(thismonth, LAITOT, type="l", main=paste("LAI ",site," ",simlen," years test",test, sep=""), 
       xlab="Simulation year",#xaxt="n",  
       ylab="LAI [m"^{2}~" m"^{-2}~"]", ylim=c(0,10),
       col="darkolivegreen", lwd=3, cex=2, cex.lab=2, cex.axis=2, las=1)
  lines(thismonth, LAISUN, col="darkolivegreen1", lwd=3)
  lines(thismonth, LAISHA, col="darkolivegreen3", lwd=3)
  #axis(1, at=xaxis_10yr, labels=xaxis_10yrlabels, cex.axis=2)  #use this to see how seasonality differs.
  axis(1, at=xaxis_100yr, labels=xaxis_100yrlabels, cex.axis=2)
  #axis(1, at=xaxis_300yr, labels=xaxis_300yrlabels, cex.axis=2)
  #axis(1, at=xaxis_500yr, labels=xaxis_500yrlabels, cex.axis=2)
  
  legend("topright",legend=legendLAI, col=legendcolLAI, lwd=3, cex=1)
  
  
  
### BTRAN  
  par(mar=c(5,6,5,2))
  plot(thismonth, BTRAN, type="l", main=paste("BTRAN ",site," ",simlen," years test",test, sep=""), 
       xlab="Month",#xaxt="n",  
       ylab="BTRAN", ylim=c(0,1),
       col="darkblue", lwd=3, cex=1.5, cex.lab=1.5, cex.axis=1.5, las=1)
  #axis(1, at=xaxis_10yr, labels=xaxis_10yrlabels, cex.axis=1.5)  #use this to see how seasonality differs.
  axis(1, at=xaxis_100yr, labels=xaxis_100yrlabels, cex.axis=2)
  #axis(1, at=xaxis_300yr, labels=xaxis_300yrlabels, cex.axis=2)
  #axis(1, at=xaxis_500yr, labels=xaxis_500yrlabels, cex.axis=2)

  par(mar=c(5,6,5,2))
  plot(BTRAN.tmp2$month, BTRAN.tmp2$BTRAN, type="l", main=paste("BTRAN ",site," test",test, sep=""), 
       xlab="Month",  
       ylab="BTRAN [mean +/- CI]", ylim=c(0,1.1),
       col="darkblue", lwd=3, cex=1.5, cex.lab=1.5, cex.axis=1.5, las=1)
  axis(1, at=xaxis_10yr, labels=xaxis_10yrlabels, cex.axis=1.5)  #use this to see how seasonality differs.
  segments(1:12, BTRAN.tmp2$BTRAN+BTRAN.CI.tmp2, 1:12, BTRAN.tmp2$BTRAN-BTRAN.CI.tmp2, col="darkblue", lwd=2)
  
  ### AGB  
  par(mar=c(5,6.5,5,2))
  plot(thismonth, (AGB/1000), type="l", main=paste("AGB ",site," ",simlen," years test",test, sep=""), 
       xlab="Simulation year",xaxt="n",  
       ylab="", ylim=c(0,18),
       col="darkred", lwd=3, cex=1.5, cex.lab=1.5, cex.axis=1.5, las=1)
  mtext("AGB [kg-C m"^{-2}~"]", side=2, line=3, cex=1.5)
  #axis(1, at=xaxis_10yr, labels=xaxis_10yrlabels, cex.axis=1.5)  #use this to see how seasonality differs.
  #axis(1, at=xaxis_100yr, labels=xaxis_100yrlabels, cex.axis=1.5)
  #axis(1, at=xaxis_300yr, labels=xaxis_300yrlabels, cex.axis=2)
  axis(1, at=xaxis_500yr, labels=xaxis_500yrlabels, cex.axis=2)

# 
#   par(mar=c(5,6,5,2))
#   plot(AGB.tmp2$month, AGB.tmp2$AGB, type="l", main=paste("AGB ",site," test",test, sep=""), 
#        xlab="Month",  
#        ylab="AGB [mean +/- CI]", ylim=c(0,1.1),
#        col="darkblue", lwd=3, cex=1.5, cex.lab=1.5, cex.axis=1.5, las=1)
#   axis(1, at=xaxis_10yr, labels=xaxis_10yrlabels, cex.axis=1.5)  #use this to see how seasonality differs.
#   segments(1:12, AGB.tmp2$AGB+AGB.CI.tmp2, 1:12, AGB.tmp2$AGB-AGB.CI.tmp2, col="darkblue", lwd=2)
  
  
  
  