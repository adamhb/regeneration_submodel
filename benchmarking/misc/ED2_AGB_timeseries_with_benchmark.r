### This version begins from 1/29/15
### It is formated to run on Odyssey for the ED_Xu output
### This version is configured to extract the ED_Xu pfts

rm(list=ls()) #clear workspace
gc()
#########################################################################################
######     USER INPUT VARIABLES          ################################################
#########################################################################################

library(h5)    #package used to open hdf5 files
library(chron) # package used for time series data

onscreen    <- FALSE
reverse     <- FALSE
yearstart   <- 2000  #starting year of the simulation
yearend     <- 2009
#end year of the simulation
endyrmos    <- 12       #number of months in the final year of the simulation
run         <- "dr00"  #dr00, avg, enso,
site        <-'BCI'     ## BCI, M34, TNF, CAX
met         <-'SOImet'  #SHEF, SOImet, SOImet_LBA, SOImet_TFE
spin        <- '2000yr'   #bg: bare ground, bt: biomass treatment, bc: biomass control
test        <- 68
cohortscal  <- c(0,900)
patchscal   <- c(0,30)

inputref  = paste("C:/Users/TLPowell/Desktop/LBNL_work/NGEE-T/",site,"/simulations/test",test,"/analy_",run,"/",site,"_Xu", sep="")

outpath   = paste("C:/Users/TLPowell/Desktop/LBNL_work/NGEE-T/",site,"/simulations/test",test,"/",sep="")

#########################################################################################
###### START CODE THAT COMPILES MULTIPLE HDF5 FILES INTO ONE DATAFRAME  #################
#########################################################################################

# Initialize variables
totmon     <- (yearend-yearstart)*12+endyrmos
thismonth  <- NULL
agbpft     <- matrix(NA,nrow=totmon,ncol=29)  #AGB must be defined as a matrix
agbtot     <- NULL


# The following code defines which input files are leap years and then ensures that
# 29 days are in February during leap years.  The fortran code that builds the hdf5 
# files leaves out a leap day every 400 years owing to the fact that each year is 
# actually slightly less than 365.25 days. A day count for each month is required to      
# make the proper unit conversions for each month.
# %% means modulus, which is the remainder after division of x by y, e.g. x%%y is modulus(x mod y) or, for example, 5%%2 is 1.
# using the modulus helps to ID the leap years b/c leap years must satisfy the criteria below.
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
m = 0
for (year in yearstart:yearend){
  lastmonth = 12
  if (year == yearend) lastmonth <- endyrmos
  for (month in 1:lastmonth){
    m              = m + 1
    cmonth         = substring(100+month,2,3)    #extract month information from file name
    ddd            = daymax(month,year)
    myfile         = paste(inputref,"-E-",year,"-",cmonth,"-00-000000-g01.h5",sep="") #build the names of each input file
    mydata         = h5file(myfile)
    thismonth[m]   = chron(dates=paste(month,1,year,sep="/"),times="00:00:00")

    pft            = mydata["PFT"][]
    paco.n         = mydata["PACO_N"][]
    tmp1           = rep(1:length(paco.n), times = paco.n)
    AREA           = mydata["AREA"][]
    area.tmp       = rep(AREA, times = paco.n)
    nplant.tmp0    = mydata["NPLANT"][]
    nplant.tmp1    = ifelse(pft==1,0,nplant.tmp0)
     
    agbpft.tmp     = mydata["AGB_PFT"][]                  #call up AGB put values into a matrix   
    agbpft[m,1:29] = agbpft.tmp[1:29]                     #put AGB values into a matrix
    agbtot[m]      = sum(agbpft.tmp)
    age.tmp         = mydata["AGE"][]
    } 
    }

#Convert the time variable to mm/dd/yyyy format
thismonth       = chron(thismonth)

thismonth.temp  = as.numeric(thismonth)
month.temp      = rep(1:12, (yearend-yearstart+1))
year.temp       = rep(yearstart:yearend, each=12)


#colors() #list R colors
xaxis_1000yr        = c(-244975,-171615,-98255,-24895,48465,121825)
xaxis_1000yrlabels  = c("0","200","400","600","800","1000")
xaxis_2000yr        = c(-244975,-171615,-98255,-24895,48465,121825,195185,268545,341905,415265,488595)
xaxis_2000yrlabels  = c("0","200","400","600","800","1000","1200","1400","1600","1800","2000")


###############################################################################
###########  AGB timeseries GRAPH  ############################################
###############################################################################

legendid2   = c("total", "early-dt","early-di","Obs","late-dt", "late-di") #labels for the legend
legendcol2  = c("black",  "dodgerblue4", "deepskyblue","red", "tomato4", "tomato")

graphsAGB <- function(b)
  {
  par(mar=c(4.5, 4.7, 3, 1))
    plot(thismonth, agbpft[,2], type="l", main=paste("AGB_",spin,"_",site,"_test",test,"_",run, sep=""),
       xaxt="n", tck=-0.04,
       xlab="Simulation year", ylab="AGB [kg-C m"^{-2}~"]", ylim=c(0,15), col="dodgerblue4", lwd=4, las=1)
 # axis(1, at=xaxis_1000yr, labels=xaxis_1000yrlabels, tck=-0.04)
  axis(1, at=xaxis_2000yr, labels=xaxis_2000yrlabels, tck=-0.04)
  lines(thismonth, agbpft[,25], type="l", col="deepskyblue", lwd=4)
  lines(thismonth, agbpft[,4], type="l", col="tomato4", lwd=4)
  lines(thismonth, agbpft[,26], type="l", col="tomato", lwd=4)
  lines(thismonth, agbtot, type="l", col="black", lwd=4)
  points(14000,14.0, pch=1, cex=1.5, lwd=3, col="red2")  # the 14.0 comes from Chave et al. 2003 J. Ecol, for BCI
  segments(14000,13,14000,15, lwd=2.5, lty=1, col="red2")
   legend(-92000,12.3,inset=0.01,bg="white",legend=legendid2, col=legendcol2, 
          lwd= c(4,4,4,3,4,4), lty= c(1,1,1,NA,1,1), pch = c(NA,NA,NA,1,NA,NA), ncol=2)
  }
#instruction for plot to display on screen or export to file.
 if (onscreen){
      graphsAGB()  #runs graphs routine if onscreen is TRUE
   }else{
     #Initialize the export function for sending the graph to a .png file.
     # Set the width and height of the figure, specify the units as pixels ("px")
     # Set the background to white, restoreConsole is require to fix a Windows bug.
     png (paste(outpath,"AGBpft_all_",spin,"_",site,"_test",test,"_",run,".png", sep=""), width = 480, height = 520, units = "px",
     pointsize = 16, bg = "white",  res = NA)
     graphsAGB()   #run the graphs routine
     graphics.off()
   }

