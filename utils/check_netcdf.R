library(ncdf4) 

myfile <- "~/cloud/gdrive/rec_submodel/data/FATES_data/bci1900_2016_1pft_vanilla/testing1pft_nc.Cfe16302fc-F7c065e21.2020-06-18.clm2.h0.2016-02.nc"
mydata <- nc_open(myfile)
grep("NPP", mydata)
ncvar_get(mydata, "RECRUITMENT")
nc_close(mydata)  

