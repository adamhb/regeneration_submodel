library(ncdf4) 
myfile <- "~/cloud/gdrive/rec_submodel/data/FATES_data/testing1pft_nc.Cfe16302fc-F7c065e21.2020-06-18.clm2.h0.1900-02.nc"
mydata <- nc_open(myfile)
grep("NPP", mydata)
ncvar_get(mydata, "NPP_SEED_SCPF")
nc_close(mydata)  


testing1pft_nc.Cfe16302fc-F7c065e21.2020-06-1