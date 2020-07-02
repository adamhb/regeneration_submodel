library(ncdf4) 

#baseline
#mydir <- "~/cloud/gdrive/rec_submodel/data/FATES_data/home/adamhb/scratch/clmed-cases/testing1pft_nc.Cfe16302fc-F7c065e21.2020-06-18/run/lnd/hist/"

#1/2 repro alloc
mydir <- "~/cloud/gdrive/rec_submodel/data/FATES_data/home/adamhb/scratch/clmed-cases/1pft_half_seed_alloc.Cfe16302fc-F7c065e21.2020-06-25/run/lnd/hist/"
myfile <- list.files(mydir)[100]
mydata <- nc_open(paste0(mydir,myfile))
grep("NPP", mydata)
ncvar_get(mydata, "RECRUITMENT")
nc_close(mydata)  


DDBH_CANOPY_SCPF