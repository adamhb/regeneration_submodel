# ========================================================================================
# ' forestgeo_benchmark_driver 
# ' a tool for generating benchmark statistics for demographic ecosystem models
# 
# forestgeo_benchmark_driver.r This script is the driver to generate a suite
# of data processing on core ForestGEO plot data intended to generate
# statistical benchmarks for model-data comparison.  The user decides which
# benchmarks should be generated, the intercomparison will assess which
# benchmarks are possible.  
# Users: Change your working directory to the location of this script.  
# For testing, we recommend you download the CTFS
# Forest Geo BCI core census data Data requests can be made here:
# http://ctfs.si.edu/webatlas/datasets/bci/ (Note: Make sure to download
# 'rtable' data if there are optional formats,also request the plot species table)
# Wood density can be found here: http://ctfs.arnarb.harvard.edu/Public/Datasets/CTFSWoodDensity/
# Original Script: Dan Johnson (~10/2015) Modified, Ryan Knox, enable
# generic sites and fine control on size structures (~10/2015) 
# Further refinements by Ryan and Dan (~5/2016)
# 
# Some important data structures:
# cnsdata[[]]$   Census indexed list holding the raw census data, its big
# sp.Glist       Census indexed list of data-frames with growth rates for each species
# sp.Mlist       Census indexed list of data-frames with mortality rates for each species,dclass
# sp.Rlist       Census indexed list of data-frames with recruitment into the smallest class
# sp.Rlist_10cm  Census indexed list of data-frames with recruitment into the 10cm class
# sp.Alist       Census indexed list of data-frames with abundance into the smallest class
# sp.BAlist      Census indexed list of data-frames with basal-area into the smallest class
# Glist       Census indexed list of data-frames with growth rates for each diameter class
# Mlist       Census indexed list of data-frames with mortality rates for each diameter class
# Stemlist    Census indexed list of data-frames with stem abundance for each diameter class
# StemBAlist  Census indexed list of data-frames with stem basal area for each diamter class
# spc$           List (not indexed by census) that holds various emergent trait indices
#
# Also see the github wiki page for more information: 
# https://github.com/NGEET/tools_benchmarking_evaluation/wiki/forestgeo_2_dembmarks
#
# ========================================================================================

# Clear the work-space (USER MAY WANT TO COMMENT/UNCOMMENT)
#rm(list=ls())

#script_dir <- dirname(sys.frame(1)$ofile)


#setwd(script_dir)


# Load CTFS tools and tables

# The full file path to the CDFS R package 
# (where all the nice functions live)
path_to_observational_data <- "~/cloud/gdrive/rec_submodel/data/observations/"
ctfsrpackfile <- "benchmarking/CTFSRPackage.rdata"
site.name <- "bci"

# Full path to the CTFS plot species data (plot species table needs to be in working directory)
sp_table_name <- paste0(path_to_observational_data,"bci.spptable.rdata")
load(sp_table_name)
plot_spp <- get(paste0(site.name,".spptable"))

# Full path to wood density data
# not currently used but is included for future functional type analysis

#setwd("C:/Users/ahanb/OneDrive/Documents/tools_benchmarking_evaluation/forestgeo_2_dembmarks")
load(paste0(path_to_observational_data,"wsg.ctfs.Rdata"))



# Initialize Error Log
err_log <- c() 

# Initialize utilities scripts
source("benchmarking/fgcb_utilities.r")   # Error logging (currently)
source("benchmarking/fgcb_plot_utils.r")  # Specialized Plots

# User should set working directory to where your raw stem data, trait,
# climate and CTFSRpackage files are located


# Load the CTFS environment and various libraries we will need
# ========================================================================================

library(xml2)
library(stringr)
library(date)
library(SparseM)
library(quantreg)
library(ggplot2)
library(codetools)
library(ncdf4)
#library(stargazer)
#library(fitdistrplus)
attach(ctfsrpackfile)


# ======================================================================================
# Load User Defined Parameters From XML
# ======================================================================================


xmlsettings <- read_xml("benchmarking/ctfs_at_settings.xml")
site.name <- str_trim(xml_text(xml_find_all(xmlsettings,"/all/site_name")),side="both")
ctfs_table_type <- str_trim(xml_text(xml_find_all(xmlsettings,"/all/ctfs_table_type")),side="both")
unit.dbh <- str_trim(xml_text(xml_find_all(xmlsettings,"/all/unit_dbh")),side="both")
large_stature_dbh95 <- as.double(xml_text(xml_find_all(xmlsettings,"/all/large_stature_dbh95")))
large_only <- as.logical(str_trim(xml_text(xml_find_all(xmlsettings,"/all/large_only")),side="both"))
sapling_dbh <- as.double(xml_text(xml_find_all(xmlsettings,"/all/sapling_dbh")))
tree_dbh <- as.double(xml_text(xml_find_all(xmlsettings,"/all/tree_dbh")))
conf_limit <- as.double(xml_text(xml_find_all(xmlsettings,"/all/conf_limit")))
plot_dim_x <- as.double(xml_text(xml_find_all(xmlsettings,"/all/plot_dim_x")))
plot_dim_y <- as.double(xml_text(xml_find_all(xmlsettings,"/all/plot_dim_y")))
plot_dim <- c(plot_dim_x,plot_dim_y)
do_kat_clim_site <- as.logical(str_trim(xml_text(xml_find_all(xmlsettings,"/all/do_kat_clim_site")),side="both"))
site.number <- as.double(xml_text(xml_find_all(xmlsettings,"/all/site_number")))
do_gen_ncdf4_outfile <- as.logical(str_trim(xml_text(xml_find_all(xmlsettings,"/all/do_gen_ncdf4_outfile")),side="both"))
do_gen_psscss_file <- as.logical(str_trim(xml_text(xml_find_all(xmlsettings,"/all/do_gen_psscss_file")),side="both"))
do_palm_filter <- as.logical(str_trim(xml_text(xml_find_all(xmlsettings,"/all/do_palm_filter")),side="both"))
do_tapercorr <- as.logical(str_trim(xml_text(xml_find_all(xmlsettings,"/all/do_tapercorr")),side="both"))
do_total_ba <- as.logical(str_trim(xml_text(xml_find_all(xmlsettings,"/all/do_total_ba")),side="both"))
do_sp_abund <- as.logical(str_trim(xml_text(xml_find_all(xmlsettings,"/all/do_sp_abund")),side="both"))
do_sp_ba <- as.logical(str_trim(xml_text(xml_find_all(xmlsettings,"/all/do_sp_ba")),side="both"))
do_spp_grate <- as.logical(str_trim(xml_text(xml_find_all(xmlsettings,"/all/do_spp_grate")),side="both"))
do_plot_spp_grate <- as.logical(str_trim(xml_text(xml_find_all(xmlsettings,"/all/do_plot_spp_grate")),side="both"))
do_spp_mrate <- as.logical(str_trim(xml_text(xml_find_all(xmlsettings,"/all/do_spp_mrate")),side="both"))
do_plot_spp_mrate <- as.logical(str_trim(xml_text(xml_find_all(xmlsettings,"/all/do_plot_spp_mrate")),side="both"))
do_spp_rec <- as.logical(str_trim(xml_text(xml_find_all(xmlsettings,"/all/do_spp_rec")),side="both"))
do_grate <- as.logical(str_trim(xml_text(xml_find_all(xmlsettings,"/all/do_grate")),side="both"))
do_mrate <- as.logical(str_trim(xml_text(xml_find_all(xmlsettings,"/all/do_mrate")),side="both"))
do_stem_abund <- as.logical(str_trim(xml_text(xml_find_all(xmlsettings,"/all/do_stem_abund")),side="both"))
do_plot_stem_abund <- as.logical(str_trim(xml_text(xml_find_all(xmlsettings,"/all/do_plot_stem_abund")),side="both"))
do_stem_ba <- as.logical(str_trim(xml_text(xml_find_all(xmlsettings,"/all/do_stem_ba")),side="both"))
do_plot_stem_ba <- as.logical(str_trim(xml_text(xml_find_all(xmlsettings,"/all/do_plot_stem_ba")),side="both"))
do_plot_am25_argr95 <- as.logical(str_trim(xml_text(xml_find_all(xmlsettings,"/all/do_plot_am25_argr95")),side="both"))
do_plot_sm25_srgr95 <- as.logical(str_trim(xml_text(xml_find_all(xmlsettings,"/all/do_plot_sm25_srgr95")),side="both"))
do_dbh_dist <- as.logical(str_trim(xml_text(xml_find_all(xmlsettings,"/all/do_dbh_dist")),side="both"))
plot_dbh_dist <- as.logical(str_trim(xml_text(xml_find_all(xmlsettings,"/all/plot_dbh_dist")),side="both"))
do_demog_tables <- as.logical(str_trim(xml_text(xml_find_all(xmlsettings,"/all/do_demog_tables")),side="both"))
do_classify_pfts <- as.logical(str_trim(xml_text(xml_find_all(xmlsettings,"/all/do_classify_pfts")),side="both"))
diameter_classes_cm <- as.double(strsplit(str_trim(xml_text(xml_find_all(xmlsettings,"/all/diameter_classes_cm")),side="both"),",")[[1]])


# Derived parameters, initilizations and constants
# =======================================================================================
# the number of square meters in a hectare
hatom2 <- 10000 
plot_dim

# The size of the plot in hectares
plotsize <- prod(plot_dim) / hatom2


# Load the core census and save it to an arbitrary list
# ========================================================================================

print(paste("Finding", site.name, "rtables"))
files <- list.files(path = path_to_observational_data, pattern = paste(site.name, ".", ctfs_table_type, 
                                                       sep = ""), full.names = TRUE)


ncens <- length(files)
if (ncens < 2) {
  stop("Not enough censuses, Rtables should be in the same folder as this driver script. Go collect more data!")
}
cens_fnames <- vector("list", length = ncens)  # Census dataset
cnsdata <- vector("list", length = ncens)  # Census dataset
for (i in seq_len(ncens)) {
  print(paste("Loading census", i, "of", ncens))
  cens_fnames[i] <- load(files[i])
  cnsdata[[i]] <- get(cens_fnames[[i]])
}
remove(cens_fnames)


# Set the species master list
# =======================================================================================
spc_master_list <- sort(unique(cnsdata[[1]]$sp))


# Convert the DBH to cm if the data is in mm
# =======================================================================================

if (unit.dbh == "mm") {
  for (i in seq_len(ncens)) {
    print(paste("Converting dbh to cm in census", i, "of", ncens))
    cnsdata[[i]]$dbh <- cnsdata[[i]]$dbh * 0.1
  }
}



# Remove species that are irrelevant to tree demographics (palms
# and figs)  (NOTE, WE WILL WANT TO RE-ADDRESS THIS FILTER AND OMIT FOR DIFFERENT,
# ANALYSIS, POTENTIAL SOLUTION IS TO KEEP ORIGINAL CENSUS AND A FILTERED FOR DIFF STUFF)
# =======================================================================================

if (do_palm_filter) {
  source("benchmarking/filter_spcs.r")
  #  err_log <- checkUsageErr(filter_spcs,err_log)
  cnsdata <- filter_spcs(cnsdata, ncens, plot_spp)
}


# Apply taper corrections
# =======================================================================================

if (do_tapercorr) {
  source("benchmarking/taper_correction.r")
  #  err_log <- checkUsageErr(taper_correction,err_log)
  cnsdata <- taper_correction(cnsdata, site.name, ncens)
}


# Calculate individuals' basal areas and the minimum dbh across all census 
#(min of the mins), BA's are at ref height
# =============================================================================
# CTFS protocol for dbh is a minimum of 1cm
min_dbh <- 1
for (i in seq_len(ncens)) {
  print(paste("Calculating BA in census", i, "of", ncens))
  cnsdata[[i]]$ba <- ba(cnsdata[[i]]$dbh, "cm")
  # The min_dbh calculation is turned off in favor of using CTFS protocol of minimum dbh of 1cm
  # Calculate minimum dbh 
  #min_dbh <- min(min_dbh, min(cnsdata[[i]]$dbh, na.rm = TRUE))
}

# Calculate species level traits that can be diagnosed from core-census
# =======================================================================================
# names: the species code (string) dbh95: dbh at 95th percentile srgrXX:
# relative growth rate for saplings, (log(dbh2/dbh1)/dtime), XX percentile
# agrgXX: relative growth rate for trees, (log(dbh2/dbh1)/dtime), XX
# percentile sm25: mortality rate for saplings who have srgr <= srgr25 am25:
# mortality rate for trees who have argr <= argr25 Note- mortality and rgr
# will also have upper and lower conf limits (see parameter climit, that is
# the one-sided conf, ie 0.05 will do 0.05-0.95)
climit <- 0.05
spc <- list()
spc$names <- spc_master_list
spc$valid <- rep(TRUE, times = length(spc$names))

# calculate 95% DBH for all species
# =======================================================================================

source("benchmarking/spc_classify_dbh95.r")
#err_log <- checkUsageErr(spc_classify_dbh95,err_log)
spc <- spc_classify_dbh95(spc, cnsdata)


# If a dbh95 is not available, that makes the species pretty much unusable
spc$valid[!(spc$dbh95 > 0)] <- FALSE


# calculate relative growth rates
# =======================================================================================

# source("benchmarking/spc_classify_rgr.r")
# #err_log <- checkUsageErr(spc_classify_rgr,err_log)
# 
# #undebug(spc_classify_rgr)
# 
# spc <- spc_classify_rgr(cnsdata, ncens, spc, large_stature_dbh95, sapling_dbh, tree_dbh, climit, site.name)
# 
# 
# # calculate mortality rates of slow growing individuals (returns saplings,
# # trees and CI's)
# # =======================================================================================
# 
# source("benchmarking/M25.r")
# #err_log <- checkUsageErr(mort25,err_log)
# spc <- mort25(cnsdata, ncens, spc, large_stature_dbh95, sapling_dbh, tree_dbh, climit)
# 
# 
# # Run a PFT classification algorithm
# # =======================================================================================
# if (do_classify_pfts) {
#   source("benchmarking/classify_pfts.r")
#   spc <- classify_pfts(spc,cnsdata,ncens,diameter_classes_cm,plot_spp)
#   }else{
#   spc$ipft <- rep(1, length(spc$names))   # Trivial, one PFT
# }

# Evaluate the m25 rgr95 space, and filter only most abundant species
# ==========================================================================

# if (do_plot_sm25_srgr95) {
#   source("benchmarking/fgcb_plot_utils.r")
#   plot_srgr95_vs_sm25(spc)
# }
# 
# if (do_plot_am25_argr95) {
#   plot_argr95_vs_am25(spc)
# }





# Basic demography functions for size and species
# =======================================================================================

if(do_spp_grate | do_spp_mrate | do_spp_rec | do_grate | do_mrate | do_stem_abund | do_stem_ba | do_total_ba | do_sp_abund | do_sp_ba){
  source("benchmarking/basic_demography.r")
  if (do_spp_grate) {
    sp.Glist<-sp.grate(cnsdata, large_only, ncens, diameter_classes_cm, min_dbh)
  }
  if (do_spp_mrate) {
    sp.Mlist<-sp.mrate(cnsdata, large_only, ncens, diameter_classes_cm, min_dbh)
  }
  if (do_spp_rec) {
    sp.Rlist_mindbh <- sp.rec(cnsdata, large_only, ncens, min_dbh)
    sp.Rlist_10cm   <- sp.rec10(cnsdata, large_only, ncens)
  }
  if (do_grate){
    Glist <- grate(cnsdata, large_only, ncens, diameter_classes_cm, min_dbh) 
  }
  if (do_mrate){
    Mlist <- mrate(cnsdata, large_only, ncens, diameter_classes_cm, min_dbh)
  }
  if (do_stem_abund){
    Stemlist<-stem_abund(cnsdata, large_only, ncens, diameter_classes_cm, min_dbh, plot_stem_abund) 
  }
  if (do_stem_ba){
    StemBAlist<-stem_ba(cnsdata, large_only, ncens, diameter_classes_cm, min_dbh, plot_stem_ba)
  }
  if (do_total_ba){
    TotBA<-tba(cnsdata, large_only, ncens)
  }
  if (do_sp_abund){
    sp.Alist<-sp_abund(cnsdata, large_only, ncens)
  }
  if (do_sp_ba){
    sp.BAlist <- sp_ba(cnsdata, large_only, ncens)
  }
}




#don't care as much about stuff below this point


















# 
# 
# 
# 
# # Calcuate two-parameter Weibull diameter distribution for each census
# #========================================================================================
# 
# if(do_dbh_dist){
#   source("dbh_dist.R")
#   if(large_only){
#     dbh.dist<-dbh_dist(cn, ncens, diameter_classes_cm, plot_dbh_dist)
#   }
#   if(!large_only){
#     dbh.dist<-dbh_dist(cnsdata, ncens, diameter_classes_cm, plot_dbh_dist)
#   }
# }
# 
# 
# # Make demography tables for growth, mortality and recruitment and save them to text files
# #===========================================================================================
# 
# if(do_demog_tables){
#   source("make_tables.R")
#   
#   demog_tables<-make.tables(cnsdata, ncens, sp.Glist, sp.Mlist, sp.Rlist_mindbh, sp.Rlist_10cm, plot_spp, diameter_classes_cm)
#   
# }
# 
# 
# # Initialize Some Dimensions For the Output File
# # =======================================================================================
# 
# out_dims <- list()
# lastdate <- array('',dim=ncens)
# for (ic in seq(cnsdata)){ lastdate[ic] <- cnsdata[[ic]]$ExactDate[which.max(cnsdata[[ic]]$date)] }
# 
# 
# source("output_dimensions.r")
# #  err_log <- checkUsageErr(filter_spcs,err_log)
# # Note the passing of the unique species names (we don't sort because we just need the length here)
# out_dims <- output_dimensions(out_dims,ncens,unique(cnsdata[[1]]$sp))
# 
# # Initialize the "out_vars" list.  This is where we keep all diagnostics that have
# # been packaged into the correct array structure for netcdf IO
# # =======================================================================================
# out_vars <- list()
# 
# 
# # Pre-process species specific diagnostics to array format
# # =======================================================================================
# 
# source("pack_species_diagnostics.r")
# out_vars <- pack_species_diagnostics(out_vars,sp.Alist,sp.BAlist,sp.Glist,sp.Mlist,sp.Rlist_10cm,sp.Rlist_mindbh,spc_master_list)
# 
# 
# # Calculate benchmarks across trait and demographic axes
# # =======================================================================================
# 
# source("benchmarks_trait_dem.r")
# out_vars <- benchmarks_trait_dem(out_vars,cnsdata,spc,out_dims,ncens,plotsize)
# 
# 
# 
# # NETCDF file I/O
# # ========================================================================================
# 
# if(do_gen_ncdf4_outfile){
#   
#   # If the NETCDF4 output file is enabled, initialize the file and declare the name
#   ncfilename = paste('census_bmks_',trim(site.name),'_',format(Sys.time(), "%y%m%d"),'.nc', sep = "")
#   
#   source("create_ncdf_output.r")
#   spc_names <- sort(unique(cnsdata[[1]]$sp))
#   create_ncdf_output(ncfilename,out_dims,out_vars,spc_names,lastdate)
#   
# }
# 
# # Write CSS/PSS files.  These are text files that define the inventory in terms of
# # Cohorts and Patchs
# # Currently, with the "Type 1" pre-processing.  All plants are each their own cohort
# # and each quadrat is its own patch.
# # ========================================================================================
# if(do_gen_psscss_file){
#   source("gen_pss_css_type1.r")
#   for (i in seq_len(ncens)) {
#     timestamp <- max(cnsdata[[i]]$date,na.rm=TRUE)
#     pss_name<-paste(site.name,"_cens_",as.date(timestamp),"_","c",format(Sys.time(),"%Y-%m-%d"),".pss",sep="")
#     css_name<-paste(site.name,"_cens_",as.date(timestamp),"_","c",format(Sys.time(),"%Y-%m-%d"),".css",sep="")
#     print(paste('Generating PSS/CSS for census ending: ',as.date(timestamp)))
#     gen_pss_css_type1(cnsdata[[i]],pss_name,css_name)
#   } 
# }
# 
# 
# 
# # Evaluation in context of Anderson-Teixeira et al. 2004
# # running this function will provie the user with monthly climate variables from
# # the CRU database that can be used to relate to demographic rates
# # =======================================================================================
# 
# if (do_kat_clim_site) {
#   source("kat_clim_sites.r")
#   #  err_log <- checkUsageErr(kat_clim_sites,err_log)
#   clim <- kat_clim_sites(site.number)
# }
# 
# #print_scope_log(err_log)
# 
# 
