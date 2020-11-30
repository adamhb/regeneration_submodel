library(tidyverse)
library(magrittr)
library(reshape2)

path_to_observational_data <- "~/cloud/gdrive/rec_submodel/data/observations/"
source("benchmarking/assigning_pfts.R")

#post-1989 shade index is for 0.5m above the ground
seed_dyn <- readRDS(paste0(path_to_observational_data,"bci_seeding_data_for_Adam.RDS"))
pfts <- pfts_nov_2018

load(paste0(path_to_observational_data,"bci.spptable.rdata"))

# solar_bci <- read.csv("daily_solar_insolation_bci.csv")
# pfts2 <- read.csv("wright_spring_2018.csv")

#creating a serial number for each observation
seed_dyn$OBnum <- seq(1:length(seed_dyn$TAGF))

#adding in the real light values to the seedling dynamics data using the RI3 numbers
seed_dyn$start.date <- as.Date(seed_dyn$start, origin = as.Date("2001-1-15", format = "%Y-%m-%d"))
seed_dyn$stop.date <- as.Date(seed_dyn$stop, origin = as.Date("2001-1-15", format = "%Y-%m-%d"))


#creating a variable in the seedling dynamics data to indicate a recruit
R <- as.numeric(as.numeric(seed_dyn$d) >= 10)
R[is.na(R)] <- 0
seed_dyn$R <- R

#adding pfts to the seedling dynamics data
#cleaning the PFT data and creating lists of early PFTs vs. late PFTs.


sum_N <- seed_dyn %>%
  mutate(sp = tolower(SPP)) %>%
  left_join(pfts, by = "sp") %>%
  drop_na(pft) %>%
  group_by(pft) %>%
  summarise(meanTR = mean(R),
            n = length(R)) %>% pull(n) %>% sum()


#daily transition rates per PFT
daily_trs <- seed_dyn %>%
  mutate(sp = tolower(SPP)) %>%
  left_join(pfts, by = "sp") %>%
  drop_na(pft) %>%
  group_by(pft,census) %>%
  summarise(totalR = sum(R), n = length(R),
            time = mean(stop.date - start.date)) %>% 
  mutate(TRperDay = totalR / n / as.numeric(time)) %>%
  group_by(pft) %>%
  summarise(TR = mean(TRperDay), n = mean(n)) %>%
  mutate(pft1 = substr(pft,start = 1,stop = 2)) %>%
  group_by(pft1) %>%
  summarise(TR = mean(TR))

a_rec_com_LD <- daily_trs$TR[1]/920
a_rec_com_ST <- daily_trs$TR[2]/92


# 
# 
# 
# pfts$sp <- paste0(pfts$g," ",pfts$s)
# names(pfts)[9] <- "Latin"
# sp_code <- bci.spptable[,c(1,2)]
# pfts <- merge(sp_code, pfts, by = "Latin")[,-c(3,4)]
# Early <- pfts[pfts$pft == "e",]$sp
# Late <- pfts[pfts$pft == "l",]$sp
# 
# 
# #Checking that pft designated species are in the seedling dynamcis plots
# pfts$SPP <- toupper(pfts$sp)
# seed_spp <- unique(seed_dyn$SPP)
# pfts$SPP %in% seed_spp
# 
# 
# #adding pft factor levels to the seedling dynamimcs data
# seed_dyn$pft <- rep("unk", length(seed_dyn$TAGF))
# seed_dyn[seed_dyn$SPP %in% pfts[pfts$pft == "e",]$SPP,]$pft <- "e"
# seed_dyn[seed_dyn$SPP %in% pfts[pfts$pft == "l",]$SPP,]$pft <- "l"
# seed_dyn[seed_dyn$SPP %in% pfts[pfts$pft == "ml",]$SPP,]$pft <- "ml"
# seed_dyn[seed_dyn$SPP %in% pfts[pfts$pft == "me",]$SPP,]$pft <- "me"
# 
# seed_dyn$pft <- factor(seed_dyn$pft, levels = c("e", "me", "ml", "l", "unk"))
# 
# 
# #removing observations where a recruit was censused more than once
# rec_dupes <- seed_dyn %>%
#   select(OBnum, TAGF, census, start, stop, Q20P5, x, y, h, d, R, mort, shd, dmin, ra, ra.d) %>%
#   arrange(TAGF, census) %>%
#   filter(R == 1) %>%
#   mutate(dupes = duplicated(TAGF)) %>%
#   filter(dupes == TRUE) %>%
#   .$OBnum
# 
# seed_dyn <- seed_dyn[!seed_dyn$OBnum %in% rec_dupes,]
# 
# 
# 
# 
# 







