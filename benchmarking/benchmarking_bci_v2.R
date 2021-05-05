source('utils/supporting_funcs.R')

#loading data
path_to_benchmarking_data <- "~/cloud/gdrive/rec_submodel/data/observations/"
path_to_observations <- path_to_benchmarking_data
path_to_benchmarking_output <- "~/cloud/gdrive/rec_submodel/output/benchmarking/"

load(paste0(path_to_benchmarking_data,"bcifull.RData"))
load(paste0(path_to_benchmarking_data,"bci.full8.rdata"))
pfts <- read_csv('benchmarking/pft_assignments.csv')
M_area_per_sp <- read_csv('benchmarking/M_area_2010.csv') 

bci.tree8.ahb <- bci.tree8 %>% mutate(bid = 8) %>%
  select(bid,sp,status,dbh,date,ExactDate,treeID) %>%
  mutate_at(.vars = "bid",.funs = as.numeric)

bci.full.ahb <- bci.full %>%
  select(bid, sp,status,dbh,date,ExactDate,treeID) %>%
  mutate_at(.vars = "bid",.funs = as.numeric)

bci.full.ahb.1 <- bci.full.ahb %>%
  rbind(bci.tree8.ahb) %>%
  filter(bid > 5) %>%
  left_join(pfts, by = "sp") %>% 
  select(-X1) %>%
  mutate_at(.vars = "dbh",.funs = function(x){x/10}) %>% #convert to cm
  drop_na(Latin)


N1 <- bci.full.ahb.1 %>%
  filter(bid == 6, status == "A", dbh < 5) %>% nrow()
IDs_alive1 <- bci.full.ahb.1 %>%
  filter(bid == 6, status == "A", dbh < 5) %>% pull(treeID)

N2 <- bci.full.ahb.1 %>%
  filter(bid == 7, status == "A", dbh < 5) %>% nrow()
IDs_alive2 <- bci.full.ahb.1 %>%
  filter(bid == 7, status == "A", dbh < 5) %>% pull(treeID)

Ns2 <- sum(IDs_alive2 %in% IDs_alive1)

 
M_a <- M_area_per_sp %>%
  left_join(pfts, by = "sp") %>%
  drop_na(pft) %>%
  filter(M_per_m2 != Inf) %>%
  pull(M_per_m2) %>% sum()

rec_Kohyama <- function(Ma,Nt,Nst,N0,t){
  Ma*(Nt-Nst)*(N0/Nst)^(1/t) / (N0-Nst)
}

rec_Kohyama(Ma = M_a, Nt = N2, Nst = Ns2, N0 = N1,t = 5) * 1e4

R_benchmarking_driver <- read_csv(file = "benchmarking/R_2010_2015_benchmarking_driver.csv")

R_benchmarking_driver %>%
  left_join(pfts, by = "sp") %>%
  drop_na(pft)



