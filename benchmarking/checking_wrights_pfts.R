library(tidyverse)


  


  

# wright.pfts %>%
#   mutate(my.pft = case_when(
#     wsg >= 0.49 ~ "l",
#     wsg < 0.49 ~ "e"
#   )) %>%
#   mutate(pft = case_when(
#     pft %in% c("e","me")  ~ "e",
#     pft %in% c("l","ml")  ~ "l"
#   )) %>%
#   mutate(Latin = paste(g,s)) %>%
#   filter(Latin %in% pfts$Latin) %>%
#   mutate(agreed = pft != my.pft) %>% pull(agreed) %>% sum(na.rm = T) / nrow(wright.pfts)

#20% of species assignments are not consistent.

