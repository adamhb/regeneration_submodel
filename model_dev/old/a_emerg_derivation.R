#derivation of a_emerg, the mean emergence when light and moisture are not limiting


# a_germ_LD <- germ_data %>%
#   left_join(pfts, by = "Latin") %>%
#   drop_na() %>%
#   group_by(pft) %>%
#   summarise(germ = mean(germ)) %>%
#   filter(pft %in% c("LD_DI","LD_DT")) %>% pull(germ) %>% mean() %>% `/` (100*60)
# 
# a_germ_ST <- germ_data %>%
#   left_join(pfts, by = "Latin") %>%
#   drop_na() %>%
#   group_by(pft) %>%
#   summarise(germ = mean(germ), n = length(germ)) %>%
#   filter(pft %in% c("ST_DI","ST_DT")) %>% pull(germ) %>% mean() %>% `/` (100*60)


0.007 = a * (1/(-1471 * 1e-5)*-1) ^ 1.6