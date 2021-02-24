l <- 0:1000
aLD <- 1.771e-5
aST <- 4.1e-5

rf <- function(l){
  aLD*l^1.0653
}

rf2 <- function(l){
  aST*l^0.8615
}




tibble(l = l, r = rf(l), pft = "LD") %>%
  rbind(tibble(l = l, r = rf2(l),pft = "ST")) %>%
  ggplot(aes(l,r,color=pft)) +
  geom_line() +
  adams_theme + 
  scale_x_continuous(limits = c(20,100)) +
  scale_y_continuous(limits = c(0,0.0015))


b <- c(1.0653, 0.8615)
names(b) <- c("LD","ST")

rf2 <- function(lam, l, l_avg, b){
  lam + lam * ((l/l_avg)-1)^b
}

ST <- rf2(lam = 0.0014,l = l, l_avg = 60, b = b["ST"])
LD <- rf2(lam = 0.0014,l = l, l_avg = 60, b = b["LD"])

rf2(lam = 0.0014, l_avg = 60, l = 60, b = b["LD"])

tibble(l = l, r = ST, pft = "ST") %>%
  rbind(tibble(l = l, r = LD, pft = "LD")) %>%
  ggplot(aes(l,r,color=pft)) +
  geom_line() +
  adams_theme +
  scale_x_continuous(limits = c(20,100))


