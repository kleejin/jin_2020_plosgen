## Load all age-at-death results for each DGRP strain
alcounts = readRDS("~/Documents/jin_2020_plosgen/data_lifespan/aad_alcounts.RDS")
drcounts = readRDS("~/Documents/jin_2020_plosgen/data_lifespan/aad_drcounts.RDS")

## Load DGRP key
dgrp_key = read.csv("~/Documents/jin_2020_plosgen/data_lifespan/dgrp_key.csv")

## Script for generating lifespan curves for each strain
par(mfrow = c(6,4), mar = c(2, 2, 2, 1))
for (strain in unique(names(alcounts))){
  al.count = plyr::count(alcounts[[strain]])
  al.count$cumdeath = cumsum(al.count$freq)
  al.count$percent = al.count$cumdeath / sum(al.count$freq)
  dr.count = plyr::count(drcounts[[strain]])
  dr.count$cumdeath = cumsum(dr.count$freq)
  dr.count$percent = dr.count$cumdeath / sum(dr.count$freq)
  
  ## Red == DR
  ## Black == AL
  plot(1 - percent ~ x, data = dr.count, type = "l", lwd = 2, col = "red", #cex.lab = 1.5, cex.axis = 1.5,
       xlab = "Age", ylab = "% Survival", main = strain)
  lines(1 - percent ~ x, data = al.count, lwd = 2)
}
