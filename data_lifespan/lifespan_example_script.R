## Load all age-at-death results for each DGRP strain
## Stored as list objects
alcounts = readRDS("~/Documents/jin_2020_plosgen/data_lifespan/aad_alcounts.RDS")
drcounts = readRDS("~/Documents/jin_2020_plosgen/data_lifespan/aad_drcounts.RDS")

## Print all of the ages of death of for strain 25174 on AL diet
print(alcounts$`25174`)


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

## Summarize data into mean, median, and sd
summary = data.frame(almeans = unlist(lapply(alcounts, function(x) mean(x))))
summary$alsds = unlist(lapply(alcounts, function(x) sd(x)))
summary$drmeans = unlist(lapply(drcounts, function(x) mean(x)))
summary$drsds = unlist(lapply(drcounts, function(x) sd(x)))
summary$almed = unlist(lapply(alcounts, function(x) median(x)))
summary$drmed = unlist(lapply(drcounts, function(x) median(x)))

## Swap out strain code with DGRP ID
dgrp_key = read.csv("~/Documents/jin_2020_plosgen/data_lifespan/dgrp_key.csv")
row.names(summary) = dgrp_key$Genotype[match(row.names(summary), as.character(dgrp_key$Stock))]


## Save summary object for later
saveRDS(summary, file = "lifespan_summary.RDS")
