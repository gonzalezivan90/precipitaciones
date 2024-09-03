library(gdalUtilities)
library(terra)
library(sf)


dateRange <- seq.Date(from = as.Date('1981-01-01'), to = as.Date('2024-05-30'), by = 1)
dateRange <- gsub(pattern = '-', '\\.', dateRange)
length(dateRange)

polPath <- 'H:/gis-data/CHIRPS/SINCHI_Region_100K_WGS84.shp'
polVect <- terra::vect(polPath)
outDir <- 'C:/temp/data/CHIRPS_month/'; dir.create(outDir)
setwd(outDir)

load('C:/temp/data/ans15856DatesRowsx16685PixCols_dateDF_posDataRast.RData') # ans, dateDF, posDataRast
rastid <- rast('C:/temp/data/clip_sinchi_ID_16685.tif')
plot(rastid)
plot(polVect, add = TRUE)

nrow(dateDF); nrow(ans)
length(posDataRast); ncol(ans)

## Sums by month
dateDF$mm <- (substr(dateDF$ymd, 0, 7))
mmDF <- data.frame(mm = unique(dateDF$mm))
mmDF$min <- mmDF$max <- mmDF$ndays <- mmDF$n0 <- NA
ansMonth <- matrix(NA, ncol = ncol(ans), nrow = nrow(mmDF))
for (m in 1:nrow(mmDF)){ # m = 1
  (m.m <- mmDF$mm[m])
  pos.m <- which(dateDF$mm == m.m)
  mmVect <- apply(ans[pos.m, ], 2, sum)
  #str(mmVect)
  #mmR <- rastid * 0
  # plot(mmR)
  #mmR[posDataRast] <- mmVect
  
  ansMonth[m,  ] <- mmVect
  
  mmDF$min[m] <- min(mmVect)
  mmDF$max[m] <- max(mmVect)
  mmDF$ndays[m] <- length(pos.m)
  mmDF$n0[m] <- length( which(mmVect == 0) )
  
  # plot(mmR)
  #terra::writeRaster(mmR, filename = paste0(outDir, '/sum_',m.m, '.tif' ))
}

(rng <- round(range(mmDF$max, mmDF$min)/100)*100)
for (m in 1:nrow(mmDF)){ # m = 1
  (m.m <- mmDF$mm[m])
  rm <- terra::rast(paste0(outDir, '/sum_',m.m, '.tif' ))
  png(filename = paste0(outDir, '/sum_',m.m, '.png'), width = 824, height =  660, units = 'px')
  terra::plot(rm, main = m.m, range = rng)
  dev.off()
}

dim(ansMonth)

mmDF$D <-  as.Date(paste0(mmDF$mm, '.01'), format = '%Y.%m.%d')
mmDF$col <- rep(terrain.colors(12), 80)[1:nrow(mmDF)]
plot(x = mmDF$D, ansMonth[, 4], type = 'l', ylab = 'mm', xlab = 'Date',
     ylim = rng, xlim = as.Date(range(mmDF$D)))

lines(as.Date(dateDF$ymd, format = '%Y.%m.%d'), ans[, 4], col = 'red', pch = 20)
mmDF$m2 <- substr(x = mmDF$mm, start = 6, 7)

plot(x = mmDF$D, ansMonth[, 4], type = 'l', ylab = 'mm', xlab = 'Date',
     ylim = rng, xlim = as.Date(range(mmDF$D)))
points(mmDF$D, rep(0, nrow(mmDF)), col = mmDF$col, pch = 20)
abline(h = median(ansMonth[, 1]), col = 'blue')
abline(h = mean(ansMonth[, 1]), col = 'red')
abline(v = as.Date(c('1981-01-01', '1990-01-01', '2010-01-01', '2020-01-01')) , col = 'darkgreen', lwd = 2)

mtr <- sapply(c(paste0('0', 1:9), '10', '11', '12'), function(x){
  #x <- '02'
  selPos <- which(mmDF$m2 == x)
  dat <- ansMonth[selPos, 4]
  lmm <- lm(dat ~ selPos)
  coef(lmm)[2]
})



plot(x = (1:length(mmDF$D)), ansMonth[, 4], type = 'l', ylab = 'mm', xlab = 'Date',
     ylim = rng)
for(j in 1:length(mtr)){ # j = 1
  abline(a = mean(ansMonth[, 4]), b = mtr[j], col = j)
}


as.data.frame(mtr)
coef(lm(ansMonth[, 4] ~ mmDF$D))[2]

for (m in 1:nrow(mmDF)){ # m = 1
  (m.m <- mmDF$mm[m])
  rm <- terra::rast(paste0(outDir, '/sum_',m.m, '.tif' ))
  png(filename = paste0(outDir, '/sum_',m.m, '.png'), width = 824, height =  660, units = 'px')
  terra::plot(rm, main = m.m, range = rng)
  dev.off()
}

plot(density(ansMonth[, 4]), xlim = rng)
abline(v = median(ansMonth[, 1]), col = 'blue')
abline(v = mean(ansMonth[, 1]), col = 'red')

dm <- as.Date(mmDF$D)
trnd <- apply(ansMonth, 2, function(x){
  #x <- ansMonth[, 1]
  #str(x)
  #str(dm)
  lmm <- lm(x ~ dm)
  coef(lmm)[2]
})

signn <- apply(ansMonth, 2, function(x){
  #x <- ansMonth[, 1]
  #str(x)
  #str(dm)
  lmm <- lm(x ~ dm)
  summary(lmm)$coefficients['dm', 'Pr(>|t|)']
})


save(ansMonth, mmDF, posDataRast, mtr, trnd, 
     file = 'C:/temp/data/ans521DatesRowsx16685PixCols_mmDF_posDataRast_mtr_trnd.RData')


trndR <- rastid * 0
trndR[posDataRast] <- trnd
sigR <- rastid * 0
sigR[posDataRast] <- signn

plot(trndR)
str(trnd)
rx <- max(abs(range(trnd)))
pal <- leaflet::colorNumeric(palette = "RdBu", domain=c(-rx, rx), reverse = T)
b <- seq(-rx, rx, 0.001)
plot(trndR, type="interval", breaks=b, col=pal(b))

plot(sigR, type="interval", breaks=c(0, .05, .1))


writeRaster(trndR, 'C:/temp/data/trend_CHIRPS.tif')

posA <- as.Date('202')


rngA <- unique(substr(seq.Date(from = as.Date('1981-01-01'), to = as.Date('2010-12-31'), by = 1), 0, 7))
rngB <- unique(substr(seq.Date(from = as.Date('1991-01-01'), to = as.Date('2020-12-31'), by = 1), 0, 7))
rngC <- unique(substr(seq.Date(from = as.Date('1981-01-01'), to = as.Date('2020-12-31'), by = 1), 0, 7))
rngD <- unique(substr(seq.Date(from = as.Date('1981-01-01'), to = as.Date('2024-05-31'), by = 1), 0, 7))

mmDF$ym <- substr(mmDF$D, 0, 7)
head(mmDF)

# ncol = ncol(ans), nrow = nrow(mmDF)
avA <- apply(ansMonth[ mmDF$ym %in% rngA,  ], 2, mean)
avAR <- rastid * 0; avAR[posDataRast] <- avA; plot(avAR, main = 'AV: 1981-2010')
sdA <- apply(ansMonth[ mmDF$ym %in% rngA,  ], 2, sd)
sdAR <- rastid * 0; sdAR[posDataRast] <- sdA; plot(sdAR, range = sdRng, main = 'SD: 1981-2010')

avB <- apply(ansMonth[ mmDF$ym %in% rngB,  ], 2, mean)
avBR <- rastid * 0; avBR[posDataRast] <- avB; plot(avBR, range = avRng, main = 'AV: 1991-2020')
sdB <- apply(ansMonth[ mmDF$ym %in% rngB,  ], 2, sd)
sdBR <- rastid * 0; sdBR[posDataRast] <- sdB; plot(sdBR, range = sdRng, main = 'SD: 1991-2020')

avC <- apply(ansMonth[ mmDF$ym %in% rngC,  ], 2, mean)
avCR <- rastid * 0; avCR[posDataRast] <- avC; plot(avCR, range = avRng, main = 'AV: 1981-2020')
sdC <- apply(ansMonth[ mmDF$ym %in% rngC,  ], 2, sd)
sdCR <- rastid * 0; sdCR[posDataRast] <- sdC; plot(sdCR, range = sdRng, main = 'SD: 1981-2020')

avD <- apply(ansMonth[ mmDF$ym %in% rngD,  ], 2, mean)
avDR <- rastid * 0; avDR[posDataRast] <- avD; plot(avDR, range = avRng, main = 'AV: 1981-2024')
sdD <- apply(ansMonth[ mmDF$ym %in% rngD,  ], 2, sd)
sdDR <- rastid * 0; sdDR[posDataRast] <- sdD; plot(sdDR, range = sdRng, main = 'SD: 1981-2024')

avRng <- range(c(avA, avB, avC, avD))
sdRng <- range(c(sdA, sdB, sdC, sdD))
par(mfrow = c(4, 2))

plot(avAR-avBR)
plot(avAR-avCR)
plot(avAR-avDR)
plot(avAR-avCR)

plot(sdAR-sdBR)
plot(sdAR-sdCR)
plot(sdAR-sdDR)
plot(sdAR-sdCR)

daA <- density(avA)
daB <- density(avB)
daC <- density(avC)
daD <- density(avD)

dsA <- density(sdA)
dsB <- density(sdB)
dsC <- density(sdC)
dsD <- density(sdD)

plot(daA, xlim = range(c(daA$x, daB$x, daC$x, daD$x)), ylim = range(c(daA$y, daB$y, daC$y, daD$y)), main = 'Av')
lines(daB$x, daB$y, col = 2)
lines(daC$x, daC$y, col = 3)
lines(daD$x, daD$y, col = 4)
legend('topleft', col = 1:4, lwd = rep(2, 4), legend = c('1981-2010', '1991-2020', '1981-2020', '1981-2024'))

plot(dsA, xlim = range(c(dsA$x, dsB$x, dsC$x, dsD$x)), ylim = range(c(dsA$y, dsB$y, dsC$y, dsD$y)), main = 'SD')
lines(dsB$x, dsB$y, col = 2)
lines(dsC$x, dsC$y, col = 3)
lines(dsD$x, dsB$y, col = 4)
legend('topleft', col = 1:4, lwd = rep(2, 4), legend = c('1981-2010', '1991-2020', '1981-2020', '1981-2024'))



dim(ansMonth)

colVi <- viridisLite::viridis(ncol(ansMonth))

plot(rep(1, length(colVi)), col = colVi)
