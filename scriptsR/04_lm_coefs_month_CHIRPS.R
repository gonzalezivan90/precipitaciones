


laod('C:/temp/data/ans521DatesRowsx16685PixCols_mmDF_posDataRast_mtr_trnd.RData')
# ansMonth, mmDF, posDataRast, mtr, trnd, 

rastid <- rast('C:/temp/data/clip_sinchi_ID_16685.tif')

plot(rastid)
points(xyFromCell(object = rastid, cell = 188), col = 2, cex = 2)
colVi <- viridisLite::viridis(ncol(ansMonth), alpha = 0.3)
plot(rep(1, length(colVi)), col = colVi)

pptRng <- range(ansMonth)

str(mmDF)
plot(x = mmDF$D, y = mmDF$n0, xlim = range(mmDF$D), ylim = pptRng,type = 'n', xlab = '', ylab = '')
vecti <- 1:ncol(ansMonth)
vecti <- sample(ncol(ansMonth))
for (i in vecti){ # i = 1
  lines(mmDF$D, ansMonth[, i], col = colVi[i])
}


par(mfrow = c(4, 3), mar = c(1.2, 1.2, .5, .5))
m2i <- c(paste0('0',1:9), 10:12)
for (j in m2i){ # j = '01'
  pos <- which(mmDF$m2 == j)
  plot(x = mmDF$D, y = mmDF$n0, xlim = range(mmDF$D), ylim = pptRng,type = 'n', xlab = '', ylab = '')
  legend('topleft', legend = j)
  for (i in vecti){ # i = 1
    vecti <- 1:ncol(ansMonth)
    vecti <- sample(ncol(ansMonth))
    lines(mmDF$D[pos], ansMonth[pos, i], col = colVi[i])
  }
}

system.time({
  coefsM <- sapply(m2i, function(x){ # x = '01'
    pos <- which(mmDF$m2 == x)
    mmx <- mmDF$D[pos]
    coefsA <- apply(ansMonth[pos, ], 2, function(y){
      #str(dm)
      # y <- ansMonth[pos, 1]
      lmm <- lm(y ~ mmx)
      coef(lmm)[2]
    })
  })
})  

system.time({
  signM <- sapply(m2i, function(x){ # x = '01'
    pos <- which(mmDF$m2 == x)
    mmx <- mmDF$D[pos]
    coefsA <- apply(ansMonth[pos, ], 2, function(y){
      #str(dm)
      # y <- ansMonth[pos, 1]
      lmm <- lm(y ~ mmx)
      #coef(lmm)[2]
      summary(lmm)$coefficients['mmx', 'Pr(>|t|)']
    })
  })
})  

rx <- max(abs(range(coefsM)))
pal <- leaflet::colorNumeric(palette = "RdBu", domain=c(-rx, rx), reverse = T)
b <- seq(-rx, rx, 0.001)

dim(coefsM)
par(mfrow = c(3,4), mar = c(1, 1, .5, .5))
for(x in 1:ncol(coefsM)){ # x = 1
  rastM <- rastid*0
  rastM[posDataRast] <- coefsM[, x]
  plot(rastM, range = c(-rx, rx), col=pal(b), main = colnames(coefsM)[x])
  #writeRaster(rastM, filename = paste0('C:/temp/data/resultados_coefs/chirps_coefs_',colnames(coefsM)[x],'.tif'))
  # legend('topleft', legend = colnames(coefsM)[x])
}


rx <- max(abs(range(signM)))
b <- c(0, 0.05, 0.1)
# b <- seq(-rx, rx, 0.1)
pal <- leaflet::colorNumeric(palette = "RdBu", domain=b, reverse = T)

dim(signM)
par(mfrow = c(3,4), mar = c(1, 1, .5, .5))
for(x in 1:ncol(signM)){ # x = 1
  rastM <- rastid*0
  rastM[posDataRast] <- signM[, x]
  plot(rastM, type="interval", breaks = b, col=pal(b), main = colnames(signM)[x])
  plot(aoibuff, add = TRUE)
  #plot(trndR, type="interval", breaks=b, col=pal(b))
  writeRaster(rastM, filename = paste0('C:/temp/data/resultados_coefs/chirps_coefsSig_',colnames(signM)[x],'.tif'))
  # legend('topleft', legend = colnames(coefsM)[x])
}
