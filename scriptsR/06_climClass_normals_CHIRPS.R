
ss <- load('C:/temp/data/ans521DatesRowsx16685PixCols_mmDF.RData')
ss <- load('C:/temp/data/ans15856DatesRowsx16685PixCols_dateDF_posDataRast.RData')
# ansMonth, mmDF, posDataRast, mtr, trnd,

mmDF[1:5, 1:5]
ansMonth[1:5, 1:5]

library(ClimClass)

data(Trent_climate)
class(lista_cli)
class(lista_cli[[1]])
head(lista_cli[[1]])
# clima_81_10 is a list of data frames of the type series, 
# each one referring to one station 
# having climatic means of temperature and precipitation 

clima_81_10<-lapply(lista_cli, FUN=climate, first.yr=1981, last.yr=2010, max.perc.missing=15)

sx <- apply(ansMonth, 2, function(x){
  (data.frame(year = substr(x = mmDF$mm, 0, 4), 
                  month = as.numeric(substr(x = mmDF$mm, 6, 7)), P = x))
})


identical(sx[[1]]$P, ansMonth[, 1])

system.time(clim80_10 <-lapply(sx, FUN=climate, first.yr=1981, last.yr=2010, max.perc.missing=15))
system.time(clim90_20 <-lapply(sx, FUN=climate, first.yr=1991, last.yr=2020, max.perc.missing=15))
system.time(clim80_24 <-lapply(sx, FUN=climate, first.yr=1981, last.yr=2024, max.perc.missing=15))

save(clim80_10, clim90_20, clim80_24, file = 'C:/temp/data/climate_normals_chirps.RData')
rastid <- rast('C:/temp/data/clip_sinchi_ID_16685.tif')

head(clim80_10[[1]])

climA <- data.frame(lapply(clim80_10, function(x){
  #x = clim80_10[[1]]
  unlist(x$P) 
}))

climB <- data.frame(lapply(clim90_20, function(x){
  #x = clim80_10[[1]]
  unlist(x$P) 
}))

climC <- data.frame(lapply(clim80_24, function(x){
  #x = clim80_10[[1]]
  unlist(x$P) 
}))


colnames(climA) <- colnames(climB) <- colnames(climC) <- posDataRast
outDir <- 'C:/temp/data/normales_chirps/'

rng <- range(c(range(climA), range(climB), range(climC)))
par(mfrow = c(4,3), mar = c(1.5, 1.5, 1, 1))
for (i in 1:12){ # i = 1
  climR <- rastid * 0
  climR[posDataRast] <- as.numeric(climA[i, ])
  plot(climR, main = paste('1981-2010: ', i), 
       range = rng)
#   legend('topleft', legend = paste('1981-2010: ', i))
  # writeRaster(climR, file = paste0(outDir, '/1981-2010_',i,'.tif'))
  # 
  # 
  # climR <- rastid * 0
  # climR[posDataRast] <- as.numeric(climB[i, ])
  # writeRaster(climR, file = paste0(outDir, '/1991-2020_',i,'.tif'))
  # 
  # 
  # climR <- rastid * 0
  # climR[posDataRast] <- as.numeric(climC[i, ])
  # writeRaster(climR, file = paste0(outDir, '/1981-2024_',i,'.tif'))
  
}

  ####
aM <- apply(climA, 1, median)
aC <- apply(climA, 1, mean)
matplot(rownames(climA), climA, type='l', xlab='Months',
        ylab='mm', col=adjustcolor('grey', alpha.f = 0.1))
lines(1:12, aM, col = 'red', lwd = 2)
lines(1:12, aC, col = 'blue', lwd = 2, lty = 2)

####
bM <- apply(climB, 1, median)
bC <- apply(climB, 1, mean)
matplot(rownames(climB), climB, type='l', xlab='Months',
        ylab='mm', col=adjustcolor('grey', alpha.f = 0.1))
lines(1:12, bM, col = 'red', lwd = 2)
lines(1:12, bC, col = 'blue', lwd = 2, lty = 2)


####

cM <- apply(climC, 1, median)
cC <- apply(climC, 1, mean)
matplot(rownames(climC), climC, type='l', xlab='Months',
        ylab='mm', col=adjustcolor('grey', alpha.f = 0.1))
lines(1:12, cM, col = 'red', lwd = 2)
lines(1:12, cC, col = 'blue', lwd = 2, lty = 2)

##

