library(R.utils)
library(gdalUtilities)
library(terra)
library(sf)


dateRange <- seq.Date(from = as.Date('1981-01-01'), to = as.Date('2024-05-30'), by = 1)
dateRange <- gsub(pattern = '-', '\\.', dateRange)
length(dateRange)

outDir <- 'C:/temp/data/CHIRPS_day_mask'
polPath <- 'H:/gis-data/CHIRPS/SINCHI_Region_100K_WGS84_buff5500m.shp'
polVect <- terra::vect(polPath)
setwd(outDir)
avTifs <- list.files(pattern = '.tif$', path = outDir)
# roi <- terra::rast(avTifs[1]) + 100
# roi <- terra::rast('C:/temp/data/sinchi2/1981.01.02.tif') + 100
# plot(roi)
# writeRaster(roi, 'C:/temp/data/clip_sinchi-plus100.tif', overwrite = TRUE)
# gdalwarp(srcfile = 'C:/temp/data/clip_sinchi-plus100.tif',
#          dstfile = 'C:/temp/data/clip_sinchi_5500mT.tif',
#          crop_to_cutline = TRUE,
#          dstnodata = 'None',
#          overwrite = TRUE,
#          cutline = polPath)

rastid <- msk <- rast('C:/temp/data/clip_sinchi_5500mT.tif')
plot(msk)
rastid[rastid[] == 0] <- NA
plot(rastid)
length(posDataRast)
# rastid[!is.na(rastid[])] <- 1:length( posDataRast ) #15689
#writeRaster(rastid, 'C:/temp/data/clip_sinchi_ID_16685.tif')
rastid <- rast('C:/temp/data/clip_sinchi_ID_16685.tif')
posDataRast <- which(!is.na(rastid[]))
plot(rastid)

length(dateRange)

ans <- matrix(NA, nrow = length(dateRange), ncol = length(posDataRast) ); format(object.size(ans), units = 'Mb')
dateDF <- data.frame(ymd = dateRange, done = 0)
head(dateDF)
dim(ans)

avTifs <- list.files(pattern = '.tif$', path = outDir)
length(avTifs)
j <- 1
for(i in j:nrow(dateDF)){ # i = 1
    # i = 8031 8049 8086
  (intif <- paste0(outDir, '/', dateDF$ymd[i], '.tif'))
  if (file.exists(intif) & dateDF$done[i] == 0){
    print(i)
    print(intif) 
    ri <- tryCatch(terra::rast(intif), error = function(e) NULL)
    # plot(ri)
    if (is.null(ri)) { next }
    #rv <- ri[]
    # summary(rv)
    vecti <- tryCatch(as.numeric(ri[posDataRast][, 1]), error = function(e) NULL)
    if (is.null(vecti)) next
    # str(vecti)
    ans[i, ] <- vecti
    dateDF$done[i] <- 1
    cat(' -- Fill -- ')
    cat(i, nrow(dateDF), '\n')
  } #else { cat(' -- Missing --') }
  j <-  i-10
}
#j = 8032
#8030

table(dateDF$done)
dim(ans)
save(ans, dateDF, posDataRast, file = 'C:/temp/data/ans15856DatesRowsx16685PixCols_dateDF_posDataRast.RData')
columns_sums <- colSums(ans)
csR <- rastNA <- rastid * NA
csR <- rastNA
csR[posDataRast] <- columns_sums
plot(csR, main = 'Total rainfall')

statx <- apply(ans, 2, sd)
csR <- rastNA <- rastid * NA
csR[posDataRast] <- statx
plot(csR, main = 'Standar deviation')

cm <- colMeans(ans)
csR <- rastNA <- rastid * NA
csR[posDataRast] <- cm
plot(csR, main = 'Average')

mx <-  apply(ans, 2, max)
csR <- rastNA <- rastid * NA
csR[posDataRast] <- mx
plot(csR, main = 'Maximun')
