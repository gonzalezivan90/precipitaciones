# source('N:/Mi unidad/sinchi_precipitaciones/src/crop_by_polygon.R')
2
library(R.utils)
library(gdalUtilities)
library(terra)
library(sf)


#chirpsPath <- 'H:/gis-data/CHIRPS/world_day/gz_world/'
chirpsPath <- 'L:/gz_world/'
chirpsPath2 <- 'L:/gz_world2/'

# polOrig <- terra::vect('H:/gis-data/CHIRPS/SINCHI_Region_100K.shp')
# plot(polOrig)
# tifOrig <- terra::rast('H:/gis-data/CHIRPS/world_day/chirps-v2.0.2023.08.25.tif')
# pol <- terra::project(polOrig, "epsg:4326")
# plot(pol)
# plot(tifOrig, add = TRUE)
# plot(pol, add = TRUE)
# polPath <- 'H:/gis-data/CHIRPS/SINCHI_Region_100K_WGS84.shp'
polPath <- 'H:/gis-data/CHIRPS/SINCHI_Region_100K_WGS84_buff5500m.shp'
# polVect <- terra::vect(polPath)
# terra::writeVector(pol, filename = polPath)
urlbase <- 'https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_daily/tifs/p05/'

dateRange <- seq.Date(from = as.Date('1981-01-01'), to = as.Date('2024-05-30'), by = 1)
dateRange <- gsub(pattern = '-', '\\.', dateRange)

outDir <- 'C:/temp/data/sinchi2/'; dir.create(outDir, recursive = TRUE)
setwd(outDir)

datevect <- 1:length(dateRange)
datevect <- rev(datevect)
datevect <- sample(datevect)
#datevect <- 12546:7000
# datevect <- 12000:length(dateRange)
#while(!all(file.exists(tifs))){
for(i in datevect ){ # i = 1
  (clipTif <- paste0(outDir, '/', dateRange[i], '.tif'))
  (itif <- paste0(chirpsPath, '/chirps-v2.0.', dateRange[i], '.tif'))
  if( !file.exists(clipTif) ){
    (igz <- paste0(chirpsPath, '/chirps-v2.0.', dateRange[i], '.tif.gz'))
   
     if( !file.exists(igz) ){
      yy <- substr(dateRange[i], 0, 4)
      (urlx <- paste0(urlbase, '/', yy, '/chirps-v2.0.', dateRange[i], '.tif.gz'))
    ## Only download --
      print(i)
      tryCatch(download.file(url = urlx, destfile =  igz), error = function(e) NULL)
    }
    
    if ( !file.exists(itif) & file.exists(igz) ){
      tryCatch(R.utils::gunzip(filename = igz, overwrite=TRUE, 
                               destname = itif),error = function(e) NULL) # outtif
    }
    
    
    tryCatch(gdalwarp(srcfile = itif, dstfile = clipTif, 
                         crop_to_cutline = TRUE,
                         dstnodata = 'None',
                         overwrite = TRUE,
                         cutline = polPath) ,error = function(e) NULL)
    #rx <- terra::rast(clipTif)
    # plot(rx)
    # plot(polVect , border = 2, add = TRUE)
    # -dstnodata None 
    # -cutline /media/aoi/mask_2.shp 
    # -crop_to_cutline 
    print(i)
  }
  if ( file.exists(clipTif) ){
    file.remove(itif)
  }
  # cat(i, itif, '\n')
}
2

  
# 
# length(alrdy <- list.files(path = outDir))
# dats <- sapply(alrdy, 
#                function(x) {substr(x = as.character(file.info(x)$ctime), 09, 15)})
# head(dats)
# tabDates <- table(unname(grep('^0', dats, value = TRUE)))
# plot(tabDates[180:268], type = 'l')
# plot(table((grep('^0', dats, value = TRUE))), type = 'l')
# 
# plot(table(dats), type = 'l')
# head(table(dats), 100)
# length(alrdy)/length(lf)*100