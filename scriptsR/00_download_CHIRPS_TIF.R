# source('N:/Mi unidad/sinchi_precipitaciones/src/download_CHIRPS_TIF.R')
2

library(R.utils)
library(gdalUtilities)

urlbase <- 'https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_daily/tifs/p05/'
polPath <- 'H:/gis-data/CHIRPS/SINCHI_Region_100K_WGS84_buff5500m.shp'

#urlbase <- 'ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/global_daily/tifs/p05/2020/'
dateRange <- seq.Date(from = as.Date('1981-01-01'), to = as.Date('2024-05-30'), by = 1)
length(dateRange)

if( F ){
  dateRange <- seq.Date(from = as.Date('1981-01-01'), to = as.Date('2000-12-31'), by = 1)
  outDir <- 'L:/gz_world_81-00/'
} else {
  dateRange <- seq.Date(from = as.Date('2001-01-01'), to = as.Date('2024-05-30'), by = 1)
  outDir <- 'L:/gz_world_01-24/'
}

length(dateRange)
lsfiles <- list.files(path = outDir, pattern = '.tif.gz')
length(lsfiles)

# outDir <- 'H:/gis-data/CHIRPS/world_day/gz_world/'

dateRange <- gsub(pattern = '-', '\\.', dateRange)
length(dateRange)
#'https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_daily/tifs/p05/2020/chirps-v2.0.2020.09.08.tif.gz'
 
setwd(outDir)
clipDir <- 'C:/temp/data/CHIRPS_day_mask/'

datevect <- 1:length(dateRange)
#datevect <- rev(datevect)
datevect <- sample(datevect)
#datevect <- 1:length(dateRange)
# i <- which(dateRange == '2019.12.26')

for(i in datevect){ # i = datevect[22]
  
  yy <- substr(dateRange[i], 0, 4)
  (urlx <- paste0(urlbase, '/', yy, '/chirps-v2.0.', dateRange[i], '.tif.gz'))
  (outfile <- paste0(outDir, 'chirps-v2.0.', dateRange[i], '.tif.gz'))
  ## Only download --
  if( !file.exists(outfile) ){
    print(i)
    print('Download')
    print(outfile)
    tryCatch(download.file(urlx, destfile =  outfile), error = function(e) NULL)
  }


  ## If clip in AOI doesnt exists
  (cliptif <- paste0(clipDir, dateRange[i], '.tif'))
  if ( !file.exists(cliptif) ){
    yy <- substr(dateRange[i], 0, 4)
    (urlx <- paste0(urlbase, '/', yy, '/chirps-v2.0.', dateRange[i], '.tif.gz'))
    outfile <- paste0(outDir, 'chirps-v2.0.', dateRange[i], '.tif.gz')
    outtif <- paste0(outDir, 'chirps-v2.0.', dateRange[i], '.tif')

    ## Download
    if( !file.exists(outfile) ){
      print('Download')
      print(outfile)
      tryCatch(download.file(urlx, destfile =  outfile), error = function(e) NULL)
      print(i)
      ## Extract
      tryCatch(R.utils::gunzip(filename = outfile, overwrite=TRUE, destname = outtif, remove=FALSE), error = function(e) NULL) # outtif
    }

    ## Extract
    if( !file.exists(outtif) & file.exists(outfile) ){
      tryCatch(R.utils::gunzip(filename = outfile, overwrite=TRUE, destname = outtif, remove=FALSE), error = function(e) NULL) # outtif
    }

    ## Clip
    (clipTif <- paste0(clipDir, '/', dateRange[i], '.tif'))
    if( !file.exists(clipTif) ){
      (itif <- paste0(outDir, '/chirps-v2.0.', dateRange[i], '.tif'))
      if ( !file.exists(itif) ){
        (igz <- paste0(outDir, '/chirps-v2.0.', dateRange[i], '.tif.gz'))
        tryCatch(R.utils::gunzip(filename = igz, overwrite=TRUE, 
                                 destname = itif), error = function(e) NULL) # outtif
      }

      print('clip')
      tryCatch(
        gdalwarp(srcfile = itif, dstfile = clipTif,
                           dstnodata = 'None', overwrite = TRUE,
                           crop_to_cutline = TRUE,
                           cutline = polPath), error = function(e) NULL)
      print(i)
    }
    if ( file.exists(clipTif) ){
      file.remove(itif)
    }
  }
}
2

# 3 R: 60 crops / 60 seg
# 4 R: 64 crops / 60 seg
# 5 R: (14119-14051) /60
# 7 R: 58 crops/ 60 seg

# install.packages('terra')
# library(R.utils)
# library(terra)
# setwd <- 'H:/gis-data/CHIRPS/world_day/gz_world/'
# r1 <- rast('chirps-v2.0.2020.06.30.tif')
# r2 <- rast('chirps-v2.0.2020.07.19.tif')