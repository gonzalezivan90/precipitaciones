library(terra)
# install.packages('Metrics')
library(Metrics)
setwd('N:\\Mi unidad\\sinchi_precipitaciones')
setwd('N:\\My Drive\\sinchi_precipitaciones')
# setwd('C:/temp/data')

outdir <- 'Producto3\\Anexos\\ideam-stack'; dir.create(outdir)
aoi <- vect('data/SINCHI_Region_100K_WGS84.shp')

#### PT -------------------------

# CHIRPS (5K)
# TRMM
# GPM 
# MERRA (5-7K)
# GPM+
# ERA5 * 2
# FLDAS
# TerraClimate


## Convert dates to format yyyy-mm-dd 
chirps <- rast('Producto2/Anexos/Anexo04_BasesSatelitales/cortado_CHIRPS_n521_1981-01_2024-05.tif') 
head(names(chirps)); tail(names(chirps))
names(chirps) <- paste0(gsub('\\.', '-', names(chirps)), '-01')
length(chirps)
(ar <- terra::cellSize(chirps[[1]], unit = 'km'))



trmm <- rast('C:/temp/data/cortado_TRMMM_n254_1998-01_2019-12.tif')
names(trmm)
names(trmm) <- as.Date(names(trmm), format = '%Y%m%d')
ar <- terra::cellSize(trmm[[1]], unit = 'km')

gpm <- rast('C:/temp/data/cortado_GPM_n256_1998-01_2019-12.tif') 
names(gpm)
names(gpm) <- as.Date(paste0(names(gpm), '01'), format = '%Y%m%d')
ar <- terra::cellSize(gpm[[1]], unit = 'km')

GPMDPRGMI <- rast('C:/temp/data/cortado_GPMDPRGMI_n508_1982-01_2024-04.tif') 
names(GPMDPRGMI)
names(GPMDPRGMI) <- paste0(names(GPMDPRGMI), '-01')
ar <- terra::cellSize(GPMDPRGMI[[1]], unit = 'km')


fldas <- rast('C:/temp/data/cortado_FLDAS_n522_1981-01_2024-06.tif') 
names(fldas)
names(fldas) <- as.Date(paste0(names(fldas), '01'), format = '%Y%m%d')
ar <- terra::cellSize(fldas[[1]], unit = 'km')


era5A <- rast('C:/temp/data/cortado_ERA5A_n522_1981-01_2024-06.tif') 
names(era5A)
names(era5A) <- as.Date(paste0(names(era5A), '01'), format = '%Y%m%d')
ar <- terra::cellSize(era5A[[1]], unit = 'km')


era5B <- rast('C:/temp/data/cortado_ERA5B_n474_1981-01_2020-06.tif') 
names(era5B)
names(era5B) <- as.Date(paste0(names(era5B), '01'), format = '%Y%m%d')
ar <- terra::cellSize(era5B[[1]], unit = 'km')


merra <- rast('C:/temp/data/cortado_MERRA_n523_1981-01_2024-07.tif') 
names(merra)
names(merra) <- gsub('b', '', names(merra))
ar <- terra::cellSize(merra[[1]], unit = 'km')


clim <- rast('C:/temp/data/cortado_TerraClimate_n516_1981-01_2020-06.tif') 
names(clim)
names(clim) <- as.Date(paste0(names(clim), '01'), format = '%Y%m%d')
ar <- terra::cellSize(clim[[1]], unit = 'km')

## Temperature --------------
# MODIS
# ERA5 * 2
# FLDAS
# TerraClimate


modis <- rast('C:/temp/data/cortado_temp_MODIS_n294_2000-02_2024-07.tif')
names(modis)
terra::cellSize(modis[[1]], unit = 'km')

era5TA <- rast('C:/temp/data/cortado_temp_ERA5A_n522_1981-01_2024-06.tif') 
names(era5TA)
names(era5TA) <- as.Date(paste0(names(era5TA), '01'), format = '%Y%m%d')
terra::cellSize(era5TA[[1]], unit = 'km')

era5TB <- rast('C:/temp/data/cortado_temp_ERA5B_n474_1981-01_2020-06.tif')
names(era5TB)
names(era5TB) <- as.Date(paste0(names(era5TB), '01'), format = '%Y%m%d')
terra::cellSize(era5TB[[1]], unit = 'km')

fldasT <- rast('C:/temp/data/cortado_temp_FLDAS_n510_1982-01_2024-06.tif')
names(fldasT)
names(fldasT) <- as.Date(paste0(names(fldasT), '01'), format = '%Y%m%d')
terra::cellSize(fldasT[[1]], unit = 'km')

terraT <- rast('C:/temp/data/cortado_temp_TerraClim_n516_1981-01_2020-06.tif')
names(terraT)
names(terraT) <- as.Date(paste0(names(terraT), '01'), format = '%Y%m%d')
terra::cellSize(terraT[[1]], unit = 'km')

## -----------

ptRList <- list(chirps = chirps, trmm = trmm, gpm = gpm, merra = merra,
                gpm2 = GPMDPRGMI,
                era5A = era5A, era5B = era5B, fldas = fldas, clim = clim)
names(ptRList)

ptNames <- c('CHIRPS', 'TRMM', 'GPM', 'MERRA', 'GPM-DPR-GMI', 'ERA5', 
'ERA5 from hours', 'FLDAS', 'terraClim')
tpRList <- list(modis = modis, era5TA = era5TA, era5TB = era5TB, 
                fldasT = fldasT, terraT = terraT)
names(tpRList)



(varsPT <- load(paste0('C:/temp/data/IDEAMPT_monthPt_listDatPt_stationsPt_164.RData')))
(varsTP <- load(paste0('C:/temp/data/IDEAMTP_monthTp_listDatTp_stationsTp_164.RData')))
range(rownames(monthTp))

xyTp <- terra::vect('C:/temp/data/tpStations_filtered_164.shp')
xyPt <- terra::vect('C:/temp/data/ptStations_filtered_181.shp')

nrow(monthPt)
nrow(monthTp)
ncol(monthPt)
ncol(monthTp)
monthPt[1:5, 1:5]
monthTp[1:5, 1:5]
rownames(monthPt) <- paste0(rownames(monthPt), '-01')
rownames(monthTp) <- paste0(rownames(monthTp), '-01')

###  ---------
### PT BD ------------
for (i in 1:length(ptRList)){ # i = 1
  
  db.i <- ptRList[[i]]
  
  exct.i <- (terra::extract(db.i, xyPt))
  exctID <- exct.i$ID
  exct.i$ID <- NULL
  
  # Keep columns as stations and rows as dates
  exct.i <- t(exct.i)
  dim(exct.i)
  exct.i[1:5, 1:5]
  
  # if(names(ptRList)[i]
  
  colnames(exct.i) <- colnames(monthPt)
  nrow( xyPt)
  nrow( exct.i)
  
  write.csv(exct.i, paste0(getwd(), '//', outdir, '//PT_extract_', names(ptRList)[i], '.csv') )
  
  dbdir <- paste0(outdir, '/PT_', names(ptRList)[i]); dir.create(dbdir)
  
  ansDB <- data.frame(bd = names(ptRList)[i], station = colnames(monthPt), 
                      var = 'PT', ndates = NA, mindate = NA, maxdate = NA,
                      cor = NA, mae = NA, rmse = NA)
  
  for (j in 1:ncol(monthPt)){ # j = 1
    ideamj <- na.omit(monthPt[, j])
    statname <- colnames(monthPt)[j]
    
    bdj <- na.omit(exct.i[, j])

    
    dt_rng_ij <- range(as.Date(c(names(bdj), paste0(names(ideamj)))))
    
    match_dates <- base::intersect(names(bdj), names(ideamj))
    if(length(match_dates) == 0){
      ansDB$ndates[j] <- 0
    } else {
      
      datij <- data.frame(raster = rep(NA, length(match_dates)), ideam = NA, row.names = match_dates)
      datij$raster  <-      bdj[names(bdj) %in% rownames(datij)]
      datij$ideam <- ideamj[names(ideamj) %in% rownames(datij)]
      
      write.csv(datij, paste0(dbdir, '/',statname, '.csv'))
      
      actual <- datij$ideam
      predicted <- datij$raster
      
      A_cor <- cor(actual, predicted)
      B_rmse <- Metrics::rmse(actual, predicted)
      C_mae <- Metrics::mae(actual, predicted)
      
      ansDB[j, c('ndates', 'mindate', 'maxdate', 'cor', 'mae', 'rmse')] <- c(
        ndates = nrow(datij), mindate = min(match_dates), maxdate = max(match_dates),
        cor = A_cor, mae = B_rmse, rmse = C_mae)
    }
  }
  write.csv(ansDB, paste0(getwd(), '//', outdir, '//PT_stats_', names(ptRList)[i], '.csv') )
  print(paste(i, names(ptRList)[i]))
}



###  ---------
### TP BD ------------
rownames(monthTp) <- paste0(rownames(monthTp), '-01')

for (i in 1:length(tpRList)){ # i = 1
  
  db.i <- tpRList[[i]]
  
  exct.i <- (terra::extract(db.i, xyTp))
  exctID <- exct.i$ID
  exct.i$ID <- NULL
  
  # Keep columns as stations and rows as dates
  exct.i <- t(exct.i)
  dim(exct.i)
  exct.i[1:5, 1:5]
  
  colnames(exct.i) <- colnames(monthTp)
  nrow( xyPt)
  nrow( exct.i)
  
  write.csv(exct.i, paste0(getwd(), '//', outdir, '//TP_extract_', names(tpRList)[i], '.csv') )
  
  dbdir <- paste0(outdir, '/TP_', names(tpRList)[i]); dir.create(dbdir)
  
  ansDB <- data.frame(bd = names(tpRList)[i], station = colnames(monthPt), 
                      var = 'TP', ndates = NA, mindate = NA, maxdate = NA,
                      cor = NA, mae = NA, rmse = NA)
  
  for (j in 1:ncol(monthTp)){ # j = 2
    ideamj <- na.omit(monthTp[, j])
    statname <- colnames(monthTp)[j]
    
    bdj <- na.omit(exct.i[, j])
    
    dtid <- as.Date(names(ideamj))
    dt_rng_ij <- range(as.Date(c(names(bdj), names(ideamj))))
    
    match_dates <- base::intersect(names(bdj), names(ideamj))
    # print(paste(statname, min(dtid), max(dtid)))
    if( length(match_dates) == 0 ){
      ansDB$ndates[j] <- 0
    } else {
      
      datij <- data.frame(raster = rep(NA, length(match_dates)), ideam = NA, row.names = match_dates)
      datij$raster  <-      bdj[names(bdj) %in% rownames(datij)]
      datij$ideam <- ideamj[names(ideamj) %in% rownames(datij)]
      
      write.csv(datij, paste0(dbdir, '/',statname, '.csv'))
      
      actual <- datij$ideam
      predicted <- datij$raster
      
      A_cor <- cor(actual, predicted)
      B_rmse <- Metrics::rmse(actual, predicted)
      C_mae <- Metrics::mae(actual, predicted)
      
      ansDB[j, c('ndates', 'mindate', 'maxdate', 'cor', 'mae', 'rmse')] <- c(
        ndates = nrow(datij), mindate = min(match_dates), maxdate = max(match_dates),
        cor = A_cor, mae = B_rmse, rmse = C_mae)
    }
  }
  write.csv(ansDB, paste0(getwd(), '//', outdir, '//TP_stats_', names(tpRList)[i], '.csv') )
  print(paste(i, names(tpRList)[i]))
}




ptRList <- list(CHIRPS = chirps, TRMM = trmm, GPM = gpm, MERRA = merra, GPM_DPR_GMI = GPMDPRGMI,
                ERA5a = era5A, ERA5b = era5B, FLDAS = fldas, TERRACLIM = clim)
names(ptRList)

tpRList <- list(MODIS = modis, ERA5a = era5TA, ERA5b = era5TB, 
                FLDAS = fldasT, TERRACLIM = terraT)

plot(merra[[1]], main = 'MERRA')
plot(aoi, add = TRUE)
plot(xyTp, add = TRUE)
 

plot(trmm[[1]], main = 'TRMM')
plot(aoi, add = TRUE)

plot(gpm[[1]], main = 'GPM')
plot(aoi, add = TRUE)

plot(chirps[[2]], main = 'CHIRPS')
plot(aoi, add = TRUE)

plot(GPMDPRGMI[[1]], main = 'GPM-DPR-GMI')
plot(aoi, add = TRUE)

plot(modis3[[7]], main = 'MODIS')
plot(aoi, add = TRUE)

plot(modis3[[7]])


for(i in 1:length(ptRList)){ # i = 1
  plot(ptRList[[i]][[1]], main = names(ptRList)[i])
  plot(aoi, add = TRUE)
  plot(xyPt, add = TRUE, pch = 20)
  #legend('bottomleft', legend = 'Estaciones lluvia', pch = 20, lwd = 1)
}

for(i in 1:length(tpRList)){
  plot(tpRList[[i]][[3]], main = names(tpRList)[i])
  plot(aoi, add = TRUE)
  plot(xyTp, add = TRUE, pch = 20)
  #legend('bottomleft', legend = 'Estaciones lluvia', pch = 20, lwd = 1)
}


## for 

