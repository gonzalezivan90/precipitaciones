library(ncdf4)
library(raster)
#library(rgdal)
library(terra)
require(lubridate)
library(terra)
library(CFtime)
require(ncdf4)

setwd('C:/temp/data/')
setwd('N:/My Drive/sinchi_precipitaciones/data')
aoi <- vect('SINCHI_Region_100K_WGS84.shp')
aoi5 <- vect('SINCHI_Region_100K_WGS84_buff5500m.shp')
#nc <- terra::rast('PT_/precip.mon.ltm.v7.1981-2010.nc')
#cf <- CFtime(f1$dim$time$units, f1$dim$time, f1$dim$time$vals)
#dates <- CFtimestamp(cf)

## Merra -----
# https://developers.google.com/earth-engine/datasets/catalog/NASA_GSFC_MERRA_flx_2#bands
merraA <- rast('PT_/MERRA_80x00.tif') # 240
merraB <- rast('PT_/MERRA_01x24.tif') # 283
names(merraA)
240 + 283

length(dtRng<-as.Date(paste0(unique(substr(seq.Date(from = as.Date('1981-01-01'), to = as.Date('2000-12-12'), by = 1), 0, 7)), '-01')))
length(dtRng<-as.Date(paste0(unique(substr(seq.Date(from = as.Date('2001-01-01'), to = as.Date('2024-07-26'), by = 1), 0, 7)), '-01')))


length(dtRng<-as.Date(paste0(unique(substr(seq.Date(from = as.Date('1981-01-01'), to = as.Date('2024-07-05'), by = 1), 0, 7)), '-01')))
merra <- rast(list(merraA , merraB )) * 86400 
names(merra) <- paste0('b', dtRng)
plot(merraA[[1]])
plot(merra[[1]])
writeRaster(merra, 'C:/temp/data/cortado_MERRA_n523_1981-01_2024-07.tif', overwrite=TRUE)


# kg/m^2/s	0*	0.110565.  86400 
##https://www.researchgate.net/post/How-do-I-convert-ERA-Interim-precipitation-estimates-from-kg-m2-s-to-mm-day


## TRMMM -----
# https://developers.google.com/earth-engine/datasets/catalog/TRMM_3B43V7
trmm <- rast('PT_/TRMM_27830.tif') # 240
names(trmm) <- substr(0, 6, x = gsub('3B43_|_7A_|_7_|precipitation','', names(trmm)))
# mmm/h
# mm/hr -- mm/month
trmm <- trmm * 24
writeRaster(trmm, 'C:/temp/data/cortado_TRMMM_n254_1998-01_2019-12.tif')

## GPM  -----
# https://developers.google.com/earth-engine/datasets/catalog/NASA_GPM_L3_IMERG_MONTHLY_V06#description
gpm <- rast('PT_/GPM_11132.tif') # 240
names(gpm) <- substr(0, 6, x = names(gpm))
# mmm/h
# mm/hr -- mm/month
gpm <- gpm * 24
writeRaster(gpm, 'C:/temp/data/cortado_GPM_n256_1998-01_2019-12.tif')

## CHIRPS ----
#https://developers.google.com/earth-engine/datasets/catalog/UCSB-CHG_CHIRPS_DAILY
setwd('C:/temp/data/CHIRPS_month/')
chirpsM <- list.files(path = 'C:/temp/data/CHIRPS_month/', pattern = '.tif$')
chirps <- rast(lapply(split(chirpsM, chirpsM), function(x) rast(x)))
names(chirps) <- gsub('sum_|.tif', '', names(chirps))
# mm/d
plot(chirps[[1]])
plot(aoi, add = TRUE)
plot(aoi5, add = TRUE)
writeRaster(chirps, 'C:/temp/data/cortado_CHIRPS_n521_1981-01_2024-05.tif', overwrite=TRUE)


## GPM DPR and GMI
# https://disc.gsfc.nasa.gov/datasets/GPM_3CMB_07/summary?keywords=3CMB
f1 <- nc_open("C:/temp/data/PT_/precip.monitor.mon.total.1x1.v2020.nc")
nc <- terra::rast('C:/temp/data/PT_/precip.monitor.mon.total.1x1.v2020.nc')
f1$dim$time
names(nc)
timeInfo(nc)
str(nc)
nc[names(nc)[2]]
#nc@ptr@.xData$time
# as.Date(nc2@ptr@.xData$time, origin = as.Date('1800-01-01 00:00:0.0'), units = "days")
# as.POSIXct(head(nc2@ptr@.xData$time),origin='1800-01-01 00:00:0.0', units = "months")

dtRng <- as.Date(paste0(unique(substr(seq.Date(from = as.Date('1981-01-01'), to = as.Date('2012-12-31'), by = 1), 0, 7)), '-01'))
length(dtRng)
dtRng <- as.Date(paste0(unique(substr(seq.Date(from = as.Date('1982-01-01'), to = as.Date('2024-04-30'), by = 1), 0, 7)), '-01'))
length(dtRng)
length(names(nc))
names(nc) <- substr(dtRng, 0, 7)

plot(nc[[4]])
xw <- nc[[1:2]]
ext(xw) <- c(-180, 180, -90, 90)
plot(xw)
terra::ext(merra)

GPMDPRGMI <- terra::crop(x = nc,
                         y = terra::ext(c(xmin = -80+360, 
                                                  xmax = -66+360, 
                                                  ymin = -5, 
                                                  ymax = 6))) #c(xmin, xmax, ymin, ymax))
plot(GPMDPRGMI[[1]])
GPMDPRGMI
#ext(GPMDPRGMI) <- c(-76 , -67 , -5 , 6)
ext(GPMDPRGMI) <- as.vector(ext(GPMDPRGMI)) + c(-360 , -360 , 0 , 0)
plot(GPMDPRGMI[[1]])
plot(aoi, add = T)
names(GPMDPRGMI)
writeRaster(GPMDPRGMI, 'C:/temp/data/cortado_GPMDPRGMI_n508_1982-01_2024-04.tif')

# FLDA 
# https://developers.google.com/earth-engine/datasets/catalog/NASA_FLDAS_NOAH01_C_GL_M_V001
fldas <- rast('C:/temp/data/PT_/FLDASpt_11132.tif')
names(fldas) <- gsub('_Rainf_f_tavg', '', names(fldas))
# kg/m^2/s
fldas <- (fldas * 86400)
plot(mask(fldas[[1]], aoi5), main = 'FLDAS')
plot(fldas[[1]], main = 'FLDAS')
plot(aoi, add = T)
writeRaster(fldas, 'C:/temp/data/cortado_FLDAS_n522_1981-01_2024-06.tif', overwrite=TRUE)

# ERA5 A
# https://developers.google.com/earth-engine/datasets/catalog/ECMWF_ERA5_LAND_MONTHLY_AGGR#description
era5a <- rast('C:/temp/data/PT_/ERA5pt_11132.tif')
names(era5a) <- gsub('_total_precipitation_sum', '', names(era5a))
# m
era5a <- (era5a * 1000)
plot(era5a[[1]], main = 'ERA5A')
plot(aoi, add = T)
writeRaster(era5a, 'C:/temp/data/cortado_ERA5A_n522_1981-01_2024-06.tif')


# ERA5 B
# https://developers.google.com/earth-engine/datasets/catalog/ECMWF_ERA5_LAND_MONTHLY_BY_HOUR#bands
era5b <- rast('C:/temp/data/PT_/ERA5pt_27830.tif')
names(era5b) <- gsub('_total_precipitation', '', names(era5b))
# m
era5b <- (era5b * 1000)
plot(era5b[[1]], main = 'ERA5B')
plot(aoi, add = T)
writeRaster(era5b, 'C:/temp/data/cortado_ERA5B_n474_1981-01_2020-06.tif')


# TerraClimate
# https://developers.google.com/earth-engine/datasets/catalog/IDAHO_EPSCOR_TERRACLIMATE#bands
terrc <- rast('C:/temp/data/PT_/TerraClimatePt_4638.tif')
names(terrc) <- gsub('_pr', '', names(terrc))
# mm
terrc <- (terrc * 1000)
plot(terrc[[1]], main = 'TerraClimate')
plot(aoi, add = T)
writeRaster(terrc, 'C:/temp/data/cortado_TerraClimate_n516_1981-01_2020-06.tif')

## Temperature ------


## MODIS ----
#https://developers.google.com/earth-engine/datasets/catalog/MODIS_061_MOD11A2
modis <- rast('C:/temp/data/PT_/MODIS3_1200.tif')
dtRng <- as.Date(paste0(unique(substr(seq.Date(from = as.Date('2000-01-01'), to = as.Date('2024-12-31'), by = 1), 0, 7)), '-01'))
ids <- as.numeric(gsub('_LST_Day_1km_mean', '', names(modis)))+1
names(modis) <- dtRng[ids]
m2 <<- (modis[[5]] * 0.02)-273.15
#system.time(modis2 <- terra::lapp(modis, fun = function(x) {x * 0.02}, filename="C:/temp/data/PT_/MODIS_corrected_1200.tif", overwrite=FALSE))
system.time(modis2 <- lapply(modis, function(x) {(x * 0.02)-273.15})) ## 2931.33 
modis2
modis3 <- rast(modis2)
writeRaster(modis3, 'C:/temp/data/cortado_temp_MODIS_n294_2000-02_2024-07.tif')
names(modis3)


# TerraClimate
# https://developers.google.com/earth-engine/datasets/catalog/IDAHO_EPSCOR_TERRACLIMATE#bands
tterrc <- rast('C:/temp/data/PT_/TerraClimateTp_4638.tif')
names(tterrc) <- gsub('_tmmx', '', names(tterrc))
#  °C, 0.1
tterrc <- tterrc * 0.1
plot(tterrc[[1]], main = 'TerraClimate')
plot(aoi, add = T)
writeRaster(tterrc, 'C:/temp/data/cortado_temp_TerraClim_n516_1981-01_2020-06.tif', overwrite=TRUE)


# ERA5 A
# https://developers.google.com/earth-engine/datasets/catalog/ECMWF_ERA5_LAND_MONTHLY_AGGR#description
tera5a <- rast('C:/temp/data/PT_/ERA5tp_11132.tif')
names(tera5a) <- gsub('_temperature_2m', '', names(tera5a))
# K
tera5a <- (tera5a - 273.15)
plot(tera5a[[1]], main = 'ERA5A')
plot(aoi, add = T)
writeRaster(tera5a, 'C:/temp/data/cortado_temp_ERA5A_n522_1981-01_2024-06.tif')


# ERA5 B
# https://developers.google.com/earth-engine/datasets/catalog/ECMWF_ERA5_LAND_MONTHLY_BY_HOUR#bands
tera5b <- rast('C:/temp/data/PT_/ERA5tp_27830.tif')
names(tera5b) <- gsub('_mean_2m_air_temperature', '', names(tera5b))
# m
tera5b <- (tera5b - 273.15)
plot(tera5b[[1]], main = 'ERA5B')
plot(aoi, add = T)
writeRaster(tera5b, 'C:/temp/data/cortado_temp_ERA5B_n474_1981-01_2020-06.tif')

# FLDA 
# https://developers.google.com/earth-engine/datasets/catalog/NASA_FLDAS_NOAH01_C_GL_M_V001
fldas <- rast('C:/temp/data/PT_/FLDAStp_11132.tif')
names(fldas) <- gsub('_Tair_f_tavg', '', names(fldas))
# K
fldas <- (fldas * 1) - 273.15
plot(mask(fldas[[1]], aoi5), main = 'FLDAS')
plot(fldas[[1]], main = 'FLDAS')
plot(aoi, add = T)
writeRaster(fldas, 'C:/temp/data/cortado_temp_FLDAS_n510_1982-01_2024-06.tif', overwrite=TRUE)



### OpenLandMap
# https://developers.google.com/earth-engine/datasets/catalog/OpenLandMap_CLM_CLM_LST_MOD11A2-DAYNIGHT_M_v01#description

### OpenLandMap
# https://developers.google.com/earth-engine/datasets/catalog/WORLDCLIM_V1_MONTHLY

# WorldClim 
# https://developers.google.com/earth-engine/datasets/catalog/WORLDCLIM_V1_MONTHLY

# FLDAS
# https://developers.google.com/earth-engine/datasets/catalog/NASA_FLDAS_NOAH01_C_GL_M_V001

# ERA5
# https://developers.google.com/earth-engine/datasets/catalog/ECMWF_ERA5_LAND_MONTHLY_AGGR#description
# https://developers.google.com/earth-engine/datasets/catalog/ECMWF_ERA5_LAND_MONTHLY_BY_HOUR#bands
# https://developers.google.com/earth-engine/datasets/catalog/ECMWF_ERA5_MONTHLY#description

# SPEIbase
# https://developers.google.com/earth-engine/datasets/catalog/CSIC_SPEI_2_9





## Plot 
chirps <- rast('C:/temp/data/cortado_CHIRPS_n521_1981-01_2024-05.tif') 
trmm <- rast('C:/temp/data/cortado_TRMMM_n254_1998-01_2019-12.tif')
names(trmm)
gpm <- rast('C:/temp/data/cortado_GPM_n256_1998-01_2019-12.tif') 
names(gpm)
merra <- rast('C:/temp/data/cortado_MERRA_n523_1981-01_2024-07.tif') 
names(merra)
GPMDPRGMI <- rast('C:/temp/data/cortado_GPMDPRGMI_n508_1982-01_2024-04.tif') 
names(GPMDPRGMI)

modiss <- rast('C:/temp/data/cortado_MODIS_n294_2000-02_2024-07.tif')

plot(merra[[1]], main = 'MERRA')
plot(aoi, add = TRUE)

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
