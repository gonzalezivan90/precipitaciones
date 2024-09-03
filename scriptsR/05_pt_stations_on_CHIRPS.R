
library(terra)


options(scipen=999)

aoi <- terra::vect('H:/gis-data/CHIRPS/SINCHI_Region_100K_WGS84.shp')
r0 <- terra::rast('H:/gis-data/CHIRPS/clip_sinchi_ID.tif')
aoibuff <- terra::vect('H:/gis-data/CHIRPS/SINCHI_Region_100K_WGS84_buff5500m.shp')


datPath <- 'C:/temp/data/PrecipitacionNacionalDiaria/'
setwd(datPath)
catalog <- read.csv('../Cat_logo_Nacional_de_Estaciones_del_IDEAM_2024-04-20.csv')
head(catalog)

txt <- list.files(path = '.', pattern = '.data'); length(txt)
datNames <- gsub('.data|.+@', '', txt)

rng <- c(0, 0)


ptStations <- data.frame(file = txt, code = datNames)
pos <- match(ptStations$code, catalog$Codigo) 
ptStations[which(!is.na(pos)), c('xy')] <- catalog[na.omit(pos), 'Ubicación']

options(scipen=999)
ptStations[, c('x')] <- unlist(sapply(ptStations$xy, function(x){
  if(is.na(x)){ x <- ''} 
  as.numeric(strsplit(gsub('\\(|\\)', '', x), ',')[[1]])[2]
}))

ptStations[, c('y')] <- unlist(sapply(ptStations$xy, function(x){
  if(is.na(x)){ x <- ''} 
  as.numeric(strsplit(gsub('\\(|\\)', '', x), ',')[[1]])[1]
}))


ptStations$dateMin <- ptStations$dateMax <- ''
ptStations$valMin <- ptStations$valMax <- NA
ptStations$nvals <- ptStations$valAve <- NA

datListPt <- list()
for (i in 1:nrow(ptStations)){ # i = 2
  datFile <- txt[i]
  cod.i <- gsub('.data|.+@', '', datFile)
  dat.i <- read.delim(datFile, sep = '|')
  dat.i$d <- as.Date(dat.i$Fecha)
  dat.i$cod <- cod.i
  dat.i$file <- datFile
  # head(dat.i)
  # tail(dat.i)
  datListPt[[i]] <- dat.i
  #rng <- range(c(rng, dat.i$Valor))
  print(i)
  ptStations$dateMin[i] <- as.character(min(dat.i$d))
  ptStations$dateMax[i] <- as.character(max(dat.i$d))
  
  ptStations$valMin[i] <- min(dat.i$Valor)
  ptStations$valMax[i] <- max(dat.i$Valor)
}

head(ptStations)
tail(ptStations)
ptStations$X0 <- ptStations$x
ptStations$Y0 <- ptStations$y
ptsPt <- terra::vect(x = as.matrix(ptStations[, c('X0', 'Y0')]), atts = ptStations, crs = "epsg:4326")
plot(r0, main = 'Precipitacion')
plot(aoi, add = TRUE)
plot(ptsPt, add = TRUE)

exVals1 <- terra::extract(y = ptsPt, r0) #
exVals2 <- terra::extract(y = ptsPt, aoi) ## so long
ptsPt$onRast <- exVals1$`1981.01.01`
ptsPt$onVect <- exVals2$st_perimet
writeVector(ptsPt, paste0('C:/temp/data/ptStations_', nrow(ptsPt),'.shp'), overwrite=TRUE)


selPts1 <- ptsPt[ na.omit(exVals1)$ID, ]
exValsFilt <- na.omit(exVals1)
posPtsKeep <- na.omit(exVals1)$ID
# stationsPt <- ptStations[ posPtsKeep, ]
stationsPt <- ptsPt[ posPtsKeep, ]
#stationsPt$rastID <- exValsFilt$`1981.01.01`
dim(stationsPt)
plot(stationsPt, add = TRUE, col = 2)


#head(stationsPt)
listDatPt <- listDat <- datListPt[posPtsKeep]
length(listDatPt)
length(posPtsKeep)

plot(stationsPt, add = TRUE, col = 2)
writeVector(stationsPt, paste0('C:/temp/data/ptStations_filtered_',nrow(stationsPt),'.shp'))

datRng <- range(c(stationsPt$dateMax, stationsPt$dateMin))
dtRng <- unique(substr(seq.Date(from = as.Date(datRng[1]), to = as.Date(datRng[2]), by = 1), 0, 7))

head(listDatPt[[1]])
identical(stationsPt$code, 
          unlist(lapply(listDatPt, function(x) x$cod[1])))


monthPt <- matrix(NA, ncol = length(listDatPt), nrow = length(dtRng), dimnames = list(dtRng, stationsPt$code))
monthPtmx <- monthPtmn <- monthPTsd <- monthPtpd <- monthPt
monthPt[1:5, 1:5]

# load('H:/gis-data/CHIRPS/sinchi_listDat181stationsPt_selXY.RData')
for( i in 1:length(listDatPt)){ # i = 170
  xd <- listDatPt[[i]]
  # head(xd)
  names(listDatPt)[i] <- xd$cod[1]
  xd$ym <- substr(0, 7, x = xd$d)
  xagg <- aggregate(xd$Valor, by = list(xd$ym), sum)
  monthPt[dtRng %in% xagg$Group.1, i ] <- xagg$x
  
  xaggsd <- aggregate(xd$Valor, by = list(xd$ym), sd)
  monthPTsd[dtRng %in% xaggsd$Group.1, i ] <- xaggsd$x
  
  xaggpd <- aggregate(xd$Valor, by = list(xd$ym), function(x) sum(x>0)/30*100)
  monthPtpd[dtRng %in% xaggpd$Group.1, i ] <- xaggpd$x
  
  xaggpd <- aggregate(xd$Valor, by = list(xd$ym), max)
  monthPtmx[dtRng %in% xaggpd$Group.1, i ] <- xaggpd$x
  
  xaggpd <- aggregate(xd$Valor, by = list(xd$ym), min)
  monthPtmn[dtRng %in% xaggpd$Group.1, i ] <- xaggpd$x
  
}

identical(stationsPt$code, 
          names(listDatPt))

#rowSums(monthPt, na.rm = TRUE)
# plot( as.Date( paste0(rownames(monthPt), '-01')),  rowMeans(monthPt, na.rm = TRUE))

save(listDatPt, stationsPt, monthPt, monthPtpd, monthPtmx, monthPtmn, monthPTsd, 
     file = paste0('C:/temp/data/IDEAMPT_monthPt_listDatPt_stationsPt_',nrow(stationsPt),'.RData'))


# --- Temperatura
datPath <- 'C:/temp/data/TemperaturaNacionalDiaria/'
setwd(datPath)
head(catalog)

txtTp <- list.files(path = '.', pattern = '.data'); length(txt)
datNamesTp <- gsub('.data|.+@', '', txtTp)
length(datNames)

rng <- c(0, 0)

tpStations <- data.frame(file = txtTp, code = datNamesTp)
pos <- match(tpStations$code, catalog$Codigo) 
tpStations[which(!is.na(pos)), c('xy')] <- catalog[na.omit(pos), 'Ubicación']

options(scipen=999)
tpStations[, c('x')] <- unlist(sapply(tpStations$xy, function(x){
  if(is.na(x)){ x <- ''} 
  as.numeric(strsplit(gsub('\\(|\\)', '', x), ',')[[1]])[2]
}))

tpStations[, c('y')] <- unlist(sapply(tpStations$xy, function(x){
  if(is.na(x)){ x <- ''} 
  as.numeric(strsplit(gsub('\\(|\\)', '', x), ',')[[1]])[1]
}))


tpStations$dateMin <- tpStations$dateMax <- ''
tpStations$valMin <- tpStations$valMax <- NA
tpStations$nvals <- tpStations$valAve <- NA

datListTp <- list()
for (i in 1:nrow(tpStations)){ # i = 1
  datFile <- txtTp[i]
  cod.i <- gsub('.data|.+@', '', datFile)
  dat.i <- read.delim(datFile, sep = '|')
  dat.i$d <- as.Date(dat.i$Fecha)
  dat.i$cod <- cod.i
  #table(substr(dat.i$Fecha, 12, 13))
  # head(dat.i)
  # tail(dat.i)
  datListTp[[i]] <- dat.i
  rng <- range(c(rng, dat.i$Valor))
  print(i)
  tpStations$dateMin[i] <- as.character(min(dat.i$d))
  tpStations$dateMax[i] <- as.character(max(dat.i$d))
  
  tpStations$valMin[i] <- min(dat.i$Valor)
  tpStations$valMax[i] <- max(dat.i$Valor)
}

head(tpStations)
tail(tpStations)
tpStations$X0 <- tpStations$x
tpStations$Y0 <- tpStations$y


tppts <- terra::vect(x = as.matrix(tpStations[, c('X0', 'Y0')]), atts = tpStations, crs = "epsg:4326")

exVals1t <- terra::extract(y = tppts, r0) #
exVals2t <- terra::extract(y = tppts, aoi) ## so long
tppts$onRast <- exVals1t$`1981.01.01`
tppts$onVect <- exVals2t$st_perimet

nrow(tppts)
writeVector(tppts, paste0('C:/temp/data/tpStations', nrow(tppts),'.shp'), overwrite = TRUE)


exValsFiltt <- na.omit(exVals1t)
posPtsKeepTp <- na.omit(exVals1t)$ID
stationsTp <- tppts[ posPtsKeepTp, ]
stationsTp$rastID <- exValsFiltt$`1981.01.01`
listDatTp <- datListTp[posPtsKeepTp]
length(listDatTp)
length(posPtsKeepTp)

nrow(stationsTp)
writeVector(stationsTp, paste0('C:/temp/data/tpStations_filtered_', nrow(stationsTp),'.shp'), overwrite=TRUE)

plot(r0, main = 'Temp')
plot(aoi, add = TRUE)
plot(tppts, add = TRUE, col = 3)
plot(stationsTp, add = TRUE, col = 4)

(datRng2 <- range(c(stationsTp$dateMax, stationsTp$dateMin)))
dtRng2 <- unique(substr(seq.Date(from = as.Date(datRng2[1]), to = as.Date(datRng2[2]), by = 1), 0, 7))

head(listDatTp[[1]])
identical(stationsTp$code, 
          unname(unlist(lapply(listDatTp, function(x) x$cod[1]))))


monthTp <- matrix(NA, ncol = length(listDatTp), nrow = length(dtRng2), dimnames = list(dtRng2, stationsTp$code))
monthTpsd <- monthTpmn <- monthTpmx <- monthTppd <- monthTp

monthTp[1:5, 1:5]

# load('H:/gis-data/CHIRPS/sinchi_listDat181stationsPt_selXY.RData')
for( i in 1:length(listDatTp)){ # i = 1
  xd <- listDatTp[[i]]
  # head(xd)
  names(listDatTp)[i] <- xd$cod[1]
  
  #mean(aggregate(xd$Valor, by = list(xd$d), mean)$x)
  #mean(xd$Valor)
  
  xagg0 <- aggregate(xd$Valor, by = list(xd$d), mean)
  xagg0$ym <- substr(0, 7, x = xagg0$Group.1)
  
  xagg <- aggregate(xagg0$x, by = list(xagg0$ym), mean)
  monthTp[which(dtRng2 %in% xagg$Group.1), i ] <- xagg$x
  # 
  # length(monthTp[dtRng2 %in% xagg$Group.1, i ])
  # length(monthTp[which(dtRng2 %in% xagg$Group.1), i ])
  # 
  # dim(monthTp)
  # sum(dtRng2 %in% xagg$Group.1)
  # dim(xagg)
  # length(xagg$x)
  
  xaggsd <- aggregate(xagg0$x, by = list(xagg0$ym), sd)
  monthTpsd[which(dtRng2 %in% xaggsd$Group.1), i ] <- xaggsd$x
  
  xaggpd <- aggregate(xagg0$x, by = list(xagg0$ym), function(x) sum(x>0)/30*100)
  monthTppd[which(dtRng2 %in% xaggpd$Group.1), i ] <- xaggpd$x

  xaggpd <- aggregate(xagg0$x, by = list(xagg0$ym), max)
  monthTpmx[which(dtRng2 %in% xaggpd$Group.1), i ] <- xaggpd$x

  xaggpd <- aggregate(xagg0$x, by = list(xagg0$ym), min)
  monthTpmn[which(dtRng2 %in% xaggpd$Group.1), i ] <- xaggpd$x
  
}

identical(stationsTp$code, 
          names(listDatTp))



dim(stationsTp)
length(listDatTp)
plot(stationsTp, add = TRUE, col = 2)
save(listDatTp, stationsTp, monthTp, monthTppd, monthTpsd, monthTpmx, monthTpmn, 
     file = paste0('C:/temp/data/IDEAMTP_monthTp_listDatTp_stationsTp_', nrow(stationsTp),'.RData'))

#load('H:/gis-data/CHIRPS/sinchi_listDatTp_stationsTp_monthTp164.RData')
#load('H:/gis-data/CHIRPS/sinchi_monthPt_listDatPt_stationsPt_181.RData')

write.csv(stationsTp, 'C:/temp/data/Estaciones_TP_lista.csv')
write.csv(stationsPt, 'C:/temp/data/Estaciones_PT_lista.csv')
writeVector(stationsTp, paste0('C:/temp/data/Estaciones_TP.shp'))


write.csv(monthTp, 'C:/temp/data/Estaciones_TP_Valores_mensuales.csv')
write.csv(monthPt, 'C:/temp/data/Estaciones_PT_Valores_mensuales.csv')
writeVector(stationsPt, paste0('C:/temp/data/Estaciones_PT.shp'))


