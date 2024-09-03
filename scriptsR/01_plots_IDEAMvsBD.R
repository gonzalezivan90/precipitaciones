library(Metrics)
library(ggplot2)
csvPath <- ('N:\\Mi unidad\\sinchi_precipitaciones\\Producto3\\Anexos\\ideam-stack');
csvPath <- ('N:\\My Drive\\sinchi_precipitaciones\\Producto3\\Anexos\\ideam-stack');


ptfiles <- list.files(path = csvPath, full.names = TRUE, pattern = 'PT_stats.+.csv')      
tpfiles <- list.files(path = csvPath, full.names = TRUE, pattern = 'TP_stats.+.csv')      

pt <- NULL
for(i in seq_along(ptfiles)){
  csvi <- read.csv(ptfiles[i])  
  csvi$file <- ptfiles[i]x|
  pt <- rbind(pt, csvi)
}


tp <- NULL
for(i in seq_along(tpfiles)){
  csvi <- read.csv(tpfiles[i])  
  csvi$file <- tpfiles[i]
  tp <- rbind(tp, csvi)
}


head(pt)
ggplot(pt, aes(x = bd, y = cor)) + geom_boxplot()
ggplot(pt, aes(x = bd, y = rmse)) + geom_boxplot() + ylim(c(0, 500))
ggplot(pt, aes(x = bd, y = mae)) + geom_boxplot() + ylim(c(0, 500))


head(tp)
ggplot(tp, aes(x = bd, y = cor)) + geom_boxplot()
ggplot(tp, aes(x = bd, y = rmse)) + geom_boxplot()
ggplot(tp, aes(x = bd, y = mae)) + geom_boxplot()


tp_med <- aggregate(tp[, c('cor', 'mae', 'rmse')], list(tp$bd), FUN = 'median', na.rm = TRUE)
pt_med <- aggregate(pt[, c('cor', 'mae', 'rmse')], list(pt$bd), FUN = 'median', na.rm = TRUE)

library(ggfortify)
library(factoextra)

pt_med0 <- pt_med
rownames(pt_med) <- pt_med$Group.1
pt_med$Group.1 <- NULL
pca_res <- prcomp(pt_med, scale. = TRUE)

autoplot(pca_res, data = pt_med0, colour = 'Group.1',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)


(plotPCA <- fviz_pca(pca_res, 
                    col.var = colnames(pt_med), 
                    ind.col = rownames(pt_med)))


### temperature
tp_med0 <- tp_med
rownames(tp_med) <- tp_med$Group.1
tp_med$Group.1 <- NULL
pca_restp <- prcomp(tp_med, scale. = TRUE)

autoplot(pca_restp, data = tp_med0, colour = 'Group.1',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

(plotPCATp <- fviz_pca(pca_restp, 
                      col.var = colnames(tp_med), 
                      ind.col = rownames(tp_med)))


### MAPS

library(ggfortify)
#install.packages('ggfortify')

library(terra)
library(raster)
library(spplot)

r <- raster(system.file("external/test.grd", package="raster"))
s <- stack(r, r*2)
names(s) <- c('meuse', 'meuse x 2')

spplot(s)

pts <- data.frame(sampleRandom(r, 10, xy=TRUE))
coordinates(pts) <- ~ x + y

spplot(s, scales = list(draw = TRUE), 
       xlab = "easting", ylab = "northing", 
       col.regions = rainbow(99, start=.1), 
       names.attr=c('original', 'times two'),
       sp.layout = list("sp.points", pts, pch=20, cex=2, col='black'),
       par.settings = list(fontsize = list(text = 12)), at = seq(0, 4000, 500))

spplot(r, scales = list(draw = TRUE), 
       xlab = "easting", ylab = "northing", 
       col.regions = rainbow(99, start=.1), 
       names.attr=c('original', 'times two'),
       sp.layout = list("sp.points", pts, pch=20, cex=2, col='black'),
       par.settings = list(fontsize = list(text = 12)), at = seq(0, 4000, 500))

library(sp)
library(spplot)
library(terra)
xyTp <- terra::vect('N:\\My Drive\\sinchi_precipitaciones\\data\\tpStations_filtered_164.shp')
xyPt <- terra::vect('N:\\My Drive\\sinchi_precipitaciones\\data\\ptStations_filtered_181.shp')

ptsx <- read.csv(ptfiles[1])
xyTp$var <-ptsx$cor
pol <- xyTp[, 'var']
r0 <- terra::rast('N:\\My Drive\\sinchi_precipitaciones\\data\\cortado_CHIRPS_n521_1981-01_2024-05.tif')
r <- subset(r0, 1) * 1
names(r) <- 'x'
tempRast <- paste0(tempdir(), '/temp.tif')
writeRaster(r, tempRast)
r2 <- rast(c(r))
r3 <- rast(tempRast)
names(r3) <- 'x'
plot(r)
rasterVis::levelplot(r, layers = 1)

rasterVis::levelplot(r3)

r9 <- rast(nrows = nrows(r))

librart
library(rasterVis)
rasterVis::levelplot(r)
rasterVis::levelplot(r, col.regions=rev(terrain.colors(4)), xlab="", ylab="")

library(sp)
spplot(pol, zlim = c(-1, 1), at = seq(-1, 1, 0.25))

spplot(pol) + levelplot(rast(r)) + spplot(pol)

library(viridis) 
p <- levelplot(r, layers=1, margin = FALSE, col.regions=viridis) # , at=seq(0,1, len=100)
levelplot(pol, layers=1, margin = FALSE, col.regions=viridis, at= seq(-1,1, len=9))
spplot(pol, at= seq(-1,1, len=9))

spplot(pol,  
       colorkey = list(height = 1, labels = list(at = seq(-1,1, len=9), 
                                                 labels = seq(-1,1, len=9)))
       )
