
library(tidyr)
library(dplyr)
library(ggplot2)
library(scico)
library(rayshader)
library(mapproj)
library(maps)

# Load data

world <- map_data("world")

regionsOfInterest <- c("Iceland", "Norway", "Sweden", "Finland", "Estonia", "Denmark")
nordicRegions <- world %>% filter(region %in% regionsOfInterest)


ghsFile <- "resources\\GHS_POP_GPW42015_GLOBE_R2015A_54009_1k_v1_0\\GHS_POP_GPW42015_GLOBE_R2015A_54009_1k_v1_0.tif"
ghsTif <- raster::raster(ghsFile)
elmat <- matrix(raster::extract(ghsTif, raster::extent(ghsTif), buffer=1000),
               nrow = ncol(ghsTif), ncol = nrow(ghsTif))

nr <- nrow(elmat)
nc <- ncol(elmat)

latMin <- min(nordicRegions$lat)
latMax <- max(nordicRegions$lat)
lonMin <- min(nordicRegions$long)
lonMax <- max(nordicRegions$long)
lonBins <- 100
latBins <- 100

popMatrix <- matrix(nrow = latBins, ncol = lonBins)

for (iLon in 1:lonBins) {
    
    for (iLat in 1:latBins) {
        
        lon <- lonMin + (lonMax - lonMin) * (iLon - 0.5) / lonBins
        lat <- latMin + (latMax - latMin) * (iLat - 0.5) / latBins
        
        lon1 <- lonMin + (lonMax - lonMin) * (iLon - 1) / lonBins
        lon2 <- lonMin + (lonMax - lonMin) * iLon / lonBins
        lat1 <- latMin + (latMax - latMin) * (iLat - 1) / latBins
        lat2 <- latMin + (latMax - latMin) * iLat / latBins
        
        p11 <- mapproject(x = lon1, y = lat1, proj = "mollweide", par = NULL, orientation = c(90, 0, 0))
        p12 <- mapproject(x = lon2, y = lat1, proj = "mollweide", par = NULL, orientation = c(90, 0, 0))
        p21 <- mapproject(x = lon1, y = lat2, proj = "mollweide", par = NULL, orientation = c(90, 0, 0))
        p22 <- mapproject(x = lon2, y = lat2, proj = "mollweide", par = NULL, orientation = c(90, 0, 0))
        
        j11 <- floor(nr * (1 - p11$y) / 4)
        j12 <- floor(nr * (1 - p12$y) / 4)
        j21 <- ceiling(nr * (1 - p21$y) / 4)
        j22 <- ceiling(nr * (1 - p22$y) / 4)
        
        i11 <- floor(nr * (p11$x + 2) / 4)
        i12 <- ceiling(nr * (p12$x + 2) / 4)
        i21 <- floor(nr * (p21$x + 2) / 4)
        i22 <- ceiling(nr * (p22$x + 2) / 4)
        
        iMin <- median(i21, i22)
        iMax <- median(i11, i12)
        jMin <- median(j11, j21)
        jMax <- median(j12, j22)
        
        if (iMin < nr && jMin < nc) {
            
            iMin <- min(nr, max(1, iMin))
            iMax <- min(nr, max(1, iMax))
            jMin <- min(nc, max(1, jMin))
            jMax <- min(nc, max(1, jMax))
            
            popMatrix[iLat, iLon] <- sum(elmat[iMin:iMax, jMin:jMax])
            
        }
    }
}

colnames(popMatrix) <- paste0("lon", 1:ncol(popMatrix))
rownames(popMatrix) <- paste0("lat", 1:nrow(popMatrix))
popDF <- as.data.frame(popMatrix) %>% 
    mutate(latBin = 1:latBins) %>%
    gather(key = "lonKey", value = "nPop", -"latBin")
popDF$lonBin <- as.numeric(factor(popDF$lonKey, levels = paste0("lon", 1:ncol(popMatrix))))
popDF$lat <- latMin + (popDF$latBin - 0.5) * (latMax - latMin) / latBins
popDF$lon <- lonMin + (popDF$lonBin - 0.5) * (lonMax - lonMin) / lonBins
popDF$nPop[is.na(popDF$nPop)] <- 0
popDF$nPopLog <- ifelse(popDF$nPop > 1, log10(popDF$nPop), 0)

ggplot() +
    geom_map(data = world, map = world, mapping = aes(map_id = region), fill = "lightyellow2") + 
    geom_path(data = nordicRegions, mapping = aes(x = long, y = lat, group = group)) + 
    geom_raster(data = popDF, mapping = aes(x = lon, y = lat, alpha = nPopLog), fill = "darkred") +
    scale_fill_scico(palette = "bilbao", na.value = "white") + 
    scale_x_continuous(limits = c(lonMin, lonMax)) +
    scale_y_continuous(limits = c(latMin, latMax)) +
    scale_alpha_continuous(range = c(0, 1)) +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.background = element_blank(),
          panel.grid = element_blank())


ggplot() +
    geom_path(data = world, mapping = aes(x=long, y=lat, group=group)) + 
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.background = element_blank(),
          panel.grid = element_blank())


ggplot() +
    geom_raster(data = popDF, mapping = aes(x = lon, y = lat, fill = log10(nPop))) +
    scale_fill_scico(palette = "bilbao", na.value = "white") + 
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.background = element_blank(),
          panel.grid = element_blank())

elmatTemp <- elmat[sort(sample(1:nrow(elmat), 1000)), sort(sample(1:ncol(elmat), 1000))]
colnames(elmatTemp) <- paste0("x", 1:ncol(elmatTemp))
rownames(elmatTemp) <- paste0("y", 1:nrow(elmatTemp))

popDFTemp <- as.data.frame(elmatTemp) %>% 
    mutate(y = -(1:nrow(elmatTemp))) %>%
    gather(key = "xKey", value = "nPop", -"y")
popDFTemp$x <- as.numeric(factor(popDFTemp$xKey, levels = paste0("x", 1:ncol(elmatTemp))))

ggplot() +
    geom_raster(data = popDFTemp, mapping = aes(x = x, y = y, fill = log10(nPop))) +
    scale_fill_scico(palette = "bilbao", na.value = "white") + 
    theme(legend.position = "none",
          panel.background = element_blank(),
          panel.grid = element_blank())

elmatTemp <- elmat2[sort(sample((nrow(elmat2)/3):(2*nrow(elmat2)/3), 2000)), sort(sample(1:(ncol(elmat2)/5), 2000))]
elmatTemp %>%
    sphere_shade(texture = "unicorn") %>%
    plot_map()

ggplot() +
    geom_raster(data = popDF, mapping = aes(x = lon, y = -lat, fill = log10(nPop))) +
    scale_fill_scico(palette = "bilbao", na.value = "white") + 
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.background = element_blank(),
          panel.grid = element_blank())


png(paste0("docs\\test.png"))
popMatrix %>%
    sphere_shade(texture = "unicorn") %>%
    plot_map()
dummy = dev.off()



iMin <- min(i11, i12, i21, i22)
iMax <- max(i11, i12, i21, i22)
jMin <- min(j11, j12, j21, j22)
jMax <- max(j11, j12, j21, j22)

popTemp <- 0

for (i in iMin:iMax) {
    
    for (j in jMin:jMax) {
        
        i1 <- i11
        if (j12 != j11) {
            i1 <- i11 + (j - j11) / (j12 - j11) * (i12 - i11)
        }
        
        i2 <- i21
        if (j22 != j21) {
            i2 <- i2 + (j - j21) / (j22 - j21) * (i22 - i21)
        }
        
        j1 <- j11
        if (i21 != i11) {
            j1 <- j1 + (i - i11) / (i21 - i11) * (j21 - j11)
        }
        
        j2 <- j12
        if (i22 != i12) {
            j2 <- j2 + (i - i12) / (i22 - i12) * (j22 - j12)
        }
        
        i1 <- max(i1, 1)
        i2 <- min(i2, nr)
        j1 <- max(j1, 1)
        j2 <- min(j2, nc)
        
        if (i >= i1 && i <= i2 && j >= j1 && j <= j2) {
            
            popTemp <- popTemp + elmat[i, j]
            foundMatrix[iLon, iLat] <- foundMatrix[iLon, iLat] + 1
            
        }
    }
}

popMatrix[iLon, iLat] <- popTemp




dr = nr / 20
dc = nc / 10

for (i in 1:20) {
    for (j in 1:10) {
        
        r1 = 1 + floor(dr * (i-1))
        r2 = ceiling(dr * i)
        
        c1 = 1 + floor(dc * (j-1))
        c2 = ceiling(dc * j)
        
        elmatTemp = elmat[r1:r2, c1:c2]
        
        png(paste0("docs\\", i, "_", j, ".png"))
        elmatTemp %>%
            sphere_shade(texture = "unicorn") %>%
            plot_map()
        dummy = dev.off()
        
    }
}

r1 = 17500
r2 = 19500
c1 = 600
c2 = 2600

elmatTemp = elmat[r1:r2, c1:c2]

png(paste0("docs\\Scandinavia.png"))
elmatTemp %>%
    sphere_shade() %>%
    add_water(detect_water(elmatTemp)) %>%
    plot_map()
dummy = dev.off()

r1 = 11500
r2 = 12000
c1 = 3500
c2 = 4000

elmatTemp = elmat[r1:r2, c1:c2]

png(paste0("docs\\Boston.png"))
elmatTemp %>%
    sphere_shade(texture = "bw") %>%
    add_water(detect_water(elmatTemp)) %>%
    plot_map()
dummy = dev.off()

r1 = 20000
r2 = 21000
c1 = 4700
c2 = 6100

elmatTemp = elmat[r1:r2, c1:c2]

png(paste0("docs\\Nile.png"))
elmatTemp %>%
    sphere_shade(texture = "desert") %>%
    plot_map()
dummy = dev.off()

r1 = 16500
r2 = 20000
c1 = 1500
c2 = 4500

elmatTemp = elmat[r1:r2, c1:c2]
png(paste0("docs\\EU.png"), width = 1600, height = 900)
elmatTemp %>%
    sphere_shade() %>%
    add_water(detect_water(elmatTemp)) %>%
    plot_map()
dummy = dev.off()





elmat %>%
    sphere_shade(texture = "desert") %>%
    plot_map()
