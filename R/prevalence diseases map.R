
library(tidyr)
library(dplyr)
library(ggplot2)
library(rayshader)
library(maps)
library(ggmap)
library(mapproj)
library(scico)
library(wbstats)

sizeX <- 160
sizeY <- 90

# Load health data



diabetesFile <- "resources\\world_bank\\prevalence_diabetes.txt"

diabetesDF <- read.table(diabetesFile, header = T, sep = "\t", stringsAsFactors = F, quote = "")

# Load population data
ghsFile = "resources\\GHS_POP_GPW42015_GLOBE_R2015A_54009_1k_v1_0\\GHS_POP_GPW42015_GLOBE_R2015A_54009_1k_v1_0.tif"
ghsTif = raster::raster(ghsFile)
popMatrix = matrix(
    raster::extract(ghsTif, raster::extent(ghsTif), buffer=1000),
               nrow = ncol(ghsTif), ncol = nrow(ghsTif))


# Load world map
world <- map_data("world")
world <- world %>% filter(region != "Antarctica")


# Bin population
longMin <- -2
longMax <- 2
longD <- longMax - longMin
latMin <- -0.8
latMax <- 0.8
latD <- latMax - latMin

populationDF <- data.frame(
    longMin = numeric(sizeX * sizeY), 
    longMax = numeric(sizeX * sizeY), 
    latMin = numeric(sizeX * sizeY), 
    latMax = numeric(sizeX * sizeY), 
    pop = numeric(sizeX * sizeY)) 

n <- 1

for (i in 1:sizeX) {
    
    long1 <- longMin + longD / sizeX * (i - 1)
    long2 <- longMin + longD / sizeX * i
    
    popI1 <- floor(nrow(popMatrix) / sizeX * (i - 1))
    popI2 <- ceiling(nrow(popMatrix) / sizeX * i)
    
    for (j in 1:sizeY) {
        
        lat1 <- latMin + latD / sizeY * (j - 1)
        lat2 <- latMin + latD / sizeY * j
        
        popJ1 <- floor(ncol(popMatrix) / sizeY * (j - 1))
        popJ2 <- ceiling(ncol(popMatrix) / sizeY * j)
        
        popTemp <- sum(popMatrix[popI1:popI2, popJ1:popJ2])
        
        populationDF[n, ] <- c(long1, long2, -lat2, -lat1, popTemp)
        
        n <- n+1
        
    }
}

populationDF <- populationDF %>% filter(pop > 0)
populationDF$popLog <- log10(populationDF$pop)

ggplot() +
    geom_rect(data = populationDF, mapping = aes(xmin = longMin, xmax = longMax, ymin = latMin, ymax = latMax, fill = pop)) +
    geom_path(data = world, mapping = aes(x=long, y=lat, group=group)) + 
    scale_fill_scico(palette = "bilbao") + 
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.background = element_blank(),
          panel.grid = element_blank())

ggplot() +
    geom_rect(data = populationDF, mapping = aes(xmin = longMin, xmax = longMax, ymin = latMin, ymax = latMax, fill = pop)) +
    scale_fill_scico(palette = "bilbao") + 
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.background = element_blank(),
          panel.grid = element_blank())

ggplot() +
    geom_path(data = world, mapping = aes(x=long, y=lat, group=group)) + 
    scale_fill_scico(palette = "bilbao") + 
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.background = element_blank(),
          panel.grid = element_blank())


