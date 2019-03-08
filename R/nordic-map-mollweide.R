
library(tidyr)
library(dplyr)
library(ggplot2)
library(scico)
library(rayshader)
library(mapproj)
library(maps)
library(sp)
library(png)
library(ggrepel)
library(gtable)
library(grid)
library(gridSVG)

# Load nordic region data

world <- map_data("world", proj = "mollweide") %>% filter(long > -0.5)

colors <- scico(n = 6, palette = "batlow")

regionsOfInterest <- c("Iceland", "Norway", "Sweden", "Finland", "Estonia", "Denmark", "Greenland", "Faroe Islands")
regionsColors <- c(colors[6], colors[5], colors[6], colors[4], colors[6], colors[1], colors[2], colors[3])
nordicRegions <- world %>% filter(region %in% regionsOfInterest)

nordicRegionsList <- list()
nordicPolygonsList <- list()

for (i in 1:length(regionsOfInterest))  {
    
    regionName <- regionsOfInterest[i]
    region <- world %>% filter(region == regionName)
    nordicRegionsList[[i]] <- region
    nordicPolygonsList[[i]] <- Polygons(list(Polygon(region[, c("long", "lat")])), regionName)
    
}

nordicSpatialPolygons <- SpatialPolygons(nordicPolygonsList)

frameRegions <- world %>% filter(region %in% c("Iceland", "Norway", "Sweden", "Finland", "Estonia", "Denmark", "Faroe Islands"))
latMin <- min(frameRegions$lat)
latMax <- max(frameRegions$lat)
latMinExt <- latMin - 0.02 * (latMax - latMin)
latMaxExt <- latMax + 0.02 * (latMax - latMin)
latRef <- (latMax + latMin) / 2

lonMin <- min(frameRegions$long)
lonMax <- max(frameRegions$long)
lonMinExt <- lonMin - 0.05 * (lonMax - lonMin)
lonMaxExt <- lonMax + 0.01 * (lonMax - lonMin)


# Load population data

ghsFile <- "resources\\GHS_POP_GPW42015_GLOBE_R2015A_54009_1k_v1_0\\GHS_POP_GPW42015_GLOBE_R2015A_54009_1k_v1_0.tif"
ghsTif <- raster::raster(ghsFile)
elmat <- matrix(raster::extract(ghsTif, raster::extent(ghsTif), buffer=1000),
               nrow = ncol(ghsTif), ncol = nrow(ghsTif))

nr <- nrow(elmat)
nc <- ncol(elmat)

# Raster matrix

elmatRaster <- matrix(nrow = nr, ncol = nc)

for (i in 2:(nr-1)) {
    for (j in 2:(nc-1)) {
        
        elmatRaster[i, j] <- (elmat[i-1, j-1] + elmat[i-1, j] + elmat[i-1, j+1] + 
            elmat[i, j-1] + elmat[i, j] + elmat[i, j+1] + 
            elmat[i+1, j-1] + elmat[i+1, j] + elmat[i+1, j+1])/9
        
    }
}



# Align

lonBins <- 2000
latBins <- 2000

iLon <- rep(1:lonBins, each = latBins)
iLat <- rep(1:latBins, times = lonBins)

popDF <- data.frame(iLon, iLat)
popDF <- popDF %>% mutate(
    lon1 = lonMinExt + (lonMaxExt - lonMinExt) * (iLon - 1) / lonBins,
    lon2 = lonMinExt + (lonMaxExt - lonMinExt) * (iLon) / lonBins,
    lat1 = latMinExt + (latMaxExt - latMinExt) * (iLat - 1) / latBins,
    lat2 = latMinExt + (latMaxExt - latMinExt) * (iLat) / latBins,
    lon = lonMinExt + (lonMaxExt - lonMinExt) * (iLon - 0.5) / lonBins,
    lat = latMinExt + (latMaxExt - latMinExt) * (iLat - 0.5) / latBins,
    regionIndex = over(x = SpatialPoints(coords = data.frame(x = lon, y = lat)), y = nordicSpatialPolygons, returnList = F)
)

lonX0 <- 0.017
lonX1 <- 1.02
lonX2 <- 0
lonY1 <- -0.02
latX0 <- 0.153
latX1 <- 1.19
latX2 <- 0

regionsPopDF <- popDF %>% 
    mutate(
        i = round(nr * (lonX0 + lonX1 * lon + lonX2 * (lon^2) + lonY1 * (lat - latRef) / (latMax - latMin) + 2) / 4),
        j = round(nc * (1 - (-latX0 + latX1 * lat + latX2 * (lat^2))) / 2)
    )

regionsPopDF$i[regionsPopDF$i < 1] <- 1
regionsPopDF$j[regionsPopDF$j < 1] <- 1
regionsPopDF$i[regionsPopDF$i > nr] <- nr
regionsPopDF$j[regionsPopDF$j > nc] <- nc

regionsPopDF <- regionsPopDF %>%
    mutate(
        k = (j - 1) * nr + i,
        nPop = elmatRaster[k]
    )

regionsPopDF$nPop[is.na(regionsPopDF$nPop)] <- 0
regionsPopDF$nPopLog <- ifelse(regionsPopDF$nPop > 1, log10(regionsPopDF$nPop), 0)
regionsPopDF <- regionsPopDF[regionsPopDF$nPopLog > 0, ]
regionsPopDF <- regionsPopDF[order(regionsPopDF$nPopLog), ]
regionsPopDF$regionIndex[is.na(regionsPopDF$regionIndex)] <- max(regionsPopDF$regionIndex, na.rm = T) + 1
regionsPopDF$region <- factor(regionsPopDF$regionIndex)

faroe <- world %>% filter(region == "Faroe Islands")
iceland <- world %>% filter(region == "Iceland")
norway <- world %>% filter(region == "Norway" & subregion != "Svalbard")
svalbard <- world %>% filter(subregion == "Svalbard")
sweden <- world %>% filter(region == "Sweden")
finland <- world %>% filter(region == "Finland")
estonia <- world %>% filter(region == "Estonia")
denmark <- world %>% filter(region == "Denmark")
aland <- world %>% filter(region == "Finland" & subregion == "Aland Islands")

labelsDF <- data.frame(
    lon = c(
        lonMinExt + 0.1 * (lonMaxExt - lonMinExt), 
        (max(faroe$long) + min(faroe$long)) / 2,
        (max(iceland$long) + min(iceland$long)) / 2,
        (max(norway$long) + min(norway$long)) / 2,
        min(svalbard$long) + 0.25 * (max(svalbard$long) - min(svalbard$long)),
        min(sweden$long) + 0.4 * (max(sweden$long) - min(sweden$long)),
        (max(finland$long) + min(finland$long)) / 2,
        (max(estonia$long) + min(estonia$long)) / 2,
        min(denmark$long) + 0.2 * (max(denmark$long) - min(denmark$long)),
        (max(aland$long) + min(aland$long)) / 2
        ),
    lat = c(
        latMinExt + 0.85 * (latMaxExt - latMinExt), 
        (max(faroe$lat) + min(faroe$lat)) / 2,
        (max(iceland$lat) + min(iceland$lat)) / 2,
        min(norway$lat) + 0.25 * (max(norway$lat) - min(norway$lat)),
        (max(svalbard$lat) + min(svalbard$lat)) / 2,
        min(sweden$lat) + 0.75 * (max(sweden$lat) - min(sweden$lat)),
        min(finland$lat) + 0.75 * (max(finland$lat) - min(finland$lat)),
        (max(estonia$lat) + min(estonia$lat)) / 2,
        (max(denmark$lat) + min(denmark$lat)) / 2,
        (max(aland$lat) + min(aland$lat)) / 2
        ),
    label = c(
        "Greenland\n56 k inhab",
        "Faroe Islands\n51 k inhab",
        "Iceland\n357 k inhab",
        "Norway\n5.3 mil inhab",
        "Svalbard\n3 k inhab",
        "Sweden\n10 mil inhab",
        "Finland\n5.5 mil inhab",
        "Estonia\n1.3 mil inhab",
        "Denmark\n5.8 mil inhab",
        "Åland Islands\n30 k inhab"),
    nx = c(
        0,
        -0.05,
        0.07,
        -0.05,
        -0.05,
        -0.075,
        0,
        0,
        -0.05,
        0.05),
    ny = c(
        0,
        -0.01,
        0.01,
        0.01,
        -0.01,
        0.02,
        0.06,
        -0.03,
        0,
        0.035
    ))

popPlot <- ggplot() + theme_bw(base_size = 16) +
    geom_map(data = world, map = world, mapping = aes(map_id = region), fill = "ivory2") + 
    geom_map(data = nordicRegions, map = nordicRegions, mapping = aes(map_id = region, fill = region), alpha = 0.5) + 
    geom_path(data = nordicRegions, mapping = aes(x = long, y = lat, group = group), col = "black") + 
    geom_rect(data = regionsPopDF, mapping = aes(xmin = lon1, xmax = lon2, ymin = lat1, ymax = lat2, alpha = nPopLog), fill = "black", col = NA) +
    geom_label_repel(data = labelsDF, mapping = aes(x = lon, y = lat, label = label), nudge_x = labelsDF$nx, nudge_y = labelsDF$ny, size = 6) +
    scale_fill_manual(values = regionsColors, guide = F) +
    scale_alpha(name = bquote('Inhab per km'^2~''), breaks = c(1:5), labels = c("10", "100", "1,000", "10,000", "100,000")) +
    scale_x_continuous(limits = c(lonMinExt, lonMaxExt), expand = c(0, 0)) +
    scale_y_continuous(limits = c(latMinExt, latMaxExt), expand = c(0, 0)) +
    theme(legend.position = "right",
          legend.text.align = 1,
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "azure3"),
          panel.border = element_rect(colour = "grey20", fill = NA))

png("docs/population.png", height = 600, width = 800)
popPlot
dummy <- dev.off()

svg(filename = "docs/population.svg", height = 600/72, width = 800/72)
popPlot
dummy <- dev.off()


# Languages

labelsLanguageDF <- data.frame(
    lon = c(
        lonMinExt + 0.1 * (lonMaxExt - lonMinExt), 
        (max(faroe$long) + min(faroe$long)) / 2,
        (max(iceland$long) + min(iceland$long)) / 2,
        (max(norway$long) + min(norway$long)) / 2,
        min(svalbard$long) + 0.25 * (max(svalbard$long) - min(svalbard$long)),
        min(sweden$long) + 0.4 * (max(sweden$long) - min(sweden$long)),
        (max(finland$long) + min(finland$long)) / 2,
        (max(estonia$long) + min(estonia$long)) / 2,
        min(denmark$long) + 0.2 * (max(denmark$long) - min(denmark$long)),
        (max(finland$long) + min(sweden$long)) / 2
    ),
    lat = c(
        latMinExt + 0.85 * (latMaxExt - latMinExt), 
        (max(faroe$lat) + min(faroe$lat)) / 2,
        (max(iceland$lat) + min(iceland$lat)) / 2,
        min(norway$lat) + 0.25 * (max(norway$lat) - min(norway$lat)),
        (max(svalbard$lat) + min(svalbard$lat)) / 2,
        min(sweden$lat) + 0.33 * (max(sweden$lat) - min(sweden$lat)),
        min(finland$lat) + 0.33 * (max(finland$lat) - min(finland$lat)),
        (max(estonia$lat) + min(estonia$lat)) / 2,
        (max(denmark$lat) + min(denmark$lat)) / 2,
        min(norway$lat) + 0.95 * (max(norway$lat) - min(norway$lat))
    ),
    label = c(
        "Inuit\n[Eskimo-Aleut]",
        "Faroese\n[North Germanic]",
        "Icelandic\n[North Germanic]",
        "Norwegian\n[North Germanic]",
        "Norwegian\n[North Germanic]",
        "Swedish\n[North Germanic]",
        "Finnish\n[Finno-Ugric]",
        "Estonian\n[Finno-Ugric]",
        "Danish\n[North Germanic]",
        "Sami\n[Finno-Ugric]"),
    nx = c(
        0,
        -0.05,
        0.07,
        -0.05,
        -0.075,
        0.05,
        0.05,
        0,
        -0.05,
        -0.05),
    ny = c(
        0,
        -0.01,
        0.02,
        0.01,
        -0.01,
        0.015,
        0.06,
        -0.03,
        0,
        0.03
    ))

mapPlot <- ggplot() + theme_bw(base_size = 13) +
    geom_map(data = world, map = world, mapping = aes(map_id = region), fill = "ivory2") + 
    geom_map(data = nordicRegions, map = nordicRegions, mapping = aes(map_id = region, fill = region), alpha = 0.5) + 
    geom_path(data = nordicRegions, mapping = aes(x = long, y = lat, group = group), col = "black") + 
    geom_label_repel(data = labelsLanguageDF, mapping = aes(x = lon, y = lat, label = label), nudge_x = labelsLanguageDF$nx, nudge_y = labelsLanguageDF$ny, size = 6) +
    scale_fill_manual(values = regionsColors, guide = F) +
    scale_alpha(name = bquote('Density [hab.km'^-2~']'), breaks = c(1:5), labels = c("10", "100", "1,000", "10,000", "100,000")) +
    scale_x_continuous(limits = c(lonMinExt, lonMaxExt), expand = c(0, 0)) +
    scale_y_continuous(limits = c(latMinExt, latMaxExt), expand = c(0, 0)) +
    theme(legend.position = "none",
          legend.text.align = 1,
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "azure3"),
          panel.border = element_rect(colour = "grey20", fill = NA))

png("docs/language.png", height = 600, width = 800)
mapPlot
dummy <- dev.off()


# Cities

labelsCitiesDF <- data.frame(
    name = c(
        "Reykjavik",
        "Bergen",
        "Oslo",
        "Gothenburg",
        "Stockholm",
        "Tampere",
        "Helsinki",
        "Tallinn",
        "Tartu",
        "Copenhagen",
        "Aarhus"
    ),
    nx = c(
        0.02,
        -0.02,
        -0.02,
        0.02,
        -0.02,
        -0.02,
        0.02,
        -0.02,
        0.02,
        0.02,
        -0.02
    ),
    type = c(
        1,
        2,
        1,
        2,
        1,
        2,
        1,
        1,
        2,
        1,
        2
    ),
    lat = c(
        64.126518,
        60.391262,
        59.913868,
        57.708870,
        59.329323,
        61.497753,
        60.169857,
        59.436962,
        58.377983,
        55.676098,
        56.162937
    ),
    lon = c(
        -21.817438,
        5.322054,
        10.752245,
        11.974560,
        18.068581,
        23.760954,
        24.938379,
        24.753574,
        26.729038,
        12.568337,
        10.203921
    )
)

labelsCitiesDF <- data.frame(
    name = c(
        "Reykjavik",
        "Bergen",
        "Oslo",
        "Gothenburg",
        "Stockholm",
        "Tampere",
        "Helsinki",
        "Tallinn",
        "Tartu",
        "Copenhagen",
        "Aarhus",
        "Longyearbyen",
        "Tórshavn",
        "Mariehamn"
    ),
    nx = c(
        0.02,
        -0.02,
        -0.02,
        0.02,
        -0.02,
        -0.02,
        0.02,
        -0.02,
        0.02,
        0.02,
        -0.02,
        -0.03,
        -0.03,
        -0.03
    ),
    type = c(
        1,
        2,
        1,
        2,
        1,
        2,
        1,
        1,
        2,
        1,
        2,
        1,
        1,
        1
    ),
    lat = c(
        64.126518,
        60.391262,
        59.913868,
        57.708870,
        59.329323,
        61.497753,
        60.169857,
        59.436962,
        58.377983,
        55.676098,
        56.162937,
        78.225357,
        62.009156,
        60.096931
    ),
    lon = c(
        -21.817438,
        5.322054,
        10.752245,
        11.974560,
        18.068581,
        23.760954,
        24.938379,
        24.753574,
        26.729038,
        12.568337,
        10.203921,
        15.625890,
        -6.774977,
        19.934820
    )
)
proj <- mapproject(x = labelsCitiesDF$lon, y = labelsCitiesDF$lat, projection = "mollweide", orientation = c(90, 0, 5))
labelsCitiesDF$x <- proj$x
labelsCitiesDF$y <- proj$y
labelsCitiesDF$shape <- factor(labelsCitiesDF$type)

mapPlot <- ggplot() + theme_bw(base_size = 16) +
    geom_map(data = world, map = world, mapping = aes(map_id = region), fill = "ivory2") + 
    geom_map(data = nordicRegions, map = nordicRegions, mapping = aes(map_id = region, fill = region), alpha = 0.5) + 
    geom_path(data = nordicRegions, mapping = aes(x = long, y = lat, group = group), col = "black") + 
    geom_point(data = labelsCitiesDF, mapping = aes(x = x, y = y, shape = shape), size = 4) +
    geom_text_repel(data = labelsCitiesDF, mapping = aes(x = x, y = y, label = name), direction = "x", nudge_x = labelsCitiesDF$nx, size = 6, force = 0.1) +
    scale_fill_manual(values = regionsColors, guide = F) +
    scale_shape_manual(values = c(15, 16)) +
    scale_alpha(name = bquote('Density [hab.km'^-2~']'), breaks = c(1:5), labels = c("10", "100", "1,000", "10,000", "100,000")) +
    scale_x_continuous(limits = c(lonMinExt, lonMaxExt), expand = c(0, 0)) +
    scale_y_continuous(limits = c(latMinExt, latMaxExt), expand = c(0, 0)) +
    theme(legend.position = "none",
          legend.text.align = 1,
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "azure3"),
          panel.border = element_rect(colour = "grey20", fill = NA))

png("docs/cities.png", height = 600, width = 800)
mapPlot
dummy <- dev.off()

lomax <- lonMaxExt
samiDF <- data.frame(
    yl = c(0.868, 0.864, 0.860, 0.856, 0.852, 0.848, 0.844, 0.840,
           0.836, 0.832, 0.828, 0.836, 0.832, 0.828, 0.824, 0.820,
           0.816, 0.812, 0.808, 0.804, 0.800, 0.796, 0.792, 0.788,
           0.784),
    x1 = c(0.100, 0.090, 0.075, 0.070, 0.060, 0.055, 0.045, 0.060,
           0.120, 0.125, 0.129, 0.057, 0.053, 0.050, 0.048, 0.047,
           0.045, 0.042, 0.038, 0.032, 0.035, 0.038, 0.040, 0.042,
           0.045),
    x2 = c(0.130, 0.140, 0.145, 0.160, 0.180, lomax, lomax, lomax,
           0.140, 0.135, 0.131, 0.110, 0.109, 0.108, 0.107, 0.106,
           0.105, 0.100, 0.095, 0.090, 0.085, 0.080, 0.077, 0.070,
           0.060)
)

germanicColors <- scico(n = 5, palette = "devon", end = 0.8)
uralicColors <- c(
    scico(n = 1, palette = "lajolla", begin = 0.35, end = 0.35),
    scico(n = 1, palette = "lajolla", begin = 0.5, end = 0.5),
    scico(n = 1, palette = "lajolla", begin = 0.8, end = 0.8)
)

inuitColors <- scico(n = 1, palette = "cork", begin = 0.75)
languages <- c("Icelandic", "Faroese", "Danish", "Norwegian", "Swedish", "Sami", "Finnish", "Estonian", "Inuit")
orderedRegions <- c("Iceland", "Faroe Islands", "Denmark", "Norway", "Sweden", "Sami", "Finland", "Estonia", "Greenland")
languagesColors <- c(germanicColors[1], germanicColors[2], germanicColors[3], germanicColors[4], germanicColors[5], uralicColors[3], uralicColors[2], uralicColors[1], inuitColors[1])
tempRegion <- data.frame(
    long = -1, 
    lat = -1, 
    group = -1,
    order = -1,
    region = "Sami",
    subregion = "Sami"
)
nordicRegionsTemp <- rbind(nordicRegions, tempRegion)
nordicRegionsTemp$orderedRegions <- factor(nordicRegionsTemp$region, levels = orderedRegions)

langCityPlot <- ggplot() + theme_bw(base_size = 16) +
    geom_map(data = world, map = world, mapping = aes(map_id = region), fill = "ivory2") + 
    geom_map(data = nordicRegionsTemp, map = nordicRegions, mapping = aes(map_id = region, fill = orderedRegions), alpha = 1) + 
    geom_map(data = aland, map = aland, mapping = aes(map_id = region), fill = germanicColors[5], alpha = 1) + 
    geom_segment(data = samiDF, mapping = aes(x = x1, xend = x2, y = yl, yend = yl), size = 1, col = uralicColors[3]) +
    geom_point(data = labelsCitiesDF, mapping = aes(x = x, y = y, shape = shape), size = 4) +
    geom_label_repel(data = labelsCitiesDF, mapping = aes(x = x, y = y, label = name), nudge_x = labelsCitiesDF$nx, size = 6, force = 0.1) +
    scale_shape_manual(values = c(15, 16), guide = F) +
    scale_fill_manual(name = "Language", values = languagesColors, labels = languages, breaks = orderedRegions) +
    scale_alpha(name = bquote('Density [hab.km'^-2~']'), breaks = c(1:5), labels = c("10", "100", "1,000", "10,000", "100,000")) +
    scale_x_continuous(limits = c(lonMinExt, lonMaxExt), expand = c(0, 0)) +
    scale_y_continuous(limits = c(latMinExt, latMaxExt), expand = c(0, 0)) +
    theme(legend.position = "right",
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "azure3"),
          panel.border = element_rect(colour = "grey20", fill = NA))

png("docs/language_cities.png", height = 600, width = 800)
langCityPlot
dummy <- dev.off()

svg(filename = "docs/language_cities.svg", height = 600/72, width = 800/72)
langCityPlot
dummy <- dev.off()

