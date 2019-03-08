
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(scico)
library(wbstats)

theme_set(theme_bw(base_size = 16))

# Load health data

wbTables <- wbcache()

populationIndicator <- "SP.POP.TOTL"
populationTitle <- wbTables$indicators$indicator[wbTables$indicators$indicatorID == populationIndicator]
populationDF <- wb(indicator = populationIndicator, mrv = 20)
populationDF$date <- as.numeric(populationDF$date)

diabetesIndicator <- "SH.STA.DIAB.ZS"
diabetesTitle <- wbTables$indicators$indicator[wbTables$indicators$indicatorID == diabetesIndicator]
diabetesDF <- wb(indicator = diabetesIndicator, mrv = 20)
diabetesDF$date <- as.numeric(diabetesDF$date)

nComDiseaseMortalityIndicator <- "SH.DYN.NCOM.ZS"
nComDiseaseMortalityTitle <- wbTables$indicators$indicator[wbTables$indicators$indicatorID == nComDiseaseMortalityIndicator]
nComDiseaseMortalityDF <- wb(indicator = nComDiseaseMortalityIndicator, mrv = 20)
nComDiseaseMortalityDF$date <- as.numeric(nComDiseaseMortalityDF$date)

overweightAdultsIndicator <- "SH.STA.OWAD.ZS"
overweightAdultsTitle <- wbTables$indicators$indicator[wbTables$indicators$indicatorID == overweightAdultsIndicator]
overweightAdultsDF <- wb(indicator = overweightAdultsIndicator, mrv = 20)
overweightAdultsDF$date <- as.numeric(overweightAdultsDF$date)


# Plot Parameters

regionOI <- data.frame(
    id = c("DNK", "EST", "FIN", "ISL", "NOR", "SWE"),
    name = c("Denmark", "Estonia", "Finland", "Iceland", "Norway", "Sweden")
)

regionWorld <- data.frame(
    id = c("WLD"),
    name = c("World")
)

regionContinent <- data.frame(
    id = c("SSF", "SAS", "EAS", "ECS", "LCN", "MEA", "NAC", "EUU"),
    name = c("Sub-Saharan Africa", "South Asia", "East Asia & Pacific", "Europe & Central Asia", "Latin America & the Caribbean", "Middle East & North Africa", "North America", "European Union")
)

regionIncome <- data.frame(
    id = c("FCS", "HPC", "LIC", "LMC", "UMC", "HIC"),
    name = c("Fragile and conflict affected situations", "Heavily indebted poor countries (HIPC)", "Low income", "Lower middle income", "Upper middle income", "High income")
)

abbreviations <- paste0(regionIncome$id, ": ", regionIncome$name, collapse = ", ")

# Population growth plot by geographic region

colorsDF <- rbind(regionContinent, regionWorld, regionOI)
colorsDF$color <- c(
    rep("gray80", nrow(regionContinent)),
    "black",
    scico(n = nrow(regionOI), palette = "hawaii", end = 0.8)
)

plotDF <- populationDF %>% 
    filter(iso3c %in% colorsDF$id) %>%
    mutate(id = factor(iso3c, levels = colorsDF$id)) %>%
    arrange(id)

plotDF$coi <- ifelse(plotDF$iso3c %in% regionOI$id, "Yes", "No")
plotDF$coi <- factor(plotDF$coi, levels = c("Yes", "No"))

plotDF$growth <- NA

for (countryId in colorsDF$id) {
    
    if (countryId %in% plotDF$iso3c) {
        
        tempDF <- plotDF %>% filter(iso3c == countryId)
        
        refValue <- tempDF$value[which.min(tempDF$date)]
        
        plotDF$growth[plotDF$iso3c == countryId] <- plotDF$value[plotDF$iso3c == countryId] / refValue * 100
        
    }
}

startDate <- min(plotDF$date)
endDate <- max(plotDF$date)
dateBreaks <- seq(from = 2000, to = 2015, by = 5)

detailedRegions <- c("WLD", as.character(regionOI$id))
lastPoint <- plotDF %>% 
    filter(date == endDate) %>%
    mutate(valueMillions = format(value/1000000, digits = 2, big.mark = ",")) %>%
    mutate(label = ifelse(iso3c %in% detailedRegions, 
                          paste0(iso3c, " (", valueMillions, ")"),
                          iso3c))

populationGrowthContinent <- ggplot() +
    geom_line(data = plotDF, mapping = aes(x = date, y = growth, col = id, size = coi, linetype = coi)) +
    geom_text_repel(data = lastPoint, mapping = aes(x = endDate, y = growth, col = id, label = label), direction = "y", nudge_x = 0.5, segment.alpha = 0.5, segment.size = 0.5, size = 6, hjust = 0) +
    scale_color_manual(values = colorsDF$color) +
    scale_size_manual(values  = c(1.2, 0.8)) +
    scale_linetype_manual(values  = c("solid", "dotted")) +
    scale_x_continuous(limits = c(startDate, endDate + 1.5), breaks = dateBreaks, expand = expand_scale(mult = 0.02)) +
    scale_y_continuous(name = paste0("Population [100 in ", startDate, "]")) +
    theme(legend.position = "none",
          axis.title.x = element_blank())

png(filename = "docs\\health\\population_continent.png", width = 1600, height = 900)
plot(populationGrowthContinent)
dummy <- dev.off()


# Population growth plot by income region

colorsDF <- rbind(regionIncome, regionWorld, regionOI)
colorsDF$color <- c(
    rep("gray80", nrow(regionIncome)),
    "black",
    scico(n = nrow(regionOI), palette = "hawaii", end = 0.8)
)

plotDF <- populationDF %>% 
    filter(iso3c %in% colorsDF$id) %>%
    mutate(id = factor(iso3c, levels = colorsDF$id)) %>%
    arrange(id)

plotDF$coi <- ifelse(plotDF$iso3c %in% regionOI$id, "Yes", "No")
plotDF$coi <- factor(plotDF$coi, levels = c("Yes", "No"))

plotDF$growth <- NA

for (countryId in colorsDF$id) {
    
    if (countryId %in% plotDF$iso3c) {
        
        tempDF <- plotDF %>% filter(iso3c == countryId)
        
        refValue <- tempDF$value[which.min(tempDF$date)]
        
        plotDF$growth[plotDF$iso3c == countryId] <- plotDF$value[plotDF$iso3c == countryId] / refValue * 100
        
    }
}

startDate <- min(plotDF$date)
endDate <- max(plotDF$date)
dateBreaks <- seq(from = 2000, to = 2015, by = 5)

detailedRegions <- c("WLD", as.character(regionOI$id))
lastPoint <- plotDF %>% 
    filter(date == endDate) %>%
    mutate(valueMillions = format(value/1000000, digits = 2, big.mark = ",")) %>%
    mutate(label = ifelse(iso3c %in% detailedRegions, 
                          paste0(iso3c, " (", valueMillions, ")"),
                          iso3c))

populationGrowthIncome <- ggplot() +
    geom_line(data = plotDF, mapping = aes(x = date, y = growth, col = id, size = coi, linetype = coi)) +
    geom_text_repel(data = lastPoint, mapping = aes(x = endDate, y = growth, col = id, label = label), direction = "y", nudge_x = 0.5, segment.alpha = 0.5, segment.size = 0.5, size = 6, hjust = 0) +
    scale_color_manual(values = colorsDF$color) +
    scale_size_manual(values  = c(1.2, 0.8)) +
    scale_linetype_manual(values  = c("solid", "dotted")) +
    scale_x_continuous(limits = c(startDate, endDate + 1.5), breaks = dateBreaks, expand = expand_scale(mult = 0.02)) +
    scale_y_continuous(name = paste0("Population [100 in ", startDate, "]")) +
    theme(legend.position = "none",
          axis.title.x = element_blank())

png(filename = "docs\\health\\population_income.png", width = 1600, height = 900)
plot(populationGrowthIncome)
dummy <- dev.off()


# Diabetes Prevalence by geographic region

colorsDF <- rbind(regionContinent, regionWorld, regionOI)
colorsDF$color <- c(
    rep("gray80", nrow(regionContinent)),
    "black",
    scico(n = nrow(regionOI), palette = "hawaii", end = 0.8)
)

plotDF <- diabetesDF %>% 
    filter(iso3c %in% colorsDF$id) %>%
    arrange(desc(value)) %>%
    mutate(id = factor(iso3c, levels = as.character(iso3c)))
colorsDF <- colorsDF %>%
    mutate(orderedId = factor(id, levels = levels(plotDF$id))) %>%
    arrange(orderedId)

plotDF$coi <- ifelse(plotDF$iso3c %in% regionOI$id, "Yes", "No")
plotDF$coi <- factor(plotDF$coi, levels = c("Yes", "No"))

detailedRegions <- c("WLD", as.character(regionOI$id))
plotDF <- plotDF %>% 
    mutate(label = ifelse(iso3c %in% detailedRegions, 
                          paste0(" ", format(value, digits = 3)),
                          ""))

diabetesContinent <- ggplot() +
    geom_col(data = plotDF, mapping = aes(x = id, y = value, fill = id)) +
    geom_text(data = plotDF, mapping = aes(x = id, y = value, col = id, label = label), angle = 90, hjust = 0, size = 6) +
    scale_fill_manual(values = colorsDF$color) +
    scale_color_manual(values = colorsDF$color) +
    ylab("Diabetes Prevalence [%, age 20-79]") +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

png(filename = "docs\\health\\diabetes_continent.png", width = 1600, height = 900)
plot(diabetesContinent)
dummy <- dev.off()


# Diabetes Prevalence by income region

colorsDF <- rbind(regionIncome, regionWorld, regionOI)
colorsDF$color <- c(
    rep("gray80", nrow(regionIncome)),
    "black",
    scico(n = nrow(regionOI), palette = "hawaii", end = 0.8)
)

plotDF <- diabetesDF %>% 
    filter(iso3c %in% colorsDF$id) %>%
    arrange(desc(value)) %>%
    mutate(id = factor(iso3c, levels = as.character(iso3c)))
colorsDF <- colorsDF %>%
    mutate(orderedId = factor(id, levels = levels(plotDF$id))) %>%
    arrange(orderedId)

plotDF$coi <- ifelse(plotDF$iso3c %in% regionOI$id, "Yes", "No")
plotDF$coi <- factor(plotDF$coi, levels = c("Yes", "No"))

detailedRegions <- c("WLD", as.character(regionOI$id))
plotDF <- plotDF %>% 
    mutate(label = ifelse(iso3c %in% detailedRegions, 
                          paste0(" ", format(value, digits = 3)),
                          ""))

diabetesIncome <- ggplot() +
    geom_col(data = plotDF, mapping = aes(x = id, y = value, fill = id)) +
    geom_text(data = plotDF, mapping = aes(x = id, y = value, col = id, label = label), angle = 90, hjust = 0, size = 6) +
    scale_fill_manual(values = colorsDF$color) +
    scale_color_manual(values = colorsDF$color) +
    ylab("Diabetes Prevalence [%, age 20-79]") +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

png(filename = "docs\\health\\diabetes_income.png", width = 1600, height = 900)
plot(diabetesIncome)
dummy <- dev.off()


# Mortality from non-communicable diseases plot by geographic region

colorsDF <- rbind(regionContinent, regionWorld, regionOI)
colorsDF$color <- c(
    rep("gray80", nrow(regionContinent)),
    "black",
    scico(n = nrow(regionOI), palette = "hawaii", end = 0.8)
)

plotDF <- nComDiseaseMortalityDF %>% 
    filter(iso3c %in% colorsDF$id) %>%
    mutate(id = factor(iso3c, levels = colorsDF$id)) %>%
    arrange(id)

plotDF$coi <- ifelse(plotDF$iso3c %in% regionOI$id, "Yes", "No")
plotDF$coi <- factor(plotDF$coi, levels = c("Yes", "No"))

startDate <- min(plotDF$date)
endDate <- max(plotDF$date)
dateBreaks <- seq(from = 2000, to = 2015, by = 5)

detailedRegions <- c("WLD", as.character(regionOI$id))
lastPoint <- plotDF %>% 
    filter(date == endDate) %>%
    mutate(label = ifelse(iso3c %in% detailedRegions, 
                          paste0(iso3c, " (", format(value, digits = 2), ")"),
                          iso3c))

mortalityNcdContinent <- ggplot() +
    geom_line(data = plotDF, mapping = aes(x = date, y = value, col = id, size = coi, linetype = coi)) +
    geom_text_repel(data = lastPoint, mapping = aes(x = endDate, y = value, col = id, label = label), direction = "y", nudge_x = 0.5, segment.alpha = 0.5, segment.size = 0.5, size = 6, hjust = 0) +
    scale_color_manual(values = colorsDF$color) +
    scale_size_manual(values  = c(1.2, 0.8)) +
    scale_linetype_manual(values  = c("solid", "dotted")) +
    scale_x_continuous(limits = c(startDate, endDate + 1.5), breaks = dateBreaks, expand = expand_scale(mult = 0.02)) +
    scale_y_continuous(name = "Mortality from CVD, Cancer, Diabetes, or CRD [%, age 30-70]") +
    theme(legend.position = "none",
          axis.title.x = element_blank())

png(filename = "docs\\health\\mortality_ncd_continent.png", width = 1600, height = 900)
plot(mortalityNcdContinent)
dummy <- dev.off()


# Mortality from non-communicable diseases plot by income region

colorsDF <- rbind(regionIncome, regionWorld, regionOI)
colorsDF$color <- c(
    rep("gray80", nrow(regionIncome)),
    "black",
    scico(n = nrow(regionOI), palette = "hawaii", end = 0.8)
)

plotDF <- nComDiseaseMortalityDF %>% 
    filter(iso3c %in% colorsDF$id) %>%
    mutate(id = factor(iso3c, levels = colorsDF$id)) %>%
    arrange(id)

plotDF$coi <- ifelse(plotDF$iso3c %in% regionOI$id, "Yes", "No")
plotDF$coi <- factor(plotDF$coi, levels = c("Yes", "No"))

startDate <- min(plotDF$date)
endDate <- max(plotDF$date)
dateBreaks <- seq(from = 2000, to = 2015, by = 5)

detailedRegions <- c("WLD", as.character(regionOI$id))
lastPoint <- plotDF %>% 
    filter(date == endDate) %>%
    mutate(label = ifelse(iso3c %in% detailedRegions, 
                          paste0(iso3c, " (", format(value, digits = 2), ")"),
                          iso3c))

mortalityNcdIncome <- ggplot() +
    geom_line(data = plotDF, mapping = aes(x = date, y = value, col = id, size = coi, linetype = coi)) +
    geom_text_repel(data = lastPoint, mapping = aes(x = endDate, y = value, col = id, label = label), direction = "y", nudge_x = 0.5, segment.alpha = 0.5, segment.size = 0.5, size = 6, hjust = 0) +
    scale_color_manual(values = colorsDF$color) +
    scale_size_manual(values  = c(1.2, 0.8)) +
    scale_linetype_manual(values  = c("solid", "dotted")) +
    scale_x_continuous(limits = c(startDate, endDate + 1.5), breaks = dateBreaks, expand = expand_scale(mult = 0.02)) +
    scale_y_continuous(name = "Mortality from CVD, Cancer, Diabetes, or CRD [%, age 30-70]") +
    theme(legend.position = "none",
          axis.title.x = element_blank())

png(filename = "docs\\health\\mortality_ncd_income.png", width = 1600, height = 900)
plot(mortalityNcdIncome)
dummy <- dev.off()


# Overweight plot by geographic region

colorsDF <- rbind(regionContinent, regionWorld, regionOI)
colorsDF$color <- c(
    rep("gray80", nrow(regionContinent)),
    "black",
    scico(n = nrow(regionOI), palette = "hawaii", end = 0.8)
)

plotDF <- overweightAdultsDF %>% 
    filter(iso3c %in% colorsDF$id) %>%
    mutate(id = factor(iso3c, levels = colorsDF$id)) %>%
    arrange(id)

plotDF$coi <- ifelse(plotDF$iso3c %in% regionOI$id, "Yes", "No")
plotDF$coi <- factor(plotDF$coi, levels = c("Yes", "No"))

startDate <- min(plotDF$date)
endDate <- max(plotDF$date)
dateBreaks <- seq(from = 2000, to = 2015, by = 5)

detailedRegions <- c("WLD", as.character(regionOI$id))
lastPoint <- plotDF %>% 
    filter(date == endDate) %>%
    mutate(label = ifelse(iso3c %in% detailedRegions, 
                          paste0(iso3c, " (", format(value, digits = 2), ")"),
                          iso3c))

overweightContinent <- ggplot() +
    geom_line(data = plotDF, mapping = aes(x = date, y = value, col = id, size = coi, linetype = coi)) +
    geom_text_repel(data = lastPoint, mapping = aes(x = endDate, y = value, col = id, label = label), direction = "y", nudge_x = 0.5, segment.alpha = 0.5, segment.size = 0.5, size = 6, hjust = 0) +
    scale_color_manual(values = colorsDF$color) +
    scale_size_manual(values  = c(1.2, 0.8)) +
    scale_linetype_manual(values  = c("solid", "dotted")) +
    scale_x_continuous(limits = c(startDate, endDate + 1.5), breaks = dateBreaks, expand = expand_scale(mult = 0.02)) +
    scale_y_continuous(name = "Prevalence of overweight [%, adults]") +
    theme(legend.position = "none",
          axis.title.x = element_blank())

png(filename = "docs\\health\\overweight_continent.png", width = 1600, height = 900)
plot(overweightContinent)
dummy <- dev.off()


# Overweight plot by income region

colorsDF <- rbind(regionIncome, regionWorld, regionOI)
colorsDF$color <- c(
    rep("gray80", nrow(regionIncome)),
    "black",
    scico(n = nrow(regionOI), palette = "hawaii", end = 0.8)
)

plotDF <- overweightAdultsDF %>% 
    filter(iso3c %in% colorsDF$id) %>%
    mutate(id = factor(iso3c, levels = colorsDF$id)) %>%
    arrange(id)

plotDF$coi <- ifelse(plotDF$iso3c %in% regionOI$id, "Yes", "No")
plotDF$coi <- factor(plotDF$coi, levels = c("Yes", "No"))

startDate <- min(plotDF$date)
endDate <- max(plotDF$date)
dateBreaks <- seq(from = 2000, to = 2015, by = 5)

detailedRegions <- c("WLD", as.character(regionOI$id))
lastPoint <- plotDF %>% 
    filter(date == endDate) %>%
    mutate(label = ifelse(iso3c %in% detailedRegions, 
                          paste0(iso3c, " (", format(value, digits = 2), ")"),
                          iso3c))

overweightIncome <- ggplot() +
    geom_line(data = plotDF, mapping = aes(x = date, y = value, col = id, size = coi, linetype = coi)) +
    geom_text_repel(data = lastPoint, mapping = aes(x = endDate, y = value, col = id, label = label), direction = "y", nudge_x = 0.5, segment.alpha = 0.5, segment.size = 0.5, size = 6, hjust = 0) +
    scale_color_manual(values = colorsDF$color) +
    scale_size_manual(values  = c(1.2, 0.8)) +
    scale_linetype_manual(values  = c("solid", "dotted")) +
    scale_x_continuous(limits = c(startDate, endDate + 1.5), breaks = dateBreaks, expand = expand_scale(mult = 0.02)) +
    scale_y_continuous(name = "Prevalence of overweight [%, adults]") +
    theme(legend.position = "none",
          axis.title.x = element_blank())

png(filename = "docs\\health\\overweight_income.png", width = 1600, height = 900)
plot(overweightIncome)
dummy <- dev.off()

    
