# Nordic Maps

This repository contains code and resources used to generate figures on the Norgdic region including the maps displayed in Njølstad _et al._, _Roadmap for A Precision Medicine Initiative in the Nordic Region_.

## Population map

This map was generated using this [script](https://github.com/mvaudel/Nordic-maps/blob/master/R/nordic-map-mollweide.R). In short, the world map using the Mollweide projection is plotted in light yellow over a light blue background. The countries of the Nordic region, composed here of Estonia, Finland, Denmark, Iceland, Norway, and Sweden, and associated territories, the Faroe Islands, Greenland, Svalbard, and the Åland Islands, were plotted in different colors and outlined in black. 
Population density as of 2015 was obtained from the [Global Human Settlement project](https://ghsl.jrc.ec.europa.eu/ghs_pop.php) at a resolution of 1 km (European Commission, Joint Research Centre (JRC); Columbia University, Center for International Earth Science Information Network - CIESIN (2015): GHS population grid, derived from GPW4, multitemporal (1975, 1990, 2000, 2015). European Commission, Joint Research Centre (JRC) PID: [http://data.europa.eu/89h/jrc-ghsl-ghs_pop_gpw4_globe_r2015a](http://data.europa.eu/89h/jrc-ghsl-ghs_pop_gpw4_globe_r2015a)). The density matrix was rasterized in 2000 bins in latitude and longitude over the Nordic region, aligned onto the world map, and displayed in shades of grey. 
Finally, the name along with the number of inhabitants was annotated for each country and associated territory. The number of inhabitants was the latest available in Wikipedia at time of writing.

![](https://github.com/mvaudel/Nordic-maps/blob/master/docs/population.png)
_svg available [here](https://github.com/mvaudel/Nordic-maps/blob/master/docs/population.svg)._

## Language and cities map

This map was generated using this [script](https://github.com/mvaudel/Nordic-maps/blob/master/R/nordic-map-mollweide.R). In short, the world map using the Mollweide projection is plotted in light yellow over a light blue background as in the population map. The Nordic countries and associated territories were colored according to the official language. The Sami language was annotated with horizontal segments. Capitals and major cities were annotated with points and names using their latitude and longitude as obtained from [latlong.net](https://www.latlong.net/). North Germanic languages were annotated using a gradient of blue, Finno-Ugric languages using a red to yellow gradient, and Inuit as part of Eskimo-Aleut languages in green.

![](https://github.com/mvaudel/Nordic-maps/blob/master/docs/language_cities.png)
_svg available [here](https://github.com/mvaudel/Nordic-maps/blob/master/docs/language_cities.svg)._

## Health data

These plots were generated using this [script](https://github.com/mvaudel/Nordic-maps/blob/master/R/prevalence%20diseases.R). In short,  data for the Nordic countries were obtained from the [World Bank](data.worldbank.org) and plotted against background composed of continental and income groups as provided by the World Bank.

**Diabetes Prevalence in 2017 (Continent)**

![](https://github.com/mvaudel/Nordic-maps/blob/master/docs/health/diabetes_continent.png)

**Diabetes Prevalence in 2017 (Income Group)**

![](https://github.com/mvaudel/Nordic-maps/blob/master/docs/health/diabetes_income.png)

**Mortality attributed to Non-communicable diseases (Continent)**

![](https://github.com/mvaudel/Nordic-maps/blob/master/docs/health/mortality_ncd_continent.png)

**Mortality attributed to Non-communicable diseases (Income Group)**

![](https://github.com/mvaudel/Nordic-maps/blob/master/docs/health/mortality_ncd_income.png)

**Overweight (Continent)**

![](https://github.com/mvaudel/Nordic-maps/blob/master/docs/health/overweight_continent.png)

**Overweight (Income Group)**

![](https://github.com/mvaudel/Nordic-maps/blob/master/docs/health/overweight_ncd_income.png)

**Population (Continent)**

![](https://github.com/mvaudel/Nordic-maps/blob/master/docs/health/population_continent.png)

**Population (Income Group)**

![](https://github.com/mvaudel/Nordic-maps/blob/master/docs/health/population_ncd_income.png)

## Dependencies

The figures were generated using the following packages and their dependencies: [tidyr](https://tidyr.tidyverse.org), [dplyr](https://dplyr.tidyverse.org), [ggplot2](https://ggplot2.tidyverse.org), [scico](https://github.com/thomasp85/scico), [mapproj](https://CRAN.R-project.org/package=mapproj), [maps](https://CRAN.R-project.org/package=maps), [ggrepel](https://github.com/slowkow/ggrepel), [wbstats](https://CRAN.R-project.org/package=wbstats).

