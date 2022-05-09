library(sf)
library(leaflet)
library(tidyverse)
library(arcos)
library(tigris)



library(tidyverse)
library(rgeos)
library(rgdal)
library(maptools)
library(viridis)
library(plotly)
library(readxl)


#https://stackoverflow.com/questions/59740419/unzipping-and-reading-shape-file-in-r-without-rgdal-installed

setwd("C:/Users/NHaswell/Downloads/shp")
url = "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Local Authority Districts (December 2019) Boundaries UK BFC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"
shp <- read_sf(url)
shp = shp %>% dplyr::filter(!str_detect(lad11cd, "W"))

CT0467 <- read_excel("C:/Users/NHaswell/Downloads/CT0467.xlsx",
                     sheet = "LA", skip = 9)
CT0467 = subset(CT0467, substr(`LA code`,1,1)!="W")

shp <- merge(shp, CT0467, by.x = 'lad11cd', by.y = 'LA code', all.x = TRUE)
shp$p = exp(shp$`Prediction of loneliness`)
shp$p = ifelse(is.na(shp$p), mean(shp$p,na.rm=TRUE), shp$p)
shp$p = shp$p * 100


shp$p %>%
    quantile(probs = seq(0, 1, length.out = 4)) -> quantiles_Area


pop = readr::read_csv("https://raw.githubusercontent.com/nujcharee/DSDA_Data/main/pop.csv")
lookup = readr::read_csv("https://raw.githubusercontent.com/nujcharee/DSDA_Data/main/Local_Authority_District_to_Region_(April_2019)_Lookup_in_England.csv")
shp = shp %>% left_join(pop %>% dplyr::select(Code, Population),by = c("lad11cd"="Code"))
shp = shp %>% left_join(lookup %>% dplyr::select(LAD19CD, RGN19NM),by = c("lad11cd"="LAD19CD"))

# create 3 buckets for Area
pop %>%  filter(Geography=="Unitary Authority") -> la
la$Population %>%
    quantile(probs = seq(0, 1, length.out = 4)) -> quantiles_Pop



bivariate_color_scale <- tibble(
    "3 - 3" = "#3F2949", # high inequality, high income
    "2 - 3" = "#435786",
    "1 - 3" = "#4885C1", # low inequality, high income
    "3 - 2" = "#77324C",
    "2 - 2" = "#806A8A", # medium inequality, medium income
    "1 - 2" = "#89A1C8",
    "3 - 1" = "#AE3A4E", # high inequality, low income
    "2 - 1" = "#BC7C8F",
    "1 - 1" = "#CABED0" # low inequality, low income
) %>%
    gather("group", "fill")



shp %<>%
    mutate(
        Area_quantiles = cut(
            p,
            breaks = quantiles_Area,
            include.lowest = TRUE
        ),
        POP_quantiles = cut(
            Population,
            breaks = quantiles_Pop,
            include.lowest = TRUE
        ),
        # by pasting the factors together as numbers we match the groups defined
        # in the tibble bivariate_color_scale
        group = paste(
            as.numeric(Area_quantiles), "-",
            as.numeric(POP_quantiles)
        )
    ) %>%
    # we now join the actual hex values per "group"
    # so each municipality knows its hex value based on the his gini and avg
    # income value
    left_join(bivariate_color_scale, by = "group")

map <- ggplot(
    # use the same dataset as before
    data = shp
) +

    # color municipalities according to their gini / income combination
    geom_sf(
        aes(
            fill = shp$fill
        ),
        # use thin white stroke for municipalities
        color = "white",
        size = 0.1
    ) +
    scale_fill_identity()+
    # add titles
    labs(x = NULL,
         y = NULL,
         title = "Population Density for Districts of Bihar",
         caption = "by :Ayush Patel\n inspired from Timo Grossenbacher") +
    # add the theme
    theme_map()




bivariate_color_scale %<>%
    separate(group, into = c("Area", "Pop"), sep = " - ") %>%
    mutate(Area = as.integer(Area),
           Pop = as.integer(Pop))

legend <- ggplot() +
    geom_tile(
        data = bivariate_color_scale,
        mapping = aes(
            x = Area,
            y = Pop,
            fill = fill)
    ) +
    scale_fill_identity() +
    labs(x = "Larger Area--->",
         y = "Higher Population--->") +
    theme_map() +
    # make font small enough
    theme(
        axis.title = element_text(size = 6)
    ) +
    # quadratic tiles
    coord_fixed()

library(cowplot)

ggdraw() +
    draw_plot(map, 0, 0, 1, 1) +
    draw_plot(legend, 0.75, 0.07, 0.15, 0.15) -> Biharbivariate

Biharbivariate
