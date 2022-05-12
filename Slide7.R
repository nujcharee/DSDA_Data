#######################################################################################################
## Bivariate Analysis: Is there a relationship between prevalence of loneliness and number of population
## Author: Nujcharee Haswell
########################################################################################################


# Load libraries ----------------------------------------------------------

#### Uncoment the following section if libraries arent already installed

# install.packages("sf")
# install.packages("tidyverse")
# install.packages("arcos")
# install.packages("tigris")
# install.packages("rgeos")
# install.packages("rgdal")
# install.packages("viridis")
# install.packages("plotly")
# install.packages("readxl")

##########


library(sf)
library(tidyverse)
library(arcos)
library(tigris)
library(rgeos)
library(rgdal)
library(viridis)
library(plotly)
library(readxl)
library(openxlsx)
source(file = "map.R")


# Read data ---------------------------------------------------------------


url = "https://raw.githubusercontent.com/nujcharee/DSDA_Data/main/Local_Authority_Districts_(December_2011)_Boundaries_EW_BFC.zip"

# Download files to temp folder

temp <- tempfile()
temp2 <- tempfile()
# Download the zip file and save to 'temp'

download.file(url, temp)


# Unzip the contents of the temp and save unzipped content in 'temp2', 'temp4'
unzip(zipfile = temp, exdir = temp2)


shp <- read_sf(temp2)
shp = shp %>% dplyr::filter(!str_detect(lad11cd, "W"))


CT0467 = read_csv("https://raw.githubusercontent.com/nujcharee/DSDA_Data/main/CT0467.csv")
CT0467 = CT0467[1:326,]

## Population
pop = readr::read_csv("https://raw.githubusercontent.com/nujcharee/DSDA_Data/main/pop.csv")



# Data wrangling ----------------------------------------------------------


shp <- merge(shp, CT0467, by.x = 'lad11cd', by.y = 'LA code', all.x = TRUE)

## COnvert point estimate to odd ra

shp$p = exp(shp$`Prediction of loneliness`) / (1+exp(shp$`Prediction of loneliness`))
shp$p = ifelse(is.na(shp$p), mean(shp$p,na.rm=TRUE), shp$p)
shp$p = shp$p * 100

# read data
shp = shp %>% left_join(pop %>% dplyr::select(Code, Population),by = c("lad11cd"="Code"))
#shp = shp %>% left_join(lookup %>% dplyr::select(LAD19CD, RGN19NM),by = c("lad11cd"="LAD19CD"))



# Bivariate Analysis ------------------------------------------------------
# https://rpubs.com/ayushbipinpatel/593942
# create 3 buckets for Area
shp$p %>%
    quantile(probs = seq(0, 1, length.out = 4)) -> quantiles_Area

# create 3 buckets for Area
pop %>%  filter(Geography=="Unitary Authority") -> la
la$Population %>%
    quantile(probs = seq(0, 1, length.out = 4)) -> quantiles_Pop


bivariate_color_scale <- tibble(
    "3 - 3" = "#3F2949", # high pop, high risk
    "2 - 3" = "#435786",
    "1 - 3" = "#4885C1", # low pop, high risk
    "3 - 2" = "#77324C",
    "2 - 2" = "#806A8A", # medium pop, medium risk
    "1 - 2" = "#89A1C8",
    "3 - 1" = "#AE3A4E", # high pop, low risk
    "2 - 1" = "#BC7C8F",
    "1 - 1" = "#CABED0" # low pop, low risk
) %>%
    gather("group", "fill")



shp = shp %>%
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
        group = paste(
            as.numeric(Area_quantiles), "-",
            as.numeric(POP_quantiles)
        )
    ) %>%

    left_join(bivariate_color_scale, by = "group")

shp = shp %>% mutate(fill = ifelse(is.na(fill), "#806A8A", fill))

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
         title = "More People, more lonely?",
         subtitle = "Bivariate Analysis showing relationships between prevalance and population by colour scale",
         caption = "Source: ONS Population Estimate 2018") +
    theme_map() +

    theme(
        text = element_text(color = "#22211d"),
        plot.background = element_rect(fill = "#f5f5f2", color = NA),
        panel.background = element_rect(fill = "#f5f5f2", color = NA),
        legend.background = element_rect(fill = "#f5f5f2", color = NA),
        #axis.text.x=element_blank(),
        plot.title = element_text(size= 20, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        plot.subtitle = element_text(size= 12, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
        plot.caption = element_text( size=8, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),

        legend.position = c(0.7, 0.09)
    )

map



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
    #labs(x="", y="") +
    labs(x = "Higher Prevalence--->",
         y = "Higher Population--->") +
    # make font small enough
    theme_map() +
    theme(plot.background = element_rect(fill = "#f5f5f2", color = NA),
          panel.background = element_rect(fill = "#f5f5f2", color = NA)) +
    coord_fixed()

legend
# library(cowplot)

#ggdraw() +
#    draw_plot(map, 0, 0, 1, 1) +
#    draw_plot(legend, 0.5, 0.01, 5, 5) -> Biharbivariate

# Biharbivariate


