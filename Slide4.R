#######################################################################################################
## Univariate Analysis: Detecting hotspots of eldery at risk of loneliness
## Author: Nujcharee Haswell
########################################################################################################

# install.packages("sf")
# install.packages("tidyverse")
# install.packages("arcos")
# install.packages("tigris")
# install.packages("rgeos")
# install.packages("rgdal")
# install.packages("viridis")
# install.packages("plotly")
# install.packages("readxl")


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


url = "https://raw.githubusercontent.com/nujcharee/DSDA_Data/main/Local_Authority_Districts_(December_2011)_Boundaries_EW_BFC.zip"
# Download files to temp folder

temp <- tempfile()
temp2 <- tempfile()


# Download the zip file and save to 'temp'

download.file(url, temp)


# Unzip the contents of the temp and save unzipped content in 'temp2', 'temp4'
unzip(zipfile = temp, exdir = temp2)



shp <- readOGR(temp2)
shp <- fortify(shp, region = 'lad11cd')
shp = shp %>% dplyr::filter(!str_detect(id, "W"))


#download.file(url1, destfile = "/tmp/file.xlsx")
CT0467 = read_csv("https://raw.githubusercontent.com/nujcharee/DSDA_Data/main/CT0467.csv")
CT0467 = CT0467[1:326,]

shp <- merge(shp, CT0467, by.x = 'id', by.y = 'LA code', all.x = TRUE)
shp <- arrange(shp, order)

## Convert point estimate to probability

shp$p = exp(shp$`Prediction of loneliness`) / (1+exp(shp$`Prediction of loneliness`))
shp$p = ifelse(is.na(shp$p), mean(shp$p,na.rm=TRUE), shp$p)
shp$p = shp$p * 100


p <- ggplot(data = shp, aes(x = long, y = lat,
                            group = group, fill = p)) +
    geom_polygon() + coord_equal() + theme_void() +
    scale_fill_viridis(trans = "log", option="mako",
                       , breaks=c( 1.261, 1.576,1.754,1.886,3.31)
                       , name="Prevalence in loneliness"
                       , direction = - 1
                       , labels = c("Very Low", "Low", "Medium", "High", "Very High")
                       , guide = guide_legend( keyheight = unit(3, units = "mm")
                                               , keywidth=unit(12, units = "mm")
                                               , label.position = "bottom"
                                               , title.position = 'top', nrow=1
                       ) ) +
    labs(
        title = "Predicted Prevalence of Loneliness in Older People (65+)\nHighest risks found in North East, North West and London",
        subtitle = "A choropleth map describing relative risk of loneliness across 333 Local Authories in England\n",
        caption = "Source Data: Age UK \nThe relative risk of loneliness is based on the Census 2011 figures for the factors: \nmarital status, self-reported health status, age and household size"
    ) +
    theme(
        text = element_text(color = "#22211d"),
        plot.background = element_rect(fill = "#f5f5f2", color = NA),
        panel.background = element_rect(fill = "#f5f5f2", color = NA),
        legend.background = element_rect(fill = "#f5f5f2", color = NA),

        plot.title = element_text(size= 20, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        plot.subtitle = element_text(size= 12, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
        plot.caption = element_text( size=8, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),

        legend.position = c(0.7, 0.09)
    )


p
