packages <- c("tidyverse", "readr", "httpuv",
              "leaflet", "sf", "tigris", "arcos",
              "sp", "rmapshaper")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())), repos = "https://cran.us.r-project.org")
}

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

setwd("C:/Users/NHaswell/Downloads/shp")
shp <- readOGR('Local_Authority_Districts_(December_2011)_Boundaries_EW_BFC.shp')
shp0 = shp
shp <- fortify(shp, region = 'lad11cd')

CT0467 <- read_excel("C:/Users/NHaswell/Downloads/CT0467.xlsx",
                          sheet = "LA", skip = 9)

shp <- merge(shp, CT0467, by.x = 'id', by.y = 'LA code', all.x = TRUE)
shp <- arrange(shp, order)

## probability

shp$p = exp(shp$`Prediction of loneliness`)
shp$p = ifelse(is.na(shp$p), mean(shp$p,na.rm=TRUE), shp$p)
shp$p = shp$p * 100

p <- ggplot(data = shp, aes(x = long, y = lat,
                            group = group, fill = p)) +
    geom_polygon() + coord_equal() + theme_void() +
    scale_fill_viridis(trans = "log", option="mako",
                       , breaks=c( 1.261, 1.576,1.754,1.886,3.31)
                       , name="Prevalence in loneliness"
                       , direction = - 1
                       , labels = c("Very low", "Low", "Medium", "High", "Very High")
                       , guide = guide_legend( keyheight = unit(3, units = "mm")
                                               , keywidth=unit(12, units = "mm")
                                               , label.position = "bottom"
                                               , title.position = 'top', nrow=1
                                                ) ) +

    labs(
        title = "Probability of older people at risk of loneliness",
        subtitle = "England, 2011",
        caption = "Data: Age UK, Designed by N Haswell"
    ) +
    theme(
        text = element_text(color = "#22211d"),
        plot.background = element_rect(fill = "#f5f5f2", color = NA),
        panel.background = element_rect(fill = "#f5f5f2", color = NA),
        legend.background = element_rect(fill = "#f5f5f2", color = NA),

        plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
        plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),

        legend.position = c(0.7, 0.09)
    )

## scale_fill_gradient(high = "#e34a33", low = "#fee8c8", guide = "colorbar") +
##coord_fixed(1.3)

p
## https://r-spatial.org/r/2018/10/25/ggplot2-sf-3.html
library(leaflet)
library(tigris)

shp %>%
    leaflet() %>%
    addTiles() %>%
    addPolygons(popup=~id)


merged_df <- geo_join(shp0, CT0467, "lad11cd", "LA code")

df = subset(merged_df, !is.na(Prediction.of.loneliness))


pal <- colorNumeric("Greens", domain=df$Prediction.of.loneliness)
# Setting up the popup text
popup_sb <- paste0(df$LA.name, ", ",
                   "</br/> Log-odd: \n",
                   as.character(df$Prediction.of.loneliness))

leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(-1.174, 52.35, zoom = 4) %>%
    addPolygons(data = shp0 ,
                fillColor = ~pal(df$p),
                fillOpacity = 1,
                weight = 0.9,
                smoothFactor = 0.2,
                stroke=TRUE,
                color="white",
                popup = ~popup_sb)
