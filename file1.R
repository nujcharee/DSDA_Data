install.packages("sf")
install.packages("tidyverse")
install.packages("arcos")
install.packages("tigris")
install.packages("rgeos")
install.packages("rgdal")
install.packages("viridis")
install.packages("plotly")
install.packages("readxl")


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
url1 = "https://raw.githubusercontent.com/nujcharee/DSDA_Data/main/CT0467.xlsx"
# Download files to temp folder

temp <- tempfile()
temp2 <- tempfile()

# Download the zip file and save to 'temp' 

download.file(url, temp)

# Unzip the contents of the temp and save unzipped content in 'temp2'
unzip(zipfile = temp, exdir = temp2)

shp <- readOGR(temp2)
shp <- fortify(shp, region = 'lad11cd')
shp = shp %>% dplyr::filter(!str_detect(id, "W"))


download.file(url1, destfile = "/tmp/file.xlsx")
CT0467 = readxl::read_excel("/tmp/file.xlsx",sheet = "LA", skip = 9)
CT0467 = subset(CT0467, substr(`LA code`,1,1)!="W")

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


# Read Population Estimates & Region lookup -----------------------------------------------

pop = readr::read_csv("https://raw.githubusercontent.com/nujcharee/DSDA_Data/main/pop.csv")
lookup = readr::read_csv("https://raw.githubusercontent.com/nujcharee/DSDA_Data/main/Local_Authority_District_to_Region_(April_2019)_Lookup_in_England.csv")
shp = shp %>% left_join(pop %>% dplyr::select(Code, Population),by = c("id"="Code"))
shp = shp %>% left_join(lookup %>% dplyr::select(LAD19CD, RGN19NM),by = c("id"="LAD19CD"))

#-3.396750623	0.033481889 (higher risk)
#-4.146532043	0.015819182 (lower risk)

# top 10
top_10 = CT0467 %>%
  left_join(lookup, by=c("LA code"="LAD19CD")) %>%
  mutate(prop = exp(`Prediction of loneliness`)) %>%
  top_n(10,`Prediction of loneliness`) %>%
  ggplot(., aes(x = reorder(`LA name`, `Prediction of loneliness`), y = exp(`Prediction of loneliness`)*100),fill=`LA name`) +
  labs(title = "Top 10 highest relative risk of loneliness"
       , subtitle = "Seen in multiple regions in England\n"
       , caption = "Note: Relative risk in the scale of 1-3 where 3 being the higest and 1 lowest"
       , y="", x="") +
  geom_bar(stat = "identity", fill= c("#E40046"
                                      , "#E40046"
                                      , "#E40046"
                                      , "#582C83"
                                      , "#00A5DF"
                                      , "#1D57A5"
                                      , "#1D57A5"
                                      , "#1D57A5"
                                      , "#1D57A5"
                                      , "#1D57A5")) +
  coord_flip() +
  geom_text(aes(label=RGN19NM), colour="white", size = 4, position = position_stack(vjust = 0.5)) +
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

## bottom 10

bom_10 = CT0467 %>%
  left_join(lookup, by=c("LA code"="LAD19CD")) %>%
  mutate(prop = exp(`Prediction of loneliness`)) %>%
  top_n(-10,`Prediction of loneliness`) %>%
  ggplot(., aes(x = reorder(`LA name`, `Prediction of loneliness`), y = exp(`Prediction of loneliness`)*100),fill=`LA name`) +
  labs(title = "Bottom 10 highest relative risk of loneliness"
       , subtitle = "South East has lowest risk overall\nException is Rutland - the smallest local authority in England\n"
       , caption = "Note: Relative risk in the scale of 0-1 where 1 being the higest and 0 lowest"
       , y="", x="") +
  coord_flip() +
  geom_bar(stat = "identity", fill= c("#1D57A5"
                                      , "#84BD00"
                                      , "#84BD00"
                                      , "#84BD00"
                                      , "#84BD00"
                                      , "#84BD00"
                                      , "#84BD00"
                                      , "#84BD00"
                                      , "#84BD00"
                                      , "#84BD00")) +
  
  geom_text(aes(label=RGN19NM), colour="white", size = 4, position = position_stack(vjust = 0.5)) +
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
## https://stackoverflow.com/questions/49735290/ggplot2-color-individual-words-in-title-to-match-colors-of-groups

## https://r-spatial.org/r/2018/10/25/ggplot2-sf-3.html