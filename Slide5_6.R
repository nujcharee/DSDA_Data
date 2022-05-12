#######################################################################################################
## Top 10 / Bottom 10: Comparison of risks by local authories / regions
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



# Read Population Estimates & Region lookup -----------------------------------------------

pop = readr::read_csv("https://raw.githubusercontent.com/nujcharee/DSDA_Data/main/pop.csv")
lookup = readr::read_csv("https://raw.githubusercontent.com/nujcharee/DSDA_Data/main/Local_Authority_District_to_Region_(April_2019)_Lookup_in_England.csv")
shp = shp %>% left_join(pop %>% dplyr::select(Code, Population),by = c("id"="Code"))
shp = shp %>% left_join(lookup %>% dplyr::select(LAD19CD, RGN19NM),by = c("id"="LAD19CD"))

# top 10
top_10 = CT0467 %>%
  left_join(lookup, by=c("LA code"="LAD19CD")) %>%
  mutate(prop = exp(`Prediction of loneliness`)) %>%
  top_n(10,`Prediction of loneliness`) %>%
  ggplot(., aes(x = reorder(`LA name`, `Prediction of loneliness`), y = exp(`Prediction of loneliness`)*100),fill=`LA name`) +
  labs(title = "Top 10 highest relative risk of loneliness"
       , subtitle = "Seen in multiple regions in England\n Relative risk in the scale of 1-3 where 3 being the highest and 1 lowest\n"
       , x="", y="") +
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
  scale_y_continuous(limits = c(0,3.5), expand = c(0, 0)) +
  geom_text(aes(label=RGN19NM), colour="white", size = 4, position = position_stack(vjust = 0.5)) +
  
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
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

top_10

## bottom 10

bom_10 = CT0467 %>%
  left_join(lookup, by=c("LA code"="LAD19CD")) %>%
  mutate(prop = exp(`Prediction of loneliness`)) %>%
  top_n(-10,`Prediction of loneliness`) %>%
  ggplot(., aes(x = reorder(`LA name`, `Prediction of loneliness`), y = exp(`Prediction of loneliness`)*100),fill=`LA name`) +
  labs(title = "Bottom 10 lowest relative risk of loneliness"
       , subtitle = "South East has lowest risk overall\nException is Rutland - the smallest local authority in England\nRelative risk in the scale of 0-1 where 1 being the higest and 0 lowest\n"
       , y="", x="") +
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
  coord_flip() +
  scale_y_continuous(limits = c(0,1.6), expand = c(0, 0)) +
  geom_text(aes(label=RGN19NM), colour="white", size = 4, position = position_stack(vjust = 0.5)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
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

bom_10
