
#AUfgabe 1
library("readr")
wildschwein_BE <- read_delim("Datasets/wildschwein_BE.csv", 
                             delim = ",", escape_double = FALSE, trim_ws = TRUE)


#Aufgabe 2
library("ggplot2")
ggplot(wildschwein_BE, aes(Long, Lat, colour = TierID))+
geom_point() +
theme(legend.position = "none")


library("sf")
wildschwein_BE_sf <- st_as_sf(wildschwein_BE,
                              coords = c("Long", "Lat"),
                              crs = 4326
)


wildschwein_BE
wildschwein_BE_sf

is.data.frame(wildschwein_BE_sf)

# subset rows
wildschwein_BE_sf[1:10, ]
wildschwein_BE_sf[wildschwein_BE_sf$TierName == "Sabi", ]

# subset colums
wildschwein_BE_sf[, 2:3]

wildschwein_BE <- st_as_sf(wildschwein_BE,
                           coords = c("Long", "Lat"),
                           crs = 4326
)

remove(wildschwein_BE_sf)
# we can remove this sf object, since it just eats up our memory


wildschwein_BE_2056 <- wildschwein_BE  |>  st_transform(2056)

#Result
wildschwein_BE




#Aufgabe 3
library("dplyr")

wildschwein_BE_grouped <- group_by(wildschwein_BE, TierID)

wildschwein_BE_grouped

wildschwein_BE_smry <- summarise(wildschwein_BE_grouped)

wildschwein_BE_smry

mcp <- st_convex_hull(wildschwein_BE_smry)


#Aufgabe 4
plot(mcp)
#vedi soluzioni 

#Importing Raster data
library("terra")

pk100_BE <- terra::rast("Datasets/pk100_BE.tif")

pk100_BE

plot(pk100_BE)

plotRGB(pk100_BE)


#Aufgabe 5
library("tmap")

tm_shape(pk100_BE) +
  tm_rgb() +
  tm_shape(mcp) +
  tm_polygons(col = "TierID", alpha = 0.4, border.col = "red") +
  tm_legend(bg.color = "white")

#Aufgabe 6
tmap_mode("view")

tm_shape(mcp) +
  tm_polygons(col = "TierID", alpha = 0.4, border.col = "red") +
  tm_legend(bg.color = "white")


####Solution
# task_1.R
################################################################################



library("readr") # move this to the top of your script

# Data import ####
wildschwein_BE <- read_delim("Datasets/wildschwein_BE.csv", ",")

# Check Timezone
attr(wildschwein_BE$DatetimeUTC, "tzone") # or
wildschwein_BE$DatetimeUTC[1]




# task_2.R
################################################################################


library("ggplot2") # move this to the top of your script

ggplot(wildschwein_BE, aes(Long, Lat, colour = TierID)) +
  geom_point() +
  theme(legend.position = "none")




# task_3.R
################################################################################



library("sf") # move this to the top of your script

# Input: Handling spatial data
wildschwein_BE <- st_as_sf(wildschwein_BE,
                           coords = c("Long", "Lat"),
                           crs = 4326
)

wildschwein_BE <- st_transform(wildschwein_BE, 2056)




# task_4.R
################################################################################



library("dplyr") # move this to the top of your script

wildschwein_BE_grouped <- group_by(wildschwein_BE, TierID)

wildschwein_BE_smry <- summarise(wildschwein_BE_grouped)

mcp <- st_convex_hull(wildschwein_BE_smry)

ggplot(mcp, aes(fill = TierID)) +
  geom_sf(alpha = 0.4)

ggplot(mcp, aes(fill = TierID)) +
  geom_sf(alpha = 0.4) +
  coord_sf(datum = 2056)




# task_5.R
################################################################################



library("tmap") # move this to the top of your script

# Input: Importing raster data
library("terra") # move this to the top of your script

pk100_BE <- terra::rast("Datasets/pk100_BE.tif")

mcp <- st_convex_hull(wildschwein_BE_smry)

tm_shape(pk100_BE) +
  tm_rgb() +
  tm_shape(mcp) +
  tm_polygons(col = "TierID", alpha = 0.4, border.col = "red") +
  tm_legend(bg.color = "white")




# task_6.R
################################################################################


tmap_mode("view")

tm_shape(mcp) +
  tm_polygons(col = "TierID", alpha = 0.4, border.col = "red") +
  tm_legend(bg.color = "white")
