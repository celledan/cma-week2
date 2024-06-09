##Exercise A
library("dplyr")

#Task 1
library("readr")
library("sf")

wildschwein_BE <- read_delim("Datasets/wildschwein_BE_2056.csv", ",")

wildschwein_BE <- st_as_sf(wildschwein_BE, coords = c("E", "N"), crs = 2056)

#Task 2
difftime_secs <- function(later, now){
  as.numeric(difftime(later, now, units = "secs"))
}


wildschwein <- wildschwein_BE |>                                            
  group_by(TierName) |>                                 
  mutate(
    timelag = difftime(lead(DatetimeUTC), DatetimeUTC)
  )

summarise(wildschwein, mean = mean(timelag, na.rm = TRUE))

# 3 individual
# For how long were the individual tracked? Are there gaps? ????????
# Sequentially
# Rosa     1411.625 secs
# Ruth     1599.368 secs
# Sabi     1286.155 secs

#Task 3
distance_by_element <- function(later, now){
  as.numeric(
    st_distance(later, now, by_element = TRUE)
  )
}


wildschwein <- wildschwein |> 

  st_as_sf(coords = c("E","N"), crs = 2056)


later <- lag(wildschwein$geometry)
now <- wildschwein$geometry

wildschwein$steplength <- distance_by_element(later, now)

#Task 4
wildschwein$timelag <- as.numeric(wildschwein$timelag, units = "secs")
wildschwein$speed <- wildschwein$steplength / wildschwein$timelag

#Task 5

wildschwein_sample <- wildschwein |>
  filter(TierName == "Sabi") |> 
  head(100)

library(tmap)
tmap_mode("view")

tm_shape(wildschwein_sample) + 
  tm_dots()


wildschwein_sample_line <- wildschwein_sample |> 
  # dissolve to a MULTIPOINT:
  summarise(do_union = FALSE) |> 
  st_cast("LINESTRING")

tmap_options(basemaps = "OpenStreetMap")

tm_shape(wildschwein_sample_line) +
  tm_lines() +
  tm_shape(wildschwein_sample) + 
  tm_dots()





###Exercise B
caro <- read_delim("Datasets/caro60.csv", ",")

library("readr")
library("sf")
library("dplyr")

difftime_secs <- function(x, y){
  as.numeric(difftime(x, y, units = "secs"))
}

distance_by_element <- function(later, now){
  as.numeric(
    st_distance(later, now, by_element = TRUE)
  )
}


caro <- caro |> st_as_sf(coords = c("E","N"), crs = 2056) |> 
  select(DatetimeUTC)


#Task 1
later <- lag(caro$geometry)
now <- lead(caro$geometry)
caro$steplength <- distance_by_element(later, now)


now <- lead(caro$DatetimeUTC)
later <- lag(caro$DatetimeUTC)
caro$timelag <- difftime_secs(now, later)


caro$speed <- caro$steplength / caro$timelag
head(caro)

#Task 2
later <- lag(caro$geometry, n = 2)
now <- lead(caro$geometry, n = 2)
caro$steplength2 <- distance_by_element(later, now)


now <- lead(caro$DatetimeUTC, n = 2)
later <- lag(caro$DatetimeUTC, n = 2)
caro$timelag2 <- difftime_secs(now, later)


caro$speed2 <- caro$steplength2 / caro$timelag2


caro |> 
  # drop geometry and select only specific columns
  # to display relevant data only
  st_drop_geometry() |> 
  select(timelag2, steplength2, speed2) |> 
  head()


#Task 3
later <- lag(caro$geometry, n = 4)
now <- lead(caro$geometry, n = 4)
caro$steplength3 <- distance_by_element(later, now)


now <- lead(caro$DatetimeUTC, n = 4)
later <- lag(caro$DatetimeUTC, n = 4)
caro$timelag3 <- difftime_secs(now, later)


caro$speed3 <- caro$steplength3 / caro$timelag3


caro |> 
  # drop geometry and select only specific columns
  # to display relevant data only
  st_drop_geometry() |> 
  select(timelag3, steplength3, speed3) |> 
  head()


#Task 4
caro |> 
  st_drop_geometry() |> 
  select(DatetimeUTC, speed, speed2, speed3)

library(ggplot2)

ggplot(caro, aes(y = speed)) + 
  # we remove outliers to increase legibility, analogue
  # Laube and Purves (2011)
  geom_boxplot(outliers = FALSE)

library(tidyr)

# before pivoting, let's simplify our data.frame
caro2 <- caro |> 
  st_drop_geometry() |> 
  select(DatetimeUTC, speed, speed2, speed3)

caro_long <- caro2 |> 
  pivot_longer(c(speed, speed2, speed3))

head(caro_long)


ggplot(caro_long, aes(name, value)) +
  # we remove outliers to increase legibility, analogue
  # Laube and Purves (2011)
  geom_boxplot(outliers = FALSE)


###Exercise C
# Load required libraries
library("readr")    # To read CSV files
library("sf")       # For spatial data operations
library("dplyr")    # For data manipulation
library("ggplot2")  # For creating graphics
library("lubridate")# For handling date and time
library("tmap")     # For creating thematic maps


# Read the GPX file using its path
file_path <- "C:/Users/danie/OneDrive - ZHAW/ZHAW/FS24/Patterns and trends in environmental data/R/Exercises/activity_muster.gpx"
gpx_data <- st_read(file_path, layer = "track_points")

# Transform the data to the Swiss LV95 coordinate system (EPSG:2056)
gpx_data <- st_transform(gpx_data, 2056)

# Ensure the correct time format and sort the data by time
gpx_data$time <- as.POSIXct(gpx_data$time, format = "%Y-%m-%dT%H:%M:%SZ")
gpx_data <- gpx_data %>% arrange(time)

# Extract coordinates after transformation
gpx_data <- gpx_data %>% mutate(
  easting = st_coordinates(geometry)[, 1],
  northing = st_coordinates(geometry)[, 2]
)



library(tmap)
tmap_mode("view")

tm_shape(gpx_data) + 
  tm_dots()


gpx_data_sample_line <- gpx_data |> 
  # dissolve to a MULTIPOINT:
  summarise(do_union = FALSE) |> 
  st_cast("LINESTRING")

tmap_options(basemaps = "OpenStreetMap")

tm_shape(gpx_data_sample_line) +
  tm_lines() +
  tm_shape(gpx_data) + 
  tm_dots()



