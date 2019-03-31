# Exercises 1
# 1. Think about how R can help you? 
# Why do you want to learn how to use it for spatial data analysis?
# 2. Have a look at [CRAN Task View: Analysis of Spatial Data](https://cran.r-project.org/web/views/Spatial.html) and check if there are any packages that may be useful to you in your daily work?


library(sf)
polska = st_read("data/polska.gpkg")

plot(polska)

polska

library(raster)
dem = raster("data/polska_srtm.tif")

plot(dem)

dem


# Exercises 2

# 1. Read the `wlkp_pn.gpkg` file containing the borders of the Wielkopolski National Park into R.
# Display this object and view its structure.
# What can you say about the contents of this file? 
# What type of data does it store? 
# What is the coordinate system used?
# How many attributes does it contain?
# What is its geometry?
# 2. Read the `wlkp_pn_dem.tif` file containing the digital elevation model for the Wielkopolska National Park area into R.
# Display this object and view its structure.
# What can you say about the contents of this file? 
# What type of data does it store? 
# What is the coordinate system used?
# How many attributes does it contain?
# How many dimensions does it have?
# What is the data resolution?


library(tmap)

tm_shape(polska) + 
tm_polygons()

tm_shape(polska) + 
tm_polygons() + 
tm_scale_bar(position = c("left", 
                  "bottom")) 

tm_shape(polska) + 
tm_polygons() + 
tm_scale_bar(position = c("left", 
                  "bottom")) + 
tm_compass()

tm_shape(polska) + 
tm_polygons() + 
tm_scale_bar(position = c("left", 
                  "bottom")) + 
tm_compass() +
tm_layout(title = "Poland")

tmap_mode("view")

tm_shape(polska) + 
tm_polygons() + 
tm_layout(title = "Poland")

tmap_mode("plot")

tm_shape(dem) + 
tm_raster()

tm_shape(dem) + 
tm_raster(style = "cont",
  midpoint = NA)

tm_shape(dem) + 
tm_raster(style = "cont", 
  midpoint = NA,
  palette = "-RdYlGn")

tmaptools::palette_explorer()

tm_shape(dem) + 
tm_raster(style = "fixed",
  breaks = c(-99, 0, 300,
             600, 9999), 
  midpoint = NA,
  palette = "-RdYlGn",
  title = "") + 
tm_layout(legend.position = c("LEFT", 
                      "BOTTOM"))

tm_shape(dem) + 
tm_raster(style = "fixed",
  breaks = c(-99, 0, 300,
             600, 9999), 
  labels = c("Depressions", 
             "Plains", 
             "Hills",
             "Mountains"),
  midpoint = NA,
  palette = "-RdYlGn",
  title = "") + 
tm_layout(legend.position = c("LEFT", 
                      "BOTTOM"))

tm_shape(dem) + 
tm_raster(style = "fixed",
  breaks = c(-99, 0, 300,
             600, 9999), 
  labels = c("Depressions", 
             "Plains", 
             "Hills",
             "Mountains"),
  midpoint = NA,
  palette = c("#5E8B73",
              "#DAE97A", 
              "#EADC70", 
              "#AF8D5C"),
  title = "") + 
tm_layout(legend.position = c("LEFT",
                      "BOTTOM"))

map1 = tm_shape(dem) + 
tm_raster(style = "fixed",
breaks = c(-99, 0, 300, 600, 9999),
labels = c("Depressions", "Plains", "Hills", "Mountains"),
midpoint = NA, 
palette = c("#5E8B73", "#DAE97A", "#EADC70", "#AF8D5C"),
title = "") +
tm_layout(legend.position = c("LEFT", "BOTTOM"))

tmap_save(map1, "my_first_map.png")

# Exercises 3

# 1. Improve the `map1` object by e.g. adding a scale, north arrow, or title.
# Also try to add the Polish border to this map.
# 2. Read the files `wlkp_pn.gpkg` and `wlkp_pn_gpkg_dem.tif` into R.
# Create a map showing the terrain model for the Wielkopolski National Park area.
# Save the obtained map to the file "WLKP_YOURNAME.png".
# 3. Additionally, try repeating the steps in the article [Geocomputation with R: maps extended](https://geocompr.github.io/geocompkg/articles/maps.html) in order to make a hillshade map of Poland.

library(dplyr)

meteo_data = read.csv("data/polska_meteo_2017.csv", encoding = "UTF-8")
head(meteo_data)

meteo_stations = st_read("data/polska_stacje.gpkg")
plot(st_geometry(meteo_stations))

meteo_data_sel = meteo_data %>% 
filter(Nazwa.stacji %in% unique(meteo_stations$NAZWA_ST))

meteo_data_sel = meteo_data %>% 
filter(Nazwa.stacji %in% unique(meteo_stations$NAZWA_ST)) %>% 
mutate(data = as.Date(paste0(Rok, "-", Miesiac, "-", Dzien)))

meteo_data_sel = meteo_data %>% 
filter(Nazwa.stacji %in% unique(meteo_stations$NAZWA_ST)) %>% 
mutate(data = as.Date(paste0(Rok, "-", Miesiac, "-", Dzien))) %>% 
dplyr::select(-Rok, -Miesiac, -Dzien)

meteo_data_sel = meteo_data %>% 
filter(Nazwa.stacji %in% unique(meteo_stations$NAZWA_ST)) %>% 
mutate(data = as.Date(paste0(Rok, "-", Miesiac, "-", Dzien))) %>% 
dplyr::select(-Rok, -Miesiac, -Dzien) %>% 
summarize(tavg = mean(tavg), pressure = mean(pressure))

meteo_data_sel = meteo_data %>% 
filter(Nazwa.stacji %in% unique(meteo_stations$NAZWA_ST)) %>% 
mutate(data = as.Date(paste0(Rok, "-", Miesiac, "-", Dzien))) %>% 
dplyr::select(-Rok, -Miesiac, -Dzien) %>% 
group_by(Kod.stacji) %>% 
summarize(tavg = mean(tavg), pressure = mean(pressure))

meteo = meteo_stations %>% 
left_join(meteo_data_sel, by = c("KOD_SZS" = "Kod.stacji"))

meteo = meteo_stations %>% 
inner_join(meteo_data_sel, by = c("KOD_SZS" = "Kod.stacji"))

plot(dem)
plot(st_geometry(meteo), axes = TRUE)

meteo = meteo_stations %>% 
inner_join(meteo_data_sel, by = c("KOD_SZS" = "Kod.stacji")) %>% 
st_transform(4326)

tm_shape(dem) +
tm_raster() +
tm_shape(meteo) +
tm_symbols(col = "tavg", palette = "RdBu")

meteo$elev = extract(dem, meteo)
head(meteo)

lc = raster("data/polska_lc.tif")
plot(lc)
wlkp_pn = st_read("data/wlkp_pn.gpkg", quiet = TRUE)
plot(st_geometry(wlkp_pn), axes = TRUE)

wlkp_pn_lc = crop(lc, wlkp_pn)
plot(wlkp_pn_lc)

wlkp_pn_lc2 = mask(wlkp_pn_lc, wlkp_pn)
plot(wlkp_pn_lc2)

# Exercises 4

# 1. Read the 2017 meteorological data and the location of the meteorological stations.
# Create a spatial object containing all measurements for three stations: `BIAŁYSTOK`, `KRAKÓW-BALICE`, and `POZNAŃ`. (Tip: use the `right_join()` function.
# 2. Calculate the average temperature in January and July for these three stations.
# 3. Determine the land cover of the three stations.
# 4. Additional: improve the average temperature map presented in the "Reprojections" section.

library(landscapemetrics) 
library(landscapetools)

show_landscape(wlkp_pn_lc, discrete = TRUE)

list_lsm()

lsm_l_ai(wlkp_pn_lc)

lsm_c_ed(wlkp_pn_lc)

lsm_p_para(wlkp_pn_lc)

calculate_lsm(wlkp_pn_lc, type = "aggregation metric")

mapa_p_para = get_lsm(wlkp_pn_lc, what = "lsm_p_para")
show_landscape(mapa_p_para[[1]][[1]])

show_lsm(wlkp_pn_lc, what = "lsm_p_para")

punkt = st_sf(st_sfc(geom = st_point(c(347500, 493000))))
plot(wlkp_pn_lc)
plot(st_geometry(punkt), add = TRUE, cex = 2)

sample_lsm(wlkp_pn_lc, punkt, shape = "circle", size = 2000, what = "lsm_c_ed")

# Exercises 5

# 1. Borders of the national parks in Poland are located in the `Poland_pn.gpkg` file.
# Read this file into R and select `"Białowieski Park Narodowy"` only.
# 2. Crop the land cover map from the `Polska_lc.tif` file to the extend of `"Białowieski Park Narodowy"`.
# 3. What land cover categories can be found in this area (you can use the `unique()` function for this purpose)?
# 4. Calculate the area of `"Białowieski Park Narodowy"` and then the areas the subsequent classes (`TA` and `CA`).
# What are the three land cover categories with the largest area?
# 5. Check how many patches are in this area (`NP`).
