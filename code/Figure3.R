# Suppress warnings
options(warn = -1)

# Load necessary libraries
library(dplyr)           # Data manipulation verbs
library(sf)              # For spatial data handling
library(rnaturalearth)  # For world map data
library(ggplot2)         # For general plotting
library(biscale)         # For biogeographic scale and classification
library(cowplot)         # For combining plots
library(ggrastr)         # For rasterizing ggplot objects
library(patchwork)       # For combining multiple plots
library(rPref)           # For preference-based analysis
library(here)             # Find your files

# Set sf package options
sf_use_s2(FALSE)

# Load datasets
inland <- readRDS(here("data","inland.rds"))
hotspots <- readRDS(here("data","hotspots_2016.rds"))
index <- readRDS("data","shortfall_index.rds")
biogeographic_list <- read.csv(here("data","biogeographic_list.csv"))

# Check for spatial overlap between basins and hotspots
overlap_index <- st_intersection(inland, hotspots, sparse = FALSE)

# Define Eckert IV equal-area projection (EPSG:54012)
eckert_iv_crs <- "+proj=eck4 +datum=WGS84 +units=m +no_defs"

# Transform projection for overlapping basins
overlap_index_eckert <- inland %>%
  filter(basin_id %in% overlap_index$basin_id) %>%
  st_transform(crs = eckert_iv_crs) %>%
  st_make_valid()

# Calculate basin areas in square meters and summarize by basin_id
basin_area <- overlap_index_eckert %>%
  group_by(basin_id) %>%
  summarise(area_m2_fix = sum(st_area(geometry))) %>%
  ungroup() %>%
  st_drop_geometry()
head(basin_area)
sum(basin_area$area_m2_fix)  # Total basin area: 6.663962e+13 m^2

# Calculate hotspot areas
hotspots_area <- hotspots %>%
  filter(Type == "hotspot area") %>%
  st_transform(crs = eckert_iv_crs) %>%
  group_by(NAME) %>%
  summarise(area_m2 = sum(st_area(geometry))) %>%
  ungroup() %>%
  st_drop_geometry()
head(hotspots_area)
sum(hotspots_area$area_m2)  # Total hotspot area: 2.602942e+13 m^2

# Merge basin area with shortfall index data
darkspots <- basin_area %>%
  left_join(index[, 1:6], by = "basin_id")
names(darkspots)[7] <- "shortfall_index"

################################################################################
#  "↑", "↑", "↑"
darkspots_final_1 <- darkspots %>%
  na.omit()%>%
  psel(high(linnaean) * high(wallacean) * high (darwinian) ,top_level = 5) %>%
  mutate(cum_area = cumsum(area_m2_fix),                       
         darkspot = if_else(cum_area <= sum(hotspots_area$area_m2), "Y", "N"),
         hotspot = "Y")  

darkspots_final_1 <- darkspots %>%
  left_join(darkspots_final_1[,c(1,10,11)], by = "basin_id")
darkspots_final_1$darkspot <- ifelse(is.na(darkspots_final_1$darkspot),"N",darkspots_final_1$darkspot)
darkspots_final_1$hotspot <- "Y"

table(darkspots_final_1$darkspot)/1986#0.015

map <- inland %>% left_join(darkspots_final_1[,c(1,8,9)], by = "basin_id")
map$darkspot <- ifelse(is.na(map$darkspot),"N",map$darkspot)
map$hotspot <- ifelse(is.na(map$hotspot),"N",map$hotspot)

# ggplot(data = map)+
#   geom_sf(aes(fill = darkspot))

scenarios1 <- map
scenarios1$group <- "LS^up * WS^up * DS^up"

darkspots %>%
  na.omit()%>%
  psel(high(linnaean) * high(wallacean) * high (darwinian) ,top_level = 5) %>%
  mutate(cum_area = cumsum(area_m2_fix),                       
         darkspot = if_else(cum_area <= sum(hotspots_area$area_m2), "Y", "N"),
         hotspot = "Y") %>%
  dplyr::filter(darkspot == "Y") %>%
  left_join(biogeographic_list, by = "basin_id") %>%
  dplyr::select(basin_id,darkspot, hotspot, biogeographic_realm, Ecoregion) %>%
  print(n=5)

#Neotropic(Amazon);Indomalayan(Dong.Nai;Zhujiang;Ganges;Mekong)


################################################################################
#  "↑", "↑", "↓"
darkspots_final_2 <- darkspots %>%
  na.omit()%>%
  psel(high(linnaean) * high(wallacean) * low (darwinian),top_level = 5) %>%
  mutate(cum_area = cumsum(area_m2_fix),                       
         darkspot = if_else(cum_area <= sum(hotspots_area$area_m2), "Y", "N"),
         hotspot = "Y")  

darkspots_final_2 <- darkspots %>%
  left_join(darkspots_final_2[,c(1,10,11)], by = "basin_id")
darkspots_final_2$darkspot <- ifelse(is.na(darkspots_final_2$darkspot),"N",darkspots_final_2$darkspot)
darkspots_final_2$hotspot <- "Y"
table(darkspots_final_2$darkspot)/1986 #0.037

map <- inland %>% left_join(darkspots_final_2[,c(1,8,9)], by = "basin_id")
map$darkspot <- ifelse(is.na(map$darkspot),"N",map$darkspot)
map$hotspot <- ifelse(is.na(map$hotspot),"N",map$hotspot)

# ggplot(data = map)+
#   geom_sf(aes(fill = darkspot))

scenarios2 <- map
scenarios2$group <- "LS^up * WS^up * DS[down]"


darkspots %>%
  na.omit()%>%
  psel(high(linnaean) * high(wallacean) * low (darwinian),top_level = 5) %>%
  mutate(cum_area = cumsum(area_m2_fix),                       
         darkspot = if_else(cum_area <= sum(hotspots_area$area_m2), "Y", "N"),
         hotspot = "Y") %>%
  dplyr::filter(darkspot == "Y") %>%
  left_join(biogeographic_list, by = "basin_id") %>%
  dplyr::select(basin_id,darkspot, hotspot, biogeographic_realm, Ecoregion) %>%
  print(n=5)
# Neotropic(Amazon,Guaiba);Indomalayan(Ann.Chaung,Ganges,Irrawaddy)
################################################################################
#  "↑", "↓", "↑"
darkspots_final_3 <- darkspots %>%
  na.omit()%>%
  psel(high(linnaean) * low(wallacean) * high (darwinian),top_level = 5) %>%
  mutate(cum_area = cumsum(area_m2_fix),                       
         darkspot = if_else(cum_area <= sum(hotspots_area$area_m2), "Y", "N"),
         hotspot = "Y")  

darkspots_final_3 <- darkspots %>%
  left_join(darkspots_final_3[,c(1,10,11)], by = "basin_id")
darkspots_final_3$darkspot <- ifelse(is.na(darkspots_final_3$darkspot),"N",darkspots_final_3$darkspot)
darkspots_final_3$hotspot <- "Y"
table(darkspots_final_3$darkspot)/1986 #0.01963746


map <- inland %>% left_join(darkspots_final_3[,c(1,8,9)], by = "basin_id")
map$darkspot <- ifelse(is.na(map$darkspot),"N",map$darkspot)
map$hotspot <- ifelse(is.na(map$hotspot),"N",map$hotspot)

# ggplot(data = map)+
#   geom_sf(aes(fill = darkspot))

scenarios3 <- map
scenarios3$group <- "LS^up * WS[down] * DS^up"


darkspots %>%
  na.omit()%>%
  psel(high(linnaean) * low(wallacean) * high (darwinian),top_level = 5) %>%
  mutate(cum_area = cumsum(area_m2_fix),                       
         darkspot = if_else(cum_area <= sum(hotspots_area$area_m2), "Y", "N"),
         hotspot = "Y") %>%
  dplyr::filter(darkspot == "Y") %>%
  left_join(biogeographic_list, by = "basin_id") %>%
  dplyr::select(basin_id,darkspot, hotspot, biogeographic_realm, Ecoregion) %>%
  print(n=5)

#Neotropic(Amazon);Palearctic(Asi,Aude);Indomalayan(Bandjarmasin,Batang.Kayan)

################################################################################
#  "↑", "↓", "↓"
darkspots_final_4 <- darkspots %>%
  na.omit()%>%
  psel(high(linnaean) * low(wallacean) * low (darwinian),top_level = 5) %>%
  mutate(cum_area = cumsum(area_m2_fix),                       
         darkspot = if_else(cum_area <= sum(hotspots_area$area_m2), "Y", "N"),
         hotspot = "Y")  

darkspots_final_4 <- darkspots %>%
  left_join(darkspots_final_4[,c(1,10,11)], by = "basin_id")
darkspots_final_4$darkspot <- ifelse(is.na(darkspots_final_4$darkspot),"N",darkspots_final_4$darkspot)
darkspots_final_4$hotspot <- "Y"
table(darkspots_final_4$darkspot)/1986 #0.01963746

map <- inland %>% left_join(darkspots_final_4[,c(1,8,9)], by = "basin_id")
map$darkspot <- ifelse(is.na(map$darkspot),"N",map$darkspot)
map$hotspot <- ifelse(is.na(map$hotspot),"N",map$hotspot)

# ggplot(data = map)+
#   geom_sf(aes(fill = darkspot))

scenarios4 <- map
scenarios4$group <- "LS^up * WS[down] * DS[down]"

darkspots %>%
  na.omit()%>%
  psel(high(linnaean) * low(wallacean) * low (darwinian),top_level = 5) %>%
  mutate(cum_area = cumsum(area_m2_fix),                       
         darkspot = if_else(cum_area <= sum(hotspots_area$area_m2), "Y", "N"),
         hotspot = "Y") %>%
  dplyr::filter(darkspot == "Y") %>%
  left_join(biogeographic_list, by = "basin_id") %>%
  dplyr::select(basin_id,darkspot, hotspot, biogeographic_realm, Ecoregion) %>%
  print(n=5)

# Neotropic(Amazon);Afrotropic(Malawi); Indomalayan(Irrawaddy,Kaladan,Mekong)
################################################################################
#  "↓", "↑", "↑"
darkspots_final_5 <- darkspots %>%
  na.omit()%>%
  psel(low(linnaean) * high(wallacean) * high (darwinian),top_level = 5) %>%
  mutate(cum_area = cumsum(area_m2_fix),                       
         darkspot = if_else(cum_area <= sum(hotspots_area$area_m2), "Y", "N"),
         hotspot = "Y")  

darkspots_final_5 <- darkspots %>%
  left_join(darkspots_final_5[,c(1,10,11)], by = "basin_id")
darkspots_final_5$darkspot <- ifelse(is.na(darkspots_final_5$darkspot),"N",darkspots_final_5$darkspot)
darkspots_final_5$hotspot <- "Y"
table(darkspots_final_5$darkspot)/1986 #0.033

map <- inland %>% left_join(darkspots_final_5[,c(1,8,9)], by = "basin_id")
map$darkspot <- ifelse(is.na(map$darkspot),"N",map$darkspot)
map$hotspot <- ifelse(is.na(map$hotspot),"N",map$hotspot)

# ggplot(data = map)+
#   geom_sf(aes(fill = darkspot))

scenarios5 <- map
scenarios5$group <- "LS[down] * WS^up * DS^up"

darkspots %>%
  na.omit()%>%
  psel(low(linnaean) * high(wallacean) * high (darwinian),top_level = 5) %>%
  mutate(cum_area = cumsum(area_m2_fix),                       
         darkspot = if_else(cum_area <= sum(hotspots_area$area_m2), "Y", "N"),
         hotspot = "Y") %>%
  dplyr::filter(darkspot == "Y") %>%
  left_join(biogeographic_list, by = "basin_id") %>%
  dplyr::select(basin_id,darkspot, hotspot, biogeographic_realm, Ecoregion) %>%
  print(n=5)

#Neotropic (Amazon);Indomalayan(Chaliyar,Chantaburi,Dong.Nai,Jiulong River)

################################################################################
#  "↓", "↑", "↓"
darkspots_final_6 <- darkspots %>%
  na.omit()%>%
  psel(low(linnaean) * high(wallacean) * low(darwinian),top_level = 5) %>%
  mutate(cum_area = cumsum(area_m2_fix),                       
         darkspot = if_else(cum_area <= sum(hotspots_area$area_m2), "Y", "N"),
         hotspot = "Y")  

darkspots_final_6 <- darkspots %>%
  left_join(darkspots_final_6[,c(1,10,11)], by = "basin_id")
darkspots_final_6$darkspot <- ifelse(is.na(darkspots_final_6$darkspot),"N",darkspots_final_6$darkspot)
darkspots_final_6$hotspot <- "Y"
table(darkspots_final_6$darkspot)/1986 #0.145

map <- inland %>% left_join(darkspots_final_6[,c(1,8,9)], by = "basin_id")
map$darkspot <- ifelse(is.na(map$darkspot),"N",map$darkspot)
map$hotspot <- ifelse(is.na(map$hotspot),"N",map$hotspot)

# ggplot(data = map)+
#   geom_sf(aes(fill = darkspot))

scenarios6 <- map
scenarios6$group <- "LS[down] * WS^up * DS[down]"


darkspots %>%
  na.omit()%>%
  psel(low(linnaean) * high(wallacean) * low(darwinian),top_level = 5) %>%
  mutate(cum_area = cumsum(area_m2_fix),                       
         darkspot = if_else(cum_area <= sum(hotspots_area$area_m2), "Y", "N"),
         hotspot = "Y") %>%
  dplyr::filter(darkspot == "Y") %>%
  left_join(biogeographic_list, by = "basin_id") %>%
  dplyr::select(basin_id,darkspot, hotspot, biogeographic_realm, Ecoregion) %>%
  print(n=5)

#Neotropic(Acaponeta,Aconcagua,Anasco);Indomalayan(Aghnashini,Andaman.Islands)

################################################################################
#  "↓", "↓", "↑"
darkspots_final_7 <- darkspots %>%
  na.omit()%>%
  psel(low(linnaean) * low(wallacean) * high (darwinian),top_level = 5) %>%
  mutate(cum_area = cumsum(area_m2_fix),                       
         darkspot = if_else(cum_area <= sum(hotspots_area$area_m2), "Y", "N"),
         hotspot = "Y")  

darkspots_final_7 <- darkspots %>%
  left_join(darkspots_final_7[,c(1,10,11)], by = "basin_id")
darkspots_final_7$darkspot <- ifelse(is.na(darkspots_final_7$darkspot),"N",darkspots_final_7$darkspot)
darkspots_final_7$hotspot <- "Y"
table(darkspots_final_7$darkspot)/1986 #0.03963746


map <- inland %>% left_join(darkspots_final_7[,c(1,8,9)], by = "basin_id")
map$darkspot <- ifelse(is.na(map$darkspot),"N",map$darkspot)
map$hotspot <- ifelse(is.na(map$hotspot),"N",map$hotspot)

# ggplot(data = map)+
#   geom_sf(aes(fill = darkspot))

scenarios7 <- map
scenarios7$group <- "LS[down] * WS[down] * DS^up"


darkspots %>%
  na.omit()%>%
  psel(low(linnaean) * low(wallacean) * high (darwinian),top_level = 5) %>%
  mutate(cum_area = cumsum(area_m2_fix),                       
         darkspot = if_else(cum_area <= sum(hotspots_area$area_m2), "Y", "N"),
         hotspot = "Y") %>%
  dplyr::filter(darkspot == "Y") %>%
  left_join(biogeographic_list, by = "basin_id") %>%
  dplyr::select(basin_id,darkspot, hotspot, biogeographic_realm, Ecoregion) %>%
  print(n=5)


# Neotropic(Amazon),Palearctic(Aliakmon,Aude),Indomalayan(Aghnashini),Australasian(Burnett)
###############################################################################
#  shortfall_index "↑"
darkspots_final_8 <- darkspots %>%
  na.omit()%>%
  psel(high (shortfall_index),top_level = 30) %>%
  #arrange(desc(shortfall_index)) %>%
  mutate(cum_area = cumsum(area_m2_fix),                       
         darkspot = if_else(cum_area <= sum(hotspots_area$area_m2), "Y", "N"),
         hotspot = "Y")  

darkspots_final_8 <- darkspots %>%
  left_join(darkspots_final_8[,c(1,10,11)], by = "basin_id")
darkspots_final_8$darkspot <- ifelse(is.na(darkspots_final_8$darkspot),"N",darkspots_final_8$darkspot)
darkspots_final_8$hotspot <- "Y"
table(darkspots_final_8$darkspot)/1986 #0.010

map <- inland %>% left_join(darkspots_final_8[,c(1,8,9)], by = "basin_id")
map$darkspot <- ifelse(is.na(map$darkspot),"N",map$darkspot)
map$hotspot <- ifelse(is.na(map$hotspot),"N",map$hotspot)

# ggplot(data = map)+
#   geom_sf(aes(fill = hotspot),size = 0)

scenarios8 <- map
scenarios8$group <- "Shortfall~index^up"


darkspots %>%
  na.omit()%>%
  psel(high (shortfall_index),top_level = 30) %>%
  mutate(cum_area = cumsum(area_m2_fix),                       
         darkspot = if_else(cum_area <= sum(hotspots_area$area_m2), "Y", "N"),
         hotspot = "Y") %>%
  dplyr::filter(darkspot == "Y") %>%
  left_join(biogeographic_list, by = "basin_id") %>%
  dplyr::select(basin_id,darkspot, hotspot, biogeographic_realm, Ecoregion) %>%
  print(n=5)

# Neotropic(Amazon, Parana, Tocantins),Indomalayan(Pearl River,Ganges )

# Combine all scenarios
scenarios <- rbind(scenarios1,scenarios2,scenarios3,scenarios4,
                   scenarios5,scenarios6,scenarios7,scenarios8)
scenarios$group <- factor(scenarios$group,levels = c("LS^up * WS^up * DS^up","LS^up * WS^up * DS[down]",
                                                     "LS^up * WS[down] * DS^up","LS^up * WS[down] * DS[down]",
                                                     "LS[down] * WS^up * DS^up","LS[down] * WS^up * DS[down]",
                                                     "LS[down] * WS[down] * DS^up","Shortfall~index^up"
                                                     )
                          )
# Custom color palette for plotting
custom_pal <- c(
  "1-1" = "#d3d3d3",  # low x, low y
  "2-1" = "#4A3A3B",  # high x, low y
  "1-2" = "#F16C31",  # low x, high y
  "2-2" = "#7C0809"   # high x, high y
  )

# Convert darkspot and hotspot to factors
scenarios$darkspot <- as.factor(scenarios$darkspot)
scenarios$hotspot <- as.factor(scenarios$hotspot)

# Classify using bi_class
scenarios <- bi_class(scenarios, x = darkspot, y = hotspot, style = "quantile", dim = 2)
table(scenarios$bi_class)

# Generate legend for bi-classified data
breaks2 <- bi_class_breaks(scenarios, x = darkspot, y = hotspot, style = "quantile", dim = 2)
legend <- bi_legend(pal = custom_pal, dim = 2, xlab = "Darkspot", ylab = "Hotspot", breaks = breaks2, arrows = FALSE, pad_width = 0, size = 6) +
  theme(plot.background = element_blank())

# World map projection
world_map <- ne_countries(scale = 50, type = "countries", returnclass = "sf") 
moll_proj <- st_crs("+proj=moll")
lat_points <- data.frame(lon = 0, lat = c(-60, 90)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)
projected_points <- st_transform(lat_points, crs = moll_proj)
y_limits <- st_coordinates(projected_points)[, "Y"]

# Final map plot
p <- ggplot(data = scenarios %>%
              st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))) +
  ggrastr::rasterise(geom_sf(data = world_map %>%
                               st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")),
                             fill = "#d3d3d3", colour = NA), dpi = 300) +
  ggrastr::rasterise(geom_sf(data = hotspots %>%
                               filter(Type == "outer limit") %>%
                               st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180")), 
                             fill = NA, color = "#F16C31", lty = 2, size = 0.1), dpi = 300) +
  ggrastr::rasterise(geom_sf(aes(fill = bi_class), color = NA, show.legend = FALSE), dpi = 300) +
  bi_scale_fill(pal = custom_pal, dim = 2) +
  bi_theme() +
  facet_wrap(. ~ group, labeller = label_parsed, nrow = 4) +
  theme(panel.spacing = unit(0.2, "lines"),
        strip.text = element_text(size = 8)) +
  coord_sf(crs = "+proj=moll +lon_0=0", ylim = c(-6876759, 9020048), expand = FALSE)

# Combine map and legend
ggdraw() +
  draw_plot(p, 0, 0, 1, 1) +
  draw_plot(legend, 0.415, 0.38, 0.15, 0.15)

# Save the final plot
ggsave("Figure3.png", dpi = 300, units = "cm", width = 18, height = 18)
