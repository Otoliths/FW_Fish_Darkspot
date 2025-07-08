# Suppress warnings
options(warn = -1)

# Load necessary libraries

library(dplyr)           # Data manipulation verbs (e.g., filter, mutate, select, etc.)
library(ggplot2)         # For creating general plots and visualizations
library(GGally)          # For enhanced plotting functions, especially for pair plots and correlation plots
library(rPref)           # For preference-based analysis (e.g., preference elicitation and choice modeling)
library(ggfortify)       # For plotting principal component analysis (PCA) and other statistical results
library(cowplot)         # For combining multiple ggplot2 plots into one figure (flexible layout)
library(patchwork)       # For arranging multiple plots in a single visualization
library(sf)              # For handling spatial data and geometry objects (e.g., shapefiles, GeoJSON)
library(rnaturalearth)  # For accessing world map data (natural Earth vector and raster datasets)
library(tidyr)           # For tidying up data, particularly reshaping and splitting data into columns
library(here)            # For handling file paths, ensuring they are relative to the project root directory

# Disable S2 geometry processing in sf package (required for certain spatial operations)
sf_use_s2(FALSE)         


# Load datasets
cost <- readRDS(here("data","cost_darkspot.rds"))
sdg <- readRDS(here("data","sdg_basin.rds"))
darkspot <- readRDS(here("data","darkspots.rds"))
hotspots <- hotspots <- readRDS(here("data","hotspots_2016.rds"))
inland <- readRDS(here("data","inland.rds"))
biogeographic_list <- read.csv(here("data","biogeographic_list.csv"))
new_discovery <- read.csv(here("data","new_discovery.csv")) %>%
  mutate(
    shortfall = forcats::fct_inorder(shortfall)
  )
# Merge biogeographic realm information into darkspot dataset
darkspot <- darkspot %>%
  left_join(biogeographic_list[, 1:2], by = "basin_id") %>%
  dplyr::select(basin_id, biogeographic_realm, ecoregion, shortfall_index, darkspot) %>%
  filter(darkspot == "Y") %>%
  distinct_all()

# Join cost and SDG data to darkspot
rrr <- darkspot %>%
  left_join(cost[, c(1, 17)], by = "basin_id") %>%
  left_join(sdg[, c(1, 22:23)], by = "basin_id")
names(rrr)[7:8] <- c("income", "protect")

################################################################################
# Principal Component Analysis (PCA) for SDG data
res <- darkspot %>%
  left_join(sdg[, c(1, 3:19)], by = "basin_id") %>%
  na.omit()

sdg.pca <- prcomp(res[, 6:22], center = TRUE, scale. = TRUE)
res$PC1 <- sdg.pca$x[, 1]
res$PC2 <- sdg.pca$x[, 2]
res$PC3 <- sdg.pca$x[, 3]
names(res)[23:24] <- c("income", "protect")

# PCA plot
autoplot(sdg.pca, data = res, 
         colour = 'ecoregion', 
         loadings = TRUE, 
         loadings.label = TRUE, 
         loadings.colour = "black", 
         loadings.label.colour = "black", 
         loadings.size = 2, 
         loadings.label.repel = TRUE,
         label.repel = TRUE,
         label = TRUE, 
         max.overlaps = 5,
         label.label = "biogeographic_realm",
         label.colour = 'ecoregion', 
         label.alpha = 0.5, 
         label.position = position_jitter(width = 0.012, height = 0.012), 
         shape = 19, alpha = 0.7, size = 2, label.size = 2, lwd = 2) + 
  scale_colour_manual("Ecoregion", values = c(
    "Afrotropic" = "#D55E00",
    "Australasia" = "#0072B2",
    "Indomalayan" = "#009E73",
    "Nearctic" = "#F0E442",
    "Neotropic" = "#E69F00",
    "Oceania" = "#56B4E9",
    "Palearctic" = "#CC79A7"
  )) + 
  theme_minimal() +
  scale_x_continuous(limits = c(-0.12, 0.12)) +
  guides(color = guide_legend(nrow = 1)) +
  theme(panel.grid.minor = element_blank(),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 9),
        legend.position = "top",
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7))

# Min-Max normalization function to avoid zero values
min_max_normalize_safe <- function(x, epsilon = 1e-6) {
  min_val <- min(x, na.rm = TRUE)
  max_val <- max(x, na.rm = TRUE)
  
  # Ensure range difference is not zero
  range_val <- max_val - min_val + epsilon
  return((x - min_val) / range_val)
}

# Normalize variables
rrr$shortfall_index <- min_max_normalize_safe(log(rrr$shortfall_index))
rrr$cost <- min_max_normalize_safe(log(rrr$cost_scale))

# Plot 1: Cost vs Shortfall Index
p1 <- ggplot() + 
  geom_point(data = rrr %>% na.omit(),
             aes(x = cost, y = shortfall_index), fill = "lightgrey", shape = 21, colour = "grey50", size = 2, alpha = 0.5) +
  geom_line(data = psel(rrr %>% na.omit(), low(shortfall_index) * high(cost), top = 10),
            aes(x = cost, y = shortfall_index), colour = "#FFC000", linewidth = 0.5) +
  geom_point(data = psel(rrr %>% na.omit(), low(shortfall_index) * high(cost), top = 10),
             aes(x = cost, y = shortfall_index), fill = "#FFC000", shape = 21, colour = "black", size = 2) +
  geom_line(data = psel(rrr %>% na.omit(), high(shortfall_index) * low(cost), top = 20),
            aes(x = cost, y = shortfall_index), colour = "#00B050", linewidth = 0.5) +
  geom_point(data = psel(rrr %>% na.omit(), high(shortfall_index) * low(cost), top = 20),
             aes(x = cost, y = shortfall_index), fill = "#00B050", shape = 21, colour = "black", size = 2) +
  annotate(geom = "text", x = 0.89, y = 0.05, label = "BWR = 17", size = 2) +
  theme_classic() +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  xlab("Cost") +
  ylab("Shortfall") +
  theme(axis.text = element_text(colour = "black", size = 7),
        axis.title = element_text(colour = "black", size = 8)) +
  annotate("point", x = 0, y = 1, fill = "#00B050", shape = 21, colour = "black", size = 3) +
  annotate("text", x = 0.31, y = 1, label = "Pareto frontier (best case)", colour = "black", size = 2) +
  annotate("point", x = 0, y = 0.88, fill = "#FFC000", shape = 21, colour = "black", size = 3) +
  annotate("text", x = 0.32, y = 0.88, label = "Pareto frontier (worst case)", colour = "black", size = 2)

# Additional normalization and PCA for other variables
rrrr <- res
rrrr$shortfall_index <- min_max_normalize_safe(log(rrrr$shortfall_index))
rrrr$income <- min_max_normalize_safe(rrrr$income)
rrrr$protect <- min_max_normalize_safe(rrrr$protect)

# Plot 2: Income vs Shortfall Index
p2 <- ggplot() + 
  geom_point(data = rrrr %>% na.omit(),
             aes(x = income, y = shortfall_index), fill = "lightgrey", shape = 21, colour = "grey50", size = 2, alpha = 0.5) +
  geom_line(data = psel(rrrr %>% na.omit(), low(shortfall_index) * low(income), top = 7),
            aes(x = income, y = shortfall_index), colour = "#FFC000", linewidth = 0.5) +
  geom_point(data = psel(rrrr %>% na.omit(), low(shortfall_index) * low(income), top = 7),
             aes(x = income, y = shortfall_index), fill = "#FFC000", shape = 21, colour = "black", size = 2) +
  geom_line(data = psel(rrrr %>% na.omit(), high(shortfall_index) * high(income), top = 8),
            aes(x = income, y = shortfall_index), colour = "#00B050", linewidth = 0.5) +
  geom_point(data = psel(rrrr %>% na.omit(), high(shortfall_index) * high(income), top = 8),
             aes(x = income, y = shortfall_index), fill = "#00B050", shape = 21, colour = "black", size = 2) +
  annotate(geom = "text", x = 0.89, y = 1, label = "BWR = 24", size = 2) +
  theme_classic() +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  xlab("Income") +
  ylab("Shortfall") +
  theme(axis.text = element_text(colour = "black", size = 7),
        axis.title = element_text(colour = "black", size = 8))

# Plot 3: Protection vs Shortfall Index
p3 <- ggplot() + 
  geom_point(data = rrrr %>% na.omit(),
             aes(x = protect, y = shortfall_index), fill = "lightgrey", shape = 21, colour = "grey50", size = 2, alpha = 0.5) +
  geom_line(data = psel(rrrr %>% na.omit(), low(shortfall_index) * low(protect), top = 5),
            aes(x = protect, y = shortfall_index), colour = "#FFC000", linewidth = 0.5) +
  geom_point(data = psel(rrrr %>% na.omit(), low(shortfall_index) * low(protect), top = 5),
             aes(x = protect, y = shortfall_index), fill = "#FFC000", shape = 21, colour = "black", size = 2) +
  geom_line(data = psel(rrrr %>% na.omit(), high(shortfall_index) * high(protect), top = 8),
            aes(x = protect, y = shortfall_index), colour = "#00B050", linewidth = 0.5) +
  geom_point(data = psel(rrrr %>% na.omit(), high(shortfall_index) * high(protect), top = 8),
             aes(x = protect, y = shortfall_index), fill = "#00B050", shape = 21, colour = "black", size = 2) +
  annotate(geom = "text", x = 0.89, y = 1, label = "BWR = 15", size = 2) +
  theme_classic() +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  xlab("Protection") +
  ylab("Shortfall") +
  theme(axis.text = element_text(colour = "black", size = 7),
        axis.title = element_text(colour = "black", size = 8))

rrrr <-  rrrr[,-c(6:22)] %>%
  left_join(cost[,c(1,17)], by = "basin_id")

rrrr$cost <- min_max_normalize_safe(log(rrrr$cost_scale))

sky <- psel(rrrr %>% na.omit(), high(shortfall_index) * low(cost) * high(income) * high(protect),top_level =5)
table(sky$.level)
# 1  2  3  4  5 
# 74 61 51 37 17 
sky$.level <- ifelse(sky$.level == 1,"best case",sky$.level)
sky$.level <- ifelse(sky$.level == 5,"worst case",sky$.level)
sky$.level <- ifelse(sky$.level == 2,"case",sky$.level)
sky$.level <- ifelse(sky$.level == 3,"case",sky$.level)
sky$.level <- ifelse(sky$.level == 4,"case",sky$.level)

#sky$group <- ifelse(sky$cost <= 0.5,"first priority","second priority")
p4 <- ggparcoord(sky,columns = c(4,10,6,7), groupColumn = 11,
                 scale = "uniminmax", showPoints = F,
                 alphaLines = 0.8)+ #globalminmax ,alphaLines = 0.8
  scale_color_manual(values = c("best case" = "#00B050",
                                "case" = "lightgrey",
                                "worst case" = "#FFC000"))+
  geom_point(size = 1)+
  geom_path(linewidth = 0.02)+
  annotate(geom = "text",x = 3.6,y = 1.05,label = "BWR = 34", size = 2)+
  theme_minimal()+
  scale_x_discrete(expand = c(0,0.05),labels = c("Shortfall","Cost","Income","Protection"))+
  xlab("")+
  ylab("")+
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey50", linewidth = 1),
        legend.position = "none",
        axis.text.x = element_text(colour = "black",size = 7),
        axis.text.y = element_text(colour = "black",size = 8)
  )



world_map <- ne_countries(scale = 50, type = "countries", returnclass = "sf") 
moll_proj <- st_crs("+proj=moll")
lat_points <- data.frame(lon = 0, lat = c(-60, 90)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)
projected_points <- st_transform(lat_points, crs = moll_proj)
y_limits <- st_coordinates(projected_points)[, "Y"]
#-------------------------------------------------------------------------------
case1 <- inland
case1$frontier <- NA
case1$frontier <- ifelse(case1$basin_id %in% psel(rrr %>% na.omit(), low(shortfall_index) * high(cost),top = 10)$basin_id,
                         "worst case",case1$frontier)
case1$frontier <- ifelse(case1$basin_id %in% psel(rrr %>% na.omit(), high(shortfall_index) * low(cost),top = 20)$basin_id,
                         "best case",case1$frontier)
case1$frontier <- factor(case1$frontier)

pp1 <- ggplot(data = case1 %>%
                na.omit() %>%
                st_wrap_dateline(options = c("WRAPDATELINE=YES","DATELINEOFFSET=180")))+
  ggrastr::rasterise(geom_sf(data = world_map %>%
                               st_wrap_dateline(options = c("WRAPDATELINE=YES","DATELINEOFFSET=180")),
                             fill = "#d3d3d3", colour = NA),dpi = 300) +
  ggrastr::rasterise(geom_sf(data = hotspots %>% 
                               filter(Type == "outer limit") %>%
                               st_wrap_dateline(options = c("WRAPDATELINE=YES","DATELINEOFFSET=180")), 
                             fill = NA, color = "#d3d3d3", lty = 2,size = 0.1),dpi = 300)+
  geom_sf(aes(fill = frontier),color = NA, show.legend = FALSE) +
  scale_fill_manual(values = c("best case" = "#00B050",
                               "worst case" = "#FFC000"))+
  theme_void()+
  coord_sf(crs = "+proj=moll +lon_0=0",ylim = c(-6876759,9020048), expand = FALSE)



pp1_hist <- ggplot(new_discovery %>% dplyr::filter(group == "case1"), aes(x = shortfall, y = mean, fill = case)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.6) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(width = 0.8),
                width = 0.2, color = "grey40",linewidth = 0.1) +
  scale_fill_manual(values = c("#00B050","#FFC000"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1,suffix = ""),
                     expand = c(0, 0),breaks = c(0,0.5,1),
                     limits = c(0, 1)) +
  labs(y = "New record proportion (%)", x = "Shortfall") +
  theme_classic() +
  theme(
    axis.text = element_text(size = 4, color = "black"),
    axis.title = element_text(size = 5, color = "black"),
    axis.line = element_line(linewidth = 0.1),
    axis.ticks = element_line(linewidth = 0.1),
    axis.ticks.length =  unit(0.05,"cm"),
    plot.background = element_blank(),
    panel.background = element_blank(),
    legend.position = "none"
  )

pp1 <- ggdraw() +
  draw_plot(pp1, 0, 0, 1, 1) +                             
  draw_plot(pp1_hist, 0.02, 0.12, 0.3, 0.7)                   


#-------------------------------------------------------------------------------
case2 <- inland
case2$frontier <- NA
case2$frontier <- ifelse(case2$basin_id %in% psel(rrrr %>% na.omit(), low(shortfall_index) * low(income),top = 7)$basin_id,
                         "worst case",case2$frontier)
case2$frontier <- ifelse(case2$basin_id %in% psel(rrrr %>% na.omit(), high(shortfall_index) * high(income),top = 8)$basin_id,
                         "best case",case2$frontier)
case2$frontier <- factor(case2$frontier)

pp2 <- ggplot(data = case2 %>%
                na.omit() %>%
                st_wrap_dateline(options = c("WRAPDATELINE=YES","DATELINEOFFSET=180")))+
  ggrastr::rasterise(geom_sf(data = world_map %>%
                               st_wrap_dateline(options = c("WRAPDATELINE=YES","DATELINEOFFSET=180")),
                             fill = "#d3d3d3", colour = NA),dpi = 300) +
  ggrastr::rasterise(geom_sf(data = hotspots %>% 
                               filter(Type == "outer limit") %>%
                               st_wrap_dateline(options = c("WRAPDATELINE=YES","DATELINEOFFSET=180")), 
                             fill = NA, color = "#d3d3d3", lty = 2,size = 0.1),dpi = 300)+
  geom_sf(aes(fill = frontier),color = NA, show.legend = FALSE) +
  scale_fill_manual(values = c("best case" = "#00B050",
                               "worst case" = "#FFC000"))+
  theme_void()+
  coord_sf(crs = "+proj=moll +lon_0=0",ylim = c(-6876759,9020048), expand = FALSE)

pp2_hist <- ggplot(new_discovery %>% dplyr::filter(group == "case2"), aes(x = shortfall, y = mean, fill = case)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.6) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(width = 0.8),
                width = 0.2, color = "grey40",linewidth = 0.1) +
  scale_fill_manual(values = c("#00B050","#FFC000"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1,suffix = ""),
                     expand = c(0, 0),breaks = c(0,0.5,1),
                     limits = c(0, 1)) +
  labs(y = "New record proportion (%)", x = "Shortfall") +
  theme_classic() +
  theme(
    axis.text = element_text(size = 4, color = "black"),
    axis.title = element_text(size = 5, color = "black"),
    axis.line = element_line(linewidth = 0.1),
    axis.ticks = element_line(linewidth = 0.1),
    axis.ticks.length =  unit(0.05,"cm"),
    plot.background = element_blank(),
    panel.background = element_blank(),
    legend.position = "none"
  )

pp2 <- ggdraw() +
  draw_plot(pp2, 0, 0, 1, 1) +                             
  draw_plot(pp2_hist, 0.02, 0.12, 0.3, 0.7)    




#-------------------------------------------------------------------------------
case3 <- inland
case3$frontier <- NA
case3$frontier <- ifelse(case3$basin_id %in% psel(rrrr %>% na.omit(), low(shortfall_index) * low(protect),top = 5)$basin_id,
                         "worst case",case3$frontier)
case3$frontier <- ifelse(case3$basin_id %in% psel(rrrr %>% na.omit(), high(shortfall_index) * high(protect),top = 8)$basin_id,
                         "best case",case3$frontier)
case3$frontier <- factor(case3$frontier)

pp3 <- ggplot(data = case3 %>%
                na.omit() %>%
                st_wrap_dateline(options = c("WRAPDATELINE=YES","DATELINEOFFSET=180")))+
  ggrastr::rasterise(geom_sf(data = world_map %>%
                               st_wrap_dateline(options = c("WRAPDATELINE=YES","DATELINEOFFSET=180")),
                             fill = "#d3d3d3", colour = NA),dpi = 300) +
  ggrastr::rasterise(geom_sf(data = hotspots %>% 
                               filter(Type == "outer limit") %>%
                               st_wrap_dateline(options = c("WRAPDATELINE=YES","DATELINEOFFSET=180")), 
                             fill = NA, color = "#d3d3d3", lty = 2,size = 0.1),dpi = 300)+
  geom_sf(aes(fill = frontier),color = NA, show.legend = FALSE) +
  scale_fill_manual(values = c("best case" = "#00B050",
                               "worst case" = "#FFC000"))+
  theme_void()+
  coord_sf(crs = "+proj=moll +lon_0=0",ylim = c(-6876759,9020048), expand = FALSE)

pp3_hist <- ggplot(new_discovery %>% dplyr::filter(group == "case3"), aes(x = shortfall, y = mean, fill = case)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.6) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(width = 0.8),
                width = 0.2, color = "grey40",linewidth = 0.1) +
  scale_fill_manual(values = c("#00B050","#FFC000"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1,suffix = ""),
                     expand = c(0, 0),breaks = c(0,0.5,1),
                     limits = c(0, 1)) +
  labs(y = "New record proportion (%)", x = "Shortfall") +
  theme_classic() +
  theme(
    axis.text = element_text(size = 4, color = "black"),
    axis.title = element_text(size = 5, color = "black"),
    axis.line = element_line(linewidth = 0.1),
    axis.ticks = element_line(linewidth = 0.1),
    axis.ticks.length =  unit(0.05,"cm"),
    plot.background = element_blank(),
    panel.background = element_blank(),
    legend.position = "none"
  )

pp3 <- ggdraw() +
  draw_plot(pp3, 0, 0, 1, 1) +                            
  draw_plot(pp3_hist, 0.02, 0.12, 0.3, 0.7)    


#-------------------------------------------------------------------------------
case4 <- inland
case4$frontier <- NA
case4$frontier <- ifelse(case4$basin_id %in% (psel(rrrr %>% na.omit(), high(shortfall_index) * low(cost) * high(income) * high(protect),top_level =5) %>%
                                                dplyr::filter(.level == 5) %>%
                                                dplyr::select(basin_id) %>% 
                                                pull()),
                         "worst case",case4$frontier)
case4$frontier <- ifelse(case4$basin_id %in% (psel(rrrr %>% na.omit(), high(shortfall_index) * low(cost) * high(income) * high(protect),top_level =5) %>%
                                                dplyr::filter(.level == 1) %>%
                                                dplyr::select(basin_id) %>% 
                                                pull()),
                         "best case",case4$frontier)
case4$frontier <- factor(case4$frontier)

pp4 <- ggplot(data = case4 %>%
                na.omit() %>%
                st_wrap_dateline(options = c("WRAPDATELINE=YES","DATELINEOFFSET=180")))+
  ggrastr::rasterise(geom_sf(data = world_map %>%
                               st_wrap_dateline(options = c("WRAPDATELINE=YES","DATELINEOFFSET=180")),
                             fill = "#d3d3d3", colour = NA),dpi = 300) +
  ggrastr::rasterise(geom_sf(data = hotspots %>% 
                               filter(Type == "outer limit") %>%
                               st_wrap_dateline(options = c("WRAPDATELINE=YES","DATELINEOFFSET=180")), 
                             fill = NA, color = "#d3d3d3", lty = 2,size = 0.1),dpi = 300)+
  geom_sf(aes(fill = frontier),color = NA, show.legend = FALSE) +
  scale_fill_manual(values = c("best case" = "#00B050",
                               "worst case" = "#FFC000"))+
  theme_void()+
  coord_sf(crs = "+proj=moll +lon_0=0",ylim = c(-6876759,9020048), expand = FALSE)


pp4_hist <- ggplot(new_discovery %>% dplyr::filter(group == "case4"), aes(x = shortfall, y = mean, fill = case)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.6) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(width = 0.8),
                width = 0.2, color = "grey40",linewidth = 0.1) +
  scale_fill_manual(values = c("#00B050","#FFC000"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1,suffix = ""),
                     expand = c(0, 0),breaks = c(0,0.5,1),
                     limits = c(0, 1)) +
  labs(y = "New record proportion (%)", x = "Shortfall") +
  theme_classic() +
  theme(
    axis.text = element_text(size = 4, color = "black"),
    axis.title = element_text(size = 5, color = "black"),
    axis.line = element_line(linewidth = 0.1),
    axis.ticks = element_line(linewidth = 0.1),
    axis.ticks.length =  unit(0.05,"cm"),
    plot.background = element_blank(),
    panel.background = element_blank(),
    legend.position = "none"
  )

pp4 <- ggdraw() +
  draw_plot(pp4, 0, 0, 1, 1) +                             
  draw_plot(pp4_hist, 0.02, 0.12, 0.3, 0.7)    


#-------------------------------------------------------------------------------

p1+pp1+p2+pp2+p3+pp3+p4+pp4 + 
  plot_layout(nrow = 4, widths = c(0.4,0.6)) + 
  plot_annotation(tag_levels = c('A')) &
  theme(plot.tag = element_text(colour = "black",size = 9, face = "bold"))

ggsave(here("image","Figure4.png"), dpi = 300, units = "cm", width = 16, height = 18)
