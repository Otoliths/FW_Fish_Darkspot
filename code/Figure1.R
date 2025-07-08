# Load necessary libraries
library(dplyr)            # For data manipulation
library(ggplot2)          # For plotting
library(ggpubr)           # For combining plots and statistical annotation
library(patchwork)        # For combining plots
library(tidyr)            # For data tidying
library(Hmisc)            # For data analysis
library(sf)               # For spatial data handling
library(rnaturalearth)    # For geographic data
library(ggtern)           # For ternary plots
library(cowplot)          # For combining plots
library(here)             # Find your files

# Suppress warnings
options(warn = -1)

# Read in data files
df <- readRDS(here("data", "shortfall_basin.rds")) # Data related to biodiversity shortfalls
inland <- readRDS(here("data", "inland.rds")) # Data related to inland regions

# Plot 1: Linnaean vs Wallacean shortfalls
p1 <- ggplot(data = df, aes(x = log(SRdesc + 1), y = log(SRnoloc + 1))) +
  geom_point(fill = "#FFC000", shape = 21, colour = "black", size = 2, alpha = 0.5, na.rm = TRUE) + # Scatter plot with customized points
  geom_rug(length = unit(0.02, "npc"), colour = "#FFC000", na.rm = TRUE) + # Rug plot to show distribution
  geom_smooth(method = "lm", colour = "#FFC000") + # Linear regression line
  annotate("text", x = 3.8, y = 2.4, label = expression(italic(p) < 0.01), size = 3, hjust = 0) + # Annotate p-value
  annotate("text", x = 1.8, y = 2.4, 
           label = paste0("n=", format(nrow(na.omit(df[, c("SRdesc", "SRnoloc")])), big.mark = ",", scientific = FALSE)), 
           size = 3, hjust = 0) + # Annotate sample size
  xlab(expression(Log(Linnaean))) + # Label for x-axis
  ylab("Log(Wallacean)") + # Label for y-axis
  theme_classic() + # Apply classic theme
  theme(axis.title = element_text(size = 10, colour = "black"), axis.text = element_text(size = 9, colour = "black")) # Customize text size and color

# Plot 2: Linnaean vs Darwinian shortfalls
p2 <- ggplot(data = df, aes(x = log(SRdesc + 1), y = log(SRnoseq + 1))) +
  geom_point(fill = "#7030A0", shape = 21, colour = "black", size = 2, alpha = 0.5, na.rm = TRUE) + # Scatter plot with customized points
  geom_rug(length = unit(0.02, "npc"), colour = "#7030A0", na.rm = TRUE) + # Rug plot to show distribution
  geom_smooth(method = "lm", colour = "#7030A0") + # Linear regression line
  annotate("text", x = 3.8, y = 3.5, label = expression(italic(p) < 0.01), size = 3, hjust = 0) + # Annotate p-value
  annotate("text", x = 1.8, y = 3.5, 
           label = paste0("n=", format(nrow(na.omit(df[, c("SRdesc", "SRnoseq")])), big.mark = ",", scientific = FALSE)), 
           size = 3, hjust = 0) + # Annotate sample size
  xlab("Log(Linnaean)") + # Label for x-axis
  ylab("Log(Darwinian)") + # Label for y-axis
  theme_classic() + # Apply classic theme
  theme(axis.title = element_text(size = 10, colour = "black"), axis.text = element_text(size = 9, colour = "black")) # Customize text size and color

# Plot 3: Wallacean vs Darwinian shortfalls
p3 <- ggplot(data = df, aes(x = log(SRnoloc + 1), y = log(SRnoseq + 1))) +
  geom_point(fill = "#00B050", shape = 21, colour = "black", size = 2, alpha = 0.5, na.rm = TRUE) + # Scatter plot with customized points
  geom_rug(length = unit(0.02, "npc"), colour = "#00B050", na.rm = TRUE) + # Rug plot to show distribution
  geom_smooth(method = "lm", colour = alpha("#00B050", 0.8)) + # Linear regression line
  annotate("text", x = 1.8, y = 3.5, label = expression(italic(p) < 0.01), size = 3, hjust = 0) + # Annotate p-value
  annotate("text", x = 0.8, y = 3.5, 
           label = paste0("n=", format(nrow(na.omit(df[, c("SRnoloc", "SRnoseq")])), big.mark = ",", scientific = FALSE)), 
           size = 3, hjust = 0) + # Annotate sample size
  xlab("Log(Wallacean)") + # Label for x-axis
  ylab("Log(Darwinian)") + # Label for y-axis
  theme_classic() + # Apply classic theme
  scale_y_continuous(breaks = c(0, 1, 2, 3)) + # Set y-axis breaks
  theme(axis.title = element_text(size = 10, colour = "black"), axis.text = element_text(size = 9, colour = "black")) # Customize text size and color

# Normalize data for ternary plot
ternary <- df

# Normalization function
normalise <- function(x) {
  (x - min(x, na.rm = TRUE)) / ((max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) + 0.01)
}

names(ternary) <- c("basin_id", "ecoregion", "linnaean", "wallacean", "darwinian") # Assign meaningful column names

# Rescale shortfall data
ternary$linnaean <- scales::rescale(ternary$linnaean, to = c(0.001, 1)) # Rescale Linnaean shortfall
ternary$wallacean <- scales::rescale(ternary$wallacean, to = c(0.001, 1)) # Rescale Wallacean shortfall
ternary$darwinian <- scales::rescale(ternary$darwinian, to = c(0.001, 1)) # Rescale Darwinian shortfall

# Calculate total of all shortfalls
ternary$total <- ternary$linnaean + ternary$wallacean + ternary$darwinian

# Handle missing data and calculate proportions for ternary plot
ternary$L <- ternary$linnaean / ternary$total
hist(ternary$L) # Proportion of Linnaean shortfall
ternary$W <- ternary$wallacean / ternary$total
hist(ternary$W) # Proportion of Wallacean shortfall
ternary$D <- ternary$darwinian / ternary$total
hist(ternary$D) # Proportion of Darwinian shortfall

# Create breaks for color intensity
ternary <- ternary %>% mutate(Lbrk = ntile(ternary$L, 10) / 10.5) # Force breaks between .4 and .8 to reduce color intensity
ternary <- ternary %>% mutate(Wbrk = ntile(ternary$W, 10) / 10.5)
ternary <- ternary %>% mutate(Dbrk = ntile(ternary$D, 10) / 10.5)

# Generate ternary color mix
ternary$tern_mix <- NA # Initialize column with NA
ok <- !(is.na(ternary$total) | is.na(ternary$Lbrk) | is.na(ternary$Wbrk) | is.na(ternary$Dbrk))
ternary$tern_mix[ok] <- rgb(ternary$Lbrk[ok], ternary$Wbrk[ok], ternary$Dbrk[ok], maxColorValue = 1) # Ternary color mix

# Create centroid for ternary plot
center <- ternary %>%
  dplyr::select(L, W, D, total) %>%
  summarise(across(everything(), ~ sum(., na.rm = TRUE))) %>%
  transmute(
    e = L / nrow(na.omit(ternary)),
    w = W / nrow(na.omit(ternary)),
    y = D / nrow(na.omit(ternary))
  ) %>%
  tidyr::pivot_longer(cols = everything()) %>%
  pull(value)

# Define grid for ternary plot
TernaryCentroidGrid <- function(center) {
  labels <- seq(-1, 1, 0.1) # Create labels for grid
  labels <- data.frame(
    L = labels[labels >= -center[1]][1:10],
    T = labels[labels >= -center[2]][1:10],
    R = labels[labels >= -center[3]][1:10]
  )
  breaks <- data.frame(
    L = labels$L + center[1],
    T = labels$T + center[2],
    R = labels$R + center[3]
  )
  list(labels = labels, breaks = breaks)
}

# Define zoom limits for ternary plot
zoom_limits <- function(df, keep_center = TRUE, one_pp_margin = FALSE, center = apply(df, 2, mean, na.rm = TRUE)) {
  mins <- apply(df, 2, min, na.rm = TRUE) # Calculate minimums
  span <- max(apply(df, 2, function(x) diff(range(x)))) # Calculate range span
  if (one_pp_margin == TRUE & min(mins, na.rm = TRUE) > .01) {
    mins <- mins - .01 # Adjust for margin
    span <- span + .01
  }
  if (keep_center == TRUE) {
    limits <- rbind(
      center - (1 / 3) * span / (sqrt(2) / 2),
      center + (2 / 3) * span / (sqrt(2) / 2)
    ) # Set limits around center
  } else {
    limits <- rbind(
      mins,
      c(
        1 - (mins[2] + mins[3]),
        1 - (mins[1] + mins[3]),
        1 - (mins[1] + mins[2])
      )
    )
  }
  return(limits)
}

# Generate legend grid and limits
legend_grid <- TernaryCentroidGrid(center)
legend_limits <- zoom_limits(df = ternary %>%
                               dplyr::select(L, W, D), keep_center = FALSE, one_pp_margin = FALSE)

# Generate ternary plot with color mix
ggtern(data = ternary, ggtern::aes(x = linnaean, y = wallacean, z = darwinian, color = tern_mix)) +
  ggrastr::rasterise(geom_point(shape = 16, size = 0.5, na.rm = TRUE), dpi = 300) + # Rasterized plot for efficiency
  scale_colour_identity() + # Use the generated color mix
  geom_Lline(data = center, Lintercept = center[1], color = "#D55E00", size = .2, lty = 2) + # Add Linnaean line
  geom_Tline(data = center, Tintercept = center[2], color = "#009E73", size = .2, lty = 2) + # Add Wallacean line
  geom_Rline(data = center, Rintercept = center[3], color = "#435792", size = .2, lty = 2) + # Add Darwinian line
  scale_L_continuous("LS") +
  scale_T_continuous("WS") +
  scale_R_continuous("DS") +
  geom_point(
    data = tibble(W = center[2], L = center[1], D = center[3]),
    ggtern::aes(x = L, y = W, D), shape = 43, color = "grey10", size = 2
  ) + # Plot centroid point
  theme_classic() +
  theme(
    plot.margin = grid::unit(rep(1, 4), "lines"),
    axis.text = element_text(size = 5, color = "black", face = "bold"),
    axis.title = element_text(size = 7, color = "black"),
    tern.axis.line.L = element_line(color = "#D55E00", size = .2),
    tern.axis.line.T = element_line(color = "#009E73", size = .2),
    tern.axis.line.R = element_line(color = "#8DA0CB", size = .2),
    tern.panel.grid.ontop = TRUE
  )


# Explanation of the ternary plot
legend_explain <- last_plot()  # Extract the last generated plot for explanation

# Assemble explanation
ggplot() +
  coord_equal(xlim = c(0, 1), ylim = c(0, .5)) + # Set coordinate limits for the plot
  scale_y_continuous(limits = c(0, .5)) + # Set y-axis limits
  annotation_custom(ggplotGrob(legend_explain), xmin = 0, xmax = .5, ymin = 0, ymax = .5) + # Add the legend explanation as a custom annotation
  theme_void() + # Apply a void theme (no background or axes)
  theme(
    plot.margin = grid::unit(rep(0, 4), "lines"), # Remove margins around the plot
    plot.background = element_rect(fill = NA, colour = NA) # Set no background for the plot
  ) +
  # Label Wallacean region
  geom_curve(
    data = tibble(x = .325, y = .35, xend = .260, yend = .33), 
    aes(x = x, y = y, xend = xend, yend = yend), linewidth = 0.2,
    curvature = .3, color = "#009E73", arrow = arrow(type = "closed", length = unit(0.1, "cm"))
  ) +
  annotate("text",
           x = .34, y = .36, hjust = 0, vjust = 1, lineheight = .9,
           label = "Higher WALLACEAN shortfall; \nlower Linnaean and Darwinian",
           size = 1.6, fontface = 3, color = "#009E73"
  ) +
  # Label Wallacean and Darwinian regions
  geom_curve(
    data = tibble(x = .15, y = .42, xend = .23, yend = .28),
    aes(x = x, y = y, xend = xend, yend = yend), linewidth = 0.2,
    curvature = 0.2, color = "#D55E00", arrow = arrow(type = "closed", length = unit(0.1, "cm"))
  ) +
  annotate("text",
           x = .01, y = .45, hjust = 0, vjust = .5, lineheight = .9,
           label = "Lower LINNAEAN shortfall;\ngreater Wallacean and Darwinian",
           size = 1.6, fontface = 3, color = "#D55E00"
  ) +
  # Label Darwinian region
  geom_curve(
    data = tibble(x = .32, y = .08, xend = .34, yend = .16), # x = .45, y = .18, xend = .32, yend = .18
    aes(x = x, y = y, xend = xend, yend = yend), linewidth = 0.2,
    curvature = 0.2, color = "#435792", arrow = arrow(type = "closed", length = unit(0.1, "cm"))
  ) +
  annotate("text",
           x = .25, y = .05, hjust = 0, vjust = .5, lineheight = .9,
           label = "Greater DARWINIAN shortfall; \nlower Linnaean and Wallacean",
           size = 1.6, fontface = 3, color = "#435792"
  )

explanation <- last_plot()  # Store the final explanation plot

# Prepare ecological data for mapping
ecodata <- inland %>% left_join(ternary[, -2], by = "basin_id")  # Merge data on basin_id
ecodata <- st_wrap_dateline(ecodata, options = c("WRAPDATELINE=YES"))  # Wrap the data for world map
world_map <- ne_countries(scale = 50, type = "countries", returnclass = "sf")  # Load world map data

# Set projection system for the map
moll_proj <- st_crs("+proj=moll")  # Mollweide projection
lat_points <- data.frame(lon = 0, lat = c(-60, 90)) %>%  # Create points for latitudes
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

projected_points <- st_transform(lat_points, crs = moll_proj)  # Project latitudes into Mollweide

y_limits <- st_coordinates(projected_points)[, "Y"]  # Extract Y coordinates for limits

# Generate the map plot with ecological data
p <- ggplot(data = ecodata) +
  ggrastr::rasterise(geom_sf(data = world_map, fill = "lightgrey", colour = NA), dpi = 300) + # Rasterized world map
  ggrastr::rasterise(geom_sf(aes(fill = tern_mix), colour = NA), dpi = 300) + # Rasterized ecological data
  scale_fill_identity() + # Use the generated color mix
  theme_void() +  # Apply a void theme (no background or axes)
  coord_sf(crs = "+proj=moll +lon_0=0", ylim = y_limits, expand = FALSE)  # Set projection and limits

# Combine all the plots and save the final figure
map <- ggdraw(p) +
  draw_plot(explanation, x = -0.33, y = -0.1, scale = 0.6)  # Add explanation plot to the main map

# Combine main map and individual plots
map / (p1 + p2 + p3) +
  plot_layout(heights = c(0.7, 0.3)) +  # Adjust heights of individual plots
  plot_annotation(tag_levels = "A") &  # Add plot tags
  theme(plot.tag = element_text(size = 10, face = "bold"))  # Customize plot tag appearance

# Save the final figure as a PNG
ggsave(here("image", "Figure1.png"), width = 18, height = 12, units = "cm", dpi = 300)  # Save the combined plot
