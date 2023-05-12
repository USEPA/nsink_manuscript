library(ggplot2)
library(sf)
library(terra)
library(tidyterra)
library(nsink)
library(dplyr)
aea <- 5072
niantic_huc_id <- nsink_get_huc_id("Niantic River")$huc_12
niantic_data <- nsink_prep_data(niantic_huc_id, projection = aea,
                                data_dir = "nsink_niantic_data")
niantic_removal <- nsink_calc_removal(niantic_data)

# Select location on map for starting point using cursor and click
pta <- c(1954039, 2291592)
start_loc_inter_a <- st_sf(st_sfc(st_point(pta), crs = aea))
niantic_fp_a <- nsink_generate_flowpath(start_loc_inter_a, niantic_data)
niantic_fp_removal_a <- nsink_summarize_flowpath(niantic_fp_a, niantic_removal)

ptb <- c(1953529, 2290811)
start_loc_inter_b <- st_sf(st_sfc(st_point(ptb), crs = aea))
niantic_static_maps <- nsink_generate_static_maps(niantic_data, niantic_removal, 450)

niantic_fp_b <- nsink_generate_flowpath(start_loc_inter_b, niantic_data)
niantic_fp_removal_b <- nsink_summarize_flowpath(niantic_fp_b, niantic_removal)
huc <- niantic_data$huc
streams <- st_intersection(huc, niantic_data$streams)
x <- c(pta[1], ptb[1])
y <- c(pta[2], ptb[2])
labels <- c("A", "B")
pts_df <- data.frame(x,y,labels)
ab_plot <- st_as_sf(pts_df, coords = c("x","y"), crs = aea)

# ggplot stuff starts here

from <- c(seq(0,90,10), NA)
to <- c(seq(10,100,10), NA)
matrix(c(from,to,to),ncol = 3)
tnsport_idx <- classify(rast(niantic_static_maps$transport_idx),matrix(c(from,to,to),ncol = 3))
values(tnsport_idx) <- factor(values(tnsport_idx))
ggplot() +
  geom_spatraster(data = tnsport_idx) +
  scale_fill_manual(values = c("#38A1D0", "#7AB4C0", "#A2C8B0","#C3DC9D", 
                                        "#E2F088", "#F5E871", "#F9C159", 
                                        "#F99844", "#F56A2E","#EF2820"), 
                                        na.value = NA, na.translate = FALSE) +
  geom_sf(data = streams, color = "dodgerblue", linewidth = 0.7) +
  geom_sf(data = niantic_data$lakes, fill = "dodgerblue", color = "dodgerblue") +
  geom_sf(data = niantic_fp_a$flowpath_network, color = "grey40", linewidth = 2.5) +
  geom_sf(data = niantic_fp_a$flowpath_ends, color = "grey40", linewidth = 2.5) +
  geom_sf(data = niantic_fp_b$flowpath_network, color = "grey0", linewidth = 0.9) +
  geom_sf(data = niantic_fp_b$flowpath_ends, color = "grey0", linewidth = 0.9) +
  geom_sf(data = ab_plot, pch = 16, size = 2) + 
  geom_sf_label(data = ab_plot, aes(label = labels))

from <- c(seq(0,90,10), NA)
to <- c(seq(10,100,10), NA)
matrix(c(from,to,to),ncol = 3)
tnsport_idx <- classify(rast(niantic_static_maps$transport_idx),matrix(c(from,to,to),ncol = 3))
values(tnsport_idx) <- factor(values(tnsport_idx))
re <- rast(niantic_static_maps$removal_effic)
values(re)[values(re)==0] <- NA  
ggplot() +
  geom_spatraster(data = tnsport_idx) +
  scale_fill_viridis_d(na.value = NA) +
  new_scale_fill() +
  geom_spatraster(data = re, alpha = 0.2) +
  scale_fill_continuous(na.value = NA)

