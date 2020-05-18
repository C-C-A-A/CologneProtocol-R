library("magrittr")

# Basics -----------------------------------------------------------------------

# Turn scientific notation off
options(scipen = 999)

# Plot of archaeological sites and Thiessen polygons ------------------------

plot_sites_voronoi <- function(){
plot(vertices_spdf@coords[,1], vertices_spdf@coords[,2],
     type = "n",
     asp = 1,
     main = "Sites and corresponding voronoi diagram",
     xlab = your_projection,
     ylab = NA)
points(sites,
       pch = 20,
       col = "red")
points(vertices_spdf,
       pch = 20,
       col = "black")
plot(deldir::deldir(sites@coords[,1], sites@coords[,2]),
     wlines = "tess",
     wpoints = "none",
     number = FALSE,
     lty = 1,
     add = TRUE)
legend(x = "topleft",
       legend = c("Archaeological sites", "Voronoi vertices", "Voronoi polygons"),
       pch = c(20, 20, NA),
       lty = c(NA, NA, 1),
       col = c("red", "black", "black"),
       merge = TRUE,
       bty = "n",
       bg = "transparent")
}
plot_sites_voronoi()

# Variogram / semivariogram ----------------------------------------------------
plot_vario <-
plot(vertices_vario,
     vertices_vario_fit)
plot_vario

# Zoom in on range and sill plateau (range and sill.platau + 20 %)
plot_vario_zoom <-
plot(vertices_vario,
     vertices_vario_fit,
     xlim = c(0, range.plateau+(range.plateau/100)*20),
     ylim = c(0, sill.plateau+(sill.plateau/100)*20))

plot_vario_zoom

# Kriging results --------------------------------------------------------------
plot_kriging <- function(){
LEC_kriged %>% as.data.frame %>%
  ggplot2::ggplot(ggplot2::aes(x=x, y=y)) +  
  ggplot2::geom_tile(ggplot2::aes(fill=var1.pred))+
  ggplot2::coord_equal() +
  ggplot2::scale_fill_gradientn(colours = terrain.colors(255)) +
  ggplot2::theme_bw() +
  ggplot2::geom_point(data = as.data.frame(sites),
                      ggplot2::aes(x = coords.x1, y = coords.x2), shape=21, size=2, 
                      colour = "black", fill= "red", alpha=1/5)
}
plot_kriging()
  
# Variance of kriging results --------------------------------------------------
plot_variance <- function(){
LEC_kriged %>% as.data.frame %>%
  ggplot2::ggplot(ggplot2::aes(x=x, y=y)) + 
  ggplot2::geom_tile(ggplot2::aes(fill=var1.var)) + 
  ggplot2::coord_equal() +
  ggplot2::scale_fill_gradient(low = "yellow", high="red") +
  ggplot2::theme_bw() +
  ggplot2::geom_point(data = as.data.frame(sites),
                      ggplot2::aes(x = coords.x1, y = coords.x2), shape=21, size=2, 
                      colour = "white", fill= "black", alpha=1/2)
}
plot_variance()

# Change units of all data.frames ----------------------------------------------
Isolines_stats[, 1] <- Isolines_stats[, 1] / 1000 # for easy reading


# Plot of descriptive properties of isolines -----------------------------------

# Number of distinct areas per isoline
plot_n_area <-
Isolines_stats %>%
  ggplot2::ggplot(ggplot2::aes(x = km_isoline, y = number_Area)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::labs(title = "Number of areas with a specific site density",
                x = "Isoline [km]",
                y = "Number of distinct areas") +
  ggplot2::scale_x_continuous(limit = your_limit,
                              breaks = your_breaks) +
  ggplot2::theme_bw() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                 axis.text.x = ggplot2::element_text(angle = 90))

plot_n_area
  
if(merge_polygons == TRUE){
# EXPERIMENTAL:
# Number of distinct areas per isoline MERGED
Isolines_stats %>%
  ggplot2::ggplot(ggplot2::aes(x = km_isoline, y = number_Area_merged)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::labs(title = "Number of areas with a specific site density MERGED",
                x = "Isoline [km]",
                y = "Number of distinct areas") +
  ggplot2::scale_x_continuous(limit = your_limit,
                              breaks = your_breaks) +
  ggplot2::theme_bw() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                 axis.text.x = ggplot2::element_text(angle = 90))
}

# Number of sites per isoline
plot_n_sites <- 
Isolines_stats %>%
  ggplot2::ggplot(ggplot2::aes(x = km_isoline, y = number_Sites)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::labs(title = "Number of sites per Isoline",
                x = "Isoline [km]",
                y = "Number of sites") +
  ggplot2::scale_x_continuous(limit = your_limit,
                              breaks = your_breaks) +
  ggplot2::theme_bw() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                 axis.text.x = ggplot2::element_text(angle = 90))

plot_n_sites

# Percent of sites per isoline
plot_perc_sites <-
Isolines_stats %>%
  ggplot2::ggplot(ggplot2::aes(x = km_isoline, y = percent_Sites)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::labs(title = "Percentage of sites per Isoline",
                x = "Isoline [km]",
                y = "Percentage of sites [%]") +
  ggplot2::scale_x_continuous(limit = your_limit,
                              breaks = your_breaks) +
  ggplot2::theme_bw() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                 axis.text.x = ggplot2::element_text(angle = 90))

plot_perc_sites

# Area per isoline
plot_area <-
Isolines_stats %>%
  ggplot2::ggplot(ggplot2::aes(x = km_isoline, y = Area)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::labs(title = "Enclosed area per isoline",
                x = "Isoline [km]",
                y = "Area [km²]") +
  ggplot2::scale_x_continuous(limit = your_limit,
                              breaks = your_breaks) +
  ggplot2::theme_bw() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                 axis.text.x = ggplot2::element_text(angle = 90))

plot_area
  
# Plot of increase of number of sites and area per equidistance ----------------

# Increase of number of sites
plot_incr_sites <-
Isolines_stats %>%
  ggplot2::ggplot(ggplot2::aes(x = km_isoline, y = increase_Sites)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::labs(title = "Increase of number of sites per equidistance",
                x = "[km]",
                y = "number of sites") +
  ggplot2::scale_x_continuous(limit = your_limit,
                              breaks = your_breaks) +
  ggplot2::theme_bw() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                 axis.text.x = ggplot2::element_text(angle = 90))

plot_incr_sites

# Increase of area
plot_incr_area <-
Isolines_stats %>%
  ggplot2::ggplot(ggplot2::aes(x = km_isoline, y = increase_Area)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::labs(title = "Increase of area per equidistance",
                x = "[km]",
                y = "Area [km²]") +
  ggplot2::scale_x_continuous(limit = your_limit,
                              breaks = your_breaks) +
  ggplot2::theme_bw() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                 axis.text.x = ggplot2::element_text(angle = 90))

plot_incr_area
# Plot of difference in increase of number of sites and area per equidistance --

# Difference of increase of number of sites
plot_diff_sites <-
Isolines_stats %>%
  ggplot2::ggplot(ggplot2::aes(x = km_isoline, y = diff_Sites)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::labs(title = "Difference of increase of number of sites per equidistance",
                x = "[km]",
                y = "Difference") +
  ggplot2::scale_x_continuous(limit = your_limit,
                              breaks = your_breaks) +
  ggplot2::theme_bw() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                 axis.text.x = ggplot2::element_text(angle = 90))

plot_diff_sites

# Difference increase of area
plot_diff_area <-
Isolines_stats %>%
  ggplot2::ggplot(ggplot2::aes(x = km_isoline, y = diff_Area)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::labs(title = "Difference of increase of area per equidistance",
                x = "[km]",
                y = "Area [km²]") +
  ggplot2::scale_x_continuous(limit = your_limit,
                              breaks = your_breaks) +
  ggplot2::theme_bw() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                 axis.text.x = ggplot2::element_text(angle = 90))

plot_diff_area

# Save plots as images ---------------------------------------------------------

# export raster maps as tiffs
tiff("output/01_kriging.tiff", width = 7, height = 7, units = 'in', res = 300)
plot_kriging()
dev.off()

tiff("output/02_variance.tiff", width = 7, height = 7, units = 'in', res = 300)
plot_variance()
dev.off()


# export plots to svg
im_width = 7

svg("output/00_sites_voronoi.svg", width=im_width)
plot_sites_voronoi()
dev.off()

svg("output/03_vario.svg", width=im_width)
plot_vario
dev.off()

svg("output/04_vario_zoom.svg", width=im_width)
plot_vario_zoom
dev.off()

svg("output/05_n_area.svg", width=im_width)
plot_n_area
dev.off()

svg("output/06_n_sites.svg", width=im_width)
plot_n_sites
dev.off()

svg("output/07_perc_sites.svg", width=im_width)
plot_perc_sites
dev.off()

svg("output/08_area.svg", width=im_width)
plot_area
dev.off()

svg("output/09_incr_sites.svg", width=im_width)
plot_incr_sites
dev.off()

svg("output/10_incr_area.svg", width=im_width)
plot_incr_area
dev.off()

svg("output/11_diff_sites.svg", width=im_width)
plot_diff_sites
dev.off()

svg("output/12_diff_area.svg", width=im_width)
plot_diff_area
dev.off()

# Save data --------------------------------------------------------------------

# Isoline_stats
write.table(Isolines_stats,
            "output/Isolines_stats.csv",
            sep = ";",
            dec = ",",
            row.names = FALSE)

# Polygons of Isolines and raster of Kriging
if(export_raster == TRUE){
  
  # Polygons of isolines as shape file
  rgdal::writeOGR(isoline_polygons,
                  dsn = "output",
                  layer = "isoline_polygons",
                  driver = "ESRI Shapefile",
                  check_exists = TRUE,
                  overwrite_layer = TRUE)
  
  # Rewrite .prj-File with WKT
  rgdal::showWKT(your_projection, file="output/isoline_polygons.prj")
  
  if(merge_polygons == TRUE){
  # Merged Polygons of isolines as shape file
  rgdal::writeOGR(isoline_merged,
                  dsn = "output",
                  layer = "isoline_merged",
                  driver = "ESRI Shapefile",
                  check_exists = TRUE,
                  overwrite_layer = TRUE)
  # Rewrite .prj-File with WKT
  rgdal::showWKT(your_projection, file="output/isoline_merged.prj")
  
  # export smoothed Isolines
  
  rgdal::writeOGR(iso_merged_lines_smooth,
  dsn = "output",
  layer = "iso_merged_lines_smooth",
  driver = "ESRI Shapefile",
  check_exists = TRUE,
  overwrite_layer = TRUE)

  # Rewrite .prj-File with WKT
  rgdal::showWKT(your_projection, file="output/iso_merged_lines_smooth.prj")
  
  }
  
  # Vornoi diagrams as shape file
  rgdal::writeOGR(voronoi_tiles,
                  dsn = "output",
                  layer = "voronoi_tiles",
                  driver = "ESRI Shapefile",
                  check_exists = TRUE,
                  overwrite_layer = TRUE)
  # Rewrite .prj-File with WKT
  rgdal::showWKT(your_projection, file="output/voronoi_tiles.prj")
  
  
  # Vertices as shape file
  rgdal::writeOGR(vertices_spdf,
                  dsn = "output",
                  layer = "vertices",
                  driver = "ESRI Shapefile",
                  check_exists = TRUE,
                  overwrite_layer = TRUE)
  # Rewrite .prj-File with WKT
  rgdal::showWKT(your_projection, file="output/vertices.prj")
  
  # Polygons of isolines as shape file
  rgdal::writeOGR(sites,
                  dsn = "output",
                  layer = "sites",
                  driver = "ESRI Shapefile",
                  check_exists = TRUE,
                  overwrite_layer = TRUE)
  
  # Rewrite .prj-File with WKT
  rgdal::showWKT(your_projection, file="output/sites.prj")
  
  # Write raster files as GeoTiff and grd-File for use in GIS-Programms like QGIS
  
  # Kriging-Results
  r <- raster::rasterFromXYZ(data.frame(x = sp::coordinates(LEC_kriged)[,1],
                                        y = sp::coordinates(LEC_kriged)[,2],
                                        z = LEC_kriged$var1.pred),
                             crs = sp::CRS(your_projection))
  
  
  
  raster::writeRaster(r, "output/Kriging_raster.tif", format="GTiff", overwrite=T)
  raster::writeRaster(r, "output/Kriging_raster.grd",format="raster", overwrite=T)
  
  
  # Variance (Quality Measure)
  v <- raster::rasterFromXYZ(data.frame(x = sp::coordinates(LEC_kriged)[,1],
                                        y = sp::coordinates(LEC_kriged)[,2],
                                        z = LEC_kriged$var1.var),
                             crs = sp::CRS(your_projection))
  
  
  
  raster::writeRaster(v, "output/Variance_raster.tif", format="GTiff", overwrite=T, prj=T)
  raster::writeRaster(v, "output/Variance_raster.grd",format="raster", overwrite=T, prj=T)
  
}

