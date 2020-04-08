library("magrittr")

# Basics -----------------------------------------------------------------------

# Turn scientific notation off
options(scipen = 999)

# Plot of archaeological sites and Thiessen polygons ------------------------

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

# Variogram / semivariogram ----------------------------------------------------
plot(vertices_vario,
     vertices_vario_fit)

# Zoom in on range and sill plateau (range and sill.platau + 20 %)
plot(vertices_vario,
     vertices_vario_fit,
     xlim = c(0, range.plateau+(range.plateau/100)*20),
     ylim = c(0, sill.plateau+(sill.plateau/100)*20))

# Kriging results --------------------------------------------------------------
LEC_kriged %>% as.data.frame %>%
  ggplot2::ggplot(ggplot2::aes(x=x, y=y)) +  
  ggplot2::geom_tile(ggplot2::aes(fill=var1.pred))+
  ggplot2::coord_equal() +
  ggplot2::scale_fill_gradientn(colours = terrain.colors(255)) +
  ggplot2::theme_bw() +
  ggplot2::geom_point(data = as.data.frame(sites),
                      ggplot2::aes(x = coords.x1, y = coords.x2), shape=21, size=2, 
                      colour = "black", fill= "red", alpha=1/5)

# Variance of kriging results --------------------------------------------------
LEC_kriged %>% as.data.frame %>%
  ggplot2::ggplot(ggplot2::aes(x=x, y=y)) + 
  ggplot2::geom_tile(ggplot2::aes(fill=var1.var)) + 
  ggplot2::coord_equal() +
  ggplot2::scale_fill_gradient(low = "yellow", high="red") +
  ggplot2::theme_bw() +
  ggplot2::geom_point(data = as.data.frame(sites),
                      ggplot2::aes(x = coords.x1, y = coords.x2), shape=21, size=2, 
                      colour = "white", fill= "black", alpha=1/2)


# Change units of all data.frames ----------------------------------------------
Isolines_stats[, 1] <- Isolines_stats[, 1] / 1000 # for easy reading


# Plot of descriptive properties of isolines -----------------------------------

# Number of distinct areas per isoline
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

  
# Number of sites per isoline
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

# Percent of sites per isoline
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

# Area per isoline
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

# Plot of increase of number of sites and area per equidistance ----------------

# Increase of number of sites
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

# Increase of area
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

# Plot of difference in increase of number of sites and area per equidistance --

# Difference of increase of number of sites
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

# Difference increase of area
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

# Save data --------------------------------------------------------------------

# Isoline_stats
write.table(Isolines_stats,
            "output/Isolines_stats.csv",
            sep = ";",
            dec = ",")

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
  
  # Merged Polygons of isolines as shape file
  rgdal::writeOGR(isoline_merged,
                  dsn = "output",
                  layer = "isoline_merged",
                  driver = "ESRI Shapefile",
                  check_exists = TRUE,
                  overwrite_layer = TRUE)
  # Rewrite .prj-File with WKT
  rgdal::showWKT(your_projection, file="output/isoline_merged.prj")
  
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
