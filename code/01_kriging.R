library("magrittr")

# Basics -----------------------------------------------------------------------

# Turn scientific notation off
options(scipen = 999)

# Variogram --------------------------------------------------------------------

# Define Bounding Box Diagonal
bbox_diag <- sp::spDists(t(vertices_spdf@bbox))[1, 2]

# Lagdistance = Bounding Box Diagonal / 250
lagdist <- bbox_diag/250

# Sample variogram
vertices_vario <- gstat::variogram(radiusLEC~1,
                                   vertices_spdf,
                                   width = lagdist)

# Identify first plateau for fitting theoretical variogram  
range.plateau <- vertices_vario %$%
  gamma %>%
  diff() %>%
  {vertices_vario[2][which.max(./.[1] < 0.1), ]}

sill.plateau <- vertices_vario$gamma[vertices_vario$dist == range.plateau]
  
# Fitting theoretical variogram
vertices_vario_fit <- gstat::fit.variogram(vertices_vario,
                                           # Zimmermann et al 2004, 52
                                           gstat::vgm(nugget = 0,
                                                      model  = your_model,
                                                      psill  = sill.plateau,
                                                      range  = range.plateau),
                                           fit.sills = FALSE,
                                           fit.ranges = FALSE)

# Kriging ----------------------------------------------------------------------

# Create a grid for kriging
grid <- expand.grid(x = seq(as.integer(range(vertices_spdf@coords[, 1]))[1],
                            as.integer(range(vertices_spdf@coords[, 1]))[2],
                            by = your_grid_spacing),
                    y = seq(as.integer(range(vertices_spdf@coords[, 2]))[1],
                            as.integer(range(vertices_spdf@coords[, 2]))[2],
                            by = your_grid_spacing)) %>%
  {sp::SpatialPoints(coords = .[1:2], proj4string = sp::CRS(your_projection))}
 
# Kriging
LEC_kriged <- gstat::krige(radiusLEC~1,
                           vertices_spdf,
                           grid,
                           model = vertices_vario_fit,
                           nmin = 3,
                           nmax = 10,
                           maxdist = bbox_diag/2,
                           debug.level = -1)

# Create isolines ------------------------------------------------------------
isoline_polygons <- LEC_kriged %>%
  {raster::rasterFromXYZ(data.frame(x = sp::coordinates(.)[, 1],
                                  y = sp::coordinates(.)[, 2],
                                  z = .[[1]]),
                         crs = sites@proj4string)} %>%
  as("SpatialGridDataFrame") %>%
  inlmisc::Grid2Polygons(level = TRUE, at = your_isoline_steps)

# Rename the isolines because Grid2Polygon names them with the middle value
isoline_polygons@data[, 1] <- your_isoline_steps[-1]

# Save data --------------------------------------------------------------------

if(export_raster == TRUE){

# Polygons of isolines as shape file
rgdal::writeOGR(isoline_polygons,
                dsn = "output",
                layer = "isoline_polygons",
                driver = "ESRI Shapefile",
                check_exists = TRUE,
                overwrite_layer = TRUE)

# Write raster files as GeoTiff and grd-File for use in GIS-Programms like QGIS

# Kriging-Results
r <- raster::rasterFromXYZ(data.frame(x = sp::coordinates(LEC_kriged)[,1],
                                      y = sp::coordinates(LEC_kriged)[,2],
                                      z = LEC_kriged$var1.pred),
                                    crs = sp::CRS(your_projection))



raster::writeRaster(r, "output/Kriging_raster.tif", format="GTiff", overwrite=T, prj=T)
raster::writeRaster(r, "output/Kriging_raster.grd",format="raster", overwrite=T, prj=T)


# Variance (Quality Measure)
v <- raster::rasterFromXYZ(data.frame(x = sp::coordinates(LEC_kriged)[,1],
                                      y = sp::coordinates(LEC_kriged)[,2],
                                      z = LEC_kriged$var1.var),
                                    crs = sp::CRS(your_projection))



raster::writeRaster(v, "output/Variance_raster.tif", format="GTiff", overwrite=T, prj=T)
raster::writeRaster(v, "output/Variance_raster.grd",format="raster", overwrite=T, prj=T)

}