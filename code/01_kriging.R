library("magrittr")

# basics -----------------------------------------------------------------------

# turn scientific notation off
options(scipen = 999)

# variogram --------------------------------------------------------------------

# sample variogram
Thiessen_vertices_vario <- gstat::variogram(radiusLEC~1,
                                           Thiessen_vertices_spdf,
                                           width = sp::spDists(t(Thiessen_vertices_spdf@bbox))[1,2]/250)

# identify first plateau for fitting theoretical variogram
range.plateau <- Thiessen_vertices_vario %$%
  gamma %>%
  diff() %>%
  {Thiessen_vertices_vario[2][which.max(./.[1] < 0.1), ]}

sill.plateau <- Thiessen_vertices_vario$gamma[Thiessen_vertices_vario$dist == range.plateau]
  
# fitting theoretical variogram
Thiessen_vertices_vario_fit <- gstat::fit.variogram(Thiessen_vertices_vario,
                                                    gstat::vgm(nugget = 0, # Zimmermann 2004, 52
                                                        model = "Sph",
                                                        psill = sill.plateau,
                                                        range = range.plateau),
                                                    fit.sills = FALSE,
                                                    fit.ranges = FALSE)

# kriging ----------------------------------------------------------------------

# creating a grid for kriging
grid <- expand.grid(x = seq(as.integer(range(Thiessen_vertices_spdf@coords[,1]))[1],
                            as.integer(range(Thiessen_vertices_spdf@coords[,1]))[2],
                            by = 200),
                    y = seq(as.integer(range(Thiessen_vertices_spdf@coords[,2]))[1],
                            as.integer(range(Thiessen_vertices_spdf@coords[,2]))[2],
                            by = 200)) %>%
  {sp::SpatialPoints(coords = .[1:2], proj4string = sp::CRS("+init=epsg:25832"))}
 
# kriging
LEC_kriged <- gstat::krige(radiusLEC~1,
                           Thiessen_vertices_spdf,
                           grid,
                           model = Thiessen_vertices_vario_fit,
                           nmin = 3,
                           nmax = 10,
                           maxdist = sp::spDists(t(Thiessen_vertices_spdf@bbox))[1,2]/2,
                           debug.level = -1)

# creating isolines ------------------------------------------------------------
isoline_polygons <- LEC_kriged %>%
  {raster::rasterFromXYZ(data.frame(x = sp::coordinates(.)[,1],
                                  y = sp::coordinates(.)[,2],
                                  z = .[[1]]),
                         crs = sites_spdf@proj4string)} %>%
  as("SpatialGridDataFrame") %>%
  inlmisc::Grid2Polygons(level = TRUE, at = seq(0,70000,500))

# save data --------------------------------------------------------------------

# polygons of isolines as shape file
rgdal::writeOGR(isoline_polygons,
                dsn = "output",
                layer = "isoline_polygons",
                driver = "ESRI Shapefile",
                check_exists = TRUE,
                overwrite_layer = TRUE)
