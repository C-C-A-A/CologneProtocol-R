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
                           nmin = your_nmin,
                           nmax = your_nmax,
                           maxdist = bbox_diag/2,
                           debug.level = -1)

# Create isolines --------------------------------------------------------------
isoline_polygons <- LEC_kriged %>%
  {raster::rasterFromXYZ(data.frame(x = sp::coordinates(.)[, 1],
                                    y = sp::coordinates(.)[, 2],
                                    z = .[[1]]),
                         crs = sites@proj4string)} %>%
  as("SpatialGridDataFrame") %>%
  inlmisc::Grid2Polygons(level = TRUE, at = your_isoline_steps)

# This is not a reprojection!
sp::proj4string(isoline_polygons) <- sp::CRS(your_projection)

# Rename the isolines because Grid2Polygon names them with the middle value
isoline_polygons@data[, 1] <- your_isoline_steps[2:c(length(isoline_polygons@data[, 1])+1)]


# Merge polygons ---------------------------------------------------------------

# THIS IS EXPERIMENTAL

# Please note: the running time of the following code may be very long
# Create new SpatialPolygonsDataFrame with merged Polygons in order to reduce
# errors when calculating the number of areas with a specific site density.

# Until now the values of the following code need to be adjusted by hand

# copy of isoline_polygons
isoline_polygons_copy <- isoline_polygons

# Get Isoline steps/equidistance variables
equidist <- diff(your_isoline_steps)[1]
penultimate_iso <- max(your_isoline_steps) - equidist

# New SPDF with only areas of the lowest site density
print(paste0("Creating Contour-Line ", 1,"/",length(seq(equidist, penultimate_iso, equidist)),": ",equidist))
isoline_merged <- isoline_polygons_copy[isoline_polygons_copy@data[, 1] == equidist, ]

# Variable needed for printing progress
n = 2

# The following loop merges the polygons.
for (i in seq(equidist, penultimate_iso, equidist)) {
  
  # Print progress
  print(paste0("Creating Contour-Line ", n,"/",length(seq(equidist, penultimate_iso, equidist))+1,": ",i + equidist))
  flush.console()
  n = n + 1
  
  # change the value of site density of a polygon to one higher equidistance
  isoline_polygons_copy[isoline_polygons_copy@data[, 1] == i, ] <- i + equidist
  
  # aggregate these polygons
  merged_iso_i <- raster::aggregate(isoline_polygons_copy[isoline_polygons_copy@data[, 1] == i + equidist, ], by = "z")
  
  # merge new SPDF with aggregated polygons
  isoline_merged <- rbind(isoline_merged,
                          merged_iso_i)
  
  rm(merged_iso_i)
  
}

# delete copy of isoline_polygons
rm(isoline_polygons_copy)

