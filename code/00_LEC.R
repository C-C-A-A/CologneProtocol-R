library("magrittr")

# Basics -----------------------------------------------------------------------

# Turn scientific notation off
options(scipen = 999)

# Definition of used variables throughout the script
your_projection <- "+init=epsg:25832"

# Load data --------------------------------------------------------------------

# Archaeological sites
sites <- rgdal::readOGR(dsn = "data", layer = "earlyneolithic_ce_sites")

# Ignore warining! It's not a reprojection. But some functions need this step. 
sp::proj4string(sites) <- sp::CRS(your_projection)

# Largest Empty Circles (LEC) --------------------------------------------------

# calculate vertices of voronoi diagram
voronoi_vertices <- deldir::deldir(sites@coords[, 1],
                                   sites@coords[, 2]) %$%
  dirsgs

# rearrange voronoi_vertices in preparation for transformation into an spdf
voronoi_vertices <- rbind(setNames(voronoi_vertices[,c(1:2, 5, 7, 9)],
                                   c("x", "y", "ind", "bp", "thirdv")),
                          setNames(voronoi_vertices[,c(3:4, 6, 8, 10)],
                                   c("x", "y", "ind", "bp", "thirdv")))

# transformation of voronoi_vertices into an spdf
vertices_spdf <- sp::SpatialPointsDataFrame(coords = voronoi_vertices[1:2],
                                            data = voronoi_vertices[, 3:5])

# Ignore warining! It's not a reprojection. But some functions need this step.
sp::proj4string(vertices_spdf) <- sp::CRS(your_projection) 

# remove dublicates and border points
vertices_spdf <- sp::remove.duplicates(vertices_spdf) %>%
  {.[.[[2]] == FALSE, ]}

# calculate radius of LEC and add this information to Thiessen_vertices_spdf
vertices_spdf@data$radiusLEC <- rgeos::gDistance(sites,
                                                 vertices_spdf,
                                                 byid = TRUE) %>%
  apply(1, min)
