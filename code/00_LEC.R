library("magrittr")

# basics -----------------------------------------------------------------------

# turn scientific notation off
options(scipen = 999)

# load data --------------------------------------------------------------------

# archaeological sites
sites_spdf <- rgdal::readOGR(dsn = "data", layer = "Roessen")
sp::proj4string(sites_spdf) <- sp::CRS("+init=epsg:25832") # !No reprojection, data is already in EPSG 25832!

# Largest Empty Circles (LEC) --------------------------------------------------

# calculate vertices of Thiessen polygons
Thiessen_vertices <- deldir::deldir(sites_spdf@coords[,1], sites_spdf@coords[,2]) %$%
  dirsgs

# rearrange Thiessen_vertices in preparation for transformation into an spdf
Thiessen_vertices <- rbind(setNames(Thiessen_vertices[,c(1:2,5,7,9)], c("x", "y", "ind", "bp", "thirdv")), setNames(Thiessen_vertices[,c(3:4,6,8,10)], c("x", "y", "ind", "bp", "thirdv")))

# transformation of Thiessen_vertices into an spdf
Thiessen_vertices_spdf <- sp::SpatialPointsDataFrame(coords = Thiessen_vertices[1:2], data = Thiessen_vertices[,3:5] )
sp::proj4string(Thiessen_vertices_spdf) <- sp::CRS("+init=epsg:25832") # !No reprojection, data is already in EPSG 25832!

# remove dublicates and border points !! see issue for border points !!
Thiessen_vertices_spdf <- sp::remove.duplicates(Thiessen_vertices_spdf) %>%
  {.[.[[2]] == FALSE, ]}

# calculate radius of LEC and add this information to Thiessen_vertices_spdf
Thiessen_vertices_spdf@data$radiusLEC <- rgeos::gDistance(sites_spdf, Thiessen_vertices_spdf, byid = TRUE) %>%
  apply(1,min)
