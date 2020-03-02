

#### load data ####

# archaeological sites
sites_spdf <- rgdal::readOGR(dsn = "data", layer = "Roessen")
sp::proj4string(sites_spdf) <- sp::CRS("+init=epsg:25832") # !No reprojection, data is already in EPSG 25832!

#### Largest Empty Circles (LEC) ####

# calculate Thiessen polygons
Thiessen_polygons <- deldir::deldir(sites_spdf@coords[,1], sites_spdf@coords[,2]) %>%
  deldir::tile.list()

# Thiessen vertices
Thiessen_vertices <- data.frame(x = unlist(sapply(Thiessen_polygons, "[", "x"),use.names = F),
                                y = unlist(sapply(Thiessen_polygons, "[", "y"),use.names = F))


 

#### needs to be deleted ####
plot(sites_spdf@coords[,1], sites_spdf@coords[,2], type = "n", asp = 1)
points(sites_spdf, pch = 20, col = "red")
points(sites_nodes_spdf, pch = 20, col = "black")
plot(deldir(sites_spdf@coords[,1], sites_spdf@coords[,2]), wlines = "tess", wpoints = "none", number = FALSE, lty = 1, add = TRUE)


