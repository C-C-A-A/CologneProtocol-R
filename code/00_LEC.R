library("magrittr")

# Dependencies -----------------------------------------------------------------

# to install all dependencies run:
# automagic::install_deps_file()


# Basics -----------------------------------------------------------------------

# Turn scientific notation off
options(scipen = 999)

# Definition of used variables throughout the script ---------------------------

# variables used for loading and creating data 
your_projection <- "+init=epsg:31467" # the projection of your data
your_grid_spacing <- 1000 # choose some value [m] between 200 and 1000 
your_isoline_steps <- seq(0, 20000, 500) # min, max, step (equidistance)
remove_border_points <- FALSE # Boolean (TRUE/FALSE), wether border points are removed or not
export_raster <- TRUE # Boolean (TRUE/FALSE), wether kriging and variance raster are exported
merge_polygons <- FALSE # Boolean (TRUE/FALSE), wether polygons should be merged (experimental) to get no of parts

# variables used for kriging
your_model <- c("Sph") # choose from gstat::show.vgm(), 
your_nmin <- 3 # minimum number of sites used for interpolation (your_nmin = 3 is default, Schmidt et al. in prep)
your_nmax <- 10 # maximum number of sites used for interpolation (your_nmax = 10 is default, Schmidt et al. in prep)

# variables used for plotting statistics of isolines
# choose according to your_isoline_steps
your_limit <- c(1, 20) # limits the x-axis
your_breaks <- seq(1, 20, 1) # sets the tick marks on x-axis


# Load data --------------------------------------------------------------------

# Sample data of Early Neolithic sites from Preuss 1998
# Data is available from the CRC 806 database
# The accuracy of the localization of the sites is 2.5 km (Zimmermann et al. 2004, 55)
url_link <- "http://sfb806srv.uni-koeln.de/owsproxy.php?service=WFS&version=1.0.0&request=GetFeature&typeNames=geonode%3A_13_earlyneolithic_ce_sites_wgs84&outputFormat=csv"

# load date as a data.frame
sites <- read.csv(url(url_link))

# Conversion into SPDF
sites <- sp::SpatialPointsDataFrame(sp::SpatialPoints(
  cbind(sites$RECHTS, sites$HOCH)),
  sites,
  proj4string = CRS(your_projection))

sp::proj4string(sites) <- sp::CRS(your_projection)

# Alternatively you can load your local data (copy it to folder data)
# sites <- rgdal::readOGR(dsn = "data", layer = "earlyneolithic_ce_sites")
# sp::proj4string(sites) <- sp::CRS(your_projection)

# Largest Empty Circles (LEC) --------------------------------------------------

# calculate vertices of voronoi diagram
voronoi_vertices <- deldir::deldir(sites@coords[, 1],
                                   sites@coords[, 2],
                                   rw=c(t(sites@bbox)[1,1],
                                        t(sites@bbox)[2,1],
                                        t(sites@bbox)[1,2],
                                        t(sites@bbox)[2,2])) %$%
  # extraction of vertices
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


# If Condition wether border points are removed or not
if(remove_border_points == TRUE){
# remove dublicates and border points
vertices_spdf <- sp::remove.duplicates(vertices_spdf) %>%
  {.[.[[2]] == FALSE, ]}
} else {
  # just remove dublicates
  vertices_spdf <- sp::remove.duplicates(vertices_spdf)  
}


# calculate radius of LEC and add this information to vertices_spdf
vertices_spdf@data$radiusLEC <- rgeos::gDistance(sites,
                                                 vertices_spdf,
                                                 byid = TRUE) %>%
  apply(1, min)


# Extraction of Voronoi tiles --------------------------------------------------

# THIS IS EXPERIMENTAL
# The goal of this code is to convert the voronoi tiles of deldir into
# SpatialPolygonsDataFrame for export and use in a GIS


# Define function to duplicate the top row of a data.frame and bind it as last entry
# This structure is needed to create Polygons
df.bind <- function(x) {
  rbind(x, x[1, ])
}

# creating a list of all tiles 
voronoi_list <- deldir::deldir(sites@coords[, 1],
                             sites@coords[, 2]) %>%
  deldir::tile.list() %>%
  lapply('[', c("x", "y")) %>%
  lapply(as.data.frame) %>%
  lapply(df.bind) %>%
  lapply(as.matrix) %>%
  lapply(sp::Polygon, hole = FALSE)

# crate a vector of "names" for the attribute ID, which is needed in sp::Polygons
# This part may be improved by taking meaningful IDs
names_ID <- as.character(c(1:length(voronoi_list)))

# Covert Polygon class into Polygons class
# This code was provided by user Ben from stackexchange:
# https://gis.stackexchange.com/questions/171124/data-frame-to-spatialpolygonsdataframe-with-multiple-polygons
voronoi_tiles <- lapply(seq_along(voronoi_list), function(i) sp::Polygons(list(voronoi_list[[i]]),
                                                                 ID = names_ID[i]))

# Convert Polygons class into SpatialPolygons
voronoi_tiles <- sp::SpatialPolygons(voronoi_tiles, proj4string = sp::CRS(your_projection))

# Coerce to SpatialPolygonsDataFrame. This is needed for output via writeOGR
# Note: The Data has no meaning
voronoi_tiles <- as(voronoi_tiles, "SpatialPolygonsDataFrame")
