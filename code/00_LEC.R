library("magrittr")

# Dependencies -----------------------------------------------------------------

# to install all dependencies run:
# automagic::install_deps_file()


# Basics -----------------------------------------------------------------------

# Turn scientific notation off
options(scipen = 999)

# Definition of used variables throughout the script ---------------------------

# variables used for loading and creating data 
your_projection <- "+init=epsg:25832" # the projection of your data
your_grid_spacing <- 500 # choose some value [m] between 200 and 1000 
your_isoline_steps <- seq(0, 10000, 500) # min, max, step (equidistance)
remove_border_points <- FALSE # Boolean (TRUE/FALSE), wether border points are removed or not

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

