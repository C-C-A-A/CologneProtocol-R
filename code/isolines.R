library("magrittr")

#### basics ####

# turn scientific notation off
options(scipen = 999)

#### variogram ####

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
                                                    gstat::vgm(nugget = NA,
                                                        model = "Sph",
                                                        psill = sill.plateau,
                                                        range = range.plateau),
                                                    fit.sills = FALSE,
                                                    fit.ranges = FALSE)

#### kriging ####

# creating a grid for kriging
grid <- expand.grid(x = seq(as.integer(range(Thiessen_vertices_spdf@coords[,1]))[1],
                            as.integer(range(Thiessen_vertices_spdf@coords[,1]))[2],
                            by = 300),
                    y = seq(as.integer(range(Thiessen_vertices_spdf@coords[,2]))[1],
                            as.integer(range(Thiessen_vertices_spdf@coords[,2]))[2],
                            by = 300)) %>%
  {sp::SpatialPoints(coords = .[1:2], proj4string = sp::CRS("+init=epsg:25832"))}
 
# kriging
LEC_kriged <- gstat::krige(radiusLEC~1,
                           Thiessen_vertices_spdf,
                           grid,
                           model = Thiessen_vertices_vario_fit,
                           nmin = 3,
                           nmax = 10,
                           debug.level = -1)

#### creating isolines ####
isoline_polygons <- LEC_kriged %>%
  {raster::rasterFromXYZ(data.frame(x = sp::coordinates(.)[,1],
                                  y = sp::coordinates(.)[,2],
                                  z = .[[1]]),
                         crs = sites_spdf@proj4string)} %>%
  as("SpatialGridDataFrame") %>%
  inlmisc::Grid2Polygons(level = TRUE, at = seq(0,70000,500))

#### descriptive propertives of isolines ####

# initialize data.frame
Isolines_stats <- data.frame(km_isoline = integer(),
                                number_Ar = integer(), 
                                number_Si = integer(), 
                                Pro_Si = integer(),
                                Area = integer(),
                                stringsAsFactors=FALSE)

# counting the numbers of distinct areas per isoline
for (i in 1:length(isoline_polygons)) {
  Isolines_stats[i,2] <- length(isoline_polygons@polygons[[i]]@Polygons) - sum(sapply(isoline_polygons@polygons[[i]]@Polygons,                                                                                   function(x) {sum(isTRUE(x@hole), na.rm = TRUE)}))
}

# insert name of isolines
Isolines_stats[, 1] <- isoline_polygons@data[, 1] + isoline_polygons@data[1, 1]

# calculate number of sites within each isoline
Pt_overlay <- sp::over(sites_spdf, isoline_polygons)
Pt_overlay$ID <- rownames(Pt_overlay)
sites_spdf@data$id <- rownames(sites_spdf@data)
sites_spdf@data <- dplyr::left_join(sites_spdf@data, Pt_overlay, by = c("id" = "ID"))

Isolines_stats[1,3] <- sum(sites_spdf@data$z == Isolines_stats[1,1]-250)

for (i in 2:length(Isolines_stats[,3])) {
  Isolines_stats[i,3] <- sum(sites_spdf@data$z == Isolines_stats[i,1]-250) + Isolines_stats[i-1,3]
}

# calculate the percentage increase in the nummber of site per isoline
for (i in 1:length(Isolines_stats[,4])) {
  
  Isolines_stats[i,4] <- (Isolines_stats[i,3] * 100) / length(sites_spdf)
  
}

# calculate area enclosed by each isoline


#### save data ####

rgdal::writeOGR(isoline_polygons,
                dsn = "output",
                layer = "isoline_polygons",
                driver = "ESRI Shapefile",
                check_exists = TRUE,
                overwrite_layer = TRUE)

