library("magrittr")

#### basics ####

# turn scientific notation off
options(scipen = 999)

#### variogram ####

# sample variogram
Thiessen_vertices_vario <- gstat::variogram(radiusLEC~1,
                                           Thiessen_vertices_spdf,
                                           width = 200,
                                           cutoff = 20000)

# identify first plateau for fitting theoretical variogram
plateau <- gstat::variogram(radiusLEC~1,
                            Thiessen_vertices_spdf,
                            width = 200,
                            cutoff = 20000) %$%
  gamma %>%
  diff() %>%
  {Thiessen_vertices_vario[2][which.max(./.[1] < 0.1), ]}
  
# fitting theoretical variogram
Thiessen_vertices_vario_fit <- gstat::fit.variogram(Thiessen_vertices_vario,
                                                    gstat::vgm(nugget = 0,
                                                        model = "Exp",
                                                        sill = plateau),
                                                    fit.sills = FALSE,
                                                    fit.ranges = TRUE)

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
                           model = Thiessen_vertices_vario_fit)

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
                                Anzahl_Fl = integer(), 
                                Anzahl_Fst = integer(), 
                                Pro_Fst = integer(),
                                Area = integer(),
                                stringsAsFactors=FALSE)

# Zaehlen der Anzahl an Flaechen je Isolinie:
for (i in 1:length(isoline_polygons)) {
  # der SPDF besteht aus einer Verschachtelung mehrerer Listen, es wird von der Gesamtanzahl an Polygonen einer Liste die Anzahl an Polygonen abgezogen, die im slot hole TRUE stehen haben.
  Isolines_stats[i,2] <- length(isoline_polygons@polygons[[i]]@Polygons) - sum(sapply(isoline_polygons@polygons[[i]]@Polygons, function(x) {sum(isTRUE(x@hole), na.rm = TRUE)}))
}



#### needs to be deleted ####

rgdal::writeOGR(isoline_polygons, dsn = "output", layer = "isoline_polygons", driver = "ESRI Shapefile")

