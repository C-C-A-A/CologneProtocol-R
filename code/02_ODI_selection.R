library("magrittr")

# basics -----------------------------------------------------------------------

# turn scientific notation off
options(scipen = 999)

# basic descriptive properties of isolines -------------------------------------

# initialize data.frame
Isolines_stats <- data.frame(km_isoline = integer(),
                             number_Area = integer(), 
                             number_Sites = integer(), 
                             percent_Sites = integer(),
                             Area = integer(),
                             stringsAsFactors=FALSE)

# counting the numbers of distinct areas per isoline
for (i in 1:length(isoline_polygons)) {
  Isolines_stats[i,2] <- length(isoline_polygons@polygons[[i]]@Polygons) - sum(sapply(isoline_polygons@polygons[[i]]@Polygons,                                                                                   function(x) {sum(isTRUE(x@hole), na.rm = TRUE)}))
}

# insert name of isolines
Isolines_stats[, 1] <- isoline_polygons@data[, 1] + (isoline_polygons@data[2, 1] - isoline_polygons@data[1, 1])/2
Isolines_stats[, 1] <- Isolines_stats[, 1]/1000

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
Isolines_stats[1, 5] <- raster::area(isoline_polygons)[1]/1000000

for (i in 2:length(Isolines_stats[,5])) {
  Isolines_stats[i,5] <- raster::area(isoline_polygons)[i]/1000000 + Isolines_stats[i-1,5]
}

# increase of number of sites and area per km ----------------------------------

# initialize data.frame
Isolines_increase <- data.frame(km_isoline = integer(),
                                increase_Sites = integer(), 
                                increase_Area = integer(),
                                stringsAsFactors=FALSE)

# calculate increase in numbers of site per km
for (i in 1:length(Isolines_stats[,1])) {
  Isolines_increase[i,2] <- (Isolines_stats[i+1,3] - Isolines_stats[i,3]) * 2
}

# insert name of isolines
Isolines_increase[, 1] <- isoline_polygons@data[, 1] + (isoline_polygons@data[2, 1] - isoline_polygons@data[1, 1])/2 + 500
Isolines_increase[, 1] <- Isolines_increase[, 1]/1000


# calculate increase in area of polygon per km
for (i in 1:length(Isolines_stats[,1])) {
  Isolines_increase[i,3] <- (Isolines_stats[i+1,5] - Isolines_stats[i,5]) * 2
}

# Difference in increase of number of sites and area per km --------------------

# initialize data.frame
Isolines_diff <- data.frame(km_isoline = integer(),
                            diff_Sites = integer(), 
                            diff_Area = integer(),
                            stringsAsFactors=FALSE)

# calculate difference of increase of number of site per km
for (i in 1:length(Isolines_increase[, 1])) {
  Isolines_diff[i,2] <- Isolines_increase[i,2] - Isolines_increase[i+1,2]
}

# insert name of isolines
Isolines_diff[, 1] <- isoline_polygons@data[, 1] + (isoline_polygons@data[2, 1] - isoline_polygons@data[1, 1])/2 + 500
Isolines_diff[, 1] <- Isolines_diff[, 1]/1000

# calculate difference in increase of area per km
for (i in 1:length(Isolines_increase[, 1])) {
  Isolines_diff[i,3] <- Isolines_increase[i,3] - Isolines_increase[i+1,3]
}
