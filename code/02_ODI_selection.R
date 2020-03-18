library("magrittr")

# Basics -----------------------------------------------------------------------

# Turn scientific notation off
options(scipen = 999)

# Basic descriptive properties of isolines -------------------------------------

# Initialize data.frame
Isolines_stats <- data.frame(km_isoline = integer(),
                             number_Area = integer(),
                             number_Area_merged = integer(),
                             number_Sites = integer(), 
                             percent_Sites = integer(),
                             Area = integer(),
                             increase_Sites = integer(),
                             diff_Sites = integer(),
                             increase_Area = integer(),
                             diff_Area = integer(),
                             stringsAsFactors = FALSE)

# Counting the numbers of distinct areas with specif site density
for (i in 1:length(isoline_polygons)) {
  Isolines_stats[i,2] <- length(isoline_polygons@polygons[[i]]@Polygons) -
    sum(sapply(isoline_polygons@polygons[[i]]@Polygons,                                                                                 function(x) {sum(isTRUE(x@hole), na.rm = TRUE)}))
}

# EXPERIMENTAL:
# Counting the numbers of distinct areas with specif site density MERGED

number <- data.frame(matrix(
  # nrow is to long, but this doesn't effect the results
  nrow = max(Isolines_stats$number_Area),
  ncol = length(isoline_merged)))

for (i in 1:length(isoline_merged)) {
  
  for (j in 1:length(isoline_merged@polygons[[i]]@Polygons)) {
    # TRUE if area of polygon is larger than cell size
    number[j, i] <- isoline_merged@polygons[[i]]@Polygons[[j]]@area != your_grid_spacing * your_grid_spacing
  }
}  

Isolines_stats$number_Area_merged <- as.vector(apply(number, 2, function(x) sum(x, na.rm = TRUE)))


# Insert name of isolines
Isolines_stats[, 1] <- isoline_polygons@data[, 1]

# Calculate number of sites within within a certain site distance
sites_n <- sapply(sp::over(isoline_polygons, sites, returnList = TRUE), nrow)
Isolines_stats$number_Sites <- cumsum(sites_n)

# Calculate the percentage increase in the nummber of site per isoline
Isolines_stats$percent_Sites <- (Isolines_stats[, 4] * 100) / length(sites)

# Calculate area enclosed by each isoline
iso_area <- raster::area(isoline_polygons)/1000000
Isolines_stats$Area <- cumsum(iso_area)


# Increase of number of sites and area per equidistance ------------------------

# calculate increase in numbers of sites per equidistance
Isolines_stats$increase_Sites <- c(NA, sites_n[-1])

# calculate increase in area of polygon per equidistance
Isolines_stats$increase_Area <- c(NA, iso_area[-1])


# Difference in increase of number of sites and area per equidistance ----------

# Calculate difference of increase of number of sites per equidistance
Isolines_stats$diff_Sites <- c(NA, diff(Isolines_stats[, 7]))

# Calculate difference in increase of area per equidistance
Isolines_stats$diff_Area <- c(NA, diff(Isolines_stats[, 9]))


# Add Isolines_stats to isoline_polygons ---------------------------------------

isoline_polygons@data <- Isolines_stats
