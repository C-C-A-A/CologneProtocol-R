library("magrittr")

# Basics -----------------------------------------------------------------------

# Turn scientific notation off
options(scipen = 999)

# Basic descriptive properties of isolines -------------------------------------

# Initialize data.frame
Isolines_stats <- data.frame(km_isoline = integer(),
                             number_Area = integer(), 
                             number_Sites = integer(), 
                             percent_Sites = integer(),
                             Area = integer(),
                             stringsAsFactors = FALSE)

# Counting the numbers of distinct areas per isoline
for (i in 1:length(isoline_polygons)) {
  Isolines_stats[i,2] <- length(isoline_polygons@polygons[[i]]@Polygons) -
    sum(sapply(isoline_polygons@polygons[[i]]@Polygons,                                                                                 function(x) {sum(isTRUE(x@hole), na.rm = TRUE)}))
}

# Insert name of isolines
Isolines_stats[, 1] <- isoline_polygons@data[, 1]

# change unit from [m] to [km]
# Isolines_stats[, 1] <- Isolines_stats[, 1] / 1000

# Calculate number of sites within each isoline
Pt_overlay <- sp::over(sites, isoline_polygons)
Pt_overlay$ID <- rownames(Pt_overlay)
sites@data$id <- rownames(sites@data)
sites@data <- dplyr::left_join(sites@data,
                                    Pt_overlay, by = c("id" = "ID"))

Isolines_stats[1, 3] <- sum(sites@data$z == Isolines_stats[1, 1])

for (i in 2:length(Isolines_stats[, 3])) {
  Isolines_stats[i, 3] <- sum(sites@data$z == Isolines_stats[i, 1]) +
    Isolines_stats[i-1, 3]
}

# Calculate the percentage increase in the nummber of site per isoline
for (i in 1:length(Isolines_stats[,4])) {
  Isolines_stats[i, 4] <- (Isolines_stats[i, 3] * 100) / length(sites)
}

# Calculate area enclosed by each isoline
Isolines_stats[1, 5] <- raster::area(isoline_polygons)[1] / 1000000

for (i in 2:length(Isolines_stats[, 5])) {
  Isolines_stats[i, 5] <- raster::area(isoline_polygons)[i] / 1000000 + 
    Isolines_stats[i-1, 5]
}

# Increase of number of sites and area per km ----------------------------------

# Initialize data.frame
Isolines_increase <- data.frame(km_isoline = integer(),
                                increase_Sites = integer(), 
                                increase_Area = integer(),
                                stringsAsFactors = FALSE)

# calculate increase in numbers of site per km
for (i in 1:length(Isolines_stats[, 1])) {
  Isolines_increase[i, 2] <- (Isolines_stats[i+1, 3] - Isolines_stats[i, 3]) * 2
}

# insert name of isolines
Isolines_increase[, 1] <- isoline_polygons@data[, 1]


# calculate increase in area of polygon per km
for (i in 1:length(Isolines_stats[, 1])) {
  Isolines_increase[i, 3] <- (Isolines_stats[i+1, 5] - Isolines_stats[i, 5]) * 2
}

# Difference in increase of number of sites and area per km --------------------

# Initialize data.frame
Isolines_diff <- data.frame(km_isoline = integer(),
                            diff_Sites = integer(), 
                            diff_Area = integer(),
                            stringsAsFactors = FALSE)

# Calculate difference of increase of number of site per km
for (i in 1:length(Isolines_increase[, 1])) {
  Isolines_diff[i, 2] <- Isolines_increase[i, 2] - Isolines_increase[i+1, 2]
}

# Insert name of isolines
Isolines_diff[, 1] <- isoline_polygons@data[, 1]

# Calculate difference in increase of area per km
for (i in 1:length(Isolines_increase[, 1])) {
  Isolines_diff[i, 3] <- Isolines_increase[i, 3] - Isolines_increase[i+1, 3]
}

# Change units of all data.frames ----------------------------------------------
Isolines_stats[, 1] <- Isolines_stats[, 1] / 1000
Isolines_increase[, 1] <- Isolines_increase[, 1] / 1000
Isolines_diff[, 1] <- Isolines_diff[, 1] / 1000




# Save data --------------------------------------------------------------------

# Isoline_stats
write.table(Isolines_stats,
            "output/Isolines_stats.csv",
            sep = ";",
            dec = ",")

# Isoline_increase
write.table(Isolines_increase,
            "output/Isolines_increase.csv",
            sep = ";",
            dec = ",")

# Isoline_diffrence
write.table(Isolines_diff,
            "output/Isolines_difference.csv",
            sep = ";",
            dec = ",")
