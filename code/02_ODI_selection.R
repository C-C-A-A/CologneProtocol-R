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

# Calculate number of sites within each isoline
sites_n <- sapply(sp::over(isoline_polygons, sites, returnList = TRUE), nrow)
Isolines_stats$number_Sites <- cumsum(sites_n)

# Calculate the percentage increase in the nummber of site per isoline
Isolines_stats$percent_Sites <- (Isolines_stats[, 3] * 100) / length(sites)

# Calculate area enclosed by each isoline
iso_area <- raster::area(isoline_polygons)/1000000
Isolines_stats$Area <- cumsum(iso_area)


# Increase of number of sites and area per km ----------------------------------

# Initialize data.frame
Isolines_increase <- data.frame(km_isoline = integer(length(sites_n[-1])), # number of columns needs to be specified
                                increase_Sites = integer(length(sites_n[-1])), 
                                increase_Area = integer(length(sites_n[-1])),
                                stringsAsFactors = FALSE)

equidst_km <- (Isolines_stats$km_isoline[2]-Isolines_stats$km_isoline[1])/1000

# calculate increase in numbers of site per km
Isolines_increase[, 2] <- c(sites_n[-1]) / (equidst_km) # Zimmermann et al. 2004, Tab. 1


# insert name of isolines
Isolines_increase[, 1] <- isoline_polygons@data[-c(1), 1] # '-c(1)' is used to remove name of first isoline

# calculate increase in area of polygon per km
Isolines_increase[, 3] <- c(iso_area[-1]) / (equidst_km) # Zimmermann et al. 2004, Tab. 1

# Difference in increase of number of sites and area per km --------------------

# Initialize data.frame
Isolines_diff <- data.frame(km_isoline = integer(length(sites_n[-1]) - 1), # number of columns needs to be specified
                            diff_Sites = integer(length(sites_n[-1]) - 1), 
                            diff_Area = integer(length(sites_n[-1]) - 1),
                            stringsAsFactors = FALSE)

# Calculate difference of increase of number of site per km
Isolines_diff[, 2] <- diff(Isolines_increase[, 2])

# Insert name of isolines
Isolines_diff[, 1] <- isoline_polygons@data[-c(1, 2), 1]

# Calculate difference in increase of area per km
Isolines_diff[, 3] <- diff(Isolines_increase[, 3])


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
