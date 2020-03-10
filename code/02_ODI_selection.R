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
sites_n <- sapply(sp::over(isoline_polygons,sites,returnList = TRUE), nrow)
Isolines_stats$number_Sites <- cumsum(sites_n)

# Calculate the percentage increase in the nummber of site per isoline
Isolines_stats$percent_Sites <- (Isolines_stats[, 3] * 100) / length(sites)


# Calculate area enclosed by each isoline
iso_area <- raster::area(isoline_polygons)/1000000
Isolines_stats$Area <- cumsum(iso_area)



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

# Why "* 2"?
increase_sites_alt <- c(sites_n[-1],NA) *2
names(increase_sites_alt) <- NULL

all.equal(Isolines_increase$increase_Sites, increase_sites_alt)


# insert name of isolines
Isolines_increase[, 1] <- isoline_polygons@data[, 1]


# calculate increase in area of polygon per km
for (i in 1:length(Isolines_stats[, 1])) {
  Isolines_increase[i, 3] <- (Isolines_stats[i+1, 5] - Isolines_stats[i, 5]) * 2
}

# Why 2? because of "per km"?
increase_area_alt <- c(iso_area[-1],NA) *2
names(increase_area_alt) <- NULL

all.equal(Isolines_increase$increase_Area, increase_area_alt)



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

# Alternativ without for-Loop, Make these absolute numbers with abs()?
diff_Sites_alt <- c(diff(Isolines_increase$increase_Sites)*-1,NA)
all.equal(Isolines_diff$diff_Sites, diff_Sites_alt)

# Insert name of isolines
Isolines_diff[, 1] <- isoline_polygons@data[, 1]

# Calculate difference in increase of area per km
for (i in 1:length(Isolines_increase[, 1])) {
  Isolines_diff[i, 3] <- Isolines_increase[i, 3] - Isolines_increase[i+1, 3]
}

# Alternativ without for-Loop, Make these absolute numbers with abs()?
diff_Area_alt <- c(diff(Isolines_increase$increase_Area)*-1,NA)
all.equal(Isolines_diff$diff_Area, diff_Area_alt)


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
