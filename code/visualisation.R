library("magrittr")

#### basics #####

# turn scientific notation off
options(scipen = 999)

#### plot of archaeological sites and Thiessen polygons ####

plot(sites_spdf@coords[,1], sites_spdf@coords[,2],
     type = "n",
     asp = 1,
     main = "Sites and corresponding Thiessen polygons",
     xlab = "[EPSG 25832]",
     ylab = NA)
points(sites_spdf, pch = 20, col = "red")
points(Thiessen_vertices_spdf, pch = 20, col = "black")
plot(deldir::deldir(sites_spdf@coords[,1], sites_spdf@coords[,2]),
     wlines = "tess",
     wpoints = "none",
     number = FALSE,
     lty = 1,
     add = TRUE)
legend(x = 500000, y = 5650000,
       legend = c("Archaeological sites", "Thiessen vertices", "Thiessen polygons"),
       pch = c(20, 20, NA),
       lty = c(NA, NA, 1),
       col = c("red", "black", "black"),
       merge = TRUE,
       bty = "n",
       bg = "transparent")
