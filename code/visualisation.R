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

#### h-scatterplots ####

#### correlogram ####

#### variogram / semivariogram ####

# plot sample variogram
plot(Thiessen_vertices_vario, Thiessen_vertices_vario_fit, xlim = c(0,40000), ylim = c(0,50000000))
plot(Thiessen_vertices_vario, Thiessen_vertices_vario_fit)


#### Kriging results ####

as.data.frame(LEC_kriged) %>%
  ggplot2::ggplot(aes(x = x, y = y)) +
  ggplot2::geom_tile(aes(fill=var1.pred)) +
  ggplot2::coord_equal() +
  ggplot2::scale_fill_gradient2(midpoint = mean(LEC_kriged$var1.pred),
                                low = "red", mid = "yellow", high = "green") +
  ggplot2::geom_point(data = as.data.frame(sites_spdf), aes(x = coords.x1, y = coords.x2)) +
  ggplot2::theme_bw()
