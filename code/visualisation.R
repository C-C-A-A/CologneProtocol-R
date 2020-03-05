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

#### variogram / semivariogram ####

# plot sample variogram
plot(Thiessen_vertices_vario, Thiessen_vertices_vario_fit, xlim = c(0,40000), ylim = c(0,50000000))
plot(Thiessen_vertices_vario, Thiessen_vertices_vario_fit)

#### Kriging results ####
LEC_kriged %>%
  as.data.frame() %>%
  ggplot2::ggplot(ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_tile(ggplot2::aes(fill=var1.pred)) +
  ggplot2::coord_equal() +
  ggplot2::scale_fill_gradient2(midpoint = mean(LEC_kriged$var1.pred),
                                low = "red", mid = "yellow", high = "green") +
  ggplot2::geom_point(data = as.data.frame(sites_spdf), ggplot2::aes(x = coords.x1, y = coords.x2)) +
  ggplot2::theme_bw()

#### Variance of kriging results ####
LEC_kriged %>%
  as.data.frame() %>%
  ggplot2::ggplot(ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_tile(ggplot2::aes(fill = var1.var)) +
  ggplot2::coord_equal() +
  ggplot2::scale_fill_gradient(low = "yellow", high = "red") +
  ggplot2::theme_bw()

#### descriptive statistics of isoline ####

# number of distinct areas per isoline
Isolines_stats %>%
  ggplot2::ggplot(ggplot2::aes(x = km_isoline, y = number_Area)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::labs(title = "Number of distinct areas per Isoline",
                x = "Isoline [km]",
                y = "Number of distinct areas") +
  ggplot2::scale_x_continuous(limit = c(0.5,20),
                              breaks = seq(0.5,20,0.5)) +
  ggplot2::theme_bw() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                 axis.text.x = ggplot2::element_text(angle = 90))
  

# number of sites per isoline
Isolines_stats %>%
  ggplot2::ggplot(ggplot2::aes(x = km_isoline, y = number_Sites)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::labs(title = "Number of sites per Isoline",
                x = "Isoline [km]",
                y = "Number of sites") +
  ggplot2::scale_x_continuous(limit = c(0.5,20),
                              breaks = seq(0.5,20,0.5)) +
  ggplot2::theme_bw() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                 axis.text.x = ggplot2::element_text(angle = 90))

# percent of sites per isoline
Isolines_stats %>%
  ggplot2::ggplot(ggplot2::aes(x = km_isoline, y = percent_Sites)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::labs(title = "Percentage of sites per Isoline",
                x = "Isoline [km]",
                y = "Percentage of sites [%]") +
  ggplot2::scale_x_continuous(limit = c(0.5,20),
                              breaks = seq(0.5,20,0.5)) +
  ggplot2::theme_bw() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                 axis.text.x = ggplot2::element_text(angle = 90))

# area per isoline
Isolines_stats %>%
  ggplot2::ggplot(ggplot2::aes(x = km_isoline, y = Area)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::labs(title = "Enclosed area per isoline",
                x = "Isoline [km]",
                y = "Area [km²]") +
  ggplot2::scale_x_continuous(limit = c(0.5,20),
                              breaks = seq(0.5,20,0.5)) +
  ggplot2::theme_bw() +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                 axis.text.x = ggplot2::element_text(angle = 90))
  
  
  