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
                                                    vgm(nugget = 0,
                                                        model = "Hol",
                                                        range = 3000,
                                                        sill = plateau),
                                                    fit.sills = FALSE,
                                                    fit.ranges = TRUE)

#### kriging ####

# creating a grid for kriging





rm(d1, test)
