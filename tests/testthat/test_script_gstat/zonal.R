# This script ("zonal"-demo) was taken from the gstat-package version 1.1-4 for testing purposes
# https://CRAN.R-project.org/package=gstat

library(sp)
demo(meuse, ask = FALSE, echo = FALSE)
library(gstat)
v = variogram(log(zinc) ~ 1, meuse, alpha = c(0, 45, 90, 135))
vm = vgm(.25, "Sph", 1000, anis = c(45, 0.5))
plot(v, vm, main = "geometric")
zonal = vgm(.5, "Sph", 1e9, anis = c(45, 1 / 1e6))
# range is 1e9, effectively infinity, in 45 direction;
# it is 1e9/1e6 = 1000 in 135 direction.
vm = vgm(.25, "Sph", 1000, add.to = zonal)
plot(v, vm, main = "zonal")
