#install.packages('soilDB', dependencies = TRUE)
#or
# remotes::install_github("ncss-tech/soilDB", dependencies = FALSE)

library(soilDB)
library(aqp)

g_to_kg <- 0.001

loc <- data.frame(id  = c("A"), lat = c(51.1536), lon = c(-0.8582), 
                  stringsAsFactors = FALSE)
x <- try(fetchSoilGrids(loc))
  
aqp::plotSPC(x, name = NA, color = "socQ50")

y <- try(fetchSoilGrids(loc))
h <- aqp::horizons(y)

# simple mean for layers and two top layers for organic soil
#clay <- mean(h$claymean) / 100.
#silt <- mean(h$siltmean) / 100.
#sand <- mean(h$sandmean) / 100.
#org <- mean(h$socmean[1:2]) * 10 # mean top two layers, g/kg
#org <- org * g_to_kg # kg/kg
#print(c(sand, silt, clay, org))

# calculate weighted depth average instead
thickness <- h$hzdepb - h$hzdept
layer_frac <- thickness / sum(thickness)

clay <- sum(h$claymean * layer_frac) / 100.
silt <- sum(h$siltmean * layer_frac) / 100.
sand <- sum(h$sandmean * layer_frac) / 100.
clay <- sum(h$claymean * layer_frac) / 100.
org <- sum(h$socmean * layer_frac) * 10 # g/kg
org <- org * g_to_kg # kg/kg
print(c(sand, silt, clay, org))

# calculate weighted depth average for texture but use the top
# top two layers for the SOC as it declines massively with depth
#thickness <- h$hzdepb - h$hzdept
#layer_frac <- thickness / sum(thickness)

#clay <- sum(h$claymean * layer_frac) / 100.
#silt <- sum(h$siltmean * layer_frac) / 100.
#sand <- sum(h$sandmean * layer_frac) / 100.
#clay <- sum(h$claymean * layer_frac) / 100.

# Just use top two layers
#thickness <- h$hzdepb[1:2] - h$hzdept[1:2]
#layer_frac <- thickness / sum(thickness)

#org <- sum(h$socmean[1:2] * layer_frac) * 10 # g/kg
#org <- org * g_to_kg # kg/kg
#print(c(sand, silt, clay, org))


# From CABLE organic layer and site info

bch = (1.0-org) * ( 3.1 + 15.7*clay- 0.3*sand ) + org*3.0
hyds = (1.0-org) * 0.00706 * ( 10.0 ** (-0.60 + 1.26*sand - 0.64*clay) ) + org*10**(-4)

print(c(bch, hyds))
