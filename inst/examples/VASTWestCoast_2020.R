
library(VASTWestCoast)
args(VASTWestCoast::VAST_spp)
?VASTWestCoast::VAST_spp

# Below is an example for how to run VAST for sablefish
VAST_spp(dir = getwd(), species = "sablefish")
