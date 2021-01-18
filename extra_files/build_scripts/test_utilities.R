library(leaflet)
library(shiny)
library(shinyFiles)
library(raster)
library(rgdal)
library(rgeos)
library(maptools)
library(magrittr)
library(MASS)
library(sp)
library(devtools)

lintr::lint_package()

devtools::test()
devtools::check()

roads_path <- system.file("extdata/roads.shp", package = "roadHotspots")
count_path <- system.file("extdata/count.csv", package = "roadHotspots")

output <- road_kernel(count_path, roads_path)

# library(CodeDepends) # nao percebo muito bem como isto funciona
# gg = makeCallGraph("package:roadHotspots")
# if(require(Rgraphviz)) {
#   gg = layoutGraph(gg, layoutType = "circo")
#   graph.par(list(nodes = list(fontsize=55)))
#   renderGraph(gg) ## could also call plot directly
# } 

library(DependenciesGraphs)
launch.app()
