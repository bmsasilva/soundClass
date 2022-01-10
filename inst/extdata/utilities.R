devtools::install_github("datastorm-open/DependenciesGraphs")
library(DependenciesGraphs)

library(soundClass)
deps <- funDependencies("package:soundClass", "spectro_calls")
plot(deps)

deps <- funDependencies("package:soundClass", "peaks2spec")
plot(deps)


###colors
#gray.colors(255, start = 0.1, end = 0.8, gamma = 0.1),