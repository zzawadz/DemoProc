#' @title Class for population pyramid
#' @export
#' 
#' @description Class for population pyramid
#' 
setClass("PopulationPyramid", representation = list(ages = "vector", males = "vector", females = "vector"))


setMethod("as.numeric", signature = list(x = "PopulationPyramid"), function(x) c(x@males, x@females))
setMethod("[", signature = list(x = "PopulationPyramid"), 
          function(x, i) vector2PopulationPyramid(ages = x@ages[i],males = x@males[i],females = x@females[i]))


setMethod("merge", signature = list(x = "PopulationPyramid", y = "PopulationPyramid"),
  function(x, y) 
  {
    ages = c(x@ages,y@ages)
    males = c(x@males,y@males)
    females = c(x@females,y@females)
    vector2PopulationPyramid(ages,males,females)
  })
