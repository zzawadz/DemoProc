#' @title Burden
#' @export
#' 
burden = function(pyramid, production = c("20", "64"), type = "all")
{
  prod = get_production_population(pyramid, production = production) %>% as.numeric %>% sum
  unprod = get_unproductive_population(pyramid, production = production, type = type) %>% as.numeric %>% sum
  unprod/prod  
}


#' @title Get productive population
#' @export
#'
get_productive_population = function(pyramid, production = c("20", "64"))
{
  x1 = grep(pyramid@ages, pattern = production[1])
  x2 = grep(pyramid@ages, pattern = production[2])
  production = x1:x2
  pyramid[production]
}

#' @title Get unproductive population
#' @export
#' 
get_unproductive_population = function(pyramid, production = c("20", "64"), type = "all")
{
  x1 = grep(pyramid@ages, pattern = production[1])
  x2 = grep(pyramid@ages, pattern = production[2])
  
  if(type == "lower") return(pyramid[1:(x1-1)])
  if(type == "upper") return(pyramid[(x2+1):length(pyramid@ages)])
  
  merge(pyramid[1:(x1-1)], pyramid[(x2+1):length(pyramid@ages)])
}

#' @title feminisation
#' @export
#' 
feminisation = function(pyramid, total = TRUE)
{
  if(total) return(sum(pyramid@females)/sum(pyramid@males) * 100)
  
  data.frame(ages = pyramid@ages, feminisation = pyramid@females/pyramid@males * 100)
}
