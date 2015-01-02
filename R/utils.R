#' @title Tranform vectors into PopulationPyramid object.
#' @export
#' 
#' @examples
#'  
#' ages = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34",
#' "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
#' "70-74","75-79","80-84","85+")
#' males = c(1734,1746,2031,2671,2546,2688,2461,2100,2125,2181,2275,1991,1477,998,918,710,419,226)
#' females = c(1636,1689,1922,2403,2604,2594,2361,2257,2122,2261,2350,2295,1809,1358,1515,1325,930,570)
#' pyramid = vector2PopulationPyramid(ages, males, females)
#' 
vector2PopulationPyramid = function(ages, males, females)
{
  new("PopulationPyramid", ages = ages, males = males, females = females)
}


#' @title Get path to example data
#' @export
#' 
get_path2exData = function()
{
  path = .libPaths()
  pkgs = dir(path, full.names = TRUE)
  path = pkgs[grep(pkgs, pattern = "DemoProc")]
  path = file.path(path, "ex_data/str09_13.xlsx")
  path
}

#' @title read pyramid data
#' @export
#' 
#' @examples
#' path = get_path2exData()
#' data = read_pyramid_data(path)
#' 
read_pyramid_data = function(path, sheetIndex = 1, endCol = 200)
{
  struct = read.xlsx(path, sheetIndex = sheetIndex, rowIndex = 2:182, colIndex = 1:3)
  sex = struct[[1]]
  whs = which(!is.na(sex))
  
  sex[whs[1]:(whs[2]-1)] = sex[whs[1]]
  sex[whs[2]:length(sex)] = sex[whs[2]]
  
  years = struct[[2]]
  ywh = which(!is.na(years))
  ywh = c(ywh, length(years)+1)
  
  for(i in 1:(length(ywh)-1))
  {
    years[ywh[i]:(ywh[i+1]-1)] = years[ywh[i]]
  }
  
  ages = struct[[3]]
  
  data = read.xlsx(path, sheetIndex = sheetIndex, rowIndex = 2:182, colIndex = 4:endCol)
  data = cbind(years, sex, ages, data)
  data 
}

#' @title Get pyramid data from dataframe
#' @export
#' 
get_pyramid_from_data = function(data, col = 1, year = 2009)
{
  data = data %>% group_by(years, sex) %>% do(result = (function(x) x[,-(1:2)])(.))
  result = data %>% filter(years == year)
  
  females = result %>% filter(sex == "Females") %>% "$"("result") %>% "[["(1) %>% "[["(col+1)
  males = result %>% filter(sex != "Females") %>% "$"("result") %>% "[["(1) %>% "[["(col+1)
  ages = result %>% filter(sex != "Females") %>% "$"("result") %>% "[["(1) %>% "[["(1)
  
  vector2PopulationPyramid(ages, males, females)
}
