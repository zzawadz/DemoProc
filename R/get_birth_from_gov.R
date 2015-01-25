#' @title create function for downloading data from gov
#' @export
#' 
get_data_from_gov = function(url, fname)
{
  force(url)
  force(fname)
  function(year = 2010, shIdx = 1)
  {
    
    url = sprintf(url, year)
    download_path = "~/../Downloads/"
    fnames = dir(download_path, full.names = TRUE)
    file = fnames[grep(fnames, pattern = sprintf(fname, year))[1]]
    if(is.na(file))
    {
      browseURL(url)
      print(file)
      i = 1
      while(is.na(file))
      {
        Sys.sleep(0.1)
        i = i + 1
        print(i)
        fnames = dir(download_path, full.names = TRUE)
        file = fnames[grep(fnames, pattern = sprintf(paste0(fname,"\\.zip"), year))[1]]
      }
      extr = gsub(file, pattern = "\\.zip", replacement = "")
      dir.create(extr)
      unzip(file, exdir = extr)
    }
    
    extr = gsub(file, pattern = "\\.zip", replacement = "")
    path = dir(extr, full.names = TRUE)[1]
  
    dtAll = read.xlsx(path, sheetIndex = shIdx, encoding = "UTF-8")
    dtAll
  }
}

#' get birth from internet
#'@export
#'
get_birth_from_gov = get_data_from_gov(
  url = "http://demografia.stat.gov.pl/bazademografia/Downloader.aspx?file=pl_uro_%s_00_1p.zip&sys=uro",
  fname = "pl_uro_%s_00_1p")

#' get death from internet
#'@export
#'
get_death_from_gov = get_data_from_gov(
  url = "http://demografia.stat.gov.pl/bazademografia/Downloader.aspx?file=pl_zgo_%s_00_59A.zip&sys=zgo",
  fname = "pl_zgo_%s_00_59A")



#' @title extract province
#' @export
extract_birth_province = function(dtAll, province = "tatrza")
{
  dtNames = dtAll[[1]] %>% as.character
  pos = grep(dtNames, pattern = province)
  
  names = dtAll[6,]
  names = names[,3:ncol(names)] %>% unlist %>% as.character
  
  dt = dtAll[pos:(pos+8),]
  
  names = gsub(names, pattern = " ", replacement = "")
  names[1] = "-14"
  names[length(names)] = "55+"
  
  colnames(dt) = c("type","Total", names)
  
  for(i in 2:ncol(dt))
  {
    dt[,i] = dt[,i] %>% as.character %>% as.numeric
    dt[dt[,i] %>% is.na,i] = 0
  }
  dt
}


#' @title extract death
#' @export
#' 
extract_death_province = function(dtAll, province = "tatrza", zero = FALSE)
{
  dtNames = dtAll[[1]] %>% as.character
  pos = grep(dtNames, pattern = province)
  
  dt = dtAll[pos,-1] %>% unlist %>% as.character %>% as.numeric
  dt[is.na(dt)] = 0
  
  names = dtAll[5,] %>% unlist
  names = gsub(names, pattern = " ", replacement = "")[-1]
  names[c(1,2,20)] = c("Total","0","85+")
  names(dt) = names
  
  #dt = femaleDeath
  if(!zero)
  {
    names2 = names(dt)[-2]
    names2[2] = "0-4"
    dt[3] = dt[3] + dt[2]
    dt = dt[-2]
    names(dt) = names2
  }
  
  dt
}

#' @title extract pop struct
#' @export
#' 
extract_pop_struct = function(dtAll, province = "tatrza")
{
  dtNames = dtAll[[1]] %>% as.character
  pos = grep(dtNames, pattern = province)
  
  dt = dtAll[pos:(pos + 100),]
  
  dt = dt[!is.na(dt[,1]),]
  dt[,1] = as.character(dt[,1])
  dt[,1] = gsub(dt[,1], pattern = " ", replacement = "")
  dt[c(1,nrow(dt)),1] = c("Total","70+")
  dt = dt[,c(1,3:4)]
  dt[,2] = dt[,2] %>% as.character %>% as.numeric
  dt[,3] = dt[,3] %>% as.character %>% as.numeric
  
  
  colnames(dt) = c("Age","Male","Female") 
  dt  
}

#' @title transofrm
#' @export
#' 
transform_popStruct2ranges = function(dt, zero = TRUE)
{
  result = dt[grep(dt[,1],pattern = "[-+]"),]
  if(!zero) return(result)
  
  result = rbind(dt[3,],result)
  result[2,2:3] = dt[4:7,-1] %>% colSums
  result[2,1] = "1-4"
  result
}

#' @title transform 2 poppyr
#' @export
#' 
rangesStrct2PopulationPyramid = function(dt)
{
  vector2PopulationPyramid(ages  = dt[,1],males = dt[,2],females = dt[,3])
}
