#' get birth from internet
#'@export
get_birth_from_gov = function(year = 2010)
{


  url = "http://demografia.stat.gov.pl/bazademografia/Downloader.aspx?file=pl_uro_%s_00_1p.zip&sys=uro"
  url = sprintf(url, year)

  file = fnames[grep(fnames, pattern = sprintf("pl_uro_%s_00_1p", year))[1]]
  if(is.na(file))
  {
    browseURL(url)
    download_path = "~/../Downloads/"
    fnames = dir(download_path, full.names = TRUE)
  }
  
  file = fnames[grep(fnames, pattern = sprintf("pl_uro_%s_00_1p", year))[1]]
  extr = gsub(file, pattern = "\\.zip", replacement = "")
  unzip(file, exdir = extr)
  path = dir(extr, full.names = TRUE)[1]

  dtAll = read.xlsx(path, sheetIndex = 1, encoding = "UTF-8")
  dtAll
}

#' @title extract province
#' @export
extract_province = function(dtAll, province = "tatrza")
{
  dtNames = dtAll[[1]] %>% as.character
  pos = grep(dtNames, pattern = province)
  
  names = dtAll[6,]
  names = names[,3:ncol(names)] %>% unlist %>% as.character
  
  dt = dtAll[pos:(pos+8),]
  colnames(dt) = c("type","Total", names)
  
  for(i in 3:ncol(dt))
  {
    dt[,i] = dt[,i] %>% as.character %>% as.numeric
    dt[dt[,i] %>% is.na,i] = 0
  }
  dt
}


extract_province(dtAll)


