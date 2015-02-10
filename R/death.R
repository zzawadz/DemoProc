#' @title plot_death
#' @export
plot_death = function(death, deathForecast)
{
  colors = heat_hcl(nrow(death))
  matplot(death %>% t, type = "l", xlim = c(1, ncol(death)+ ncol(deathForecast)), col = colors, lty = 1, lwd = 2, xaxt = "n")
  
  lapply(1:ncol(deathForecast), function(i) points(rep(i+ncol(death), nrow(deathForecast)), deathForecast[,i],col = colors, pch = 19, cex = 1.3))
  legend("topleft", rownames(death), col = colors, lwd = 2, ncol = 2)
  axis(1,1:(ncol(death)+ ncol(deathForecast)), 2002:2020)  
}

#' @title forecast_death
#' @export
forecast_death = function(death, tol = 0.6)
{
  deathModels = lapply(1:nrow(death), function(i)
  {
    y = death[i,]
    time = 1:length(y)
    hypTime = 1/time
    logTime = log(time)
    
    models = list(
      #fit   = lm(y ~ time) ,
      fit2  = lm(y ~ hypTime),
      fit3  = lm(y ~ logTime))
    
    rsquared = models %>% sapply(function(x) summary(x)$r.squared)
    
    if(all(rsquared < tol)) return(rep(mean(y,na.rm = TRUE), 7))
    
    mod = which.max(rsquared)
    fit = models[[mod]]
    
    type = colnames(fit$model)[2]
    time2 = 13:19
    if(type == "hypTime") time2 = 1/time2
    if(type == "logTime") time2 = log(time2)
    time2 = data.frame(time2)
    colnames(time2) = type  
    predict(fit, time2)
  })

  deathForecast = Reduce(deathModels, f = rbind)
  rownames(deathForecast) = rownames(death)
  colnames(deathForecast) = 2014:2020
  deathForecast
}


#' @title get_death_stats_by_province
#' @export
#' 
get_death_stats_by_province = function(province)
{
  data(popStructLesserPolandSomeProvinces)
  data(deathAllListFemale)
  data(deathAllListMale)
  
  femaleDeath = sapply(deathAllListFemale, extract_death_province, province = province)
  maleDeath   = sapply(deathAllListMale, extract_death_province, province = province)
  popStructYears = extract_province_from_popStructLesserPolandSomeProvinces(popStructLesserPolandSomeProvinces, province = province)
  
  result = list(femaleDeath = femaleDeath, maleDeath = maleDeath, popStructYears = popStructYears)
}
#' @title aa
#' @export
#'
extract_death_rate_merge = function(deathMaleAll, deathFemaleAll, year)
{
  deathRate = cbind(deathMaleAll[,year],deathFemaleAll[,year])
  rownames(deathRate) = rownames(deathMaleAll)
  deathRate = deathRate %>% as.data.frame
  deathRate = cbind(Age = rownames(deathRate), deathRate)
  colnames(deathRate)[2:3] = c("Male", "Female") 
  deathRate
}