#' @title Get fertelity rate based on population structure and birth vector
#' @export
get_fertility_rate = function(structPop, births)
{
  fmNumber = structPop[["Female"]]
  age = structPop[["Age"]]
  
  births = births[-1] # usuwanie total
  nmBirths = names(births)
  
  start = grep(age, pattern = nmBirths[1])
  end = start+length(nmBirths)-1 
  fmProd = fmNumber[start:end]
  names(fmProd) = age[start:end]
  births/fmProd
}

#' @title Get death rate based on population structure and death vector
#' @export
death_rate = function(structPop, death, type = "Female")
{
  death = death[-1] # usuwanie total
  maxAge = structPop[,"Age"] %>% tail(1) %>% substr(1,2)
  start = death %>% names %>% grep(pattern = maxAge)
  
  death2 = death[1:nrow(structPop)]
  names(death2) = structPop[,"Age"]
  death2[length(death2)] = sum(death[start:length(death)])
  
  death2/structPop[,type]
}

#' @title Get death rate as dataFrame
#' @export
get_death_rate_as_dataFrame = function(structPop, mlDeath, fmDeath)
{
  femaleDeathRate = death_rate(structPop, fmDeath, type = "Female")
  maleDeathRate   = death_rate(structPop, mlDeath, type = "Male")
  data.frame(Age = structPop[,"Age"], Male = maleDeathRate, Female = femaleDeathRate)
}

#' @title Get death rate as dataFrame
#' @export
get_A = function(deathRate, zeroYear = FALSE)
{
  drate = deathRate["Male"]
  maleA = rep(0.5, nrow(drate))
  
  drate = deathRate["Female"]
  femaleA = rep(0.5, nrow(drate))
  
  if(zeroYear)
  {
    maleA[1] = 0.05 + 3*drate[1,1]
    maleA[2] = (1.653 - 3.013*drate[1,1])*0.25
    
    femaleA[1] = 0.047 + 2.875*drate[1,1]
    femaleA[2] = (1.524 - 1.625*drate[1,1])*0.25
  }
  
  deathA = deathRate
  deathA$Male = maleA
  deathA$Female = femaleA
  deathA
}

#' @title Get death rate as dataFrame
#' @export
extract_births_fromLiveBirth = function(liveBirth, year = "2010")
{
  x = liveBirth[,year]
  x %>% unlist
}

#' @title Get death rate as dataFrame
#' @export
get_death_prob = function(deathRate, h = 5, zeroYear = FALSE)
{
  death = deathRate[,2:3] %>% as.matrix
  deathA = get_A(deathRate)[,2:3] %>% as.matrix
  
  deathProb = h*death/(1 + (1-deathA)*h*death)
  deathProb[nrow(deathProb),] = 1
  
  if(zeroYear)
  {
    h = min(c(h,4))
    deathProb[2,] = h*death[2,]/(1 + (1-deathA[2,])*h*death[2,])
    h = min(c(h,1))
    deathProb[1,] = h*death[1,]/(1 + (1-deathA[1,])*h*death[1,])
  }
  deathProb   
}

#' @title Get death rate as dataFrame
#' @export
get_BigL = function(deathRate, zero = FALSE)
{
  a = get_A(deathRate)[,2:3] %>% as.matrix
  q = get_death_prob(deathRate, h = 5)
  h = rep(5, nrow(q)); 
  if(zero) h[1:2] = c(1,4); # gdyby zerolatki zostaly wlaczone
  h = cbind(h,h)
  mul = h*(1-(1-a)*q)
  
  l = matrix(1e5, ncol = 2, nrow = nrow(q))
  for(i in 2:nrow(l)) l[i,] = l[i-1,] * (1-q[i-1,])
  
  
  bigL = l*mul
  
  bigL[nrow(bigL),] = (l[nrow(bigL),]/deathRate[nrow(bigL),2:3]) %>% unlist
  bigL
  rownames(bigL) =  deathRate$Age
  colnames(bigL) = colnames(deathRate)[-1]
  bigL
}

#' @title Title
#' @export
get_SmallL = function(deathRate, zero = FALSE)
{
  a = get_A(deathRate)[,2:3] %>% as.matrix
  q = get_death_prob(deathRate, h = 5)
  h = rep(5, nrow(q)); 
  if(zero) h[1:2] = c(1,4); # gdyby zerolatki zostaly wlaczone
  h = cbind(h,h)
  mul = h*(1-(1-a)*q)
  
  l = matrix(1e5, ncol = 2, nrow = nrow(q))
  for(i in 2:nrow(l)) l[i,] = l[i-1,] * (1-q[i-1,])
  l
}


#' @title get ageing coeff
#' @export
get_ageing_coeff = function(bigL)
{
  apply(bigL,2, function(x){
    xx = x[-1]/x[-length(x)]
    xx[length(xx)] = tail(x,1)/sum(tail(x,2))
    names(xx) = head(names(x),-1)
    xx
  })
}

#' pierwszy krok forecastingu
#' @export
forecast_population_step1 = function(structPop, bigL)
{
  stp = structPop[,2:3]
  nk = nrow(stp)
  stp[nk-1,] = colSums(stp[(nk-1):nk,])
  stp = stp[-nk,]
  stp = stp * get_ageing_coeff(bigL)
  
  stp # postarzona populacja bez dzieci nowo urodzonych
  stp = cbind(Age = structPop$Age[-1], stp)
  stp
}

#' @title forecast step 2
#' @export
forecast_population_step2 = function(structPop, stp, fertelityRate, maleProp = 0.512820674114243)
{
  names(fertelityRate) = gsub(names(fertelityRate), pattern = "\\+", replacement = "-")
  femaleNext = stp$Female[sapply(names(fertelityRate),grep, x = stp$Age)]
  femalePrev = structPop$Female[sapply(names(fertelityRate),grep, x = structPop$Age)]
  femaleMean = (femaleNext+femalePrev)/2
  
  totalNewborn = sum(5*femaleMean*fertelityRate)
  
  stp$Age = stp$Age%>%as.character
  stp = rbind(stp[1,],stp)
  stp[1,1] = "0-5"
  stp[1,2] = totalNewborn*maleProp
  stp[1,3] = totalNewborn*(1-maleProp)
  
  stp
}

#' @title forecast
#' @export
forecast_population = function(structPop, fertelityRate, bigL, maleProp = 0.512820674114243)
{
  stp = forecast_population_step1(structPop, bigL)
  forecast_population_step2(structPop, stp, fertelityRate, maleProp)
}
