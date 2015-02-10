provincesDeathData = list(
tatrz = get_death_stats_by_province("tatrz"),
targ = get_death_stats_by_province("nowotarski"),
nowosadecki = get_death_stats_by_province("nowosądecki"),
limanowski = get_death_stats_by_province("limanowski")
)

femaleDeath = lapply(provincesDeathData, "[[",1) %>% Reduce(f = "+")
maleDeath   = lapply(provincesDeathData, "[[",2) %>% Reduce(f = "+")

popStructYears = provincesDeathData[[1]]$popStructYears
for(i in seq_along(provincesDeathData)[-1])
{
  for(j in seq_along(popStructYears))
      popStructYears[[j]][,2:3] = (popStructYears[[j]][,2:3]) + (provincesDeathData[[i]]$popStructYears[[j]][,2:3])
}

deathList = list()
i = 1
deathByYearMale = NULL
deathByYearFemale = NULL
for(year in colnames(femaleDeath))
{
  fmDeath = femaleDeath[,year]
  mlDeath = maleDeath[,year]
  structPop = popStructYears[[year]]
  deathRate = get_death_rate_as_dataFrame(structPop, mlDeath, fmDeath)
  deathList[[i]] = deathRate
  
  deathByYearMale = cbind(deathByYearMale,deathRate$Male)  
  deathByYearFemale = cbind(deathByYearFemale,deathRate$Female)  
  
  i = i+1
}

rownames(deathByYearFemale) = rownames(femaleDeath)[-1]
colnames(deathByYearFemale) = colnames(femaleDeath)
rownames(deathByYearMale) = rownames(maleDeath)[-1]
colnames(deathByYearMale) = colnames(maleDeath)


deathFemaleForecast = forecast_death(deathByYearFemale)
deathMaleForecast = forecast_death(deathByYearMale)

deathFemaleAll = cbind(deathByYearFemale, deathFemaleForecast)
deathMaleAll = cbind(deathByYearMale, deathMaleForecast)
#plot_death(death, deathForecast)


