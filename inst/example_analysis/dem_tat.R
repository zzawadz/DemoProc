library(DemoProc)


province = "tatrza"
#data(popStructListLesserPoland)
#fullStruct = lapply(popStructListLesserPoland, extract_pop_struct, province = province)
data(popStructLesserPolandSomeProvinces)
popStructYears = extract_province_from_popStructLesserPolandSomeProvinces(popStructLesserPolandSomeProvinces, province = province)




data(birthAllList) #pobranie listy z urodzeniami dla kazdego roku:
provBirth = lapply(birthAllList, extract_birth_province, province = province)
liveBirth = sapply(provBirth, function(x) x[2,-1])
births = liveBirth[,"2010"] %>% unlist

### na razie dla roku 2010
#structPop = transform_popStruct2ranges(fullStruct[["2010"]], zero = FALSE)
structPop = popStructYears[["2010"]] 
fertelityRate = get_fertility_rate(structPop, births) # wspolczynniki urodzen

popPyramid = rangesStrct2PopulationPyramid(structPop)
popPyramid %>% plot_pyramid

totalFertelityRate = fertelityRate %>% sum %>% "*"(5)
totalFertelityRate

data(deathAllListFemale)
data(deathAllListMale)

# wyciaganie danych dotyczacych smierci dla plci
femaleDeath = sapply(deathAllListFemale, extract_death_province, province = province)
maleDeath   = sapply(deathAllListMale, extract_death_province, province = province)

fmDeath = femaleDeath[,"2010"]
mlDeath = maleDeath[,"2010"]

deathRate = get_death_rate_as_dataFrame(structPop, mlDeath, fmDeath)

bigL = get_BigL(deathRate)

forecast = forecast_population(structPop, fertelityRate, bigL)

forecast %>% rangesStrct2PopulationPyramid %>% plot_pyramid

