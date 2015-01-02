library(DemoProc)

data(birthAllList)

province = "tatrza"

#pobranie listy z urodzeniami dla kazdego roku:
prov_birth = lapply(birthAllList, extract_birth_province, province = province)

# wyciagniecie danych dotyczacych zywych urodzen:
live_birth = sapply(prov_birth, function(x) x[2,-1])

data(deathAllListFemale)
data(deathAllListMale)

# wyciaganie danych dotyczacych smierci dla plci
femaleDeath = sapply(deathAllListFemale, extract_death_province, province = province)
maleDeath   = sapply(deathAllListMale, extract_death_province, province = province)

femaleDeath
maleDeath

totalDeath = femaleDeath + maleDeath
totalDeath

# dane dla struktury ludnosci dla malopolski
# takie zostaly dodane do pakietu
# z nich wybierane sa dane dla powiatu tatrzanskiego
data(popStructListLesserPoland)

fullStruct = lapply(popStructListLesserPoland, extract_pop_struct, province = province)
fullStruct[["2010"]] # forma dla przykladowego roku
transform_popStruct2ranges(fullStruct[["2010"]]) # zero latki jako osobna grupa
transform_popStruct2ranges(fullStruct[["2010"]], zero = FALSE) # zerolatki wlaczane do przedzialu od 0-4

# transformacja dla kazdego roku
# czyli transormacja calej listy fullStruct
shortStruct = lapply(fullStruct, transform_popStruct2ranges, zero = FALSE)
shortStruct[["2010"]]

# piramida wieku - jest to specjalny obiekt na ktorym mozna sobie ladnie dzialac:
sh2010 = shortStruct[["2010"]]
popPyr = rangesStrct2PopulationPyramid(sh2010) # tutaj jest transformacja do tego obiektu

plot_pyramid(popPyr)

burden(popPyr) #ogolne
burden(popPyr, type = "lower") # mlodszymi <20 lat
burden(popPyr, type = "upper") # mlodszymi 65+ lat


feminisation(popPyr,total = TRUE)
feminisation(popPyr,total = FALSE)

# przerobienie piramid dla kazdego roku
allPyr = lapply(shortStruct, rangesStrct2PopulationPyramid)
sapply(allPyr, feminisation)
sapply(allPyr, burden)
sapply(allPyr, burden, type = "lower")
sapply(allPyr, burden, type = "upper")

sapply(allPyr, plot_pyramid, xlim = c(-4000,4000))

nyears = length(allPyr)
library(colorspace)
colors = heat_hcl(nyears)
dt = sapply(allPyr, function(x) x@females) 
matplot(dt, type = "l", lty = 1, lwd = 1, col = colors, xaxt = "n")
legend("topright", names(allPyr), col = colors, lwd = 2, ncol = 3)
ages = allPyr[[1]]@ages
axis(1, at = 1:length(ages), ages)


