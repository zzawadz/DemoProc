library(DemoProc)

path = get_path2exData()
data = read_pyramid_data(path)

# powiat tatrzanski
pp = get_pyramid_from_data(data, 13, year = 2009)
plot_pyramid(pp)
title("2009")


burden(pp)
feminisation(pp)

dtAll = get_birth_from_gov(2009)


all = get_death_from_gov(2003)

all

#birthAllList = 2002:2013 %>% lapply(function(x) { print(x); get_birth_from_gov(x)})
#names(birthAllList) = 2002:2013
#save(birthAllList, file = "data/birthAllList.rda")

deathAllList = 2002:2013 %>% lapply(function(x) { print(x); get_death_from_gov(x)})
names(deathAllList) = 2002:2013
save(deathAllList, file = "data/deathAllList.rda")

deathAllListMale = 2002:2013 %>% lapply(function(x) { print(x); get_death_from_gov(x, 2)})
names(deathAllListMale) = 2002:2013
save(deathAllListMale, file = "data/deathAllListMale.rda")

deathAllListFemale = 2002:2013 %>% lapply(function(x) { print(x); get_death_from_gov(x, 3)})
names(deathAllListFemale) = 2002:2013
save(deathAllListFemale, file = "data/deathAllListFemale.rda")



get_struct_pop_for_lesserPoland = get_data_from_gov(
          url = "http://demografia.stat.gov.pl/bazademografia/Downloader.aspx?file=pl_lud_%s_12_05.zip&sys=lud",
          fname = "pl_lud_%s_12_05")

popStructListLesserPoland = 2002:2013 %>% lapply(function(x) { print(x); get_struct_pop_for_lesserPoland(x, 1)})
names(popStructListLesserPoland) = 2002:2013
save(popStructListLesserPoland, file = "data/popStructListLesserPoland.rda")




data = read.csv2("inst/ex_data/DP_20141231_135459.csv", encoding = "UTF-8", header = FALSE, stringsAsFactor = FALSE)
data = data[,-ncol(data)] # jest tam samo na
age = data[1,] %>% as.character 
age = age[-c(1:2)]
age[grep(age,pattern = "85")] = "85+"

sex = data[2,-c(1:2)] %>% as.character

dtclean = data[grep(data[,2], pattern = "Powiat"),]
dtclean

result = NULL
for(i in 1:nrow(dtclean))
{
  tmp = dtclean[i,-(1:2)] %>% as.character %>% as.numeric
  result = rbind(result,data.frame(province = dtclean[i,2],sex = sex, age = age, size = tmp))
}

lesserPolandPopStructDataFrame = result

save(lesserPolandPopStructDataFrame, file = "data/lesserPolandPopStructDataFrame.rda")

