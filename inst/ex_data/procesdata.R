data = read.csv2("inst/ex_data/DP_20150124_184337.csv", encoding = "UTF-8", header = FALSE, stringsAsFactor = FALSE)

data = data[,-1]
data = data %>% as.matrix %>% t

data = data[,-4] # pozbycie sie czegos tam

colnames(data) = data[1,]
data = data[-1,]
colnames(data)[1:3] = c("Age","Sex","Year")

data = as.data.frame(data) 
data = data[-nrow(data),]

data[nrow(data),]
data[1:5,1:5]

head(data)

for(i in 4:ncol(data))
{
  data[[i]] = data[[i]]%>% as.character %>% as.numeric
}

data[[ncol(data)-1]]

data$Age = data$Age %>% as.character

data$Age[grep("85 ",data$Age)] = "85+"

data$Sex = data$Sex %>% as.character
data$Sex[!grepl(pattern = "czy",data$Sex)] = "Female"
data$Sex[grepl(pattern = "czy",data$Sex)] = "Male"


popStructLesserPolandSomeProvinces = data
save(popStructLesserPolandSomeProvinces , file = "data/popStructLesserPolandSomeProvinces.rda")


