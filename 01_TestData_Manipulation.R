# Test data manipulation

## Load objects

setwd(dirRdata)
load(file="TestData.Rdata")

setwd(MainWD)

testset <- data.frame(id=paste(envipe[,1], envipe[,2], envipe[,3], envipe[,5], sep=""))

testset$extorsiones <- as.numeric(as.character(envipe$AP7_4_09))
summary(testset$extorsiones)

testset$secuestros <- as.numeric(as.character(envipe$AP7_4_12))
summary(testset$secuestros)

testset$robos <- as.numeric(as.character(envipe$AP7_4_05))
summary(testset)

testset$vivienda <- envipe$VIV_SEL
testset$control <- envipe$CONTROL
testset$hogar <- envipe$HOGAR

length(testset$id)
length(envipe.viv$ENT)

ents <- envipe.viv[,c("CONTROL", "VIV_SEL", "ENT")]

testset <- merge(testset, ents, by.x=c("control", "vivienda"), by.y=c("CONTROL", "VIV_SEL"))

testset$ENT <- as.integer(testset$ENT)

testset <- testset[,1:8]

testset <- merge(testset, cat_entidades, by.x="ENT", by.y="CVE_ENT", all.x=T)

testset$ENT <- as.factor(testset$ENT)

summary(testset)

homicidios <- read.csv("ENVIPE//homicidios_tasas_2013.csv", header=T)

homicidios <- homicidios[,-1]

testset <- merge(testset, homicidios, by.x="ENT", by.y="CVE_ENT", all.x=T)
