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

# Number of years = envipe.viv$AP2_1
# Sector = envipe.viv$AP1_1
# Size = envipe.viv$AP1_2
extra <- envipe.viv[,c("CONTROL", "VIV_SEL", "AP2_1", "AP1_1", "AP1_2")]

testset <- merge(testset, extra, by.x=c("control", "vivienda"), 
                 by.y=c("CONTROL", "VIV_SEL"), all.x=T)

colnames(testset)[14:16] <- c("personas", "clas_viv", "barrera")

colnames(testset)

str(testset)

testset$ENT <- as.factor(testset$ENT)

testset$personas <- as.integer(testset$personas)

levels(testset$clas_viv) <- c("Casa", "Departamento", "Vecindad", "Azotea", "Local")

levels(testset$clas_viv)

summary(testset$clas_viv)

levels(testset$barrera) <- c("Existe", "No existe", "No se observó")

summary(testset$barrera)


summary(testset)

## Change NAs in count fields to zero

testset$extorsiones[is.na(testset$extorsiones)] <- 0
testset$robos[is.na(testset$robos)] <- 0
testset$secuestros[is.na(testset$secuestros)] <- 0



## Save objects for use latter
setwd(dirRdata)
save(list=ls(all=TRUE), file="TestData.Rdata")

setwd(MainWD)
