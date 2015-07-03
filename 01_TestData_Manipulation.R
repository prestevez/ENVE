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

testset <- merge(testset, ents, by.x=c("control", "vivienda"), 
                 by.y=c("CONTROL", "VIV_SEL"))

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

str(testset)


b_test <- testset[,c("extorsiones", "robos", "NOM_ABR", "ENT",
                     "tasahom", "personas", "clas_viv", "barrera")]


# str(b_test)

colnames(b_test) <- c("extortions", "bribes", "state", "state_code",
                      "state_hom", "years", "sector", "size")

levels(b_test$sector) <- c("retail", "services", "manufacturing", "office", "other")
levels(b_test$size) <- c("small", "medium", "large")

## Create a new b_test with better distributed covariates

# b_test_backup <- b_test
# b_test <- b_test_backup

# years.b <- rpois(length(b_test$extortions), 5)
# years.b[which(years.b == 0)] <- 1
# 
# sector.b <- sample(1:5, length(b_test$extortions), replace=TRUE)
# size.b <- sample(1:3, length(b_test$extortions), replace=TRUE)
# 
# b_test$years <- years.b
# b_test$sector <- as.factor(sector.b)
# levels(b_test$sector) <- c("retail", "services", "manufacturing", "offices", "other")
# b_test$size <- as.factor(size.b)
# levels(b_test$size) <- c("small", "medium", "large")

save(b_test, file="Rdata/modeldata.Rdata")

## create a subset of b_test for test

b_test_sample <- b_test[sample(nrow(b_test),10000),]

table(b_test_sample$extortions)

save(b_test_sample, file="Rdata/modeldata.Rdata")
write.dta(b_test_sample, file = "b_test_sample.dta")

b_test <- b_test_backup

## Save objects for use latter
setwd(dirRdata)
save(list=ls(all=TRUE), file="b_test.Rdata")
save(list=ls(all=TRUE), file="TestData.Rdata")

setwd(MainWD)
