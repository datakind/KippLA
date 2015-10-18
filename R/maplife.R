#MAP scores analysis

#Code to merge the sbac scores with 2015 nwea scores and model the relationship.
#There is a medium correlation between the scores (r-squared 0.5682 for ela/reading, .6546 for math).
#I plot the data and predicton intervals for individuals' scores

#1. Get the SBAC data ####
sbac <- read.csv("~/Dropbox/Teradata DataDive 2015/KIPP LA/SBAC.csv")
dim(sbac) #2301  ,  3

#2. Combine with correct year NWEA DATA ####

nwea15 <- read.csv("~/Dropbox/Teradata DataDive 2015/KIPP LA/NWEA-2015.csv")
match1 = match(nwea15$student_id, sbac$student_id)
nwea15sub <- nwea15[, c(2,3,6,15)]

nwea14<- read.csv("~/Dropbox/Teradata DataDive 2015/KIPP LA/NWEA-2014.csv")
match1 = match(nwea14$student_id, sbac$student_id)
nwea14sub <- nwea14[, c(2,3,6,15)]
sum(!is.na(match(nwea14$student_id, sbac$student_id))

# 2.1 give each student single row with their scores across disciplines/times ####

#reformat NAs
nwea15sub$student_id[nwea15sub$student_id == "\\N"] = NA
nwea15sub$nwea_2015_TestPercentile[nwea15sub$nwea_2015_TestPercentile == "\\N"] = NA
#factors -> numeric:
nwea15sub$student_id = as.numeric(levels(nwea15sub$student_id))[nwea15sub$student_id]
nwea15sub$nwea_2015_TestPercentile = as.numeric(levels(nwea15sub$nwea_2015_TestPercentile))[nwea15sub$nwea_2015_TestPercentile]
nwea15subcast <- dcast(nwea15sub, student_id ~ nwea_2015_Discipline+nwea_2015_TermName, 
      fun.aggregate = max)
dim(nwea15subcast)
nwea15subcast[sapply(nwea15subcast, is.infinite)] = NA
head(nwea15subcast)
#remove strange second column of NA
nwea15subcast = nwea15subcast[,c(1,3:8)]
head(nwea15subcast)
dim(nwea15subcast) #4087    7

# 2.2 Add any sbac scores to the nwea table that we can match by student id -> nwea15sbac data frame####

nwea15sbac = cbind(nwea15subcast, data.frame(matrix(NA,dim(nwea15subcast)[1],2)))
dim(nwea15sbac) ##4087, 40
match1 = match(nwea15sbac$student_id, sbac$student_id)
nwea15sbac[!is.na(match1),8:9] = sbac[match1[!is.na(match1)],c(2,3)]
names(nwea15sbac)[8:9] = names(sbac)[2:3]
head(nwea15sbac)
dim(nwea15sbac)
#write.csv(nwea15sbac, "nwea15sbac.csv")

#3. Analyze, correlation sbac15 and nwea15? ####

#> names(nwea15sbac)
#[1] "student_id"                   "Mathematics_Fall 2014-2015"   "Mathematics_Spring 2014-2015"
#[4] "Mathematics_Winter 2014-2015" "Reading_Fall 2014-2015"       "Reading_Spring 2014-2015"    
#[7] "Reading_Winter 2014-2015"     "Math"                         "ELA"  

#Math plots
plot(nwea15sbac$"Mathematics_Fall 2014-2015", nwea15sbac$"Math")
plot(nwea15sbac$"Mathematics_Spring 2014-2015", nwea15sbac$"Math")
plot(nwea15sbac$"Mathematics_Winter 2014-2015", nwea15sbac$"Math")
plot(rowMeans(nwea15sbac[,c(2:4)],na.rm=T), nwea15sbac$"Math", main = "Math")

#Math model
model.math15 = lm(nwea15sbac$"Math"~rowMeans(nwea15sbac[,c(2:4)],na.rm=T))
summary(model.math15)
abline(model.math15$coefficients, col="red")

#Reading plots
plot(nwea15sbac$"Reading_Fall 2014-2015", nwea15sbac$"ELA")
plot(nwea15sbac$"Reading_Spring 2014-2015", nwea15sbac$"ELA")
plot(nwea15sbac$"Reading_Winter 2014-2015", nwea15sbac$"ELA")

plot(rowMeans(nwea15sbac[,c(5:7)],na.rm=T), nwea15sbac$"ELA", main = "Reading/ELA")
 #more curved for y on within-class percentile scale:?
 #plot(rowMeans(nwea15sbac[,c(5:7)],na.rm=T), 100*rank(nwea15sbac$"ELA", na.last="keep")/max(rank(nwea15sbac$"ELA", na.last="keep"),na.rm=T))
model.ela15 = lm(nwea15sbac$"ELA"~rowMeans(nwea15sbac[,c(5:7)],na.rm=T))
summary(model.ela15)
abline(model.ela15$coefficients, col="red")

# prediction plot ####
# shows that although there is a definite correlation the prediction isn't precise
par(mfrow = c(2,1))

#ela
plot(rowMeans(nwea15sbac[,c(5:7)],na.rm=T), nwea15sbac$"ELA", 
     main = "Reading Data/Trend/Prediction Interval (.95)", 
     xlab = "Avg. NWEA 15 Reading",
     ylab = "SBAC ELA", cex = .8)
abline(model.ela15$coefficients, col="red")
predict.ela15 <- predict(model.ela15, newdata = data.frame(x = 1:100), interval = "prediction")
matpoints(rowMeans(nwea15sbac[,c(5:7)]), predict.ela15, col = c(2,3,4),
        lty = c(1,2,2), type = "l", ylab = "predicted y")

#math
plot(rowMeans(nwea15sbac[,c(5:7)],na.rm=T), nwea15sbac$"Math", 
     main = "Math Data/Trend/Prediction Interval (.95)",
     xlab = "Avg. NWEA 15 Math",
     ylab = "SBAC Math", cex = .8)
abline(model.math15$coefficients, col="red")
predict.math15 <- predict(model.math15, newdata = data.frame(x = 1:100), interval = "prediction")
matpoints(rowMeans(nwea15sbac[,c(2:4)]), predict.math15, col = c(2,3,4),
          lty = c(1,2,2), type = "l", ylab = "predicted y")

# #x. Combine with kippdata IGNORE - not enough data ####
# 
# #get kippdata
# kippdata <- read.csv("~/Dropbox/Teradata DataDive 2015/KIPP LA/Clean Data/Objective # 1/kippdata.csv")
# dim(kippdata) #4438, 37
# head(kippdata$student_id)
# sum(!is.na(match(sbac$student_id, kippdata$student_id))) #627, this is the column we want to match
# 
# #adding sback scores to the rows with matching student id
# kippsbac = cbind(kippdata, data.frame(matrix(NA,dim(kippdata)[1],dim(sbac)[2])))
# names(kippsbac)[38:40] = names(sbac)
# dim(kippsbac) ##4438, 40
# match1 = match(kippdata$student_id, sbac$student_id)
# kippsbac[!is.na(match1),38:40] = sbac[match1[!is.na(match1)],]
# dim(kippsbac) #4438   40 
# 
# #model
# model.sbacmath <- lm(kippsbac$COMPETITIVENESS_INDEX__C ~ kippsbac$Math)
# summary(model.sbacmath)
# model.sbacla <- lm(kippsbac$COMPETITIVENESS_INDEX__C ~ kippsbac$ELA)
# avgsbac = rowMeans(cbind(kippsbac$Math, kippsbac$ELA), na.rm=T)
# model.sbacavg <- lm(kippsbac$COMPETITIVENESS_INDEX__C ~ avgsbac)

