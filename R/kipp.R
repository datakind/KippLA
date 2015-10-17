#KIPP LA

#distribution of colleges attended by tier

#competitiveness ranking
#competitive index
#adjusted 6 year minority graduation rate

#### PREPROCESSING ####

college <- read.csv("~/Dropbox/Teradata DataDive 2015/KIPP LA/CollegeTierMapping.csv")
salesforce <- read.csv("~/Dropbox/Teradata DataDive 2015/KIPP LA/SALESFORCE-Enrollment.csv")

# add college info to salesforce main table:
salesforce_names <- names(salesforce)
salesforce[,39:44] = college[match(salesforce$SCHOOL__C, college$Enrollment.SCHOOL__C),]
#660 matches

#join ids (#salesforce$STUDENT__C <-> masterid$Salesforce.ID)
masterid <- read.csv("~/Dropbox/Teradata DataDive 2015/KIPP LA/MasterIDTable.csv")

#ignore those without salesforce ids
masterid_salesforce = masterid[masterid$Salesforce.ID!="", ]
salesforce = merge(salesforce, masterid_salesforce, by.x = "STUDENT__C", by.y = "Salesforce.ID")
dim(salesforce) #4438   46

#add in 2010 test scores (used percentiles instead for easier comparison between subjects)
require(reshape2)
nwea10 <- read.csv("~/Dropbox/Teradata DataDive 2015/KIPP LA/NWEA-2010.csv")
nwea10sub <- nwea10[,c(1,3,4,6,15)] #  "nwea_2010_id" "nwea_2010_TermName"   "nwea_2010_localStudentID" "nwea_2010_Discipline"  "nwea_2010_TestPercentile" 
nwea10subcast<- dcast(nwea10sub, nwea_2010_localStudentID~nwea_2010_Discipline+nwea_2010_TermName, 
                      fun.aggregate = mean) #lose the "nwea_2010_id", but that's OK
#seems that in one quarter a single student can register two scores in the same subject, we average them (fun.aggregate=mean)
nwea10subcast[sapply(nwea10subcast, is.nan)]=NA
dim(nwea10subcast) #521,9
salesforce10 = merge(nwea10subcast, salesforce, by.x = "nwea_2010_localStudentID", by.y = "local_student_id", all.y=T)
dim(salesforce10) #4438, 54

#add in 2011 test scores
nwea11 <- read.csv("~/Dropbox/Teradata DataDive 2015/KIPP LA/NWEA-2011.csv")
nwea11sub <- nwea11[,c(1,3,4,6,14)]
nwea11subcast<- dcast(nwea11sub, nwea_2011_localStudentID~nwea_2011_Discipline+nwea_2011_TermName, 
                      fun.aggregate = mean) #lose the "nwea_2011_id", but that's OK
#seems that in one quarter a single student can register two scores in the same subject, we average them (fun.aggregate=mean)
nwea11subcast[sapply(nwea11subcast, is.nan)]=NA
dim(nwea11subcast) #977,7
salesforce11 = merge(nwea11subcast, salesforce10, by.x = "nwea_2011_localStudentID", by.y = "nwea_2010_localStudentID", all.y=T)
dim(salesforce11) #4438, 60

#shrink the data
salesforce.small = salesforce11[, c(1:17,28:31, 34, 38:39, 43, 45:47, 53:60)]
dim(salesforce.small) #4438, 36

#### ANALYSIS, college quality <- early test scores ####

kippdata = salesforce.small
summary(kippdata)

#how many students have any past scores? (columns 2-15)
rowavg = rowMeans(kippdata[,2:15], na.rm=T)
percentile.mean <- rowavg[!is.na(rowavg)]
length(percentile.mean)/(dim(kippdata)[1]) 
#about 38% have at least one score

#For those who have any scores, how many scores do they have? most have 6-8
table(14 - rowSums(is.na(kippdata[,2:15])))
barplot(table(14 - rowSums(is.na(kippdata[,2:15])))[2:14])

#SCORES -> COLLEGE?

#looking at the zeros of college competitiveness:
comp0 <- kippdata[kippdata$COMPETITIVENESS_INDEX__C==0 & !is.na(kippdata$COMPETITIVENESS_INDEX__C==0),]
summary(comp0)
data.frame(comp0$COMPETITIVENESS_RANKING__C, comp0$School.Name)

#suggests we should remove collges labeled highly or very competitive which have the 0 competitive index
removeindex = c(which(kippdata$COMPETITIVENESS_INDEX__C==0 & 
        (kippdata$COMPETITIVENESS_RANKING__C=="Very Competitive"|kippdata$COMPETITIVENESS_RANKING__C=="Highly Competitive")))
removeindex = c(removeindex, 1994) #based on below outlier
#make them NA:
kippdata$COMPETITIVENESS_INDEX__C[removeindex]=NA

#initial model -> competitiveness index as a function of mean score (percentile) for what's available
model1 <- lm(kippdata$COMPETITIVENESS_INDEX__C~rowavg)
plot(rowavg, kippdata$COMPETITIVENESS_INDEX__C) 
summary(model1) #113 data points (after removing 1994)
plot(model1) #one outlier (1994) has only one score and it's for science so retroactively removed
abline(model1$coefficients, col = "red")

#perhaps some predictive power, but very low R-squared. try removing all 0 who go to 2-year college?
kippdata2 = kippdata 
removeindex2 = which(kippdata2$COMPETITIVENESS_INDEX__C == 0 & kippdata2$COMPETITIVENESS_RANKING__C == "2 year (Noncompetiti")
kippdata2$COMPETITIVENESS_INDEX__C[removeindex2]=NA

model2 <- lm(kippdata2$COMPETITIVENESS_INDEX__C~rowavg) #higher correlation
plot(rowavg, kippdata2$COMPETITIVENESS_INDEX__C) 
summary(model2) #74 data points
plot(model2) 
abline(model2$coefficients, col = "red")

#why not more predictive? especially curoius about the high score students who go to colleges less than -1
compneg1 <- kippdata[kippdata$COMPETITIVENESS_INDEX__C < -1 & !is.na(kippdata$COMPETITIVENESS_INDEX__C ), c(31,34:35)]

#??? some "very competitive" schools have negative competition index ???

#Try using competiveness scale instead?? doesn't really help. could just get rid of the bad data instead.
compval = as.factor(kippdata$COMPETITIVENESS_RANKING__C)
levels(compval) = c(0,0,1,3,-1,4,5,0,NA,2)
compval = as.numeric(levels(compval))[compval]
model3 <- lm(compval~rowavg)
summary(model3)
plot(rowavg, compval)
