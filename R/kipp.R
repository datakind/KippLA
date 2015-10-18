#KIPP LA

##1.  PREPROCESSING ####

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
                      fun.aggregate = max) #lose the "nwea_2010_id", but that's OK
#seems that in one quarter a single student can register two scores in the same subject, we take the max (fun.aggregate=mean)
nwea10subcast[sapply(nwea10subcast, is.infinite)]=NA
dim(nwea10subcast) #521,9
salesforce10 = merge(nwea10subcast, salesforce, by.x = "nwea_2010_localStudentID", by.y = "local_student_id", all.y=T)
dim(salesforce10) #4438, 54

#add in 2011 test scores
nwea11 <- read.csv("~/Dropbox/Teradata DataDive 2015/KIPP LA/NWEA-2011.csv")
nwea11sub <- nwea11[,c(1,3,4,6,14)]
nwea11subcast<- dcast(nwea11sub, nwea_2011_localStudentID~nwea_2011_Discipline+nwea_2011_TermName, 
                      fun.aggregate = max) #lose the "nwea_2011_id", but that's OK
#seems that in one quarter a single student can register two scores in the same subject, we average them (fun.aggregate=mean)
nwea11subcast[sapply(nwea11subcast, is.infinite)]=NA
dim(nwea11subcast) #977,7
salesforce11 = merge(nwea11subcast, salesforce10, by.x = "nwea_2011_localStudentID", by.y = "nwea_2010_localStudentID", all.y=T)
dim(salesforce11) #4438, 60

#shrink the data
salesforce.small = salesforce11[, c(1:17,28:31, 34, 38:39, 43, 45:47, 53:60)]
dim(salesforce.small) #4438, 36


##2. ANALYSIS, college quality <- early test scores ####

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


##3. SCORES -> COLLEGE? ####

#looking at the zeros of college competitiveness:
comp0 <- kippdata[kippdata$COMPETITIVENESS_INDEX__C==0 & !is.na(kippdata$COMPETITIVENESS_INDEX__C==0),]
summary(comp0)
data.frame(comp0$COMPETITIVENESS_RANKING__C, comp0$School.Name)

#initial model -> competitiveness index as a function 
#of mean score (percentile) of all available scores

# (0s for two-year colleges have not been removed)
model1 <- lm(kippdata$COMPETITIVENESS_INDEX__C~rowavg)
summary(model1) #113 data points (after removing 1994)
plot(model1) #one outlier (1994) has only one score and it's for science so retroactively removed
plot(rowavg, kippdata$COMPETITIVENESS_INDEX__C) 
abline(model1$coefficients, col = "red")
#very low R-squared (.1237). 

#we should remove colleges labeled highly or very competitive which have the 0 competitive index
removeindex = c(which(kippdata$COMPETITIVENESS_INDEX__C==0 & 
              (kippdata$COMPETITIVENESS_RANKING__C=="Very Competitive"|kippdata$COMPETITIVENESS_RANKING__C=="Highly Competitive")))
removeindex = c(removeindex, 1994) #based on below outlier
#make them NA:
kippdata$COMPETITIVENESS_INDEX__C[removeindex]=NA

kippdata2 = kippdata 
removeindex2 = which(kippdata2$COMPETITIVENESS_INDEX__C == 0 & kippdata2$COMPETITIVENESS_RANKING__C == "2 year (Noncompetiti")
kippdata2$COMPETITIVENESS_INDEX__C[removeindex2]=NA

#competitiveness index as a function of mean score (percentile) for what's available, 0 for 2-years colleges removed
model2 <- lm(kippdata2$COMPETITIVENESS_INDEX__C~rowavg) #higher correlation
summary(model2) #74 data points, r-squared .32
plot(model2) 
plot(rowavg, kippdata2$COMPETITIVENESS_INDEX__C, ylab ="Competitiveness Index",
     xlab="Avg. Percentile NWEA Score (2010,2011)", main = "NWEA Score -> College") 
abline(model2$coefficients, col = "red")
#ok. why not more predictive? -> in part because it's college attended not best accepted?

# Data anomalies? ####

#some high score students go to colleges less than -1 competitiveness index /  some "very competitive" colleges have competitiveness index < -1
compneg1 <- kippdata[kippdata$COMPETITIVENESS_INDEX__C < -1 & !is.na(kippdata$COMPETITIVENESS_INDEX__C ), c(31,34:35)]

#Try using competiveness scale instead?? doesn't really help. could just get rid of the bad data instead.
compval = as.factor(kippdata$COMPETITIVENESS_RANKING__C)
levels(compval) = c(0,0,1,3,-1,4,5,0,NA,2)
compval = as.numeric(levels(compval))[compval]
model3 <- lm(compval~rowavg)
summary(model3)
plot(rowavg, compval)

# Tier x cutoff ####

#2015 HS COHORT
kipp2015 = kippdata[kippdata$STUDENT_HS_COHORT__C==2015 & kippdata$COMPETITIVENESS_RANKING__C!="2 year (Noncompetiti",]
#Fall
#remove outlier
kipp2015 = kipp2015[-53,]
plot(kipp2015$"Mathematics_Fall 2010-2011"~kipp2015$COMPETITIVENESS_INDEX__C, main = "2015 Cohort")
abline(h = 86, col = "green", lty = 2)
abline(h = 61, col = "red", lty = 2)

plot(kipp2015$"Reading_Fall 2010-2011"~kipp2015$COMPETITIVENESS_INDEX__C)
abline(h = 79, col = "green", lty = 2)
abline(h = 55, col = "red", lty = 2)

#Spring
plot(kipp2015$"Mathematics_Spring 2010-2011"~kipp2015$COMPETITIVENESS_INDEX__C)
plot(kipp2015$"Reading_Spring 2010-2011"~kipp2015$COMPETITIVENESS_INDEX__C)
