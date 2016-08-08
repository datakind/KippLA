library(data.table)
library(ggplot2)
library(bit64)

data.dir <- '/Users/whigh/data/datakind_kippla/dropbox'
enrollment.file <- 'SALESFORCE-Enrollment.csv'
college.file <- 'CollegeTierMapping.csv'
student.id.file <- 'MasterIDTable.csv'
nwea.math.file <- 'Clean Data/Objective # 2/NWEA-2010.csv'

enr.dat <- fread(paste(data.dir,enrollment.file,sep='/'))
col.dat <- fread(paste(data.dir,college.file,sep='/'))
sid.dat <- fread(paste(data.dir,student.id.file,sep='/'))
rit.dat <- subset(fread(paste(data.dir,nwea.file,sep='/')),
                  nwea_2010_Discipline %in% c('Mathematics','Reading')
                  & grepl("Fall",nwea_2010_TermName)
                  & !grepl("N",student_id),
                  select=c('student_id','nwea_2010_TermName',
                           'nwea_2010_localStudentID','nwea_2010_Discipline',
                           'nwea_2010_TestRITScore'))

name.v <- colnames(col.dat)
name.v[1] <- 'SCHOOL__C'
setnames(col.dat,name.v)

enr.dat.subset <- subset(enr.dat,
                         TYPE__C == 'College'
                         & STATUS__C == 'Attending',
                         select=c('ID','TYPE__C','SCHOOL__C'))

tier.merge <- merge(enr.dat.subset,col.dat,by='SCHOOL__C')

rit.dat.melt <- melt(rit.dat,id.vars=c('student_id','year'))


p <- ggplot(tier.merge,aes(COMPETITIVENESS_INDEX__C)) 
p <- p + geom_histogram()
print(p)


