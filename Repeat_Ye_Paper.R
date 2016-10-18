library(foreign)

profile2013 <- read.spss(file.choose(),to.data.frame = T)
core <- grepl('^c',names(profile2013), ignore.case = T)
core[1] <- T
profile2013_core <- profile2013[,core]

#recode population variable
library(car)
Ye <- NA
Ye$population <- recode(profile2013_core$c0population,
                        "NA = NA;                       
                        1:24999 = '<25000';
                        25000:49999 = '25000-49999';
                        50000:99999 = '50000-99999';
                        100000:499999 = '100000-499999';
                        else = '500000+'")
Ye[1] <- NULL
#recode governance type
Ye$governance_type <- profile2013_core$c0govcat
#recode budget cut
Ye$budget <- recode(profile2013_core$c10q301,"c('less than')='with budget cuts';c('greater than','same')='without budget cuts';else=NA")
#recode BOH with authority related variable
Ye$BOH_1 <- profile2013_core$c2q6a
Ye$BOH_2 <- profile2013_core$c2q7a
Ye$BOH_3 <- profile2013_core$c2q8a
Ye$BOH_4 <- profile2013_core$c2q9a
Ye$BOH_5 <- profile2013_core$c2q10a
Ye$BOH_6 <- profile2013_core$c2q11a
Ye$BOH_7 <- profile2013_core$c2q14a
Ye$BOH_8 <- profile2013_core$c2q15a
#recode weight variable
Ye$weight01 <- profile2013_core$c0coreweight_s
Ye$weight02 <- profile2013_core$c0coreweight_p
#set as data frame
Ye <- as.data.frame(Ye)
#subset non-missing in weight
Ye_1 <- subset(Ye,is.na(Ye$weight01)==F)
Ye_2 <- subset(Ye,is.na(Ye$weight02)==F)
#weight
library(survey)
Ye_1 <- svydesign(ids = ~1, data = Ye_1, weights = Ye_1$weight01)
Ye_2 <- svydesign(ids = ~1, data = Ye_2, weights = Ye_2$weight02)
  

prop.table(svytable(~population+budget, design=Ye_1),2)
prop.table(svytable(~population+budget, design=Ye_2),2)
prop.table(svytable(~governance_type+budget, design=Ye_1),2)
prop.table(svytable(~governance_type+budget, design=Ye_2),2)
prop.table(svytable(~BOH_1+budget, design=Ye_1),2)
prop.table(svytable(~BOH_1+budget, design=Ye_2),2)
