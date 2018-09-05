#create a smaller df2merge  flat file with data elements required for analysis 
df2mergev1<-select(.data=df2merge,
                   Age,
                   Categorised_Ethnicity,
                   Sex,
                   RecodedSex,
                   Deduction.date,
                   Registered.CCG,
                   Registered.practice,
                   Registered.practice.ID,
                   Registration.date, 
                   Registration.status,
                   AF0011.High.Risk.Atrial.Fibrillation.Patients.with.LTCs..Patient.ID,
                   Event.date,
                   earliest.chadsvasc.date,
                   earliest.chads2.date,
                   afeventdatebeforehasbled,
                   earliest.chadsvasc.eligible.pop,
                   earliest.anticoagulation.issue.start.date.sameday.or.after.af.diagnosis.date,
                   latest.anticoagulation.issue.start.date.before.af.diagnosis.date,
                   earliest.antiplatelet.issue.start.date.sameday.as.or.after.af.diagnosis.date,
                   Patient.ID,
                   afatriskecgeventdate,
                   populationecgeventdate)

#rename Event.Date to AF diagnosis date
df2mergev1<-rename(df2mergev1,AF.diagnosis.date="Event.date")

#add col to df2mergev1 to show if patient has AF diagnosis 1 if yes 0 if no
df2mergev1$AF.diagnosis<-ifelse(is.na(df2mergev1$AF.diagnosis.date),0,1)

#categorise the Age column  
library(data.table)
agebreaks <- c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,130)
agelabels <- c("0-4","5-9","10-14","15-19", "20-24","25-29","30-34","35-39","40-44",
               "45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99","100+")
  
setDT(df2mergev1)[ , agegroups := cut(Age, 
                                breaks = agebreaks, 
                                right = FALSE, 
                                labels = agelabels)]

agebreaks2 <- c(0,10,20,30,40,50,60,70,80,90,100,130)
agelabels2 <- c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90-99","100+")

setDT(df2mergev1)[ , agegroups2:= cut(Age,
                                      breaks = agebreaks2,
                                      right = FALSE,
                                      labels = agelabels2)]

save(df2mergev1, file="data/df2mergev1.rda")
