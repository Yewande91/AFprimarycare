#create a smaller df2merge  flat file with data elements required for analysis 
df2mergev1<-select(.data=df2merge,Age.x,
                   Categorised_Ethnicity,
                   Sex.x,
                   RecodedSex.x,
                   Deduction.date.x,
                   Registered.CCG.x,
                   Registered.practice.x,
                   Registered.practice.ID.x,
                   Registration.date.x, 
                   Registration.status.x,
                   AF0011.High.Risk.Atrial.Fibrillation.Patients.with.LTCs..Patient.ID,
                   Event.date,
                   earliest.chadsvasc.date,
                   M08ba...Number.of.AF.Patients.with.Cong.heart.fail.hypertens.age.diab.stoke.2.risk.score.Assessment.Documented..Numeric.value.earliest.chadsvasc.date,
                   M07a...Number.of.AF.Patients.with.CHAD2DS2.VAc.Assessment.Documented..Numeric.value.earliest.chadsvasc.date,
                   M10a...Number.of.AF.patients.with.HAS.BLED.Assessment.Documented..Numeric.value,
                   earliest.chads2.date,
                   M08ba...Number.of.AF.Patients.with.Cong.heart.fail.hypertens.age.diab.stoke.2.risk.score.Assessment.Documented..Numeric.value.earliest.chads2.date,
                   M07a...Number.of.AF.Patients.with.CHAD2DS2.VAc.Assessment.Documented..Numeric.value.earliest.chads2.date,
                   afeventdatebeforehasbled,
                   M10a...Number.of.AF.patients.with.HAS.BLED.Assessment.Documented..Numeric.value,
                   M12b...WMUH.Number.of.Newly.Diagnosed.AF.Patients.with.CHAD2DS2.VAc.Score.over.the.threshold.and.HAS.BLED.Score.under.Threshold..Females...Numeric.value,
                   M12c...Patients.with.HAS.BLED.Score.under.3.recorded..Numeric.value,
                   M12a...WMUH.Number.of.Newly.Diagnosed.AF.Patients.with.CHAD2DS2.VAc.Score.over.the.threshold.and.HAS.BLED.Score.under.Threshold..Males...Numeric.value,
                   earliest.chadsvasc.eligible.pop,
                   earliest.anticoagulation.issue.start.date.sameday.or.after.af.diagnosis.date,
                   latest.anticoagulation.issue.start.date.before.af.diagnosis.date,
                   Drug,
                   Drug.ID,
                   Issue.duration,
                   Issue.start.date,
                   Issue.end.date,
                   earliest.antiplatelet.issue.start.date.sameday.as.or.after.af.diagnosis.date,
                   Drug2,
                   Drug.ID2,
                   Issue.duration2,
                   Issue.start.date2,
                   Issue.end.date2,
                   Patient.ID)

#rename Event.Date to AF diagnosis date
df2mergev1<-rename(df2mergev1,AF.diagnosis.date="Event.date")

#add col to df2mergev1 to show if patient has AF diagnosis 1 if yes 0 if no
df2mergev1$AF.diagnosis<-ifelse(is.na(df2mergev1$AF.diagnosis.date),0,1)

#categorise the Age.x column  
library(data.table)
agebreaks <- c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,130)
agelabels <- c("0-4","5-9","10-14","15-19", "20-24","25-29","30-34","35-39","40-44",
               "45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99","100+")
  
setDT(df2mergev1)[ , agegroups := cut(Age.x, 
                                breaks = agebreaks, 
                                right = FALSE, 
                                labels = agelabels)]

agebreaks2 <- c(0,10,20,30,40,50,60,70,80,90,100,130)
agelabels2 <- c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90-99","100+")

setDT(df2mergev1)[ , agegroups2:= cut(Age.x,
                                      breaks = agebreaks2,
                                      right = FALSE,
                                      labels = agelabels2)]

save(df2mergev1, file="data/df2mergev1.rda")
