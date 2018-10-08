##ignore data_frames[[1]]

#data_frames[[2]] shows patients on practice list registered before date data was run
#assign data_frames[[2]] to df2 
df2<-data_frames[[2]]

#registration date
#for vignette
#how many registration dates in df2 are greater than 2017-11-16  i.e. date data was run or
#with no registration date at all?
#answer is 3311
df2_reg_date_greater_than_date_data_was_run_or_no_reg_date<-subset(df2,df2$Registration.date>"2017-11-16"|is.na(df2$Registration.date))

#na_rm_min function if only na for a specific patient it will return na, if a mix of a date 
#and na it will return minimum date
na_rm_min <- function(x) {
  x_no_na <- x[!is.na(x)]
  if(length(x_no_na)==0) {
    return(NA)
  } else {
    min(x, na.rm = TRUE)
  }
}

#for vignette
#in df2_reg_date_greater_than_date_data_was_run_or_no_reg_date_or_no_reg_date remove duplicate ids
#there are no duplicate ids
df2_reg_date_greater_than_date_data_was_run_or_no_reg_date<-df2_reg_date_greater_than_date_data_was_run_or_no_reg_date %>% 
  group_by(Patient.ID) %>% 
  summarise(df2_reg_date_min=na_rm_min(Registration.date))%>%
  ungroup()

#for vignette
#find out how many of the 3311 registered patients have an AF diagnosis using df6
#data_frames[[6]] shows patients diagnosed with Atrial Fibrillation before date data was run
#104 have an af diagnosis date
df6<-data_frames[[6]]
df6test<-df6 %>% 
  group_by(Patient.ID) %>% 
  summarise(af_diagnosis_date = na_rm_min(Event.date))%>%
  ungroup()

df2_reg_date_greater_than_date_data_was_run_or_no_reg_date<-left_join(x=df2_reg_date_greater_than_date_data_was_run_or_no_reg_date,
                                                       y=df6test,by="Patient.ID",copy = FALSE)
df2_reg_date_greater_than_date_data_was_run_or_no_reg_date_AF_only<-subset(df2_reg_date_greater_than_date_data_was_run_or_no_reg_date,
                                                            (!is.na(df2_reg_date_greater_than_date_data_was_run_or_no_reg_date[,3])))
#for the vignette
#show distribution of the 104 af diagnosis dates to see if they are spread out in the entire study period 
# 2011-01-01 to 2017-10-31
#or all concentrated before of after the intervention period (October 2014) as that may cause bias
library(lubridate)
library(ggplot2)
qplot(year(af_diagnosis_date), data=df2_reg_date_greater_than_date_data_was_run_or_no_reg_date_AF_only, geom = "bar")


#filter first to remove reg dates greater than 2017-11-16 and  blank reg dtaes
#group by patient id
#Remove duplicate patient ids in df2 to retain the earliest registration date
library(tidyverse)
df2merge<-df2 %>%
  filter(Registration.date<=as.Date("2017-11-16",format="%Y-%m-%d")) %>%
           group_by(Patient.ID) %>% 
  slice(which.min(Registration.date)) %>%
  ungroup()

#column names of df2merge
#[1] "Age"                    "Ethnicity"              "Sex"                    "SOA..lower.layer."     
#[5] "SOA..middle.layer."     "Deduction.date"         "Registered.CCG"         "Registered.practice"   
#[9] "Registered.practice.ID" "Registration.date"      "Registration.status"    "Patient.ID"            
#[13] "Patient.Count"          "RecodedSex"     

#for the vignette
#how many of the 3311 patients still remain in the sliced df2merge, answer is 193 patients remain, of which
# 18 have an AF diagnosis equating to loss of 86 patients diagnosed with AF due to regisration dates being NA
# or registration dates being in the future
sliced_df2merge<-df2merge
df2_reg_date_greater_than_date_data_was_run_or_no_reg_date$reg_date_greater_than_date_data_was_run <-c(TRUE)
sliced_df2merge<-left_join(x=sliced_df2merge,y=df2_reg_date_greater_than_date_data_was_run_or_no_reg_date,
                           by="Patient.ID",copy = FALSE)
sliced_df2merge2<-subset(sliced_df2merge,sliced_df2merge$reg_date_greater_than_date_data_was_run=="TRUE")
sliced_df2merge3<-subset(sliced_df2merge2,(!is.na(sliced_df2merge2[,16])))

#delete all cols in df2merge except for col 10 i.e. minimum reg date and col 12 patient id 
df2merge<-df2merge[,-c(1:9,11,13:14)]



#deduction date
#for the vignette
#how many deduction dates in df2 are greater than 2017-11-16. 
#answer is 563
df2_ded_date_greater_than_date_data_was_run<-subset(df2,df2$Deduction.date>"2017-11-16")

#this function finds the max date and returns the row number

na_which_max <- function(x) {
  x_no_na <- x[!is.na(x)]
  if(length(x_no_na)==0) {
    return(1)
  } else {
    which.max(x)
  }
}

#for the vignette
#in df2_ded_date_greater_than_date_data_was_run remove duplicate ids
#answer is 562, so there was only one duplicate
df2_ded_date_greater_than_date_data_was_run<-df2_ded_date_greater_than_date_data_was_run %>% 
  group_by(Patient.ID) %>% 
  slice(na_which_max(Deduction.date)) %>%
  ungroup()

#delete all cols in df2_ded_date_greater_than_date_data_was_run except for col 12 patient id, 
# col 15 af diagnosis date and col 16 ded_date_greater_than_date_data_was_run
df2_ded_date_greater_than_date_data_was_run<-df2_ded_date_greater_than_date_data_was_run[,-c(1:5,7:11,13:14)]

#for the vignette
#find out how many of the 562  patients with a deduction date have an AF diagnosis using df6
#answer is only 1 has an af diagnosis date 

df2_ded_date_greater_than_date_data_was_run<-left_join(x=df2_ded_date_greater_than_date_data_was_run,y=df6test,by="Patient.ID",copy = FALSE)
df2_ded_date_greater_than_date_data_was_run_AF_only<-subset(df2_ded_date_greater_than_date_data_was_run,(!is.na(df2_ded_date_greater_than_date_data_was_run[,3])))


#filter first to remove deduction dates greater than 2017-11-16
#group by patient id
#Remove duplicate patient ids in df2 to retain the latest deduction date or NAs

df2_ded_date<-df2 %>%
  filter(Deduction.date<="2017-11-16"|is.na(Deduction.date)) %>%
  group_by(Patient.ID) %>% 
  slice(na_which_max(Deduction.date)) %>%
  ungroup()


#for the vignette
#how many of the 562 patients still remain in the sliced df2_ded_date, answer is 218 patients remain, of which
#  1 have an AF diagnosis equating to loss of 0 patients diagnosed with AF due to deduction dates being 
#  being after date data was run
sliced_df2_ded_date<-df2_ded_date
df2_ded_date_greater_than_date_data_was_run$ded_date_greater_than_date_data_was_run <-c(TRUE)
sliced_df2_ded_date<-left_join(x=sliced_df2_ded_date,y=df2_ded_date_greater_than_date_data_was_run,
                           by="Patient.ID",copy = FALSE)
sliced_df2_ded_date2<-subset(sliced_df2_ded_date,sliced_df2_ded_date$ded_date_greater_than_date_data_was_run=="TRUE")
sliced_df2_ded_date3<-subset(sliced_df2_ded_date2,(!is.na(sliced_df2_ded_date2[,16])))

#remove registration date col from df2_ded_date, so its not confused with the min reg date in df2 merge
df2_ded_date<-df2_ded_date[,-c(10)]

#left join df2_ded_date into df2merge
df2merge<-left_join(x=df2merge,y=df2_ded_date,by="Patient.ID",copy = FALSE)



#for vignette
#are all the deduction dates in df2merge after the registration dates
#answer is 391 where Deduction.date < Registration.date
#of this 391 patients , 0 have an AF diagnosis so 0 AF diagnosed patients which will be lost
df2merge_check1<-df2merge %>%
  filter(Deduction.date < Registration.date)

df2merge_check1<-left_join(x=df2merge_check1,y=df6test,by="Patient.ID",copy = FALSE)

df2merge_check1_AF_only<-subset(df2merge_check1,(!is.na(df2merge_check1[,15])))

#remove 391 patients in df2merge where deduction date < registration date
df2merge<-df2merge %>%
  filter(Deduction.date >= Registration.date | is.na(Deduction.date))



#for vignette
#how many cases are there where the registration and the deduction dates are the same date
#981 patients 
#of which 4 have an AF diagnosis
#i have chosen to keep these patients in as it could still be plausible to have the same date, so the 
#4 af diagnosed will not be lost
df2merge_check2<-df2merge %>%
  filter(Deduction.date == Registration.date)

df2merge_check2<-left_join(x=df2merge_check2,y=df6test,by="Patient.ID",copy = FALSE)

df2merge_check2_AF_only<-subset(df2merge_check2,(!is.na(df2merge_check2[,15])))



#for vignette
#check that if registration status is current , deduction date is NA 
#there are 3 patients where it says current but the deduction date is not blank 
#none of these 3 patients have an AF diagnosis
df2merge_check3<-df2merge %>%
  filter(Registration.status =="Current")

df2merge_check3a<-df2merge_check3 %>%
  filter(!is.na(Deduction.date))

df2merge_check3a<-left_join(x=df2merge_check3a,y=df6test,by="Patient.ID",copy = FALSE)

df2merge_check3a_AF_only<-subset(df2merge_check3a,(!is.na(df2merge_check3a[,15])))

#check that if registration status  "Deducted", "Deceased, Deducted" or "Deceased" 
#the deduction date is not NA
#32 patients where registration status  "Deducted", "Deceased, Deducted" or "Deceased" 
#but deduction date is NA
# 1 of these 32 patients has an AF diagnosis 
df2merge_check4<-df2merge %>%
  filter(Registration.status =="Deducted" | Registration.status =="Deceased, Deducted" |
           Registration.status == "Deceased" )

df2merge_check4a<-df2merge_check4 %>%
  filter(is.na(Deduction.date))

df2merge_check4a<-left_join(x=df2merge_check4a,y=df6test,by="Patient.ID",copy = FALSE)

df2merge_check4a_AF_only<-subset(df2merge_check4a,(!is.na(df2merge_check4a[,15])))

#check how many patients where the registration status is NA
#there are 7 patients where this happens , none of whom have an AF diagnosis
#this is because these patients were not in the df2_ded_date merge in as there deduction dates 
#were greater than the date data was run 
df2merge_check5<-subset(df2merge,(is.na(df2merge[,12])))

df2merge_check5a<-left_join(x=df2merge_check5,y=df6test,by="Patient.ID",copy = FALSE)

df2merge_check5a_AF_only<-subset(df2merge_check5a,(!is.na(df2merge_check5a[,15])))

# filter out from df2merge the 3 patients where it says current but the deduction date is not blank 
#and the 32 patients where registration status  "Deducted", "Deceased, Deducted" or "Deceased" but deduction date is NA
#and the 7 cases where registration ststus is na
df2merge<-df2merge %>%
  filter(Registration.status =="Current" & is.na(Deduction.date) |
           Registration.status =="Deducted" & !is.na(Deduction.date)| 
           Registration.status =="Deceased, Deducted" & !is.na(Deduction.date)|
            Registration.status == "Deceased" & !is.na(Deduction.date))




#...
#data_frames[[3]] shows snapshot of patients on the AF at risk register
#assign data_frames[[3]] to df3
df3<-data_frames[[3]]

#colnames which are present in both df3 and df2merge (numbers correspond to colnames in df3)
#[1] "Deduction.date"                                                                        
#[2] "Registered.CCG"                                                                        
#[3] "Registered.practice"                                                                   
#[4] "Registered.practice.ID"                                                                
#[5] "Registration.date"                                                                     
#[6] "Registration.status"                                                                   
#[14] "Patient.Count"

#remove cols already present in df2merge
df3merge<-df3[,-c(1:6,14)]

#rename"M02a...Number.of.Patients.on.the.Atrial.Fibrillation..AF..At.Risk.Register..Patient.ID" col 
#in df3merge to "Patient.ID", although colname different it refers to same thing
df3merge<-rename(df3merge,Patient.ID ="M02a...Number.of.Patients.on.the.Atrial.Fibrillation..AF..At.Risk.Register..Patient.ID")

#this function finds the min  date and returns the row number

na_which_min <- function(x) {
  x_no_na <- x[!is.na(x)]
  if(length(x_no_na)==0) {
    return(1)
  } else {
    which.min(x)
  }
}


#Remove duplicate patient ids in df3 merge
df3merge<-df3merge[!duplicated(df3merge$Patient.ID), ]

#leftjoin all cols in df3merge to df2merge 
#note in df2merge some patients who were previously on the af at risk register may not be identified as df3 is only
#a snapshot
df2merge<-left_join(x=df2merge,y=df3merge,by="Patient.ID",copy = FALSE)


#...
#data_frames[[6]] shows patients diagnosed with Atrial Fibrillation before date data was run
#assign data_frames[[6]] to df6 
df6<-data_frames[[6]]

#colnames which are present in both df6 and df2merge (colnames as ordered in df6), note patient ID does not 
#count as this column was used for the leftjoin
#[2]"Age"                    
#[3]"Ethnicity"              
#[4]"Sex"                   
#[5] "SOA..lower.layer."      
#[6]"SOA..middle.layer."     
#[10]"Deduction.date"         
#[11]"Registered.CCG"         
#[12]"Registered.practice"   
#[13] "Registered.practice.ID" 
#[14]"Registration.date"      
#[15]"Registration.status"    
#[17] "Patient.Count"          
#[18]"RecodedSex"  

#remove cols already present in df2merge 
df6merge<-df6[,-c(2:6,10:15,17:18)]

#group by patient id and slice to keep row with earliest event date
df6merge<-df6merge %>%
  group_by(Patient.ID)%>%
  slice(na_which_min(Event.date))%>%
  ungroup()

#leftjoin all cols in df6merge to df2merge 
#note in the merge there will be lots of NAs as not all patients have an AF diagnosis
df2merge<-left_join(x=df2merge,y=df6merge,by="Patient.ID",copy = FALSE)



#ignore data_frames [[7]] as chadsvasc is already noted in data_frames[[8]]
#data_frames[[8]] shows patients  with AF who have a either a CHAD2DS2-VAc or CHADS2 Assessment Documented
#assign data_frames[[8]] to df8
df8<-data_frames[[8]]

#make a df8 duplicate called df8 duplicate to use for tests
df8duplicate<-df8

#check if the two CHADSVASC eventdate columns in df8duplicate are identical
#these two columns are not identical
df8duplicate$Event.date.test<-as.integer(ifelse(df8duplicate$M07a...Number.of.AF.Patients.with.CHAD2DS2.VAc.Assessment.Documented..Event.date
                                                ==df8duplicate$M07aa...Number.of.AF.Patients.with.Read.code.XaY6i..Event.date,1,0))

#assign df8 to df8merge
df8merge<-df8

#leftjoin patient ID column and AF diagnosis date column in df2merge to df8merge
df8merge<- df8merge %>% 
  rename(Patient.ID="M08a...Number.of.Patients.with.AF.who.have.a.either.a.CHAD2DS2.VAc.or.CHADS2.Assessment.Documented.or.both..Patient.ID")%>%
  left_join(select(.data=df2merge,Patient.ID, Event.date),by="Patient.ID",copy = FALSE)

#create new column in df8merge,if merged in AF event.date is before or same as M07a...Number.of.AF.Patients.with.CHAD2DS2.VAc.Assessment.Documented..Event.date
#show the chadsvasc date for that row, if not show NA
df8merge$afeventdatebeforeorsamedayaschadsvasc<-if_else(df8merge$Event.date<=
                                               df8merge$M07a...Number.of.AF.Patients.with.CHAD2DS2.VAc.Assessment.Documented..Event.date,
                                             df8merge$M07a...Number.of.AF.Patients.with.CHAD2DS2.VAc.Assessment.Documented..Event.date,as.Date(NA,"%Y-%m-%d"))


#create new column in df8merge,if merged in AF event.date is before or same as M07aa...Number.of.AF.Patients.with.Read.code.XaY6i..Event.date
#show the chadsvasc date for that row , if not show NA
df8merge$afeventdatebeforeorsamedayaschadsvasc2<-if_else(df8merge$Event.date<=df8merge$M07aa...Number.of.AF.Patients.with.Read.code.XaY6i..Event.date,
                                              df8merge$M07aa...Number.of.AF.Patients.with.Read.code.XaY6i..Event.date,as.Date(NA,"%Y-%m-%d"))


#create a subset of df8merge where either df8merge$afeventdatebeforeorsamedayaschadsvasc  or df8merge$afeventdatebeforeorsamedayaschadsvasc2
#columns have no NAs
# note there are still be some NAs that appear in df8merge_subset$afeventdatebeforeorsamedayaschadsvasc 
df8merge_subset<-subset(df8merge,(!is.na(df8merge[,23])) | (!is.na(df8merge[,24])))


#create column in df8merge_subset containing the earliest CHADSVASC date that is on same day or comes after the AF event date
#note this column takes both chadsvasc columns into consideration 
df8merge_subset<-df8merge_subset%>%
  mutate(earliest.chadsvasc.date = pmin(afeventdatebeforeorsamedayaschadsvasc,afeventdatebeforeorsamedayaschadsvasc2,na.rm = TRUE))

#remove duplicates from df8merge_subset 
df8merge_subset<-df8merge_subset %>% 
  group_by(Patient.ID) %>% 
  slice(which.min(earliest.chadsvasc.date))%>%
  ungroup()



#remove event.date,  afeventdatebeforechadsvasc and afeventdatebeforechadsvasc2 columns in df8merge_subset
df8merge_subset<-df8merge_subset[,-c(1:4,6:12,21:24)]

#left_join df8merge_subset to df2merge
df2merge<-left_join(x=df2merge,y=df8merge_subset,by="Patient.ID",copy = FALSE)




#...

#make a df8 duplicate called df8duplicate2
df8duplicate2<-df8

#check if the two CHADS2 eventdate columns in df8duplicate2 are identical
#these two cols are not identical
df8duplicate2$Event.date.test<-as.integer(ifelse(df8duplicate2$M08bb...Number.of.AF.Patients.with.Read.code.XaP9J..Event.date
                                                ==df8duplicate2$M08ba...Number.of.AF.Patients.with.Cong.heart.fail.hypertens.age.diab.stoke.2.risk.score.Assessment.Documented..Event.date,1,0))


#assign df8 to df8merge2
df8merge2<-df8

#leftjoin patient ID column and AF diagnosis date column in df2merge to df8merge2
df8merge2<- df8merge2 %>% 
  rename(Patient.ID="M08a...Number.of.Patients.with.AF.who.have.a.either.a.CHAD2DS2.VAc.or.CHADS2.Assessment.Documented.or.both..Patient.ID")%>%
  left_join(select(.data=df2merge,Patient.ID, Event.date),by="Patient.ID",copy = FALSE)

#create new column in df8merge2,if merged in AF event.date is before or same as M08bb...Number.of.AF.Patients.with.Read.code.XaP9J..Event.date
#show the chads2 date , if not show NA
df8merge2$afeventdatebeforeorsamedayaschads2<-if_else(df8merge2$Event.date<=
                                               df8merge2$M08bb...Number.of.AF.Patients.with.Read.code.XaP9J..Event.date,
                                             df8merge2$M08bb...Number.of.AF.Patients.with.Read.code.XaP9J..Event.date,as.Date(NA,"%Y-%m-%d"))

#create new column in df8merge2,if merged in AF event.date is before or same as M08ba...Number.of.AF.Patients.with.Cong.heart.fail.hypertens.age.diab.stoke.2.risk.score.Assessment.Documented..Event.date
#show the chads2 date , if not show NA
df8merge2$afeventdatebeforeorsamedayaschads2col2<-if_else(df8merge2$Event.date<=
                                                df8merge2$M08ba...Number.of.AF.Patients.with.Cong.heart.fail.hypertens.age.diab.stoke.2.risk.score.Assessment.Documented..Event.date,
                                              df8merge2$M08ba...Number.of.AF.Patients.with.Cong.heart.fail.hypertens.age.diab.stoke.2.risk.score.Assessment.Documented..Event.date,as.Date(NA,"%Y-%m-%d"))

#create a subset of df8merge2, where either df8merge2$afeventdatebeforeorsamedayaschads2 or df8merge2$afeventdatebeforeorsamedayaschads2col2
#have no NAs
df8merge2_subset<-subset(df8merge2,(!is.na(df8merge2[,23]))|(!is.na(df8merge2[,24])))

#create column in df8merge2_subset containing the earliest CHADS2 date that is on same day 
#or comes after the AF event date
#note this column takes both chads2 columns into consideration 
df8merge2_subset<-df8merge2_subset%>%
  mutate(earliest.chads2.date = pmin(afeventdatebeforeorsamedayaschads2,afeventdatebeforeorsamedayaschads2col2,na.rm = TRUE))

#remove duplicates from df8merge2_subset by selecting earliest.chads2.date for same patient id
df8merge2_subset<-df8merge2_subset %>% 
  group_by(Patient.ID) %>% 
  slice(which.min(earliest.chads2.date))

#remove event.date,  afeventdatebeforechads2 and afeventdatebeforeorsamedayaschads2col2 columns in df8merge2_subset
df8merge2_subset<-df8merge2_subset[,-c(1:4,13:24)]

#left_join df8merge2_subset to df2merge
df2merge<-left_join(x=df2merge,y=df8merge2_subset,by="Patient.ID",copy = FALSE)


#...
#assign data_frames[[9]] to df9
df9<-data_frames[[9]]

#assign df9 to df9merge
df9merge<-df9

#creat new col to see if the hasbled columns are identical 
#both hasbled columns are identical
df9merge$HASBLED.eventdate.test<-as.integer(ifelse(df9merge$M10a...Number.of.AF.patients.with.HAS.BLED.Assessment.Documented..Event.date
                                                   ==df9merge$M10b...Number.of.AF.Patients.with.Read.code.XaY6z..Event.date,1,0))

#rename M10...Number.of.Patients.with.AF.who.have.a.HAS.BLED.Assessment.Documented.and.with.Read.Code.XaY6z..Patient.ID
#col in df9merge to Patient.ID
df9merge<-rename(df9merge,Patient.ID="M10...Number.of.Patients.with.AF.who.have.a.HAS.BLED.Assessment.Documented.and.with.Read.Code.XaY6z..Patient.ID")

#left join patient id and event date from df2merge into df9merge
df9merge<-left_join(df9merge,select(.data=df2merge,Patient.ID, Event.date),by="Patient.ID",copy = FALSE)

#create new col in df9merge to check if af diagnosis event date is before hasbled date in 
#column M10a...Number.of.AF.patients.with.HAS.BLED.Assessment.Documented..Event.date
df9merge$afeventdatebeforehasbled<-if_else(df9merge$Event.date<=
                                               df9merge$M10a...Number.of.AF.patients.with.HAS.BLED.Assessment.Documented..Event.date,
                                             df9merge$M10a...Number.of.AF.patients.with.HAS.BLED.Assessment.Documented..Event.date,as.Date(NA,"%Y-%m-%d"))

#create df9merge_subset where there are no NAs for afeventdatebeforehasbled column
df9merge_subset<-df9merge[complete.cases(df9merge$afeventdatebeforehasbled),]

#group by patient id and retain earliest hasbled date
df9merge_subset<-df9merge_subset %>% 
  group_by(Patient.ID) %>% 
  slice(which.min(afeventdatebeforehasbled))

#remove event.date col, "HASBLED.eventdate.test" col and  "afeventdatebeforehasbled" col   
df9merge_subset<-df9merge_subset[,-c(1:6,17:19)]

#left_join df9merge_subset to df2merge
df2merge<-left_join(x=df2merge,y=df9merge_subset,by="Patient.ID",copy = FALSE)



#assign data_frames[[10]] to df10
df10<-data_frames[[10]]

#assign df10 to df10merge
df10merge<-df10

#rename M12...Number.of.Patients.with.AF.who.have.a.CHAD2DS2.VAc.Score.over.the.threshold.and.HAS.BLED.Score.under.Threshold...Patient.ID
#col in df10merge to Patient.ID
df10merge<-rename(df10merge,Patient.ID="M12...Number.of.Patients.with.AF.who.have.a.CHAD2DS2.VAc.Score.over.the.threshold.and.HAS.BLED.Score.under.Threshold...Patient.ID")

#left join patient id and earliest.chadsvasc.date from df2merge into df10merge
df10merge<-left_join(df10merge,select(.data=df2merge,Patient.ID,earliest.chadsvasc.date),by="Patient.ID",copy = FALSE)

#create new col in df10merge to check if M12b...WMUH.Number.of.Newly.Diagnosed.AF.Patients.with.CHAD2DS2.VAc.Score.over.the.threshold.and.HAS.BLED.Score.under.Threshold..Females...Event.date
#from df10merge is same as the merged in earliest.chadsvasc.date
#returning back chadsvasc date if true
df10merge$female.chadsvasc.event.date.same.as.earliest.chadsvasc.date<-if_else(df10merge$M12b...WMUH.Number.of.Newly.Diagnosed.AF.Patients.with.CHAD2DS2.VAc.Score.over.the.threshold.and.HAS.BLED.Score.under.Threshold..Females...Event.date==
                                                                                 df10merge$earliest.chadsvasc.date,df10merge$M12b...WMUH.Number.of.Newly.Diagnosed.AF.Patients.with.CHAD2DS2.VAc.Score.over.the.threshold.and.HAS.BLED.Score.under.Threshold..Females...Event.date,as.Date(NA,"%Y-%m-%d"))

#create new col in df10merge to check if M12a...WMUH.Number.of.Newly.Diagnosed.AF.Patients.with.CHAD2DS2.VAc.Score.over.the.threshold.and.HAS.BLED.Score.under.Threshold..Males...Event.date
#from df10merge is same as the merged in earliest.chadsvasc.date
#returning back chadsvasc date if true
df10merge$male.chadsvasc.event.date.same.as.earliest.chadsvasc.date<-if_else(df10merge$M12a...WMUH.Number.of.Newly.Diagnosed.AF.Patients.with.CHAD2DS2.VAc.Score.over.the.threshold.and.HAS.BLED.Score.under.Threshold..Males...Event.date==
                                                                                 df10merge$earliest.chadsvasc.date,df10merge$M12a...WMUH.Number.of.Newly.Diagnosed.AF.Patients.with.CHAD2DS2.VAc.Score.over.the.threshold.and.HAS.BLED.Score.under.Threshold..Males...Event.date,as.Date(NA,"%Y-%m-%d"))

#create a new column which contains (male or female) chadsvasc dates for the eligible population (i.e.eligible population is af patients with chadsvasc over teh threshold and hasbled under the threshold)
df10merge<-df10merge%>%
  mutate(earliest.chadsvasc.eligible.pop=pmin(female.chadsvasc.event.date.same.as.earliest.chadsvasc.date,male.chadsvasc.event.date.same.as.earliest.chadsvasc.date,na.rm = TRUE))

#create df10merge_subset where there are no NAs for earliest.chadsvasc.eligible.pop column
df10merge_subset<-df10merge[complete.cases(df10merge$earliest.chadsvasc.eligible.pop),]

#group by patient id and retain earliest chadsvasc event date in the eligible population 
df10merge_subset<-df10merge_subset %>% 
  group_by(Patient.ID) %>% 
  slice(which.min(earliest.chadsvasc.eligible.pop))%>%
  ungroup()

#remove earliest.chadsvasc.date, female.chadsvasc.event.date.same.as.earliest.chadsvasc.date, 
#male.chadsvasc.event.date.same.as.earliest.chadsvasc.date from df10merge-subset
df10merge_subset<-df10merge_subset[,-c(1:4,21:24)]


#left_join df10merge_subset to df2merge
df2merge<-left_join(x=df2merge,y=df10merge_subset,by="Patient.ID",copy = FALSE)


#assign data_frames[[11]] to df11
df11<-data_frames[[11]]

#assign df11 to df11merge
df11merge<-df11

#rename event.date col in df11 merge to anticoagulation.event.date
df11merge<-rename(df11merge,anticoagulation.event.date="Event.date")

#left join patient id and event date from df2merge into df11merge
df11merge<-left_join(df11merge,select(.data=df2merge,Patient.ID,Event.date),by="Patient.ID",copy = FALSE)

#create a new col in df11merge to see if issue start.date is same day or after af diagnosis date, if so return the
#issue start date
df11merge$anticoagulation.issue.start.date.sameday.or.after.af.diagnosis.date<-if_else(df11merge$Issue.start.date>=df11merge$Event.date,df11merge$Issue.start.date,as.Date(NA,"%Y-%m-%d"))

#create a subset of df11merge without NAs in the issue.start.date.sameday.or.after.af.diagnosis.date col
df11merge_subset<-df11merge[complete.cases(df11merge$anticoagulation.issue.start.date.sameday.or.after.af.diagnosis.date),]

#show for a unique patient 
#earliest issue start date, that is same day or after af event/diagnosis date
df11merge_subset<-df11merge_subset %>% 
  group_by(Patient.ID) %>% 
  slice(which.min(anticoagulation.issue.start.date.sameday.or.after.af.diagnosis.date))%>%
  ungroup()

#rename anticoagulation.issue.start.date.sameday.or.after.af.diagnosis.date to earliest.anticoagulation.issue.start.date.sameday.or.after.af.diagnosis.date
df11merge_subset<-rename(df11merge_subset,earliest.anticoagulation.issue.start.date.sameday.or.after.af.diagnosis.date="anticoagulation.issue.start.date.sameday.or.after.af.diagnosis.date")

#del cols in df11merge_subset i.e event.date
df11merge_subset<-df11merge_subset[,-c(12:17,19:20)]

#rename Event.done.at col in df11merge_subset
df11merge_subset<-rename(df11merge_subset,Anticoagulation.Event.done.at="Event.done.at")

#rename Event.done.at.ID col in df11merge_subset
df11merge_subset<-rename(df11merge_subset,Anticoagulation.Event.done.at.ID="Event.done.at.ID")

#leftjoin df11merge_subset into df2merge by patient id
df2merge<-left_join(x=df2merge,y=df11merge_subset,by="Patient.ID",copy = FALSE)


#assign df11 to df11merge2
df11merge2<-df11

#rename event.date col in df11merge2 to anticoagulation.event.date
df11merge2<-rename(df11merge2,anticoagulation.event.date="Event.date")

#left join patient id and event date from df2merge into df11merge2
df11merge2<-left_join(df11merge2,select(.data=df2merge,Patient.ID,Event.date),by="Patient.ID",copy = FALSE)

#create a new col in df11merge2 to see if anticoagulation.issue start date is before the af diagnosis/event date, if so return the
#anticoagulation issue start date 
df11merge2$anticoagulation.issue.start.date.before.af.diagnosis.date<-if_else(df11merge2$Issue.start.date<df11merge2$Event.date,df11merge2$Issue.start.date,as.Date(NA,"%Y-%m-%d"))

#show df11merge2 without NAs in the anticoagulation.issue.start.date.before.af.diagnosis.date col and assign to df11merge2_subset
df11merge2_subset<-df11merge2[complete.cases(df11merge2$anticoagulation.issue.start.date.before.af.diagnosis.date),]

#in df11merge2_subset show for a unique patient 
#latest anticoagulation event date before af diagnosis/event date
df11merge2_subset<-df11merge2_subset %>% 
  group_by(Patient.ID) %>% 
  slice(which.max(anticoagulation.issue.start.date.before.af.diagnosis.date))%>%
  ungroup()

#rename anticoagulation.issue.start.date.before.af.diagnosis.date to latest.anticoagulation.issue.start.date.before.af.diagnosis.date
df11merge2_subset<-rename(df11merge2_subset,latest.anticoagulation.issue.start.date.before.af.diagnosis.date="anticoagulation.issue.start.date.before.af.diagnosis.date")

#del cols in df11merge2_subset i.e event.date
df11merge2_subset<-df11merge2_subset[,-c(12:17,19:20)]

#rename the following cols in df11merge2_subset
df11merge2_subset<-rename(df11merge2_subset,Anticoagulation.Event.done.at2="Event.done.at")

df11merge2_subset<-rename(df11merge2_subset,Anticoagulation.Event.done.at.ID2="Event.done.at.ID")

df11merge2_subset<-rename(df11merge2_subset,anticoagulation.event.date.2="anticoagulation.event.date")

df11merge2_subset<-rename(df11merge2_subset,Drug2="Drug")

df11merge2_subset<-rename(df11merge2_subset,Drug.ID2="Drug.ID")

df11merge2_subset<-rename(df11merge2_subset,Issue.duration2="Issue.duration")

df11merge2_subset<-rename(df11merge2_subset,Issue.end.date2="Issue.end.date")

df11merge2_subset<-rename(df11merge2_subset,Issue.start.date2="Issue.start.date")

df11merge2_subset<-rename(df11merge2_subset,Medication.type2="Medication.type")

df11merge2_subset<-rename(df11merge2_subset,Product.Type2="Product.Type")

df11merge2_subset<-rename(df11merge2_subset,Repeat.drug2="Repeat.drug")

#leftjoin df11merge2_subset into df2merge by patient ID
df2merge<-left_join(x=df2merge,y=df11merge2_subset,by="Patient.ID",copy = FALSE)



#assign data_frames[[12]] to df12
df12<-data_frames[[12]]

#assign df12 to df12merge
df12merge<-df12

#rename patient id col in df12 merge 
df12merge<-rename(df12merge,Patient.ID="M16...Number.of.patients.with.AF.Prescribed.Aspirin.but.without.Clopidogrel..Patient.ID")

#left join patient id and event date from df2merge into df12merge
df12merge<-left_join(df12merge,select(.data=df2merge,Patient.ID,Event.date),by="Patient.ID",copy = FALSE)

#create a new col in df12merge to see if antiplatelet.issue.start.date.sameday.as.or.after.af.diagnosis.date, if so return the
#antiplatelet issue.start. date
df12merge$antiplatelet.issue.start.date.sameday.as.or.after.af.diagnosis.date<-if_else(df12merge$M16a.Diagnosed.AF.Patients.Prescribed.with.Antiplatelet.Drug..Issue.start.date>=df12merge$Event.date,df12merge$M16a.Diagnosed.AF.Patients.Prescribed.with.Antiplatelet.Drug..Issue.start.date,as.Date(NA,"%Y-%m-%d"))

#show df12merge without NAs in the antiplatelet.issue.start.date.sameday.as.or.after.af.diagnosis.date col
df12merge_subset<-df12merge[complete.cases(df12merge$antiplatelet.issue.start.date.sameday.as.or.after.af.diagnosis.date),]

#for a unique patient show
#earliest antiplatelet issue start date, that is same day or after af event/diagnosis date
df12merge_subset<-df12merge_subset %>% 
  group_by(Patient.ID) %>% 
  slice(which.min(antiplatelet.issue.start.date.sameday.as.or.after.af.diagnosis.date))%>%
  ungroup()

#rename antiplatelet.issue.start.date.sameday.as.or.after.af.diagnosis.date to earliest.antiplatelet.issue.start.date.sameday.as.or.after.af.diagnosis.date
df12merge_subset<-rename(df12merge_subset,earliest.antiplatelet.issue.start.date.sameday.as.or.after.af.diagnosis.date="antiplatelet.issue.start.date.sameday.as.or.after.af.diagnosis.date")


#del cols in df12merge_subset i.e event.date,   
df12merge_subset<-df12merge_subset[,-c(1:3,16:17)]

#leftjoin df12merge_subset into df2merge by patient id
df2merge<-left_join(x=df2merge,y=df12merge_subset,by="Patient.ID",copy = FALSE)

#note for df4 and df5 below, if we are provided with more than just the most recent read code delete the code  
#below and left joing the age col from df2merge into the the separate data frames

#assign data_frames[[4]] to df4
df4<-data_frames[[4]]

#assign df4 to df4merge
df4merge<-df4

#for a unique patient show
#latest event.date i.e. most recent ecg i.e. a.	The read codes options are
#i.	Electrocardiography (32..)
#ii.	ECG interpretation (XM0AE)
#iii.	ECG: presence findings (Xa7s8)

df4merge_subset<-df4merge %>% 
  group_by(Patient.ID) %>% 
  slice(which.max(Event.date))%>%
  ungroup()

#rename Event.date to afatriskecgeventdate
df4merge_subset<-rename(df4merge_subset,afatriskecgeventdate="Event.date")

#del cols in df4merge_subset    
df4merge_subset<-df4merge_subset[,-c(1,3:10,12)]

#leftjoin df4merge_subset into df2merge by patient id
df2merge<-left_join(x=df2merge,y=df4merge_subset,by="Patient.ID",copy = FALSE)

#assign data_frames[[5]] to df5
df5<-data_frames[[5]]

#assign df5 to df5merge
df5merge<-df5

#for a unique patient show
#latest event.date i.e. most recent ecg i.e. a.	The read codes options are
#i.	Electrocardiography (32..)
#ii.	ECG interpretation (XM0AE)
#iii.	ECG: presence findings (Xa7s8)

df5merge_subset<-df5merge %>% 
  group_by(Patient.ID) %>% 
  slice(which.max(Event.date))%>%
  ungroup()

#rename Event.date to populationecgeventdate
df5merge_subset<-rename(df5merge_subset,populationecgeventdate="Event.date")

#del cols in df5merge_subset    
df5merge_subset<-df5merge_subset[,-c(1,3:10,12)]

#leftjoin df5merge_subset into df2merge by patient id
df2merge<-left_join(x=df2merge,y=df5merge_subset,by="Patient.ID",copy = FALSE)