#data_frames[[2]] shows patients on practice list registered before date data was run
#assign data_frames[[2]] to df2 
df2<-data_frames[[2]]

#Remove duplicate patient ids in df2 to retain the earliest registration date
df2<-df2 %>% group_by(Patient.ID) %>% 
  slice(which.min(Registration.date))

#column names of df2
#[1] "Age"                    "Ethnicity"              "Sex"                    "SOA..lower.layer."     
#[5] "SOA..middle.layer."     "Deduction.date"         "Registered.CCG"         "Registered.practice"   
#[9] "Registered.practice.ID" "Registration.date"      "Registration.status"    "Patient.ID"            
#[13] "Patient.Count"          "RecodedSex"     

#data_frames[[2]] shows patients on practice list registered before date data was run
#assign data_frames[[2]] to df2a 
#df2a<-data_frames[[2]]

#Remove duplicate patient ids in df2a to retain the latest deduction date
#but retain NA if thats the latest???
#Deduction.date.mergein <- df2a %>% group_by(Patient.ID) %>% summarise(max_ded = max(Deduction.date))

#rename deduction.date col in df2a to Deduction.date.max
#Deduction.date.mergein<-rename(Deduction.date.mergein,Deduction.date.max="Deduction.date")
  
#left join in only the deduction.date.max col from df2a note all the upcoming deduction.date.cols
#have not been compared with deduction.date.max
#df2merge<-df2 %>% left_join(select(.data=Deduction.date.mergein,Patient.ID, Deduction.date.max),by="Patient.ID",copy = FALSE)


#ignore data_frames[[1]]

#...
#data_frames[[3]] shows snapshot of patients on the AF at risk register
#assign data_frames[[3]] to df3
df3<-data_frames[[3]]

#Remove duplicate patient ids in df3 to retain the earliest registration date
df3<-df3 %>% group_by(M02a...Number.of.Patients.on.the.Atrial.Fibrillation..AF..At.Risk.Register..Patient.ID) %>% 
  slice(which.min(Registration.date))

#colnames which are present in both df3 and df2 (numbers correspond to colnames in df3)
#[1] "Deduction.date"                                                                        
#[2] "Registered.CCG"                                                                        
#[3] "Registered.practice"                                                                   
#[4] "Registered.practice.ID"                                                                
#[5] "Registration.date"                                                                     
#[6] "Registration.status"                                                                   
#[14] "Patient.Count"

#rename"M02a...Number.of.Patients.on.the.Atrial.Fibrillation..AF..At.Risk.Register..Patient.ID" col in df3 
#to "Patient.ID", although colname different it refers to same thing
df3<-rename(df3,Patient.ID ="M02a...Number.of.Patients.on.the.Atrial.Fibrillation..AF..At.Risk.Register..Patient.ID")


#leftjoin all cols in df3 to df2merge 
#note in df2merge some patients who were previously on the af at risk register may not be identified as df3 is only
#a snapshot
df2merge<-left_join(x=df2,y=df3,by="Patient.ID",copy = FALSE)

#create a Subset_df2merge which shows rows for only those on the at risk register
#(AF0011.High.Risk.Atrial.Fibrillation.Patients.with.LTCs..Patient.ID is the column of interest)
Subset_df2merge<-df2merge[complete.cases(df2merge$AF0011.High.Risk.Atrial.Fibrillation.Patients.with.LTCs..Patient.ID),]

#create new columns to test if duplicated columns are identical , 1 = true , 0 =false
Subset_df2merge$Deduction.date.test<-as.integer(ifelse(Subset_df2merge$Deduction.date.x
                                            ==Subset_df2merge$Deduction.date.y,1,0))


Subset_df2merge$Registered.CCG.test<-as.integer(ifelse(Subset_df2merge$Registered.CCG.x
                                            ==Subset_df2merge$Registered.CCG.y,1,0))


Subset_df2merge$Registered.practice.test<-as.integer(ifelse(Subset_df2merge$Registered.practice.x
                                            ==Subset_df2merge$Registered.practice.y,1,0))


Subset_df2merge$Registered.practice.ID.test<-as.integer(ifelse(Subset_df2merge$Registered.practice.ID.x
                                            ==Subset_df2merge$Registered.practice.ID.y,1,0))


Subset_df2merge$Registration.date.test<-as.integer(ifelse(Subset_df2merge$Registration.date.x
                                            ==Subset_df2merge$Registration.date.y,1,0))


Subset_df2merge$Registration.status.test<-as.integer(ifelse(Subset_df2merge$Registration.status.x
                                            ==Subset_df2merge$Registration.status.y,1,0))


Subset_df2merge$Patient.count.test<-as.integer(ifelse(Subset_df2merge$Patient.Count.x
                                            ==Subset_df2merge$Patient.Count.y,1,0))


#delete duplicated columns that are identical in df2merge and keep duplicate columns that are not identical
#so information is not lost
#remove Deduction.date.y column as it is all NAs [15]
#remove Registered.CCG.y column [16]
#remove Registered.practice.y column [17]
#remove Registered.practice.ID.y column [18]
#remove Registration.date.y column (df3) [19] as although not identical to Registration.date.x (df2) dates 
#in df2 are all earlier than in df2 
#remove Patient.count.y column [27]

df2merge<-df2merge[,-c(15:19,27)]


#registration.status
#when i subset to find out all the instances where this duplicated column is not identical, it tends to because 
#df2 has an earlier registration date than df3
#and instances where df2 and df3 have the same registration date
#i checked the excel files and it seems in df2,the person was duplicated and 
#the row selectedjust happened not to be the one that matched to the df3 file
#at first i thought i could create a new Registration status column which keeps registration status 
#from df2 if not on the at risk registerand if on the register it pulls the registration status from df3
#however still the df2 registrations if not on the risk register would still be incorrect if duplicated as 
#our function pulls the earliest registration date

#rename registration.status.y in df2merge to registration.status.df3
df2merge<-rename(df2merge,Registration.status.df3 ="Registration.status.y")

#...
#data_frames[[6]] shows patients diagnosed with Atrial Fibrillation before date data was run
#assign data_frames[[6]] to df6 
df6<-data_frames[[6]]

#Remove duplicate patient ids in df6 to retain the earliest registration date
df6<-df6 %>% group_by(Patient.ID) %>% 
  slice(which.min(Registration.date))

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

#leftjoin all cols in df6 to df2merge 
#note in the merge there will be lots of NAs as not all patients have an AF diagnosis
#also some of the duplicated colnames do not have a .y at the end of the names as in before this new merge 
#the duplicated cols had .x beside their name
df2merge<-left_join(x=df2merge,y=df6,by="Patient.ID",copy = FALSE)

#create Subset_df2merge  which shows only rows 
#where event.date is not equal to NA
#this limits the data to only those diagnosed with AF
Subset_df2merge<-df2merge[complete.cases(df2merge$Event.date),]

#create new columns to test if duplicated columns are identical , 1 = true , 0 =false

Subset_df2merge$Age.test<-as.integer(ifelse(Subset_df2merge$Age.x
                                            ==Subset_df2merge$Age.y,1,0))


Subset_df2merge$Ethnicity.test<-as.integer(ifelse(Subset_df2merge$Ethnicity.x
                                 ==Subset_df2merge$Ethnicity.y,1,0))


Subset_df2merge$Sex.test<-as.integer(ifelse(Subset_df2merge$Sex.x
                                 ==Subset_df2merge$Sex.y,1,0))


Subset_df2merge$SOA..lower.layer..test<-as.integer(ifelse(Subset_df2merge$SOA..lower.layer..x
                                 ==Subset_df2merge$SOA..lower.layer..y,1,0))


Subset_df2merge$SOA..middle.layer..test<-as.integer(ifelse(Subset_df2merge$SOA..middle.layer..x
                                 ==Subset_df2merge$SOA..middle.layer..y,1,0))


Subset_df2merge$Deduction.date.test<-as.integer(ifelse(Subset_df2merge$Deduction.date.x
                                            ==Subset_df2merge$Deduction.date,1,0))


Subset_df2merge$Registered.CCG.test<-as.integer(ifelse(Subset_df2merge$Registered.CCG.x
                                            ==Subset_df2merge$Registered.CCG,1,0))


Subset_df2merge$Registered.practice.test<-as.integer(ifelse(Subset_df2merge$Registered.practice.x
                                                 ==Subset_df2merge$Registered.practice,1,0))


Subset_df2merge$Registered.practice.ID.test<-as.integer(ifelse(Subset_df2merge$Registered.practice.ID.x
                                                    ==Subset_df2merge$Registered.practice.ID,1,0))


Subset_df2merge$Registration.date.test<-as.integer(ifelse(Subset_df2merge$Registration.date.x
                                               ==Subset_df2merge$Registration.date,1,0))


Subset_df2merge$Registration.status.test<-as.integer(ifelse(Subset_df2merge$Registration.status.x
                                                 ==Subset_df2merge$Registration.status,1,0))

#this is to compare registration.status in that came from df3 with that that came from df6
Subset_df2merge$Registration.status.test2<-as.integer(ifelse(Subset_df2merge$Registration.status.df3
                                                 ==Subset_df2merge$Registration.status,1,0))


Subset_df2merge$Patient.count.test<-as.integer(ifelse(Subset_df2merge$Patient.Count.x
                                           ==Subset_df2merge$Patient.Count,1,0))


Subset_df2merge$RecodedSex.test<-as.integer(ifelse(droplevels(Subset_df2merge$RecodedSex.x)
                                                   ==Subset_df2merge$RecodedSex.y,1,0))


#delete duplicated columns that are identical in df2merge and keep duplicate columns that are not identical
#so information is not lost
#remove Age.y [23]
#remove Sex.y [25]
#remove Registered.CCG column [32]
#remove Registered.practice column [33]
#remove Registered.practice.ID column [34]
#remove Registration.date column [35] as although not identical to Registration.date.x, 
#dates in df2 are all earlier than or equal to dates in df6
#remove RecodedSex.y [38]
#remove Patient.Count column [37]

df2merge<-df2merge[,-c(23,25,32:35,37,38)]

#in 67 rows Ethnicity.y is not identical to Ethnicity.x (note test were done by subsetting)
#eg.test<-subset(Subset_df2merge,Subset_df2merge$Ethnicity.test==0)
#looking at excel files discrepancy still exists, it seems new coding of ethnicity may be causing the issue
#rename Ethnicity.y to Ethnicity.df6
df2merge<-rename(df2merge,Ethnicity.df6="Ethnicity.y")

#in 61 rows SOA..lower.layer..x is not identical to SOA..lower.layer..y
#looking at excel files discrepancy still exists, maybe this is due to movement of patient to different address
df2merge<-rename(df2merge,SOA..lower.layer..df6="SOA..lower.layer..y")

#in 57 rows SOA..middle.layer..x is not identical to SOA..middle.layer..y
#looking at excel files discrepancy still exists, maybe this is due to movement of patient to different address
df2merge<-rename(df2merge,SOA..middle.layer..df6="SOA..middle.layer..y")

#in 43 rows Deduction.date is not identical to Deduction.date.x
#looking at excel files same discrepancy exists, i would like to delete Deduction.date as is all but one row
#these the dates are after the Deduction.date.x
df2merge<-rename(df2merge,Deduction.date.df6="Deduction.date")

#in 111 rows Registration.status is not identical to Registration.status.x
#discrepancy still occurs in excel files, same issue as above with df3 file merge
df2merge<-rename(df2merge,Registration.status.df6="Registration.status")

#...
#data_frames[[8]] shows patients  with AF who have a either a CHAD2DS2-VAc or CHADS2 Assessment Documented
#assign data_frames[[8]] to df8
df8<-data_frames[[8]]

#make a df8 duplicate called df8 duplicate
df8duplicate<-df8

#check if the two CHADSVASC eventdate columns in df8duplicate are identical
df8duplicate$Event.date.test<-as.integer(ifelse(df8duplicate$M07a...Number.of.AF.Patients.with.CHAD2DS2.VAc.Assessment.Documented..Event.date
                                                ==df8duplicate$M07aa...Number.of.AF.Patients.with.Read.code.XaY6i..Event.date,1,0))

#create test to look at 3 instances where CHADSVASC eventdate columns not identical
#in the 3 cases where not identical, M07a...Number.of.AF.Patients.with.CHAD2DS2.VAc.Assessment.Documented..Event.date
#is earlier than M07aa...Number.of.AF.Patients.with.Read.code.XaY6i..Event.date
test<-subset(df8duplicate,
             df8duplicate$M07a...Number.of.AF.Patients.with.CHAD2DS2.VAc.Assessment.Documented..Event.date!=
               df8duplicate$M07aa...Number.of.AF.Patients.with.Read.code.XaY6i..Event.date)

#create test 2 to check how many unique 'event dates'M07a...Number.of.AF.Patients.with.CHAD2DS2.VAc.Assessment.Documented..Event.date'
#for the same patient ID.'M08a...Number.of.Patients.with.AF.who.have.a.either.a.CHAD2DS2.VAc.or.CHADS2.Assessment.Documented.or.both..Patient.ID' 
#column used for patient ID
test2<-df8duplicate %>% group_by(M08a...Number.of.Patients.with.AF.who.have.a.either.a.CHAD2DS2.VAc.or.CHADS2.Assessment.Documented.or.both..Patient.ID) %>%
  summarise(length(unique(M07a...Number.of.AF.Patients.with.CHAD2DS2.VAc.Assessment.Documented..Event.date)))

#assign df8 to df8merge
df8merge<-df8

#leftjoin patient ID column and AF diagnosis date column in df2merge to df8merge
df8merge<- df8merge %>% rename(Patient.ID="M08a...Number.of.Patients.with.AF.who.have.a.either.a.CHAD2DS2.VAc.or.CHADS2.Assessment.Documented.or.both..Patient.ID")%>%
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
df8merge_subset<-subset(df8merge,(!is.na(df8merge[,23]))|(!is.na(df8merge[,24])))


#create column in df8merge_subset containing the earliest CHADSVASC date that is on same day or comes after the AF event date
#note this column takes both chadsvasc columns into consideration 
df8merge_subset<-df8merge_subset%>%
  mutate(earliest.chadsvasc.date = pmin(afeventdatebeforeorsamedayaschadsvasc,afeventdatebeforeorsamedayaschadsvasc2,na.rm = TRUE))

#remove duplicates from df8merge_subset by selecting earliest chadsvasc  date for same patient id
df8merge_subset<-df8merge_subset %>% group_by(Patient.ID) %>% 
  slice(which.min(earliest.chadsvasc.date))

#remove event.date,  afeventdatebeforechadsvasc and afeventdatebeforechadsvasc2 columns in df8merge_subset
df8merge_subset<-df8merge_subset[,-c(22:24)]

#left_join df8merge_subset to df2merge
df2merge<-left_join(x=df2merge,y=df8merge_subset,by="Patient.ID",copy = FALSE)


#create a Subset_df2merge which shows only rows 
#where earliest.chadsvasc.date is not equal to NA
#this limits the data to only those who have had a chadsvasc done
Subset_df2merge<-df2merge[complete.cases(df2merge$earliest.chadsvasc.date),]


#create new columns to test if merged in duplicated columns from df8merge_subset are identical to those
#already in Subset_df2merge,true =identical , false = not identical
#duplicated cols from df8merge_subset are:
#"Deduction.date"                                                                        
#"Registered.CCG"                                                                        
#"Registered.practice.ID"                                                                
#"Registration.status"  
#Patient.count

#these columns are not identical
Subset_df2merge$Deduction.date.test<-Subset_df2merge$Deduction.date.x==Subset_df2merge$Deduction.date

#these columns are not identical
Subset_df2merge$Deduction.date.test2<-Subset_df2merge$Deduction.date.df6==Subset_df2merge$Deduction.date

#these columns are identical
Subset_df2merge$Registered.CCG.test<-Subset_df2merge$Registered.CCG.x==Subset_df2merge$Registered.CCG

#these columns are identical
Subset_df2merge$Registered.practice.ID.test<-Subset_df2merge$Registered.practice.ID.x==Subset_df2merge$Registered.practice.ID

#these columns are not identical
Subset_df2merge$Registration.status.test<-Subset_df2merge$Registration.status.x==Subset_df2merge$Registration.status

#these columns are not identical
Subset_df2merge$Registration.status.test2<-Subset_df2merge$Registration.status.df3==Subset_df2merge$Registration.status

#these columns are not identical
Subset_df2merge$Registration.status.test3<-Subset_df2merge$Registration.status.df6==Subset_df2merge$Registration.status

#these columns are identical
Subset_df2merge$Patient.Count.test<-Subset_df2merge$Patient.Count.x==Subset_df2merge$Patient.Count

#rename non-identical duplicated columns
df2merge<-rename(df2merge,Deduction.date.df8="Deduction.date")

df2merge<-rename(df2merge,Registration.status.df8="Registration.status")

#delete identical duplicated columns 
df2merge<-df2merge[,-c(32:33,50)]

#...

#make a df8 duplicate called df8duplicate2
df8duplicate2<-df8

#check if the two CHADS2 eventdate columns in df8duplicate2 are identical
df8duplicate2$Event.date.test<-as.integer(ifelse(df8duplicate2$M08bb...Number.of.AF.Patients.with.Read.code.XaP9J..Event.date
                                                ==df8duplicate2$M08ba...Number.of.AF.Patients.with.Cong.heart.fail.hypertens.age.diab.stoke.2.risk.score.Assessment.Documented..Event.date,1,0))

#create test3 to check why the chads2 cols are not identical
#in all 695 cases where the CHADS2 columns are not identical this is because 
#M08ba...Number.of.AF.Patients.with.Cong.heart.fail.hypertens.age.diab.stoke.2.risk.score.Assessment.Documented..Event.date
#has dates which are earlier than M08bb...Number.of.AF.Patients.with.Read.code.XaP9J..Event.date
test3<-subset(df8duplicate2,df8duplicate2$M08bb...Number.of.AF.Patients.with.Read.code.XaP9J..Event.date!=
                df8duplicate2$M08ba...Number.of.AF.Patients.with.Cong.heart.fail.hypertens.age.diab.stoke.2.risk.score.Assessment.Documented..Event.date)

test3$m08balessthanm08bb<-test3$M08ba...Number.of.AF.Patients.with.Cong.heart.fail.hypertens.age.diab.stoke.2.risk.score.Assessment.Documented..Event.date<test3$M08bb...Number.of.AF.Patients.with.Read.code.XaP9J..Event.date

table(test3$m08balessthanm08bb)

#...

#assign df8 to df8merge2
df8merge2<-df8

#leftjoin patient ID column and AF diagnosis date column in df2merge to df8merge2
df8merge2<- df8merge2 %>% rename(Patient.ID="M08a...Number.of.Patients.with.AF.who.have.a.either.a.CHAD2DS2.VAc.or.CHADS2.Assessment.Documented.or.both..Patient.ID")%>%
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
df8merge2_subset<-df8merge2_subset %>% group_by(Patient.ID) %>% 
  slice(which.min(earliest.chads2.date))

#remove event.date,  afeventdatebeforechads2 and afeventdatebeforeorsamedayaschads2col2 columns in df8merge2_subset
df8merge2_subset<-df8merge2_subset[,-c(22:24)]

#left_join df8merge2_subset to df2merge
df2merge<-left_join(x=df2merge,y=df8merge2_subset,by="Patient.ID",copy = FALSE)

#create Subset_df2merge which shows only rows 
#where earliest.chads2.date is not equal to NA
#this limits the data to only those who have had a chads2 done
Subset_df2merge<-df2merge[complete.cases(df2merge$earliest.chads2.date),]

#create new columns to test if merged in duplicated columns from df8merge_subset are identical to those
#already in Subset_df2merge,true =identical , false = not identical
#duplicated cols from df8merge2_subset are:
#Registered CCG
#Registered practice ID
#Patient Count
#columns starting M08bb/M08ba/m07a/M07aa are also duplicated

#these columns are identical
Subset_df2merge$Registered.CCG.test<-Subset_df2merge$Registered.CCG.x==Subset_df2merge$Registered.CCG

#these columns are identical
Subset_df2merge$Registered.practice.ID.test<-Subset_df2merge$Registered.practice.ID.x==Subset_df2merge$Registered.practice.ID

#these columns are identical
Subset_df2merge$Patient.Count.test<-Subset_df2merge$Patient.Count.x==Subset_df2merge$Patient.Count

#rename columns to know whether they came from the df8merge_subset (chadsvasc) merge in or 
#df8merge_subset (chads2) merge into df2merge
df2merge<-rename(df2merge,M08bb...Number.of.AF.Patients.with.Read.code.XaP9J..Read.code.earliest.chadsvasc.date=
"M08bb...Number.of.AF.Patients.with.Read.code.XaP9J..Read.code.x")                                                                   
df2merge<-rename(df2merge,M08bb...Number.of.AF.Patients.with.Read.code.XaP9J..Read.code.description.earliest.chadsvasc.date=
                 "M08bb...Number.of.AF.Patients.with.Read.code.XaP9J..Read.code.description.x")                                                       
df2merge<-rename(df2merge,M08bb...Number.of.AF.Patients.with.Read.code.XaP9J..Event.date.earliest.chadsvasc.date=
                   "M08bb...Number.of.AF.Patients.with.Read.code.XaP9J..Event.date.x")                                                                  
df2merge<-rename(df2merge,M08ba...Number.of.AF.Patients.with.Cong.heart.fail.hypertens.age.diab.stoke.2.risk.score.Assessment.Documented..Numeric.value.earliest.chadsvasc.date=
                   "M08ba...Number.of.AF.Patients.with.Cong.heart.fail.hypertens.age.diab.stoke.2.risk.score.Assessment.Documented..Numeric.value.x")   
df2merge<-rename(df2merge,M08ba...Number.of.AF.Patients.with.Cong.heart.fail.hypertens.age.diab.stoke.2.risk.score.Assessment.Documented..Read.code.earliest.chadsvasc.date=
                   "M08ba...Number.of.AF.Patients.with.Cong.heart.fail.hypertens.age.diab.stoke.2.risk.score.Assessment.Documented..Read.code.x")       
df2merge<-rename(df2merge,M08ba...Number.of.AF.Patients.with.Cong.heart.fail.hypertens.age.diab.stoke.2.risk.score.Assessment.Documented..Event.date.earliest.chadsvasc.date=
                   "M08ba...Number.of.AF.Patients.with.Cong.heart.fail.hypertens.age.diab.stoke.2.risk.score.Assessment.Documented..Event.date.x")      
df2merge<-rename(df2merge,M08ba...Number.of.AF.Patients.with.Cong.heart.fail.hypertens.age.diab.stoke.2.risk.score.Assessment.Documented..Event.done.at.ID.earliest.chadsvasc.date=
                   "M08ba...Number.of.AF.Patients.with.Cong.heart.fail.hypertens.age.diab.stoke.2.risk.score.Assessment.Documented..Event.done.at.ID.x")
df2merge<-rename(df2merge,M07a...Number.of.AF.Patients.with.CHAD2DS2.VAc.Assessment.Documented..Numeric.value.earliest.chadsvasc.date=
                   "M07a...Number.of.AF.Patients.with.CHAD2DS2.VAc.Assessment.Documented..Numeric.value.x")                                             
df2merge<-rename(df2merge,M07a...Number.of.AF.Patients.with.CHAD2DS2.VAc.Assessment.Documented..Read.code.earliest.chadsvasc.date=
                   "M07a...Number.of.AF.Patients.with.CHAD2DS2.VAc.Assessment.Documented..Read.code.x")                                                 
df2merge<-rename(df2merge, M07a...Number.of.AF.Patients.with.CHAD2DS2.VAc.Assessment.Documented..Event.date.earliest.chadsvasc.date=
                 "M07a...Number.of.AF.Patients.with.CHAD2DS2.VAc.Assessment.Documented..Event.date.x")                                                
 df2merge<-rename(df2merge,M07a...Number.of.AF.Patients.with.CHAD2DS2.VAc.Assessment.Documented..Event.done.at.ID.earliest.chadsvasc.date=
                    "M07a...Number.of.AF.Patients.with.CHAD2DS2.VAc.Assessment.Documented..Event.done.at.ID.x")                                          
df2merge<-rename(df2merge,M07aa...Number.of.AF.Patients.with.Read.code.XaY6i..Read.code.earliest.chadsvasc.date=
                 "M07aa...Number.of.AF.Patients.with.Read.code.XaY6i..Read.code.x")                                                                   
df2merge<-rename(df2merge,M07aa...Number.of.AF.Patients.with.Read.code.XaY6i..Read.code.description.earliest.chadsvasc.date=
                 "M07aa...Number.of.AF.Patients.with.Read.code.XaY6i..Read.code.description.x")                                                       
df2merge<-rename(df2merge,M07aa...Number.of.AF.Patients.with.Read.code.XaY6i..Event.date.earliest.chadsvasc.date=
                 "M07aa...Number.of.AF.Patients.with.Read.code.XaY6i..Event.date.x")                                                                  
 df2merge<-rename(df2merge, M07aa...Number.of.AF.Patients.with.Read.code.XaY6i..Event.done.at.ID.earliest.chadsvasc.date=
                  "M07aa...Number.of.AF.Patients.with.Read.code.XaY6i..Event.done.at.ID.x")                                                          

 
 df2merge<-rename(df2merge,M08bb...Number.of.AF.Patients.with.Read.code.XaP9J..Read.code.earliest.chads2.date=
                    "M08bb...Number.of.AF.Patients.with.Read.code.XaP9J..Read.code.y")                                                                   
 df2merge<-rename(df2merge,M08bb...Number.of.AF.Patients.with.Read.code.XaP9J..Read.code.description.earliest.chads2.date=
                    "M08bb...Number.of.AF.Patients.with.Read.code.XaP9J..Read.code.description.y")                                                       
 df2merge<-rename(df2merge,M08bb...Number.of.AF.Patients.with.Read.code.XaP9J..Event.date.earliest.chads2.date=
                    "M08bb...Number.of.AF.Patients.with.Read.code.XaP9J..Event.date.y")                                                                  
 df2merge<-rename(df2merge,M08ba...Number.of.AF.Patients.with.Cong.heart.fail.hypertens.age.diab.stoke.2.risk.score.Assessment.Documented..Numeric.value.earliest.chads2.date=
                    "M08ba...Number.of.AF.Patients.with.Cong.heart.fail.hypertens.age.diab.stoke.2.risk.score.Assessment.Documented..Numeric.value.y")   
 df2merge<-rename(df2merge,M08ba...Number.of.AF.Patients.with.Cong.heart.fail.hypertens.age.diab.stoke.2.risk.score.Assessment.Documented..Read.code.earliest.chads2.date=
                    "M08ba...Number.of.AF.Patients.with.Cong.heart.fail.hypertens.age.diab.stoke.2.risk.score.Assessment.Documented..Read.code.y")       
 df2merge<-rename(df2merge,M08ba...Number.of.AF.Patients.with.Cong.heart.fail.hypertens.age.diab.stoke.2.risk.score.Assessment.Documented..Event.date.earliest.chads2.date=
                    "M08ba...Number.of.AF.Patients.with.Cong.heart.fail.hypertens.age.diab.stoke.2.risk.score.Assessment.Documented..Event.date.y")      
 df2merge<-rename(df2merge,M08ba...Number.of.AF.Patients.with.Cong.heart.fail.hypertens.age.diab.stoke.2.risk.score.Assessment.Documented..Event.done.at.ID.earliest.chads2.date=
                    "M08ba...Number.of.AF.Patients.with.Cong.heart.fail.hypertens.age.diab.stoke.2.risk.score.Assessment.Documented..Event.done.at.ID.y")
 df2merge<-rename(df2merge,M07a...Number.of.AF.Patients.with.CHAD2DS2.VAc.Assessment.Documented..Numeric.value.earliest.chads2.date=
                    "M07a...Number.of.AF.Patients.with.CHAD2DS2.VAc.Assessment.Documented..Numeric.value.y")                                             
 df2merge<-rename(df2merge,M07a...Number.of.AF.Patients.with.CHAD2DS2.VAc.Assessment.Documented..Read.code.earliest.chads2.date=
                    "M07a...Number.of.AF.Patients.with.CHAD2DS2.VAc.Assessment.Documented..Read.code.y")                                                 
 df2merge<-rename(df2merge, M07a...Number.of.AF.Patients.with.CHAD2DS2.VAc.Assessment.Documented..Event.date.earliest.chads2.date=
                    "M07a...Number.of.AF.Patients.with.CHAD2DS2.VAc.Assessment.Documented..Event.date.y")                                                
 df2merge<-rename(df2merge,M07a...Number.of.AF.Patients.with.CHAD2DS2.VAc.Assessment.Documented..Event.done.at.ID.earliest.chads2.date=
                    "M07a...Number.of.AF.Patients.with.CHAD2DS2.VAc.Assessment.Documented..Event.done.at.ID.y")                                          
 df2merge<-rename(df2merge,M07aa...Number.of.AF.Patients.with.Read.code.XaY6i..Read.code.earliest.chads2.date=
                    "M07aa...Number.of.AF.Patients.with.Read.code.XaY6i..Read.code.y")                                                                   
 df2merge<-rename(df2merge,M07aa...Number.of.AF.Patients.with.Read.code.XaY6i..Read.code.description.earliest.chads2.date=
                    "M07aa...Number.of.AF.Patients.with.Read.code.XaY6i..Read.code.description.y")                                                       
 df2merge<-rename(df2merge,M07aa...Number.of.AF.Patients.with.Read.code.XaY6i..Event.date.earliest.chads2.date=
                    "M07aa...Number.of.AF.Patients.with.Read.code.XaY6i..Event.date.y")                                                                  
 df2merge<-rename(df2merge, M07aa...Number.of.AF.Patients.with.Read.code.XaY6i..Event.done.at.ID.earliest.chads2.date=
                    "M07aa...Number.of.AF.Patients.with.Read.code.XaY6i..Event.done.at.ID.y")  
 

 #these columns are not identical
 Subset_df2merge$Deduction.date.test<-Subset_df2merge$Deduction.date.x==Subset_df2merge$Deduction.date
 
 #these columns are not identical
 Subset_df2merge$Deduction.date.test2<-Subset_df2merge$Deduction.date.df6==Subset_df2merge$Deduction.date
 
 #these columns are not identical
 Subset_df2merge$Deduction.date.test3<-Subset_df2merge$Deduction.date.df8==Subset_df2merge$Deduction.date
 
 #these columns are not identical
 Subset_df2merge$Registration.status.test<-Subset_df2merge$Registration.status.x==Subset_df2merge$Registration.status
 
 #these columns are not identical
 Subset_df2merge$Registration.status.test2<-Subset_df2merge$Registration.status.df3==Subset_df2merge$Registration.status
 
 #these columns are not identical
 Subset_df2merge$Registration.status.test3<-Subset_df2merge$Registration.status.df6==Subset_df2merge$Registration.status
 
 #these columns are not identical
 Subset_df2merge$Registration.status.test4<-Subset_df2merge$Registration.status.df8==Subset_df2merge$Registration.status
 
 
 #rename non-identical duplicated columns
 df2merge<-rename(df2merge,Deduction.date.df8.earliest.chads2.date="Deduction.date")
 
 df2merge<-rename(df2merge,Registration.status.df8.earliest.chads2.date="Registration.status")
 
 
#delete identical duplicated columns 
df2merge<-df2merge[,-c(50,51,68)]

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
df9merge_subset<-df9merge_subset %>% group_by(Patient.ID) %>% 
  slice(which.min(afeventdatebeforehasbled))

#remove event.date col, "HASBLED.eventdate.test" col and  "afeventdatebeforehasbled" col   
df9merge_subset<-df9merge_subset[,-c(18:19)]

#left_join df9merge_subset to df2merge
df2merge<-left_join(x=df2merge,y=df9merge_subset,by="Patient.ID",copy = FALSE)

#create Subset_df2merge where there are no NAs in the afeventdatebeforehasbled column
Subset_df2merge<-df2merge[complete.cases(df2merge$afeventdatebeforehasbled),]

#create new columns to test if merged in duplicated columns from df9merge_subset are identical to those
#already in Subset_df2merge,true =identical , false = not identical
#duplicated cols from df9merge2_subset are:

#these columns are identical
Subset_df2merge$Registered.CCG.test<-Subset_df2merge$Registered.CCG.x==Subset_df2merge$Registered.CCG

#these columns are identical
Subset_df2merge$Registered.practice.test<-Subset_df2merge$Registered.practice.x==Subset_df2merge$Registered.practice

#these columns are identical
Subset_df2merge$Registered.practice.ID.test<-Subset_df2merge$Registered.practice.ID.x==Subset_df2merge$Registered.practice.ID

#these columns are identical
Subset_df2merge$Patient.Count.test<-Subset_df2merge$Patient.Count.x==Subset_df2merge$Patient.Count

#these columns are not identical
Subset_df2merge$Deduction.date.test<-Subset_df2merge$Deduction.date.x==Subset_df2merge$Deduction.date

#these columns are not identical
Subset_df2merge$Deduction.date.test2<-Subset_df2merge$Deduction.date.df6==Subset_df2merge$Deduction.date

#these columns are not identical
Subset_df2merge$Deduction.date.test3<-Subset_df2merge$Deduction.date.df8==Subset_df2merge$Deduction.date

#these columns are not identical
Subset_df2merge$Deduction.date.test4<-Subset_df2merge$Deduction.date.df8.earliest.chads2.date==Subset_df2merge$Deduction.date

#these columns are not identical
Subset_df2merge$Registration.status.test<-Subset_df2merge$Registration.status.x==Subset_df2merge$Registration.status

#these columns are not identical
Subset_df2merge$Registration.status.test2<-Subset_df2merge$Registration.status.df3==Subset_df2merge$Registration.status

#these columns are not identical
Subset_df2merge$Registration.status.test3<-Subset_df2merge$Registration.status.df6==Subset_df2merge$Registration.status

#these columns are not identical
Subset_df2merge$Registration.status.test4<-Subset_df2merge$Registration.status.df8==Subset_df2merge$Registration.status

#these columns are not identical
Subset_df2merge$Registration.status.test5<-Subset_df2merge$Registration.status.df8.earliest.chads2.date==Subset_df2merge$Registration.status

#these columns are not identical
Subset_df2merge$Registration.date.test<-Subset_df2merge$Registration.date.x==Subset_df2merge$Registration.date

#rename non-identical duplicated columns
df2merge<-rename(df2merge,Deduction.date.hasbled="Deduction.date")

df2merge<-rename(df2merge,Registration.status.hasbled="Registration.status")

df2merge<-rename(df2merge,Registration.date.hasbled="Registration.date")

#delete identical duplicated columns 
df2merge<-df2merge[,-c(68:70,82)]

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
df10merge_subset<-df10merge_subset %>% group_by(Patient.ID) %>% 
  slice(which.min(earliest.chadsvasc.eligible.pop))

#remove earliest.chadsvasc.date, female.chadsvasc.event.date.same.as.earliest.chadsvasc.date, 
#male.chadsvasc.event.date.same.as.earliest.chadsvasc.date from df10merge-subset
df10merge_subset<-df10merge_subset[,-c(22:24)]


#left_join df10merge_subset to df2merge
df2merge<-left_join(x=df2merge,y=df10merge_subset,by="Patient.ID",copy = FALSE)


#create Subset_df2merge where there are no NAs in the earliest.chadsvasc.eligible.pop column
Subset_df2merge<-df2merge[complete.cases(df2merge$earliest.chadsvasc.eligible.pop),]

#create new columns to test if merged in duplicated columns from df10merge_subset are identical to those
#already in Subset_df2merge,true =identical , false = not identical
#duplicated cols from df10merge2_subset are:
#"Deduction.date"                                                                                                                                             
#"Registered.practice.ID"                                                                                                                                     
#"Registration.date"                                                                                                                                          
#"Registration.status"
#Patient count

#these columns are identical
Subset_df2merge$Registered.practice.ID.test<-Subset_df2merge$Registered.practice.ID.x==Subset_df2merge$Registered.practice.ID

#these columns are identical
Subset_df2merge$Patient.Count.test<-Subset_df2merge$Patient.Count.x==Subset_df2merge$Patient.Count

#these columns are not identical
Subset_df2merge$Deduction.date.test<-Subset_df2merge$Deduction.date.x==Subset_df2merge$Deduction.date

#these columns are not identical
Subset_df2merge$Deduction.date.test2<-Subset_df2merge$Deduction.date.df6==Subset_df2merge$Deduction.date

#these columns are not identical
Subset_df2merge$Deduction.date.test3<-Subset_df2merge$Deduction.date.df8==Subset_df2merge$Deduction.date

#these columns are not identical
Subset_df2merge$Deduction.date.test4<-Subset_df2merge$Deduction.date.df8.earliest.chads2.date==Subset_df2merge$Deduction.date

#these columns are not identical
Subset_df2merge$Deduction.date.test5<-Subset_df2merge$Deduction.date.hasbled==Subset_df2merge$Deduction.date

#these columns are not identical
Subset_df2merge$Registration.status.test<-Subset_df2merge$Registration.status.x==Subset_df2merge$Registration.status

#these columns are not identical
Subset_df2merge$Registration.status.test2<-Subset_df2merge$Registration.status.df3==Subset_df2merge$Registration.status

#these columns are not identical
Subset_df2merge$Registration.status.test3<-Subset_df2merge$Registration.status.df6==Subset_df2merge$Registration.status

#these columns are not identical
Subset_df2merge$Registration.status.test4<-Subset_df2merge$Registration.status.df8==Subset_df2merge$Registration.status

#these columns are not identical
Subset_df2merge$Registration.status.test5<-Subset_df2merge$Registration.status.df8.earliest.chads2.date==Subset_df2merge$Registration.status

#these columns are not identical
Subset_df2merge$Registration.status.test6<-Subset_df2merge$Registration.status.hasbled==Subset_df2merge$Registration.status

#these columns are not identical
Subset_df2merge$Registration.date.test<-Subset_df2merge$Registration.date.x==Subset_df2merge$Registration.date

#these columns are not identical
Subset_df2merge$Registration.date.test2<-Subset_df2merge$Registration.date.hasbled==Subset_df2merge$Registration.date

#rename non-identical duplicated columns
df2merge<-rename(df2merge,Deduction.date.eligiblepop="Deduction.date")

df2merge<-rename(df2merge,Registration.status.eligiblepop="Registration.status")

df2merge<-rename(df2merge,Registration.date.eligiblepop="Registration.date")

#delete identical duplicated columns 
df2merge<-df2merge[,-c(81,99)]

# test to check that earliest hasbled date is same as that merged in from df10
#the merged in hasbled col is identical to the earliest hasbled date
Subset_df2merge$hasbledcolareidentical<-Subset_df2merge$afeventdatebeforehasbled==Subset_df2merge$M12c...Patients.with.HAS.BLED.Score.under.3.recorded..Event.date

# test to check that earliest chasvasc date is same as that merged in from df10
#the merged in chadsvasc col is identical to the earliest chadsvasc date
Subset_df2merge$chadsvasccolareidentical<-Subset_df2merge$earliest.chadsvasc.date==Subset_df2merge$earliest.chadsvasc.eligible.pop

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
df11merge_subset<-df11merge_subset %>% group_by(Patient.ID) %>% 
  slice(which.min(anticoagulation.issue.start.date.sameday.or.after.af.diagnosis.date))

#rename anticoagulation.issue.start.date.sameday.or.after.af.diagnosis.date to earliest.anticoagulation.issue.start.date.sameday.or.after.af.diagnosis.date
df11merge_subset<-rename(df11merge_subset,earliest.anticoagulation.issue.start.date.sameday.or.after.af.diagnosis.date="anticoagulation.issue.start.date.sameday.or.after.af.diagnosis.date")

#del cols in df11merge_subset i.e event.date
df11merge_subset<-df11merge_subset[,-c(20)]

#rename Event.done.at col in df11merge_subset
df11merge_subset<-rename(df11merge_subset,Anticoagulation.Event.done.at="Event.done.at")

#rename Event.done.at.ID col in df11merge_subset
df11merge_subset<-rename(df11merge_subset,Anticoagulation.Event.done.at.ID="Event.done.at.ID")

#leftjoin df11merge_subset into df2merge by patient id
df2merge<-left_join(x=df2merge,y=df11merge_subset,by="Patient.ID",copy = FALSE)

#create Subset_df2merge where there are no NAs in the earliest.anticoagulation.event.date.after.af.diagnosis.date column
Subset_df2merge<-df2merge[complete.cases(df2merge$earliest.anticoagulation.issue.start.date.sameday.or.after.af.diagnosis.date),]

#using Subset_df2merge , create new columns to test if merged in duplicated columns from df11merge_subset are identical to those
#already in df2merge,true =identical , false = not identical
#duplicated cols from df11merge_subset are:
#"Deduction.date"                                          
#"Registered.CCG"                                          
#"Registered.practice"                                     
#"Registered.practice.ID"                                  
#"Registration.date"                                       
#"Registration.status"                                     
#"Patient.Count"       

#these columns are identical
Subset_df2merge$Registered.practice.ID.test<-Subset_df2merge$Registered.practice.ID.x==Subset_df2merge$Registered.practice.ID

#these columns are identical
Subset_df2merge$Patient.Count.test<-Subset_df2merge$Patient.Count.x==Subset_df2merge$Patient.Count

#these columns are identical
Subset_df2merge$Registered.CCG.test<-Subset_df2merge$Registered.CCG.x==Subset_df2merge$Registered.CCG

#these columns are identical
Subset_df2merge$Registered.practice.test<-Subset_df2merge$Registered.practice.x==Subset_df2merge$Registered.practice



#these columns are not identical
Subset_df2merge$Deduction.date.test<-Subset_df2merge$Deduction.date.x==Subset_df2merge$Deduction.date

#these columns are not identical
Subset_df2merge$Deduction.date.test2<-Subset_df2merge$Deduction.date.df6==Subset_df2merge$Deduction.date

#these columns are not identical
Subset_df2merge$Deduction.date.test3<-Subset_df2merge$Deduction.date.df8==Subset_df2merge$Deduction.date

#these columns are not identical
Subset_df2merge$Deduction.date.test4<-Subset_df2merge$Deduction.date.df8.earliest.chads2.date==Subset_df2merge$Deduction.date

#these columns are not identical
Subset_df2merge$Deduction.date.test5<-Subset_df2merge$Deduction.date.hasbled==Subset_df2merge$Deduction.date

#these columns are not identical
Subset_df2merge$Deduction.date.test6<-Subset_df2merge$Deduction.date.eligiblepop==Subset_df2merge$Deduction.date


#these columns are not identical
Subset_df2merge$Registration.status.test<-Subset_df2merge$Registration.status.x==Subset_df2merge$Registration.status

#these columns are not identical
Subset_df2merge$Registration.status.test2<-Subset_df2merge$Registration.status.df3==Subset_df2merge$Registration.status

#these columns are not identical
Subset_df2merge$Registration.status.test3<-Subset_df2merge$Registration.status.df6==Subset_df2merge$Registration.status

#these columns are not identical
Subset_df2merge$Registration.status.test4<-Subset_df2merge$Registration.status.df8==Subset_df2merge$Registration.status

#these columns are not identical
Subset_df2merge$Registration.status.test5<-Subset_df2merge$Registration.status.df8.earliest.chads2.date==Subset_df2merge$Registration.status

#these columns are not identical
Subset_df2merge$Registration.status.test6<-Subset_df2merge$Registration.status.hasbled==Subset_df2merge$Registration.status

#these columns are not identical
Subset_df2merge$Registration.status.test7<-Subset_df2merge$Registration.status.eligiblepop==Subset_df2merge$Registration.status


#these columns are not identical
Subset_df2merge$Registration.date.test<-Subset_df2merge$Registration.date.x==Subset_df2merge$Registration.date

#these columns are not identical
Subset_df2merge$Registration.date.test2<-Subset_df2merge$Registration.date.hasbled==Subset_df2merge$Registration.date

#these columns are not identical
Subset_df2merge$Registration.date.test3<-Subset_df2merge$Registration.date.eligiblepop==Subset_df2merge$Registration.date

#rename non-identical duplicated columns
df2merge<-rename(df2merge,Deduction.date.AC="Deduction.date")

df2merge<-rename(df2merge,Registration.status.AC="Registration.status")

df2merge<-rename(df2merge,Registration.date.AC="Registration.date")

#delete identical duplicated columns in df2merge
df2merge<-df2merge[,-c(111:113,116)]



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
df11merge2_subset<-df11merge2_subset %>% group_by(Patient.ID) %>% 
  slice(which.max(anticoagulation.issue.start.date.before.af.diagnosis.date))

#rename anticoagulation.issue.start.date.before.af.diagnosis.date to latest.anticoagulation.issue.start.date.before.af.diagnosis.date
df11merge2_subset<-rename(df11merge2_subset,latest.anticoagulation.issue.start.date.before.af.diagnosis.date="anticoagulation.issue.start.date.before.af.diagnosis.date")

#del cols in df11merge2_subset i.e event.date
df11merge2_subset<-df11merge2_subset[,-c(20)]

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

#create Subset_df2merge where there are no NAs in the latest.anticoagulation.issue.start.date.before.af.diagnosis.date column
Subset_df2merge<-df2merge[complete.cases(df2merge$latest.anticoagulation.issue.start.date.before.af.diagnosis.date),]

#create new columns to test if merged in duplicated columns from df11merge2_subset are identical to those
#already in Subset_df2merge,true =identical , false = not identical
#duplicated cols from df11merge2_subset are:
#"Deduction.date"                                          
#"Registered.CCG"                                          
#"Registered.practice"                                     
#"Registered.practice.ID"                                  
#"Registration.date"                                       
#"Registration.status"                                     
#"Patient.Count"       

#these columns are identical
Subset_df2merge$Registered.practice.ID.test<-Subset_df2merge$Registered.practice.ID.x==Subset_df2merge$Registered.practice.ID

#these columns are identical
Subset_df2merge$Patient.Count.test<-Subset_df2merge$Patient.Count.x==Subset_df2merge$Patient.Count

#these columns are identical
Subset_df2merge$Registered.CCG.test<-Subset_df2merge$Registered.CCG.x==Subset_df2merge$Registered.CCG

#these columns are identical
Subset_df2merge$Registered.practice.test<-Subset_df2merge$Registered.practice.x==Subset_df2merge$Registered.practice



#these columns are not identical
Subset_df2merge$Deduction.date.test<-Subset_df2merge$Deduction.date.x==Subset_df2merge$Deduction.date

#these columns are not identical
Subset_df2merge$Deduction.date.test2<-Subset_df2merge$Deduction.date.df6==Subset_df2merge$Deduction.date

#these columns are not identical
Subset_df2merge$Deduction.date.test3<-Subset_df2merge$Deduction.date.df8==Subset_df2merge$Deduction.date

#these columns are not identical
Subset_df2merge$Deduction.date.test4<-Subset_df2merge$Deduction.date.df8.earliest.chads2.date==Subset_df2merge$Deduction.date

#these columns are not identical
Subset_df2merge$Deduction.date.test5<-Subset_df2merge$Deduction.date.hasbled==Subset_df2merge$Deduction.date

#these columns are not identical
Subset_df2merge$Deduction.date.test6<-Subset_df2merge$Deduction.date.eligiblepop==Subset_df2merge$Deduction.date

#i think these cols are identical but for time being i will still rename the duplicate
Subset_df2merge$Deduction.date.test7<-Subset_df2merge$Deduction.date.AC==Subset_df2merge$Deduction.date



#these columns are not identical
Subset_df2merge$Registration.status.test<-Subset_df2merge$Registration.status.x==Subset_df2merge$Registration.status

#these columns are not identical
Subset_df2merge$Registration.status.test2<-Subset_df2merge$Registration.status.df3==Subset_df2merge$Registration.status

#these columns are not identical
Subset_df2merge$Registration.status.test3<-Subset_df2merge$Registration.status.df6==Subset_df2merge$Registration.status

#these columns are not identical
Subset_df2merge$Registration.status.test4<-Subset_df2merge$Registration.status.df8==Subset_df2merge$Registration.status

#these columns are not identical
Subset_df2merge$Registration.status.test5<-Subset_df2merge$Registration.status.df8.earliest.chads2.date==Subset_df2merge$Registration.status

#these columns are not identical
Subset_df2merge$Registration.status.test6<-Subset_df2merge$Registration.status.hasbled==Subset_df2merge$Registration.status

#these columns are not identical
Subset_df2merge$Registration.status.test7<-Subset_df2merge$Registration.status.eligiblepop==Subset_df2merge$Registration.status

#i think these cols are identical but for time being i will still rename the duplicate
Subset_df2merge$Registration.status.test8<-Subset_df2merge$Registration.status.AC==Subset_df2merge$Registration.status



#these columns are not identical
Subset_df2merge$Registration.date.test<-Subset_df2merge$Registration.date.x==Subset_df2merge$Registration.date

#these columns are not identical
Subset_df2merge$Registration.date.test2<-Subset_df2merge$Registration.date.hasbled==Subset_df2merge$Registration.date

#these columns are not identical
Subset_df2merge$Registration.date.test3<-Subset_df2merge$Registration.date.eligiblepop==Subset_df2merge$Registration.date

#i think these cols are identical but for time being i will still rename the duplicate
Subset_df2merge$Registration.date.test4<-Subset_df2merge$Registration.date.AC==Subset_df2merge$Registration.date


#rename duplicated columns
df2merge<-rename(df2merge,Deduction.date.AC2="Deduction.date")

df2merge<-rename(df2merge,Registration.status.AC2="Registration.status")

df2merge<-rename(df2merge,Registration.date.AC2="Registration.date")


#delete identical duplicated columns in df2merge
df2merge<-df2merge[,-c(126:128,131)]


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
df12merge_subset<-df12merge_subset %>% group_by(Patient.ID) %>% 
  slice(which.min(antiplatelet.issue.start.date.sameday.as.or.after.af.diagnosis.date))

#rename antiplatelet.issue.start.date.sameday.as.or.after.af.diagnosis.date to earliest.antiplatelet.issue.start.date.sameday.as.or.after.af.diagnosis.date
df12merge_subset<-rename(df12merge_subset,earliest.antiplatelet.issue.start.date.sameday.as.or.after.af.diagnosis.date="antiplatelet.issue.start.date.sameday.as.or.after.af.diagnosis.date")


#del cols in df12merge_subset i.e event.date,   
df12merge_subset<-df12merge_subset[,-c(17)]

#leftjoin df12merge_subset into df2merge by patient id
df2merge<-left_join(x=df2merge,y=df12merge_subset,by="Patient.ID",copy = FALSE)

#create Subset_df2merge where there are no NAs in the earliest.antiplatelet.event.date.sameday.as.or.after.af.diagnosis.date column
Subset_df2merge<-df2merge[complete.cases(df2merge$earliest.antiplatelet.issue.start.date.sameday.as.or.after.af.diagnosis.date),]

#using Subset_df2merge , create new columns to test if merged in duplicated columns from df12merge_subset are identical to those
#already in df2merge,true =identical , false = not identical
#duplicated cols from df12merge_subset are:
#"Registered.CCG"                                          
#"Registered.practice"                                     
#"Registered.practice.ID"                                  
#"Patient.Count"       

#these columns are identical
Subset_df2merge$Registered.practice.ID.test<-Subset_df2merge$Registered.practice.ID.x==Subset_df2merge$Registered.practice.ID

#these columns are identical
Subset_df2merge$Patient.Count.test<-Subset_df2merge$Patient.Count.x==Subset_df2merge$Patient.Count

#these columns are identical
Subset_df2merge$Registered.CCG.test<-Subset_df2merge$Registered.CCG.x==Subset_df2merge$Registered.CCG

#these columns are identical
Subset_df2merge$Registered.practice.test<-Subset_df2merge$Registered.practice.x==Subset_df2merge$Registered.practice

#delete identical duplicated columns in df2merge
df2merge<-df2merge[,-c(129:131,143)]


