#explore raw data
class(m01)
dim(m01)
names(m01)
str(m01)

#replace unknown to na and assign to m01 data frame. Note unknown in sex column will not change as this column
#is a factor column type.
m01 <- m01 %>% mutate_if(is.character, funs(replace(., . == "Unknown", NA_character_)))

#find percentage of missing data in each of the columns (note % missing data will less for sex column as unknown 
#in that column was not changed to NA, as col type is factor)
sapply(m01, function(x) sum(is.na(x)))/nrow(m01)

# rename date columns to remove space
m01<- rename(m01,
             Registration_date = "Registration date", Deduction_date = "Deduction date")

# correct_century takes the following arguments:
# tib: a tibble
# date_col: the name of a column of tib containing dates
# cutoff_date: a date
#
# it returns a tibble the same as tib, but with the dates in 
# date_col modified as follows:
#   if the date is after cutoff_date, it is replaced with the
#   20th century year with the same last two digits,
#   and if not, the date is unchanged

cutoff_date <- as.Date("2017-11-17")
correct_century <- function(tib, date_col, cutoff_date) {
  
  date_col <- enquo(date_col)
  
  tib %>% mutate(UQE(date_col) := if_else(
    UQ(date_col) > cutoff_date, 
    as.Date(format(UQ(date_col), "19%y-%m-%d")), 
    as.Date(format(UQ(date_col), "%Y-%m-%d"))
  ))
}
m01<-correct_century(m01,Registration_date,cutoff_date)
m01<-correct_century(m01,Deduction_date,cutoff_date)

#grepl search for matches to argument pattern within each element of a character vector.
#convert all ages in wks in age column to 0yrs
m01$Age[grepl("wks", m01$Age, ignore.case=FALSE)] <- "0yrs"

#gsub perform replacement of matches determined by regular expression matching
#trim the last 3 characters from the age column (yrs)
m01$Age<-gsub('.{3}$','',m01$Age)

#convert age column from a character variable to an integer variable
m01$Age<-as.integer(m01$Age)

# rename age column to Age(yrs)
m01 <- rename(m01, Ageinyrs ="Age")

#complete.cases Returns a logical vector indicating which cases are complete, i.e., have no missing values.
# create function called nadelete which deletes entire rows if a specific column of choice contains NAs
nadelete <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

#use the nadelete function on dataset of choice
m01<-nadelete(m01,"Registered practice ID")

#registered practices in hounslow
hounslow <- c("E85004","E85605","E85117","E85744","E85007","E85001","E85045","E85625","E85030","E85683","E85693",
              "E85746","E85658","E85113","E85692","E85040","E85600","E85024","E85071","E85708","E85697","E85727",
              "E85739","E85699","E85718","E85736","E85700","E85035","E85115","E85734","E85056","Y02672","E85696",
              "E85052","E85114","E85018","E85062","E85681","E85707","E85716","E85058","E85735","E85059","E85126",
              "E85713","E85015","E85060","Y02671")

#remove practices not in hounslow
m01<-m01[m01$`Registered practice ID`%in% hounslow,]

#create a new column called recodedsex, input same data as is in sex column, recode male to 1, female to 2 
#indeterminate to 3 and Unknown TO 4. Note indeterminate may mean intersex and
#thus different from Unknown.
m01$RecodedSex<-NA
m01$RecodedSex<-m01$Sex
m01$RecodedSex<-recode_factor(m01$RecodedSex, "Male"=1,"Female"=2,"Indeterminate"=3,"Unknown"=4)


#create new ethnicity column with only ethnic code ID
library(stringr)
in_brackets <- "\\([^()]+\\)"
m01$EthnicityCode<- str_extract(m01$Ethnicity, in_brackets)
m01$EthnicityCode<-substring(m01$EthnicityCode, 2, nchar(m01$EthnicityCode)-1)

#create a list of unique strings in the ethnicity column
unique(m01$EthnicityCode)

#import categorised ethnicity
importethnicity<-read_csv("data-raw/M01_Ethnicity_V03.csv")

#categorise ethnicities to fewer categories i.e. white, Black/African/Caribbean/Black British, Asian or Asian British,
#Mixed/Multiple Ethnic Group,Other ethnic group, unknown
m01<-left_join(m01,importethnicity,by="EthnicityCode")

#change categorised ethnicity to factor column type
m01$Categorised_Ethnicity<-as.factor(m01$Categorised_Ethnicity)
