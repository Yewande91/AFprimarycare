##create new ethnicity column with only ethnicity code ID
library(stringr)

#regex \\ beginning of a word, ^ start of a string, + matches at least one time, \\ end of a word ???
in_brackets <- "\\([^()]+\\)"

#create new col called df5merge_subset$EthnicityCode and extract first match to in_brackets in df5merge_subset$Ethnicity.x col
#note df5merge_subset$Ethnicity.x used and not df5merge_subset$Ethnicity.df6 
df5merge_subset$EthnicityCode<- str_extract(df5merge_subset$Ethnicity, in_brackets)

#remove the brackets
df5merge_subset$EthnicityCode<-substring(df5merge_subset$EthnicityCode, 2, nchar(df5merge_subset$EthnicityCode)-1)

#create a list of unique strings in the ethnicity column
#255 uniques this includes and NA
unique(df5merge_subset$EthnicityCode)

#import categorised ethnicity
importethnicity<-read_csv("data/Ethnicity_V03.csv")

#categorise ethnicities to fewer categories i.e. white, Black/African/Caribbean/Black British, Asian or Asian British,
#Mixed/Multiple Ethnic Group,Other ethnic group, unknown
df5merge_subset<-left_join(df5merge_subset,importethnicity,by="EthnicityCode")

#change categorised ethnicity to factor column type
df5merge_subset$Categorised_Ethnicity<-as.factor(df5merge_subset$Categorised_Ethnicity)
