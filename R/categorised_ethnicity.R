##create new ethnicity column with only ethnicity code ID
library(stringr)

#regex \\ beginning of a word, ^ start of a string, + matches at least one time, \\ end of a word ???
in_brackets <- "\\([^()]+\\)"

#create new col called df2merge$EthnicityCode and extract first match to in_brackets in df2merge$Ethnicity.x col
#note df2merge$Ethnicity.x used and not df2merge$Ethnicity.df6 
df2merge$EthnicityCode<- str_extract(df2merge$Ethnicity, in_brackets)

#remove the brackets
df2merge$EthnicityCode<-substring(df2merge$EthnicityCode, 2, nchar(df2merge$EthnicityCode)-1)

#create a list of unique strings in the ethnicity column
#255 uniques this includes and NA
unique(df2merge$EthnicityCode)

#import categorised ethnicity
importethnicity<-read_csv("data/Ethnicity_V03.csv")

#categorise ethnicities to fewer categories i.e. white, Black/African/Caribbean/Black British, Asian or Asian British,
#Mixed/Multiple Ethnic Group,Other ethnic group, unknown
df2merge<-left_join(df2merge,importethnicity,by="EthnicityCode")

#change categorised ethnicity to factor column type
df2merge$Categorised_Ethnicity<-as.factor(df2merge$Categorised_Ethnicity)
