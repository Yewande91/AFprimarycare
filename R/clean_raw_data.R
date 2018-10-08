#load tidyverse
library(tidyverse)

#load rlang
library(rlang)

#the first thing clean_dataframes function (shown at end of script) does is to replace Unknown to NA 
#Note unknown in Sex column will not change as this column is a factor column type.

#create fix_colnames function to make syntactically valid column names - this will be entered into
#clean_dataframes function
fix_colnames<-function(DF){
  v<-colnames(DF)
  v_fixed<- make.names(v, unique=TRUE)
  colnames(DF) <- v_fixed
  DF
}

#grepl search for matches to argument pattern within each element of a character vector.
#fix_agecol converts all ages in wks in age column to 0yrs
#- this will be entered into clean_dataframes
fix_agecol<-function(DF){
  if("Age" %in% colnames(DF)){
    DF$Age[grepl("wks", DF$Age, ignore.case=FALSE)] <- "0yrs"
    #gsub perform replacement of matches determined by regular expression matching
    #trim the last 3 characters from the age column (yrs)
    DF$Age <- gsub('.{3}$','',DF$Age)
    #convert age column from a character variable to an integer variable
    DF$Age <- as.integer(DF$Age)
  }
  DF
}


#complete.cases Returns a logical vector indicating which cases are complete, i.e., have no missing values.
#nadelete which deletes entire rows if a specific column of choice contains NAs
#- this will be entered into clean_dataframes
nadelete <- function(data, desiredCol) {
  completeVec <- complete.cases(data[, desiredCol])
  return(data[completeVec, ])
}

#hounslow is a vector of stings of 48 registered practices in hounslow
#i have included Hounslow practices which have closed or merged since the project period 
#spring grove practice is not included as it uses emis
#- this will be entered into clean_dataframes
hounslow <- c("E85004","E85605","E85117","E85744","E85007","E85001","E85045","E85625","E85030","E85683","E85693",
              "E85746","E85658","E85113","E85692","E85040","E85600","E85024","E85071","E85708","E85697","E85727",
              "E85739","E85699","E85718","E85736","E85700","E85035","E85115","E85734","E85056","Y02672","E85696",
              "E85052","E85114","E85018","E85062","E85681","E85707","E85716","E85058","E85735","E85059","E85126",
              "E85713","E85015","E85060","Y02671","E85732", "E85686","E85044")

#practices_only_in_houslow function remove practices not in hounslow vector
#- this will be entered into clean_dataframes
practices_only_in_houslow <-function(DF, hounslow){
  DF[DF$Registered.practice.ID %in% hounslow,]
}

#create a function called sex_as_factor which creates a new column called recodedsex, 
#input same data as is in sex column, recode male to 1, female to 2 
#indeterminate to 3 and Unknown TO 4. Note indeterminate may mean intersex and
#thus different from Unknown.
#- this will be entered into clean_dataframes
sex_as_factor<-function(DF){
  if("Sex" %in% colnames(DF)){
    DF$RecodedSex<-NA
    DF$RecodedSex<-DF$Sex
    DF$RecodedSex<-recode_factor(DF$RecodedSex, "Male"=1,"Female"=2,"Indeterminate"=3,"Unknown"=4)
  }
  DF
}

#clean_dataframes function which contains lots of functions to clean the data frames
clean_dataframes <- function(list_DF, hounslow){
  cleaned_list <- lapply(list_DF, function(y)
  {y %>% mutate_if(is.character, funs(replace(., . == "Unknown", NA_character_))) %>%
      fix_colnames %>% 
      fix_agecol %>%
      nadelete(desiredCol="Registered.practice.ID") %>%
      practices_only_in_houslow(hounslow = hounslow) %>%
      sex_as_factor
    
  })
  
  cleaned_list
}

#apply clean_dataframes function to the list of loaded data frames 
data_frames<-clean_dataframes(data_frames, hounslow = hounslow)

