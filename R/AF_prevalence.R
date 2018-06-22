#open "readxl" to use "read_excel" function
library(readxl)

#import AF prevalence data for Hounslow from QOF website
import_AFprevalence<-read_excel("data/AF_prevalence.xlsx")

#create column called AF_prevalence which shows AF prevalence
import_AFprevalence$AF_prevalence<-
  import_AFprevalence$Number_of_patients_on_the_Atrial_Fibrillation_register /import_AFprevalence$Total_registered_population *100

#save import_AFprevalence
save(import_AFprevalence, file="data/import_AFprevalence.rda")
