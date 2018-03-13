#load tidyverse
library(tidyverse)

# load csv tibble into R and change type of variable

m01<-read_csv("data-raw/M01_BaseLine_V03.csv",
col_types=cols(
  Age = col_character(),
  Ethnicity = col_character(),
  Sex = col_factor(levels = c("Female", "Male", "Indeterminate", "Unknown")),
  `SOA (lower layer)` = col_character(),
  `SOA (middle layer)` = col_character(),
  `Deduction date` = col_date(format = "%d-%b-%y"),
  `Registered CCG` = col_character(),
  `Registered practice` = col_character(),
  `Registered practice ID` = col_character(),
  `Registration date` = col_date(format = "%d-%b-%y"),
  `Registration status` = col_character(),
  `Patient ID` = col_integer(),
  `Patient Count` = col_integer())
)


