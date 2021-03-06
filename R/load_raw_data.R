#load tidyverse
library(tidyverse)

#function to find file names with a data-raw/AF_Data_ extension
fileNames<-Sys.glob("data-old/AF_Data_*.csv")

#create a function to load data files
load_data_files<-function(fN){
  lapply(fN, function(x)
  {loadfiles<-read_csv(x,
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
                         `Patient Count` = col_integer(),
                         `M02a - Number of Patients on the Atrial Fibrillation (AF) At Risk Register: Patient ID`= col_integer(),
                         `AF0011-High Risk Atrial Fibrillation Patients with LTCs: Patient ID` = col_integer(),
                         `COPD001 - Register v34: Patient ID` = col_integer(),
                         `HYP001 - Register v34: Patient ID` = col_integer(),
                         `OSA001 - Register v34: Patient ID` = col_integer(),
                         `AF0014 - Patients 60 and Over: Patient ID` = col_integer(),
                         `CHD001 - Register v34: Patient ID` = col_integer(),
                         `Read code` = col_character(),
                         `Event date` = col_date(format = "%d-%b-%y"),
                         `Event done at` = col_character(),
                         `Event done at ID` = col_character(),
                         `M08 - Number of AF Patients with AF who have a CHAD2DS2-VAc Assessment Documented and with Read code XaY6i: Patient ID` = col_integer(),
                         `M07a - Number of AF Patients with CHAD2DS2-VAc Assessment Documented: Numeric value` = col_integer(),
                         `M07a - Number of AF Patients with CHAD2DS2-VAc Assessment Documented: Read code` = col_character(),
                         `M07a - Number of AF Patients with CHAD2DS2-VAc Assessment Documented: Event date` = col_date(format = "%d-%b-%y"),
                         `M07a - Number of AF Patients with CHAD2DS2-VAc Assessment Documented: Event done at` = col_character(),
                         `M07a - Number of AF Patients with CHAD2DS2-VAc Assessment Documented: Event done at ID` = col_character(),
                         `M07aa - Number of AF Patients with Read code XaY6i: Read code` = col_character(),
                         `M07aa - Number of AF Patients with Read code XaY6i: Read code description` = col_character(),
                         `M07aa - Number of AF Patients with Read code XaY6i: Event date` = col_date(format = "%d-%b-%y"),
                         `M07aa - Number of AF Patients with Read code XaY6i: Event done at` = col_character(),
                         `M07aa - Number of AF Patients with Read code XaY6i: Event done at ID` = col_character(),
                         `M08a - Number of Patients with AF who have a either a CHAD2DS2-VAc or CHADS2 Assessment Documented or both: Patient ID` = col_integer(),
                         `M08bb - Number of AF Patients with Read code XaP9J: Read code` = col_character(),
                         `M08bb - Number of AF Patients with Read code XaP9J: Read code description` = col_character(),
                         `M08bb - Number of AF Patients with Read code XaP9J: Event date` = col_date(format = "%d-%b-%y"),
                         `M08ba - Number of AF Patients with Cong heart fail-hypertens-age-diab-stoke 2 risk score Assessment Documented: Numeric value` = col_integer(),
                         `M08ba - Number of AF Patients with Cong heart fail-hypertens-age-diab-stoke 2 risk score Assessment Documented: Read code` = col_character(),
                         `M08ba - Number of AF Patients with Cong heart fail-hypertens-age-diab-stoke 2 risk score Assessment Documented: Event date` = col_date(format = "%d-%b-%y"),
                         `M08ba - Number of AF Patients with Cong heart fail-hypertens-age-diab-stoke 2 risk score Assessment Documented: Event done at ID` = col_character(),
                         `M10 - Number of Patients with AF who have a HAS-BLED Assessment Documented and with Read Code XaY6z: Patient ID`= col_integer(),
                         `M10a - Number of AF patients with HAS-BLED Assessment Documented: Numeric value` = col_integer(),
                         `M10a - Number of AF patients with HAS-BLED Assessment Documented: Read code` = col_character(),
                         `M10a - Number of AF patients with HAS-BLED Assessment Documented: Event date`= col_date(format = "%d-%b-%y"),
                         `M10a - Number of AF patients with HAS-BLED Assessment Documented: Event done at` = col_character(),
                         `M10a - Number of AF patients with HAS-BLED Assessment Documented: Event done at ID`= col_character(),
                         `M10b - Number of AF Patients with Read code XaY6z: Read code` = col_character(),
                         `M10b - Number of AF Patients with Read code XaY6z: Event date`= col_date(format = "%d-%b-%y"),
                         `M10b - Number of AF Patients with Read code XaY6z: Event done at` = col_character(),
                         `M10b - Number of AF Patients with Read code XaY6z: Event done at ID` = col_character(),
                         `M12 - Number of Patients with AF who have a CHAD2DS2-VAc Score over the threshold and HAS-BLED Score under Threshold : Patient ID`= col_integer(),
                         `M12b - WMUH Number of Newly Diagnosed AF Patients with CHAD2DS2-VAc Score over the threshold and HAS-BLED Score under Threshold (Females): Numeric value`= col_integer(),
                         `M12b - WMUH Number of Newly Diagnosed AF Patients with CHAD2DS2-VAc Score over the threshold and HAS-BLED Score under Threshold (Females): Read code` = col_character(),
                         `M12b - WMUH Number of Newly Diagnosed AF Patients with CHAD2DS2-VAc Score over the threshold and HAS-BLED Score under Threshold (Females): Event date`= col_date(format = "%d-%b-%y"),
                         `M12b - WMUH Number of Newly Diagnosed AF Patients with CHAD2DS2-VAc Score over the threshold and HAS-BLED Score under Threshold (Females): Event done at` = col_character(),
                         `M12b - WMUH Number of Newly Diagnosed AF Patients with CHAD2DS2-VAc Score over the threshold and HAS-BLED Score under Threshold (Females): Event done at ID` = col_character(),
                         `M12c - Patients with HAS-BLED Score under 3 recorded: Numeric value`= col_integer(),
                         `M12c - Patients with HAS-BLED Score under 3 recorded: Read code` = col_character(),
                         `M12c - Patients with HAS-BLED Score under 3 recorded: Event date`= col_date(format = "%d-%b-%y"),
                         `M12c - Patients with HAS-BLED Score under 3 recorded: Event done at` = col_character(),
                         `M12c - Patients with HAS-BLED Score under 3 recorded: Event done at ID` = col_character(),
                         `M12a - WMUH Number of Newly Diagnosed AF Patients with CHAD2DS2-VAc Score over the threshold and HAS-BLED Score under Threshold (Males): Numeric value`= col_integer(),
                         `M12a - WMUH Number of Newly Diagnosed AF Patients with CHAD2DS2-VAc Score over the threshold and HAS-BLED Score under Threshold (Males): Read code` = col_character(),
                         `M12a - WMUH Number of Newly Diagnosed AF Patients with CHAD2DS2-VAc Score over the threshold and HAS-BLED Score under Threshold (Males): Event date`= col_date(format = "%d-%b-%y"),
                         `M12a - WMUH Number of Newly Diagnosed AF Patients with CHAD2DS2-VAc Score over the threshold and HAS-BLED Score under Threshold (Males): Event done at` = col_character(),
                         `M12a - WMUH Number of Newly Diagnosed AF Patients with CHAD2DS2-VAc Score over the threshold and HAS-BLED Score under Threshold (Males): Event done at ID` = col_character(),
                         `Drug` = col_character(),
                         `Drug ID`= col_integer(),
                         `Issue duration`= col_integer(),
                         `Issue end date`= col_date(format = "%d-%b-%y"),
                         `Issue start date`= col_date(format = "%d-%b-%y"),
                         `Medication type` = col_character(),
                         `Product Type ` = col_character(),
                         `Repeat drug ` = col_character(),
                         `M16 - Number of patients with AF Prescribed Aspirin but without Clopidogrel: Patient ID`= col_integer(),
                         `M16a Diagnosed AF Patients Prescribed with Antiplatelet Drug: Event date`= col_date(format = "%d-%b-%y"),
                         `M16a Diagnosed AF Patients Prescribed with Antiplatelet Drug: Event done at` = col_character(),
                         `M16a Diagnosed AF Patients Prescribed with Antiplatelet Drug: Event done at ID` = col_character(),
                         `M16a Diagnosed AF Patients Prescribed with Antiplatelet Drug: Drug` = col_character(),
                         `M16a Diagnosed AF Patients Prescribed with Antiplatelet Drug: Drug ID`= col_integer(),
                         `M16a Diagnosed AF Patients Prescribed with Antiplatelet Drug: Issue duration`= col_integer(),
                         `M16a Diagnosed AF Patients Prescribed with Antiplatelet Drug: Issue end date`= col_date(format = "%d-%b-%y"),
                         `M16a Diagnosed AF Patients Prescribed with Antiplatelet Drug: Issue start date`= col_date(format = "%d-%b-%y"),
                         `M16a Diagnosed AF Patients Prescribed with Antiplatelet Drug: Medication type` = col_character(),
                         `M16a Diagnosed AF Patients Prescribed with Antiplatelet Drug: Product Type` = col_character(),
                         `M16a Diagnosed AF Patients Prescribed with Antiplatelet Drug: Repeat drug` = col_character())
  )
  })
}

#create variable called data_frames which contains the list of dataframes with the data-raw/AF_Data_ extension
data_frames <- load_data_files(fileNames)

