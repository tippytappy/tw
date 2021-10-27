# EXTRACTING VALUES FROM LONDON WATER CONTROL CENTRE PDFS  ####################
# This blog post gives a good basis for this script
# https://www.r-bloggers.com/2018/03/extracting-pdf-text-with-r-and-creating-tidy-data/

# Charlotte gave me 3 zipped folders. each had a year's pdfs
# This script creates a function which can read and extract
# a single PDF's values, putting them in a data frame.
# I unzipped each folder and put them in a local path (F:/)
# Then I read the file names and stored them in a vector
# I pass that vector to the read function using map_df
# That creates a data frame for 1 folder
# Then I changed the folder name and re-ran the function map
# When all 3 were done I did a row-bind and exported them

# SET UP  #####################################################################
library(pdftools)
library(dplyr)
library(stringr)
library(purrr)

# PDF VALUE EXTRACTION FORMULA  ###############################################
get_pdf_values <- function(x) {
  # file name variable
  filepath <- paste0(pdfs_path, x)
  
  # read and tidy the pdf text
  full_text <- pdf_text(filepath)[1] %>% 
  readr::read_lines() %>% 
  str_squish() %>% 
  str_split(' ')

  # extract the information we want
  report_date <- full_text[[3]][3:5] %>% stringr::str_flatten(collapse = ' ')
  val1 <- full_text[[7]][2]  # Datchet_Wraysbury_inflow_discharge
  val2 <- full_text[[7]][6]  # Wraysbury_water_in_storage
  val3 <- full_text[[7]][7]  # Wraysbury_level_today
  val4 <- full_text[[7]][8]  # Wraysbury_spare_capcity
  val5 <- full_text[[13]][2]  # Walton_QE2_inflow_discharge
  val6 <- full_text[[13]][8]  # QE2_water_in_storage
  val7 <- full_text[[13]][9]  # QE2_level_today
  val8 <- full_text[[13]][10]  # QE2_spare_capacity
  val9 <- full_text[[33]][8]  # outflow_discharge_Wraysbury
  val10 <- full_text[[38]][8]  # outflow_discharge_QE2
  footer_date <- full_text[[45]][1:3] %>% stringr::str_flatten(collapse = ' ')
  
  # combine the information into a data frame
  df <- data.frame(report_date, footer_date,
                   Datchet_Wraysbury_inflow_discharge = val1, 
                   Walton_QE2_inflow_discharge = val5,
                   outflow_discharge_Wraysbury = val9, 
                   outflow_discharge_QE2 = val10,
                   Wraysbury_water_in_storage = val2,
                   Wraysbury_level_today = val3,
                   Wraysbury_spare_capcity = val4,
                   QE2_water_in_storage = val6,
                   QE2_level_today = val7,
                   QE2_spare_capacity = val8
                   )
}

# GET THE FILENAMES FROM THE UNZIPPED FILE  ###################################
pdfs_path <- 'F:/2015 abstraction reports/'
pdf_names <- list.files(pdfs_path, pattern = '^r.*')

# LOOP THE FORMULA OVER THE PDFS TO GET THE RESULTS  ##########################
pdf_values_2015 <- pdf_names %>% 
  map_df(~ get_pdf_values(.x))

# MERGE THE RESULTS AND EXPORT  ###############################################
rbind(pdf_values_2015, pdf_values_2016, pdf_values_2017) %>% 
  write.table('F:/all_pdf_values.csv', sep = ',', row.names = FALSE, col.names = TRUE)
