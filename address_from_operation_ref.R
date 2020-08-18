# SET UP  #####################################################################
library(DBI)
source('C:/Users/dylan/Documents/R/scripts/environment_variables.R')
set_ev()
con <- dbConnect(odbc::odbc(), "Netbase", 
                 UID = Sys.getenv('nb_id'), 
                 PWD = Sys.getenv('nb_pw'))

# GET THE DATA  ###############################################################
operations_string <- paste0("'",
               paste(LeadAddressesNeeded$OPERATION_ID, collapse = "','"),
               "'")

query <- paste0(
        'select
          external_id2 AS OPERATION_ID, 
          BUILDING_NAME,
          BUILDING_NUMBER,
          STREET, 
          TOWN, 
          POSTCODE
          from nb.act_operation_v
          where external_id2 in (',
        operations_string, ')')

operation_addresses <- dbGetQuery(con, query)


# EXPORT THE DATA  ############################################################
write.table(operation_addresses, 'F:/operation_addresses.txt', 
            sep = '\t', row.names = FALSE, col.names = TRUE)
