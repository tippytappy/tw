# SET UP  #####################################################################
library(sf)
library(leaflet)
library(dplyr)
library(odbc)
library(DBI)
library(rmarkdown)
source('~/R/rscripts/useful_functions.R')
source('~/R/scripts/environment_varibles.R')

# GET THE COORDINATES  ########################################################
address <- '191 Fulham Palace Road'
address_latlon <- geocodeAddress(address)
address_xy <- 
  address_latlon %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>% 
  st_transform(27700) %>% 
  st_coordinates()

# CREATE THE QUERY  ###########################################################
query_pt1 <- "SELECT V.Nblink NBID, V.OPERATION_NUMBER, V.Order_External_id2,
          V.Parent_external_id1,
          CASE
            WHEN V.ACTUAL_START_DATE is not null THEN V.ACTUAL_START_DATE
            WHEN V.DATE_FIELD6 is not null THEN V.DATE_FIELD6
            ELSE NULL
            END AS startDateFill,
          CASE
            WHEN V.ACTUAL_START_DATE is not null THEN 'ACTUAL_START_DATE'
            WHEN V.DATE_FIELD6 is not null THEN 'DATE_FIELD6'
            ELSE NULL
            END AS startDateFillSource,
          CASE
            WHEN V.ACTUAL_FINISH_DATE is not null THEN V.ACTUAL_FINISH_DATE
            WHEN V.DATE_FIELD18 is not null THEN V.DATE_FIELD18
            WHEN V.DATE_FIELD3 is not null THEN V.DATE_FIELD3
            WHEN REGEXP_INSTR(V.LONG_DESCRIPT, '(0[1-9]|[12][0-9]|3[01])[-/.](0[1-9]|1[012])[-/.](19|20)\\d\\d$') > 0
              THEN TO_DATE(
                REPLACE(
                  REPLACE(SUBSTR(V.LONG_DESCRIPT, REGEXP_INSTR(V.LONG_DESCRIPT, '(0[1-9]|[12][0-9]|3[01])[-/.](0[1-9]|1[012])[-/.](19|20)\\d\\d$'), 10), '.', '/'),
                  '-', '/'),
                  'DD/MM/YYYY')
            ELSE NULL
            END AS endDateFill,
          CASE
            WHEN V.ACTUAL_FINISH_DATE is not null THEN 'ACTUAL_FINISH_DATE'
            WHEN V.DATE_FIELD18 is not null THEN 'DATE_FIELD18'
            WHEN V.DATE_FIELD3 is not null THEN 'DATE_FIELD3'
            WHEN REGEXP_INSTR(V.LONG_DESCRIPT, '(0[1-9]|[12][0-9]|3[01])[-/.](0[1-9]|1[012])[-/.](19|20)\\d\\d$') > 0
            THEN 'REMARKS FIELD'
            ELSE NULL
          END AS endDateFillSource,
          V.Leakage_group,
          V.Nbs_bursttype,
          V.Network_group,
          V.Raised_operation_type,
          V.Current_operation_type,
          V.Text_field53 Confirmed_operation_type,
          V.Current_nb_status,
          V.Number_field6 PipeDepth,
          V.Number_field21 PipeSize,
          V.Text_field137 PipeMaterial,
          V.Property_ref,
          V.Text_field75 Building_Nr,
          V.Street,
          V.Town,
          V.Postcode,
          V.Xcoord AS x,
          V.Ycoord AS y,
          V.Validcoord
        FROM ACT_OPERATION_V V
        where
        sqrt(power(v.xcoord-"
        
query_pt2 <- ", 2)+power(v.ycoord-"
query_pt3 <- ", 2)) < 50"
query <- paste0(query_pt1, address_xy[1, 1], query_pt2, address_xy[1, 2], query_pt3)

# GET THE DATA  ###############################################################
con <- dbConnect(odbc::odbc(), "Netbase", 
                 UID = Sys.getenv('nb_id'), 
                 PWD = Sys.getenv('nb_pw'))
operations <- dbGetQuery(con, query)

# export the data
operations %>% arrange(desc(STARTDATEFILL)) %>% 
  write.table(file = paste0('E:/2/', address, ' results.csv'), 
            sep = ',', row.names = FALSE, col.names = TRUE)

# create the leaflet map
operations_map <- 
  operations %>% 
  st_as_sf(coords = c('X', 'Y'), crs = 27700) %>% 
  st_transform(4326) %>% 
  select(operation = OPERATION_NUMBER, started = STARTDATEFILL,
         finish = ENDDATEFILL, burst_type = NBS_BURSTTYPE,
         job_type = CONFIRMED_OPERATION_TYPE,
         pipe_size = PIPESIZE, pipe_material = PIPEMATERIAL)

map <- 
  operations_map %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(radius = 2, popup = popupTable(operations_map, 
                                                  row.numbers = FALSE)) %>% 
  addMeasure(position = 'bottomleft', primaryLengthUnit = 'meters', 
             primaryAreaUnit = 'sqmeters')

# RENDER THE DOCUMENT  ########################################################
render(output_format = 'html_document', 
       input = 'Operations_coordinate_buffer.Rmd', 
       output_file = paste0(address, ' results.html'))