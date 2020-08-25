# SET UP  #####################################################################
library(dplyr)
library(sf)
library(sp)
library(leaflet)
library(DBI)
library(lubridate)
# GET BURSTS  #################################################################
con <- dbConnect(odbc::odbc(), "Netbase", 
                 UID = Sys.getenv('nb_id'), 
                 PWD = Sys.getenv('nb_pw'))

query <- "
SELECT 
V.Nblink NBID,
V.OPERATION_NUMBER,
  CASE
      WHEN V.ACTUAL_START_DATE is not null THEN V.ACTUAL_START_DATE
      WHEN V.DATE_FIELD6 is not null THEN V.DATE_FIELD6
      ELSE NULL
  END AS startDateFill,
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
V.Leakage_group,
V.Nbs_bursttype,
V.Network_group,
V.Raised_operation_type,
V.Current_operation_type,
V.Text_field53 Confirmed_operation_type,
V.Current_nb_status,
V.Xcoord,
V.Ycoord
FROM ACT_OPERATION_V V
WHERE V.Nbs_bursttype IS NOT NULL AND 
V.leakage_group IN ('ACT', 'VIS')"

# make this spatial for the streets buffer query
# match column names to the mains repairs file 
# so we can bind the two
bursts <- dbGetQuery(con, query)
bursts <- bursts %>% 
  filter(STARTDATEFILL >= as.POSIXct('2015-04-01')) %>% 
  select(Activity = OPERATION_NUMBER, 
         CreatedDate = STARTDATEFILL, CompletedDate = ENDDATEFILL, 
         ActivityType = LEAKAGE_GROUP, BurstType = NBS_BURSTTYPE, 
         X = XCOORD, Y = YCOORD) %>% 
  st_as_sf(coords = c('X', 'Y'), crs = 27700)


mains_repairs <- st_read('~/work/maps/MainsRepairs.gdb', 'MainsRepair_OnPipe') 
mains_repairs <- mains_repairs %>%
  mutate(CreatedDate = lubridate::ymd_hms(CreatedDate)) %>%
  filter(CreatedDate >= lubridate::dmy("01/04/2015"),
         RegulatoryReported == "Yes",
         ActivityType %in% c("VS", "AL")) %>%
  mutate(BurstType = 'M') %>% 
  select(Activity, CreatedDate, CompletedDate, ActivityType, BurstType,
         X, Y) %>% 
  st_transform(27700)

all_bursts <- bursts %>% 
  bind_rows(mains_repairs)

# GET STREETs  ################################################################
# from the national street gazetteer
tw_proj <- list()
tw_proj$epsg <- as.integer(NA)
tw_proj$proj4string <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs"
attr(tw_proj, 'class') <- 'crs'
streets <- st_read('f:/Streets.gdb', 'NSG') %>% 
  st_transform(27700) %>% 
  select(USRN, STREET)

# GET DMAS  ###################################################################
dmas <- st_read('~/work/maps/dmaFixed.shp') %>% 
  select(DMAAREACOD) %>% 
  st_transform(27700)

# TESTING  #####################################################
# this should cut streets which span DMAs and assign each
# cut piece to the right DMAs
test_area_streets <- streets %>% 
  st_intersection(test_area)

test_area_streets <- test_area_streets %>% 
  group_by(USRN) %>% 
  summarise() %>% 
  mutate(length = st_length(.))

# join spans with the same uprn before buffering
test_area_merged <- test_area_streets %>% 
  group_by(USRN) %>% 
  summarise()

# CREATE A BUFFER FOR EACH STREET OVER X METRES
test_area_buffers <- 
  test_area_streets %>% 
  st_transform(27700) %>% 
  st_buffer(endCapStyle = 'FLAT', dist = 10) %>% 
  st_transform(4326)

test_area_buffers <- test_area_buffers %>% 
  group_by(USRN) %>% 
  summarise()

test_area_streets <- test_area_streets %>% 
  group_by(USRN) %>% 
  summarise()


leaflet() %>% 
  addTiles() %>% 
  addPolylines(data = test_area_streets) %>% 
  addPolygons(data = test_area_buffers)

# METHOD 1  ###################################################################
# join the streets by usrn
# cut to area
# buffer with flat ends
streets_backup <- streets
buffer_size <- 15

# break the streets file into groups then merge the streets in each group
merged_streets <- streets %>% 
  mutate(letter_group = substr(STREET, 1, 1)) %>% 
  group_by(letter_group) %>% 
  group_split() %>% 
  map_dfr(~ .x %>% group_by(USRN) %>% summarise())

# buffering the streets seems to fail sometimes so
# we make a safe function that will return NA 
# instead of ending the process and losing our results
safe_buffer <- safely(st_buffer)

buffered_streets <- 
  merged_streets %>% 
  mutate(band = substr(as.character(USRN), 1, 1)) %>% 
  group_by(band) %>% 
  group_split() %>%
  map(~ safe_buffer(.x, 15)) %>%
  map("result") %>%
  bind_rows()

# lots of records haven't been processed
not_processed <- merged_streets %>% 
  filter(!USRN %in% buffered_streets$USRN)

not_processed[1, ] %>% st_transform(4326) %>% leaflet() %>% addPolylines() %>% addTiles()

# why?
buffered_streets %>% 
  st_drop_geometry() %>%
  select(band) %>% 
  table()

# band 4 didn't process for some reason
# 1     2     3     5     6     7     8     9 
# 54583 95039 45793  2383  8960  5472  9803 16590 

usrn_street <- streets %>% 
  st_drop_geometry() %>% 
  select(USRN, STREET) %>%
  distinct()

buffered_streets_np <- 
  not_processed %>% 
  st_buffer(15)

# fails
# Error in CPL_geos_op("buffer", x, dist, nQ, numeric(0), logical(0)) : 
#   Evaluation error: TopologyException: depth mismatch at  at 415185 185490.

buffered_streets_np <- 
  not_processed %>% 
  safe_buffer(15)
# result = NULL

any(is.na(st_dimension(not_processed)))  # empty geometries; none
any(is.na(st_is_valid(not_processed)))  # corrupt geometries; FALSE

buffered_streets_batch_2 <- 
  not_processed %>% 
  mutate(band = substr(as.character(USRN), 2, 2)) %>% 
  group_by(band) %>% 
  group_split() %>%
  map(~ safe_buffer(.x, 15)) %>%
  map("result") %>%
  bind_rows()

buffered_streets_batch_2 %>% 
  st_drop_geometry() %>% 
  select(band) %>% 
  table()

 buffered_streets <- buffered_streets %>% 
   bind_rows(buffered_streets_batch_2)
 
# update not processed
 not_processed <- merged_streets %>% 
   filter(!USRN %in% buffered_streets$USRN)
 
 not_processed_test <- 
   not_processed %>% 
   filter(substring(USRN, 1, 2) == 40)

# loop through not processed until we hit a buffer that doesn't work
for(i in 1:nrow(not_processed)) {
  item = not_processed[i, ]
  print(item$USRN)
  st_buffer(item, 15)}
# USRN 40200288 fails
 
 # test whether looping through the not processed will end with 
 # the ok ones able to be gathered
 loop_test_data <- not_processed %>% 
   filter(USRN %in% c(40100825, 40200288))
 loop_test <- vector('list', length = nrow(loop_test_data))
 for(i in 1:nrow(loop_test_data)) {
   item = loop_test_data[i, ]
   print(item$USRN)
   loop_test[[i]] <- safe_buffer(item, 15)
 }
 
# run this then extract the ones that worked
loop_test <- vector('list', length = nrow(not_processed))
for(i in 1:nrow(not_processed)) {
   item = not_processed[i, ]
   loop_test[[i]] <- safe_buffer(item, 15)
}

buffered_not_processed <- loop_test %>%
  map("result") %>%
  bind_rows()

# add the now processed records to the other processed records
buffered_streets <- buffered_streets %>% 
  bind_rows(buffered_not_processed)
