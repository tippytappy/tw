# SET UP  #####################################################################
library(dplyr)
library(sf)
library(sp)
library(leaflet)
library(DBI)
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
  filter(CreatedDate >= lubridate::dmy("01/01/2015"),
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

streets <- streets %>% 
  group_by(USRN) %>% 
  summarise()



test_area_streets %>% st_length()
test_area_buffers %>% st_length()
