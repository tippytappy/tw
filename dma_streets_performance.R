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

# JOIN THE DATA  ###################################################################
# merge the streets by usrn
# cut the streets to dma
# join bursts to their dmas
# join burst-dma and street-dma to get burst-street-dma; the results

# set the buffer size
buffer_size <- 7.5

# break the streets file into groups 
# then merge the street spans by their USRN
merged_streets <- streets %>% 
  mutate(letter_group = substr(STREET, 1, 1)) %>% 
  group_by(letter_group) %>% 
  group_split() %>% 
  map_dfr(~ .x %>% group_by(USRN) %>% summarise())

# now we can split the merged streets into their dmas
streets_dma <- merged_streets %>% 
  mutate(band = substr(as.character(USRN), 1, 1)) %>% 
  group_by(band) %>% 
  group_split() %>%
  map_dfr(~ st_intersection(.x, dmas)) %>% 
  mutate(street_length = st_length(.))
# worked; took about 3 minutes
# drops a lot of streets which are outside dmas

# next we join the bursts to the dmas
# we do this so we can use the dma field to speed up the join later
# and because the streets-burst query may return bursts in
# neighbouring dmas, and this lets us filter those out
bursts_dma <- all_bursts %>% 
  group_by(year(CreatedDate)) %>% 
  group_split() %>%
  map_df(~ st_intersection(.x, dmas))
# worked; took about 4 minutes

# we make a list of dmas which have a burst 
# so we can use this to drive the map function
dmas_with_bursts <- unique(bursts_dma$DMAAREACOD)

# we'll use this function to join the data
# it reduces the join data sets to only a single dma
# to speed up the joins
join_bursts_to_streets <- function(x) {
  dma_bursts <- bursts_dma %>% 
    filter(DMAAREACOD == x)
  dma_streets <- streets_dma %>% 
    filter(DMAAREACOD == x)
  dma_bursts %>% 
    st_buffer(buffer_size) %>% 
  st_intersection(dma_streets) %>% 
    st_drop_geometry()
}

# we make a safe version to handle errors
safe_join_bs <- safely(join_bursts_to_streets)

# now we join the bursts to the streets, one dma at a time
bursts_dma_street <- dmas_with_bursts %>% 
  map(safe_join_bs) %>% 
  map('result') %>% 
  bind_rows() %>% 
  filter(DMAAREACOD == DMAAREACOD.1)

results <- bursts_dma_street %>% 
  select(-c(X, Y, Shape, year.CreatedDate., band, DMAAREACOD.1)) %>% 
  group_by(DMAAREACOD, USRN) %>% 
  summarise(total_bursts = n(), street_length = mean(street_length)) %>% 
  mutate(norm_bursts_score = total_bursts / street_length) %>% 
  left_join(usrn_street, by = 'USRN') %>% 
  select(DMAAREACOD, STREET, everything()) %>% 
  ungroup()