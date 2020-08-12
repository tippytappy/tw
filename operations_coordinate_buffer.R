# netbase spatial query: jobs within a radius from a point xy
# output
# - csv of the records
# - markdown file with leaflet map

# QUERY  ######################################################################
query <- '
select 
A.NBLINK                              "Internal Ident",
A.ORDER_EXTERNAL_ID2                  "notification",
A.PARENT_EXTERNAL_ID1                 "superior_order",
A.EXTERNAL_ID1                        "order",
A.EXTERNAL_ID2                        "operation",
A.EXTERNAL_ID3                        "ID3",
A.EXTERNAL_ID4                        "ID4",
A.TEXT_FIELD138                       "maint_operation",
A.TEXT_FIELD141                       "maint_work_order",
A.PRIORITY                            "priority",
A.FINANCE_CODE                        "finance_code",
A.DATE_FIELD1                         "Started Estimate",
A.ACTUAL_START_DATE                   "Date Reported",
A.ACTUAL_FINISH_DATE                  "Date Completed",
A.DATE_FIELD3                         "Completed Target Date",
A.NUMBER_FIELD8                       "Planned Hours",
A.ACTUAL_WORK_DURATION                "Total Hours",
A.TEXT_FIELD29                        "Resolution Code Vistec",
A.TEXT_FIELD145                       "Resolution Code SAP",
A.DATE_FIELD4                         "imp Job Status Date",
A.CURRENT_IMP_USER_STATUS             "imp Job Status Code",
A.CURRENT_IMP_USER_STATUS_DESC        "imp Job Status Desc",
A.CURRENT_NB_STATUS                   "nbs Status Code",
A.CURRENT_NB_STATUS_DESC              "nbs Status Description",
A.RAISED_OPERATION_TYPE               "Reported code",
A.RAISED_OPERATION_TYPE_DESC          "Reported Description",
A.CURRENT_OPERATION_TYPE              "Completed code",
A.CURRENT_OPERATION_TYPE_DESC         "Completed Description",
A.TEXT_FIELD53                        "Verified code",
A.TEXT_FIELD23                        "Billing Reference",
A.PROPERTY_REF                        "Property Reference",
A.TEXT_FIELD90                        "Building Name/Number",
A.STREET                              "Street",
A.TOWN                                "Town",
A.POSTCODE                            "Postcode",
A.DMA_REF                             "DMA Code",
A.TEXT_FIELD21                        "wpa Code",
A.TEXT_FIELD75                        "cont Building Number",
A.TEXT_FIELD76                        "cont Street",
A.TEXT_FIELD77                        "cont Town",
A.TEXT_FIELD78                        "cont County",
A.TEXT_FIELD79                        "cont Postcode",
A.TEXT_FIELD85                        "Highway Authority",
A.TEXT_FIELD84                        "Highway Notified",
A.TEXT_FIELD56                        "Highway Notice Type",
A.TEXT_FIELD86                        "Remarks Date",
A.LONG_DESCRIPT                       "Remarks",
A.TEXT_FIELD80                        "Work Request Code",
A.SHORT_DESCRIPT                      "Work Details",
A.ASSET_REFERENCE                     "Asset Reference",
A.NUMBER_FIELD20                      "Customers Affected",
A.NUMBER_FIELD9                       "LDR Number",
A.DATE_FIELD16                        "LDR Date",
A.NUMBER_FIELD19                      "Leakage Rate",
A.NUMBER_FIELD18                      "ESPB",
A.TEXT_FIELD81                        "Inspection / Repair",
A.TEXT_FIELD125                       "Failure Type",
A.TEXT_FIELD64                        "Joint Failure",
A.TEXT_FIELD66                        "Fitting Type",
A.TEXT_FIELD65                        "Fitting Failure",
A.TEXT_FIELD68                        "Seal Type",
A.TEXT_FIELD67                        "Pipe Failure",
A.TEXT_FIELD69                        "Repair Install Method",
A.NUMBER_FIELD6                       "Depth of Pipeline",
A.NUMBER_FIELD21                      "Pipe Size",
A.TEXT_FIELD137                       "Pipe Material",
A.NBS_BURSTTYPE                       "Burst Type Code",
A.NBS_BURSTTYPE_DESC                  "Burst Type Description",
A.LEAKAGE_GROUP_DESCRIPTION           "Leakage Type",
A.NETWORK_GROUP                       "Network Type",
A.XCOORD                              "X",
A.YCOORD                              "Y",
A.VALIDCOORD_DESC                     "Geo Validity Code"
from 
NB.ACT_OPERATION_V A
where
sqrt(power(a.xcoord-523730.5, 2)+power(a.ycoord-177631.5, 2)) < 50'

# SET UP  #####################################################################
library(sf)
library(leaflet)
library(dplyr)
library(odbc)
library(DBI)

# GET THE DATA  ###############################################################
con <- dbConnect(odbc::odbc(), "Netbase", UID = Sys.getenv('nb_id'), PWD = Sys.getenv('nb_pw'))
operations <- dbGetQuery(con, query)

# export the data
write.table(operations, file = 'E:/results.csv', sep = ',', row.names = FALSE, col.names = TRUE)

# create the leaflet map
operations_map <- operations %>% 
  st_as_sf(coords = )
operations <- operations %>% st_transform(4326)

leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(data = operations %>% filter(dist_filter == 'within 50 metres'))


leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(data = operations)
