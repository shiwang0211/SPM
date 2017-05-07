library(raster)

roads <- shapefile("../GIS/basemap_arcs_WGS_1984_Web_Mercator_Auxiliary_Sphere.shp")
Seminole_roads = subset(roads, roads$CO_NUM == 77)
Seminole_roads <- spTransform(Seminole_roads, CRS("+proj=longlat +datum=WGS84 +no_defs"))


NLanes <- shapefile("../GIS/COMBOLR_WGS_1984_Web_Mercator_Auxiliary_Sphere.shp")
Seminole_NLanes <- subset(NLanes, substr(NLanes$ROADWAY,1,2) == "77")
Seminole_NLanes <- spTransform(Seminole_NLanes, CRS("+proj=longlat +datum=WGS84 +no_defs"))

Counties <- shapefile("../GIS/FloridaCounties.shp")
Seminole_County<- subset(Counties, COUNTY_NAM == "SEMINOLE")
Seminole_County <- spTransform(Seminole_County, CRS("+proj=longlat +datum=WGS84 +no_defs"))

save(Seminole_roads, Seminole_NLanes, Seminole_County, file="./rda/RoadShape.rda")
