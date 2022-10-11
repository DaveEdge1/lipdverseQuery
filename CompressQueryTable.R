
#get query table
queryTable <- readr::read_csv("http://lipdverse.org/lipdverse/lipdverseQuery.csv")

points = data.frame(lon=queryTable$geo_longitude, lat=queryTable$geo_latitude)

library(sp)
library(rworldmap)

# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
coords2country = function(points)
{  
  countriesSP <- getMap(resolution='low')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  
  # convert our list of points to a SpatialPoints object
  
  # pointsSP = SpatialPoints(points, proj4string=CRS(" +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
  
  #setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  # return the ADMIN names of each country
  #indices$ADMIN  
  #indices$ISO3 # returns the ISO3 code 
  #indices$continent   # returns the continent (6 continent model)
  #indices$REGION   # returns the continent (7 continent model)
  
  return(list("country"=indices$ADMIN,
       "continent"=indices$REGION))
}

#points = data.frame(lon=c(0, 0, 5, 10, 15, 20), lat=c(51.5, 65, 50, 48.5, 47, 44.5))

geogNames=coords2country(points)

queryTable$country2 <- geogNames$country

queryTable$continent <- geogNames$continent



#Fuzzy search in variable names

allVars <- unique(queryTable$paleoData_variableName)
queryTable$varTags <- NA

fuzzyVars <- c("18o", "13c", "d2h")
#brGDGT
#leaf wax
#lake levels
#salinity
#Mg-Ca
#TEX86

for (i in fuzzyVars){
  vard18O <- allVars[grep(i, tolower(allVars))]
  indexd18O <- which(queryTable$paleoData_variableName %in% vard18O)
  d18O <- rep(NA, nrow(queryTable))
  d18O[indexd18O] <- i
  queryTable <- cbind(queryTable, d18O)
}

queryTable$varTags <- apply(queryTable[,465:467], 1, function(x) paste(x[!is.na(x)], collapse = " "))
queryTable$varTags[queryTable$varTags == ""] <- NA

authorCols <- colnames(queryTable)[c(grep("pub", colnames(queryTable)), grep("contributor", colnames(queryTable)))]

allPub <- apply(queryTable, 1, function(x) unname(unlist(x[authorCols])))
allPub <- apply(allPub, 2, function(x) unlist(x[!is.na(x)]))
queryTable$auth <- lapply(allPub, function(x) paste0(unlist(x[!is.na(x)]), collapse = ''))

keeps <- c("archiveType", "varTags", "paleoData_variableName", "geo_latitude", 
           "geo_longitude", "earliestYear", "mostRecentYear", "auth", "datasetId",
           "lipdverseLink", "dataSetName", "country2", "continent")


compressedTable <- queryTable[,colnames(queryTable) %in% keeps]
compressedTable <- apply(compressedTable,2,as.character)

zipped.csv <- function(df, zippedfile) {
  # init temp csv
  temp <- tempfile(fileext=".csv")
  # write temp csv
  write.csv(df, file=temp)
  # zip temp csv
  zip(zippedfile,temp, extras = '-j')
  # delete temp csv
  unlink(temp)
}

zipped.csv(df=compressedTable, zippedfile = "queryZip")
#write.csv(compressedTable, file = "C:/Users/dce72/Documents/LiPDsandbox/query/compQuery.csv")

library(git2rdata)

repo <- repository("~/my_git_repo")
pull(repo)
write_vc(my_data, file = "rel_path/filename", root = repo, stage = TRUE)
commit(repo, "My message")
push(repo)





