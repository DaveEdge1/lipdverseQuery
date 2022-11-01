#clear workspace
rm(list=ls())
gc()
#get query table
temp <- tempfile()
download.file("http://lipdverse.org/lipdverse/lipdverseQuery.zip", temp)
filePath <- unzip(temp, list = TRUE)$Name
unzip(temp, filePath)
queryTable <- read.csv(filePath)


points <- data.frame(lon=queryTable$geo_longitude, lat=queryTable$geo_latitude)

library(lipdR)
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


  return(list("country"=indices$ADMIN,
       "continent"=indices$REGION))
}

#points = data.frame(lon=c(0, 0, 5, 10, 15, 20), lat=c(51.5, 65, 50, 48.5, 47, 44.5))

geogNames <- coords2country(points)

queryTable$country2 <- geogNames$country

queryTable$continent <- geogNames$continent



#publication info

authorCols <- colnames(queryTable)[c(grep("pub", colnames(queryTable)), grep("contributor", colnames(queryTable)))]

allPub <- apply(queryTable, 1, function(x) unname(unlist(x[authorCols])))
allPub <- apply(allPub, 2, function(x) unlist(x[!is.na(x)]))
queryTable$auth <- lapply(allPub, function(x) paste0(unlist(x[!is.na(x)]), collapse = ''))

#interpretation variables

queryTable$interp_Vars <- paste(queryTable$interpretation1_variable,
                               queryTable$interpretation2_variable,
                               queryTable$interpretation3_variable,
                               queryTable$interpretation4_variable,
                               sep = " ")

#Interpretation Details

queryTable$interp_Details <- paste(queryTable$interpretation1_variableDetail,
                                queryTable$interpretation2_variableDetail,
                                queryTable$interpretation3_variableDetail,
                                queryTable$interpretation4_variableDetail,
                                sep = " ")



keeps <- c("archiveType", "paleoData_variableName", "geo_latitude",
           "geo_longitude", "earliestYear", "mostRecentYear", "auth", "datasetId",
           "dataSetName", "country2", "continent", "interpretation1_seasonality",
           "interp_Vars", "interp_Details", "paleoData_mostRecentCompilations")


compressedTable <- queryTable[,colnames(queryTable) %in% keeps]
compressedTable <- apply(compressedTable,2,as.character)
#
# compressedTable2 <- data.frame(matrix(nrow = nrow(compressedTable), ncol = ncol(compressedTable), data = NA))
#
# imax<-ncol(compressedTable2) * nrow(compressedTable2)
# pb <- txtProgressBar(min = 0, max = imax, style = 3)
# its <- 0
# for (i in 1:ncol(compressedTable)){
#   for (j in 1:nrow(compressedTable)){
#     #compressedTable2[j,i] <- stringi::stri_trans_general(compressedTable[j,i], "latin-ascii")
#     compressedTable2[j,i] <- iconv(compressedTable[j,i], "latin1", "ASCII", sub="")
#     its <- its+1
#     setTxtProgressBar(pb, its)
#   }
#   #compressedTable[,i] <- iconv(compressedTable[,i], "latin1", "ASCII", sub="")
#   #compressedTable[,i] <- stringi::stri_trans_general(compressedTable[,i], "latin-ascii")
# }
#
# #compressedTable <- as.data.frame(compressedTable)
# badChecks <- data.frame(col = NA,
#                         row=NA)
# imax<-ncol(compressedTable2) * nrow(compressedTable2)
# pb <- txtProgressBar(min = 0, max = imax, style = 3)
# its <- 0
# for (i in 1:ncol(compressedTable2)){
#   for (j in 1:nrow(compressedTable2)){
#     its <- its+1
#     setTxtProgressBar(pb, its)
#     #compressedTable2[j,i] <- stringi::stri_trans_general(compressedTable[j,i], "latin-ascii")
#     flagNonASCII <- grepl("[^ -~]", compressedTable2[j,i])
#     if (flagNonASCII){
#       #cat("Non-ASCII characters at column: ", i, "row: ",j)
#       #message("Non-ASCII characters at column: ", i, " row: ",j)
#       badChecks <- rbind(badChecks, c(i,j))
#
#     }
#   }
#   #compressedTable[,i] <- iconv(compressedTable[,i], "latin1", "ASCII", sub="")
#   #compressedTable[,i] <- stringi::stri_trans_general(compressedTable[,i], "latin-ascii")
# }
# badChecks <- badChecks[complete.cases(badChecks),]
#
# tools::showNonASCII(compressedTable2[badChecks[i,2], badChecks[i,1]])
# ?tools::showNonASCII
#
# checkFlags <- list()
# for (i in nrow(badChecks)){
#   checkFlags[[i]] <- tools::showNonASCII(compressedTable2[badChecks[i,2], badChecks[i,1]])
# }
# #
# # grepNonASCII <- function(x) {
# #   asc <- iconv(x, "latin1", "ASCII")
# #   ind <- is.na(asc) | asc != x
# #   which(ind)
# # }
# #
# # grepNonASCII(compressedTable2[badChecks[i,2], badChecks[i,1]])
# # grepl("[^ -~]", compressedTable2[badChecks[i,2], badChecks[i,1]])
# #
# # compressedTable2[397,13]
# # for (i in 1:500){
# #   compressedTable2[i,13] <- iconv(compressedTable[i,13], "latin1", "ASCII", sub="")
# # }
# # compressedTable2[398,13]
# #
# # grepl("[^ -~]", compressedTable[1:500,13])
# # grepl("[^ -~]", compressedTable2[1:500,13])
#
# names(compressedTable2) <- keeps

zipped.csv <- function(df, zippedfile) {
  # Create a query table dir
  dir_tmp <- lipdR::create_tmp_dir()
  dir_zip <- file.path(dir_tmp, "zip")
  dir.create(dir_zip, showWarnings=FALSE)
  # write temp csv
  filenameCSV <- paste0(dir_zip, "/queryTable.csv")
  write.csv(df, file=filenameCSV, row.names = FALSE)
  #write MD5 payload manifest
  manifest1 <- tools::md5sum(filenameCSV)
  write.table(x = manifest1,
              file = file.path(dir_zip, "manifest-md5.txt"),
              row.names = F,
              col.names = F,
              quote = F,
              sep = '\t')
  # zip temp csv
  zip(zippedfile,dir_zip, extras = '-j')
  # delete temp csv
  unlink(dir_tmp)
}

zipped.csv(df=compressedTable, zippedfile = "queryZip")



zipMD5 <- tools::md5sum(paste0(dir_zip, "/queryTable.zip"))
