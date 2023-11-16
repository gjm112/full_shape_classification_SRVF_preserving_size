library(jpeg)
library(tidyverse)
library(dplyr)
library(Momocs)
library(fdasrvf)

source("/Users/gregorymatthews/Dropbox/gladysvale/R/utility.R")
source("/Users/gregorymatthews/Dropbox/gladysvale/R/curve_functions.R")

#####################################################################################
##What does this code do? 
## 1. Connects to the database and gets the reference file.  
## 2. Reads in the black and white images which are downloaded from the database in bulk.  The extant images are located here: ./gladysvale/images/Extant
#####################################################################################

# library(jpeg)
# library(tidyverse)
# library(RMySQL)
# library(dplyr)
# library(Momocs)
# 
# mydb = dbConnect(MySQL(), user='teeth', password='bgT-45A-elF', dbname='teeth', host='gridfriday.com')
# 
# dbListTables(mydb)
# 
# q <- "select * from taxonomy where active = 1 and tribe != 'xtribe'"
# rs <- dbGetQuery(mydb, q)
# 
# #save(rs, file = "/Users/gregorymatthews/Dropbox/gladysvale/RData/rs_20210622.RData")
# 
# load("/Users/gregorymatthews/Dropbox/gladysvale/RData/rs_20210622.RData")

#Remove Tribe == "xtribe" 
#These are just example rows in the data base
#rs <- subset(rs, tribe != "xtribe")
#qntkhvn

# rs <- rs %>% filter(tribe != "xtribe")



################################
##Now import the BW images
################################
path <- "/Users/gregorymatthews/Dropbox/gladysvale/Gladysvale_medalcels"

file_list_BW_extant <- list.files(path, recursive = TRUE, full.names = TRUE)

#remove <- c(grep("DSCN2815.JPG",file_list_BW_extant), grep("DSCN2831.JPG",file_list_BW_extant), grep("DSCN4754.JPG",file_list_BW_extant))
#file_list_BW_extant <- file_list_BW_extant[-remove]

#Import the BW image files.
start <- Sys.time()
import_BW <- function(x){import_jpg(x)[[1]]}
teeth_BW_gladysvale <- lapply(as.list(file_list_BW_extant), import_BW)
names(teeth_BW_gladysvale) <- substring(file_list_BW_extant, unlist(gregexpr( "GV",file_list_BW_extant)) , unlist(gregexpr( "JPEG|jpeg|jpg",file_list_BW_extant)) -2)
end <- Sys.time()
end - start



save(teeth_BW_gladysvale, file = "/Users/gregorymatthews/Dropbox/gladysvale/RData/teeth_BW_gladysvale_medalcels.RData")



#resample the curves so they all have the same number of points representing them.  

#Makes all the teeth have the same number of points
make_same_num_points <- function(x, N = 500){
  out <- resamplecurve(t(x),N)
  return(out)
}

teeth_BW_gladysvale_500 <- lapply(teeth_BW_gladysvale, make_same_num_points)

#Convert to a matrix
teeth_BW_gladysvale_500_matrix <- do.call(rbind,teeth_BW_gladysvale_500)
row.names(teeth_BW_gladysvale_500_matrix) <- rep(names(teeth_BW_gladysvale_500),each = 2)

#Write out the shape matrix file
write.csv(teeth_BW_gladysvale_500_matrix,file = "/Users/gregorymatthews/Dropbox/gladysvale/teeth_BW_gladysvale_500_matrix.csv")

#write out the meta data file
#write.csv(rs, file = "/Users/gregorymatthews/Dropbox/gladysvale/reference_file_20210622.csv")
