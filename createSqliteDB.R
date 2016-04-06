################################################################################
################################################################################
## standalone script to create the sqlite data base from all of the dbf files.

library(RSQLite)
library(foreign)

directory <- "~/Documents/waterDataChallenge/Data/"
dbf_files <- list.files(directory, pattern = "(dbf|DBF)")

db <- dbConnect(SQLite(), paste0(directory, "waterQuality"))

## function to open dbf file and insert it as a table
## into the waterQuality db:
insertIntoDB <- function(file_name){
    name <- unlist(strsplit(file_name, "\\."))[[1]]
    temp_df <- read.dbf(paste0(directory, file_name))
    dbWriteTable(db, name, temp_df)
}

## execute the function over all the files
l_ply(dbf_files, insertIntoDB)

## confirm data has been entered:
print(dbListTables(db))
