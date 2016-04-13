wc_funs <- new.env()

source("~/Documents/R/R_convenience/helper_functions.R",
       local = wc_funs)

## handy reload-self function
wc_funs$reloadFunctions <- function(){
    source("~/Documents/greengov-challenge/yoni-code/functions.R",
           local = globalenv())
}

## function to call the reload statement:
wc_funs$inquireReload <- function(yesNoQuestion, answer = NULL){
    if(is.null(answer)){
        ans <- readline(yesNoQuestion)
        while(ans != "n" & ans != "y"){
            ans <- readline("please answer y/n: ")
        }
    } else {
        return(answer)
    }
    return(ans)
}

## load the benthic data
wc_funs$loadBenthicData <- function(reload = NULL, env = globalenv()){
    setwd("~/Documents/greengov-challenge/Data")
    ans <- wc_funs$inquireReload("reload Benthic data?",
                                 answer = reload)
    if(ans == "y"){
        benthic_data <- content(GET(paste0("https://greengov.data.ca.gov/",
                                   "resource/jzfr-fgzg.json")))
        save(benthic_data, file =  "benthic.rda")
    } 
    load("benthic.rda", envir = env)
    return(0)
}

## load the ceden data
wc_funs$loadCedenData <- function(reload = NULL, env = globalenv()){
    setwd("~/Documents/greengov-challenge/Data")
    ans <- wc_funs$inquireReload("reload ceden data?",
                                 answer = reload)
    if(ans == "y"){

        ceden_data <- read.csv("Surface_Water_Toxicity_2003-2014.csv")
        ceden_data_location <- read.csv("CEDEN-Stations.csv")
        save(ceden_data, ceden_data_location, file =  "ceden.rda")
        
    }
    load("ceden.rda", envir = env)
    return(0)
}

## get the storm water violations data
wc_funs$loadWasteWaterViolationsData <- function(reload = NULL,
                                                 env = globalenv()){
    setwd("~/Documents/greengov-challenge/Data")
    ans <- wc_funs$inquireReload("reload Waste Water Violations data?",
                                 answer = reload)
    if(ans == "y"){
        wasteWaterData <- read.csv(paste0("Water_Board_Wastewater",
                                          "_Violations_2001-2016.csv"))
        save(wasteWaterData, file =  "wasteViolationsData.rda")
    } 
    load("wasteViolationsData.rda", envir = env)
    return(0)
}

## load the water supplier data
wc_funs$loadSupplierData <- function(reload = NULL, env = globalenv()){
    setwd("~/Documents/greengov-challenge/Data/")
    ans <- wc_funs$inquireReload("reload supplier data?",
                                 answer = reload)
    if(ans == "y"){

        ## retrieve and edit raw files
        file_names <- list.files(getwd(), pattern = "16")
        waterSupplierReport <- llply(file_names, read_excel)
        waterSupplierReport <- rbind.fill(waterSupplierReport)

        colnames(waterSupplierReport) <- gsub(" ", "_",
                                              colnames(waterSupplierReport))
        colnameAbbrevs <- c("TotMonthlyH20ProdCurrent",
                            "TotMonthlyH20Prod2013",
                            "conservationStandard")
        colnames(waterSupplierReport)[c(19,20,17)] <- colnameAbbrevs

        ## shape file
        waterServiceAreas <- readShapeSpatial(paste0("Drought_Water_Service",
                                                     "_Areas_complete.shp"))

        ## create the fields of interest
        waterSupplierReport <- waterSupplierReport %>%
            dplyr::mutate(TotMonthlyH20Prod2013 =
                              as.numeric(TotMonthlyH20Prod2013),
                          TotMonthlyH20ProdCurrent =
                              as.numeric(TotMonthlyH20ProdCurrent)) %>%
            dplyr::mutate(proportionChange =
                              (TotMonthlyH20Prod2013 -
                               TotMonthlyH20ProdCurrent)/
                              TotMonthlyH20Prod2013,
                          proportionChangeGoal =
                              as.numeric(conservationStandard)) %>%
            dplyr::mutate(proportionChangeMet =
                              proportionChange >= proportionChangeGoal)

        ## supplier ID data:
        wc_funs$loadSupplierIdData(reload = "y", env = environment())
        
        save(waterService_id,
             waterSupplierReport,
             waterServiceAreas,
             file = "waterSupplierReport.rda")
    }
    load("waterSupplierReport.rda", envir = env)
    return(0)
}


## load dataframe linking Supplier_Name with PWSID_1
wc_funs$loadSupplierIdData <- function(reload = NULL, env = globalenv()){
    setwd("~/Documents/greengov-challenge/Data/")
    ans <- wc_funs$inquireReload("reload id data?",
                                 answer = reload)
    if(ans == "y"){

        ## retrieve and edit raw files
        waterService_id <- read_excel("UWMP_PWS_IDs_07-29-14.xls")
        comparison <- read.csv(file = "districtComparison.csv") %>%
            dplyr::select(Supplier_Name, Urban_Water_Supplier) %>%
            dplyr::distinct(Supplier_Name, Urban_Water_Supplier)

        ## put the files together
        waterService_id <- left_join(waterService_id, comparison,
                                     by = "Urban_Water_Supplier") %>%
            dplyr::select(Supplier_Name, PWSID_1 = PWS_ID)
        save(waterService_id, file = "supplierIdData.rda")
    }
    load("supplierIdData.rda", envir = env)
    return(0)
}

## send a sql query to the waterQuality sqlite3
## database
wc_funs$sendQueryWaterQuality <- function(sql, con = NULL){
    if (is.null(con)) {
     con <- dbConnect(SQLite(),
                    paste0("~/Documents/greengov-challenge/Data/",
                           "waterQuality"))   
    }
    dbSendQuery(con, sql)
}

wc_funs$getQueryWaterQuality <- function(sql, con = NULL){
    if (is.null(con)){
        con <- dbConnect(SQLite(),
                        paste0("~/Documents/greengov-challenge/Data/",
                               "waterQuality"))
    }
    dbGetQuery(con, sql)
}

wc_funs$getWQDataBetween <- function(lwrBnd_date = NULL,
                                     uprBnd_date = NULL,
                                     chemical = NULL,
                                     PWS_ID = NULL){

    if(is.null(lwrBnd_date) | is.null(uprBnd_date)){
        jdates <- julian(as.Date(c("2015-1-1", "2016-4-12"),
                                 format = "%Y-%m-%d"))
        lwrBnd_date <- jdates[1]
        uprBnd_date <- jdates[2]
    }

    chem_constraint <- ""
    if(!is.null(chemical)){
        sn_resp <- wc_funs$getQueryWaterQuality(paste0("
                     SELECT STORE_NUM
                     FROM storet
                     WHERE CHEMICAL__ = '", toupper(chemical),"'"))
        chem_constraint <- paste0(" AND STORE_NUM = ", sn_resp$STORE_NUM)
    }

    PWS_ID_constraint <- ""
    if(!is.null(PWS_ID)){
        PWS_ID_constraint <- paste0("WHERE SYSTEM_NO = '", gsub("CA", "",
                                                                PWS_ID), "' ")
    }
    
    db <- dbConnect(SQLite(),
                    paste0("~/Documents/greengov-challenge/Data/",
                           "waterQuality"))

    sql <- paste0("
           CREATE TEMPORARY TABLE d1 AS
           SELECT *
           FROM (
                 SELECT PRIM_STA_C, SAMP_DATE, SAMP_TIME,
                        ANADATE, STORE_NUM, XMOD, FINDING
                 FROM CHEMICAL
                 WHERE ",
           "SAMP_DATE >= ", lwrBnd_date," AND SAMP_DATE <= ", uprBnd_date,
           chem_constraint,
           ") AS a
           INNER JOIN (
                      SELECT STORE_NUM as STORE_NUMBER, CHEMICAL__, CLS,
                             RPT_UNIT, MCL, RPHL
                      FROM storet
           ) AS b
           ON (a.STORE_NUM = b.STORE_NUMBER);")
    wc_funs$sendQueryWaterQuality(sql, con = db)    
    sql <- "CREATE TEMPORARY TABLE d2 AS
            SELECT *
            FROM d1 INNER JOIN (
                               SELECT PRI_STA_C, COUNTY, DISTRICT,
                                      USER_ID, SYSTEM_NO as SYSTEM_NUM,
                                      WATER_TYPE, SOURCE_NAM, STATUS
                               FROM siteloc
            ) AS sl
            ON (d1.PRIM_STA_C = sl.PRI_STA_C);"
    wc_funs$sendQueryWaterQuality(sql, con = db)
    sql <- paste0(
        "SELECT *
         FROM d2 INNER JOIN (
                           SELECT SYSTEM_NO, SYSTEM_NAM, HQNAME, CITY,
                                  ADDRESS, POP_SERV, CONNECTION, AREA_SERVE
                           FROM watsys
         ) AS ws
         ON (d2.SYSTEM_NUM = ws.SYSTEM_NO) ",
        PWS_ID_constraint, ";")
    wc_funs$getQueryWaterQuality(sql, con = db) %>%
        dplyr::mutate(date = as.Date(SAMP_DATE, origin = "1970-01-01"))
}

wc_funs$gmapsQuery <- function(address){
    key = ## "AIzaSyDcopyvKapIzRvm-BXDB8-1tufF-v4Q7z0"
        "AIzaSyClY2vHnR3j_cTUtvVCm58OBXAnv1VTWgY"
    req_string <- paste0("https://maps.googleapis.com/maps/api/geocode/json",
                         "?address=",
                         gsub(" ", "+", paste(tolower(address), "ca")),
                         "&components=country:US",
                         "&key=", key)
    req <- GET(req_string)
    stop_for_status(req)
    content(req)
}
