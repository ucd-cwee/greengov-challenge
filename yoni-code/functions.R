wc_funs <- new.env()

wc_funs$dataDirectory <- "~/Documents/greengov-challenge/Data/"
wc_funs$containingDirectory <- "~/Documents/greengov-challenge/yoni-code/"

source("~/Documents/R/R_convenience/helper_functions.R",
       local = wc_funs)


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

## load the ceden data
wc_funs$loadCedenData <- function(reload = NULL, env = globalenv()){
    setwd(wc_funs$dataDirectory)
    ans <- wc_funs$inquireReload("reload ceden data? ",
                                 answer = reload)
    if(ans == "y"){

        ceden_data <- read.csv("Surface_Water_Toxicity_2003-2014.csv")
        ceden_data_location <- read.csv("CEDEN-Stations.csv")
        save(ceden_data, ceden_data_location, file =  "ceden.rda")
        
    }
    load("ceden.rda", envir = env)
    return(0)
}

## get the waste water violations data
wc_funs$loadWasteWaterViolationsData <- function(reload = NULL,
                                                 env = globalenv()){
    setwd(wc_funs$dataDirectory)
    ans <- wc_funs$inquireReload("reload Waste Water Violations data? ",
                                 answer = reload)
    if(ans == "y"){

        wasteWaterData <- read.csv(paste0("Water_Board_Wastewater",
                                          "_Violations_2001-2016.csv"))

        wasteWaterData <- wasteWaterData %>%
            ## some of the data appears erroneous, but in
            ## an understandable way
            dplyr::mutate(LATITUDE = ifelse(PLACE.LATITUDE > 20 &
                                            PLACE.LATITUDE < 50,
                                            PLACE.LATITUDE, as.numeric(NA)),
                          LONGITUDE = ifelse(PLACE.LONGITUDE < -100 &
                                             PLACE.LONGITUDE > -125,
                                             PLACE.LONGITUDE,
                                             ifelse(-PLACE.LONGITUDE < -100 &
                                                    -PLACE.LONGITUDE > -125,
                                                    -PLACE.LONGITUDE,
                                                    as.numeric(NA))))
        
        save(wasteWaterData, file =  "wasteViolationsData.rda")
    }

    wasteWaterServiceAreas <- foreign::read.dbf(paste0("sp_join",
                                    "/wwdata_spjoin.dbf"))

    assign("wasteWaterServiceAreas", wasteWaterServiceAreas, envir = env)
    load("wasteViolationsData.rda", envir = env)
    
    return(0)
}

## load the water supplier data
wc_funs$loadSupplierData <- function(reload = NULL, env = globalenv()){
    setwd(wc_funs$dataDirectory)
    ans <- wc_funs$inquireReload("reload supplier data? ",
                                 answer = reload)
    if(ans == "y"){

        ## retrieve and edit raw files
        file_names <- list.files(getwd(), pattern = "uw_supplier")
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
                              proportionChange >= proportionChangeGoal) %>%
            dplyr::mutate(Reporting_Month =
                              gsub("-15", "",as.character(Reporting_Month))) %>%
            dplyr::mutate(year = as.numeric(substr(Reporting_Month, 1, 4)),
                          month = as.numeric(substr(Reporting_Month, 6, 7)))

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
    setwd(wc_funs$dataDirectory)
    ans <- wc_funs$inquireReload("reload id data? ",
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
            dplyr::select(Supplier_Name, PWSID_1 = PWS_ID) %>%
            dplyr::mutate(PWSID_1 = ifelse(PWSID_1 == "NA", NA, PWSID_1))
        
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
                    paste0(wc_funs$dataDirectory,
                           "waterQuality"))   
    }
    dbSendQuery(con, sql)
}

wc_funs$getQueryWaterQuality <- function(sql, con = NULL){
    if (is.null(con)){
        con <- dbConnect(SQLite(),
                        paste0(wc_funs$dataDirectory,
                               "waterQuality"))
    }
    dbGetQuery(con, sql)
}

## query water quality data by dates
## chemical, and or PWS_ID. Dates are
## converted to julian.
wc_funs$getWQData <- function(lwrBnd_date = "2015-1-1",
                              uprBnd_date = "2016-4-12",
                              date_format = "%Y-%m-%d",
                              chemical = NULL,
                              PWS_ID = NULL){

    ## check the date types and adjust accordingly
    if(!is.Date(lwrBnd_date) | !is.Date(uprBnd_date)){
        jdates <- julian(as.Date(c(lwrBnd_date, uprBnd_date),
                                 format = date_format))
        lwrBnd_date <- jdates[1]
        uprBnd_date <- jdates[2]
    } else {
        jdates <- julian(c(lwrBnd_date, uprBnd_date))
        lwrBnd_date <- jdates[1]
        uprBnd_date <- jdates[2]
    }

    ## create
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
        sql_vals <- paste(paste0("'", gsub("CA", "", PWS_ID), "'"),
                          collapse = ", ")
        PWS_ID_constraint <- paste("WHERE SYSTEM_NO IN ", "(", sql_vals, ")")
    }
    
    db <- dbConnect(SQLite(),
                    paste0(wc_funs$dataDirectory,
                           "waterQuality"))

    ## assemble and send the sequence of queries
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
         FROM d2 INNER JOIN (", paste0("
                           SELECT SYSTEM_NO, SYSTEM_NAM, HQNAME, CITY,
                                  ADDRESS, POP_SERV, CONNECTION, AREA_SERVE
                           FROM watsys ", PWS_ID_constraint),"
         ) AS ws
         ON (d2.SYSTEM_NUM = ws.SYSTEM_NO);")

    ## return the results

    ## some field explanations:
    
    ## CLS – class for chemical (P = purgeable or VOC; A = agricultural; 
    ## T =  “Title 22” or inorganics, physical, and minerals; R = radiological;
    ## B = bna or base, neutral, acid extractable; X = other)

    ## MCL – maximum contaminant level or enforceable drinking water standard.
    ## They are health protective drinking water standards to be met by public
    ## water systems.  MCLs take into account not only a chemicals’ health risks
    ## but also factors such as their detectability and treatability, as well as
    ## costs of treatment.  Health & Safety Code §116365(a) requires California
    ## Department of Public Health to establish a contaminant’s MCL at a level
    ## as close to its Public Health Goal (PHG) as is technically and
    ## economically feasible, placing primary emphasis on the protection of
    ## public health.

    ## RPHL – recommended public health level or public health goal (PHG) is
    ## established by the State of California Office of Environmental Health
    ## Hazard Assessment (OEHHA).  It is the level of a chemical contaminant in
    ## drinking water that does not pose a significant risk to health.  PHGs are
    ## not regulatory standards; however, state law requires DHS to set drinking
    ## water standards for chemical contaminants as close to the corresponding
    ## PHG as is economically and technically feasible.

    wc_funs$getQueryWaterQuality(sql, con = db) %>%
        dplyr::mutate(date = as.Date(SAMP_DATE, origin = "1970-01-01"),
                      PWS_ID = paste0("CA", SYSTEM_NO)) %>%
        dplyr::select(date, SAMP_TIME, XMOD, FINDING, CHEMICAL__,
                      CLS, RPT_UNIT, MCL, RPHL, PWS_ID, WATER_TYPE,
                      COUNTY, HQNAME, CITY, ADDRESS, POP_SERV, CONNECTION,
                      AREA_SERVE)
}

## load the supplier-oriented water quality data
wc_funs$loadSupplierWQData <- function(reload = NULL, env = globalenv()){
    setwd(wc_funs$dataDirectory)
    ans <- wc_funs$inquireReload("reload supplier Water Quality data? ",
                                 answer = reload)
    if(ans == "y"){
        if(!exists("waterService_id")){
            wc_funs$loadSupplierIdData(reload = "n", env = environment())
        }
        ids <- na.omit(unique(waterService_id$PWSID_1))
        
        supplierWaterQualityData <- wc_funs$getWQData(PWS_ID = ids)
        save(supplierWaterQualityData, file = "supplierWaterQualityData.rda")
    }
    load("supplierWaterQualityData.rda", envir = env)
    return(0)
}


## load a summary of waste water violations and chemicals present
## for each supplier
wc_funs$loadSupplierSummaryData <- function(reload = NULL, env = globalenv(),
                                            yield_top = 5){
    setwd(wc_funs$dataDirectory)
    ans <- wc_funs$inquireReload("reload supplier Water Quality data? ",
                                 answer = reload)
    if(ans == "y"){
        wc_funs$loadWasteWaterViolationsData(env = environment(),
                                             reload = "n")
        wc_funs$loadSupplierWQData(env = environment(),
                                   reload = "n")
        ids <- wasteWaterServiceAreas %>%
            dplyr::select(facility_n, PWSID_1) %>%
            dplyr::distinct() %>%
            dplyr::select(FACILITY.NAME = facility_n,
                          PWSID_1)
        
        violations <- left_join(wasteWaterData, ids, by = "FACILITY.NAME")
        violations$PWSID_1 <- as.character(violations$PWSID_1)
        violationsSummary <- violations %>%
            dplyr::group_by(PWSID_1, VIOLATION.TYPE) %>%
            dplyr::summarise(count_violations= n()) %>%
            dplyr::ungroup() %>%
            dplyr::group_by(PWSID_1) %>%
            dplyr::top_n(n = yield_top, wt = count_violations) %>%
            dplyr::ungroup()
        
        waterQualitySummary <- supplierWaterQualityData %>%
            dplyr::filter(!(XMOD %in% c("<", "F", "I", "Q"))) %>%
            dplyr::group_by(PWS_ID, CHEMICAL__, RPT_UNIT) %>%
            dplyr::summarise(av_meas = mean(FINDING)) %>%
            dplyr::ungroup() %>%
            dplyr::group_by(PWS_ID) %>%
            dplyr::arrange(desc(av_meas)) %>%
            dplyr::top_n(n = yield_top, wt = av_meas)

        save(violationsSummary, waterQualitySummary,
             file = "SupplierSummaries.rda")
    }
    load("SupplierSummaries.rda", envir = env)
    return(0)
}

## geocode an address with google api (2500 per day, 10 per sec)
wc_funs$gmapsQuery <- function(address){
    key = "AIzaSyClY2vHnR3j_cTUtvVCm58OBXAnv1VTWgY"
    req_string <- paste0("https://maps.googleapis.com/maps/api/geocode/json",
                         "?address=",
                         gsub(" ", "+", paste(tolower(address), "ca")),
                         "&components=country:US",
                         "&key=", key)
    req <- GET(req_string)
    stop_for_status(req)
    content(req)
}
