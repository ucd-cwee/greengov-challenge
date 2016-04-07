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

## get the storm water violations data
wc_funs$loadStormWaterViolationsData <- function(reload = NULL,
                                             env = globalenv()){
    setwd("~/Documents/greengov-challenge/Data")
    ans <- wc_funs$inquireReload("reload Storm Water Violations data?",
                                 answer = reload)
    if(ans == "y"){
        stormViolationsData <- content(GET(paste0("https://greengov.data.ca.",
                                                  "gov/resource/g3wm-ares.json")
                                           ))
        save(strmData, file =  "stormViolationsData.rda")
    } 
    load("stromViolations.rda", envir = env)
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
