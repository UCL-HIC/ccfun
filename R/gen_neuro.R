#' @title Generates the neuro score for various scoring systems
#'
#' @description
#' Generates the neuro score; requires GCS
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param score score that the nero component is to be calculated for
#' @param gcs_ GCS total score
#' @param hours_ hours since ICU admission
#' @param id_ unique id for each patient
#' 
#' @examples
#' # gen_neuro(ddata,sofa, gcs_ = gcs)
#' # table(ddata$sofa_n, useNA="always")
#' # ddata[gcs<=15][sample(nrow(ddata[gcs<=15]),20), .(gcs, sofa_n)]

#' @export
gen_neuro <- function(dt, score, gcs_, hours_, id_) {
        
        #Checking that the score argument is valid so that the function is not trying to calculate
        #an impossible score.
        
        if (score != "sofa" & score != "apache ii") {
          stop("score must be a string containing either 'sofa' or 'apache ii'", call. = FALSE)      
        
        

         #Choosing which score to calculate based on the score argument.        
        } else if (score == "sofa") {
                #  ==============
                #  = SOFA - GCS =
                #  ==============
                
                # appending _ to var names for readability and to ensure uses scoped version
                
                # library(data.table)
                # data.table changes the object in place unless you use dt1 <- copy(dt)
                # so passing data.tables via function is actually just passing a reference
                
                # Non-standard evaluation
                pars <- as.list(match.call()[-1])
                
                gcs_ <- as.character(pars$gcs_)
                
                # Set to NA by default (numeric)
                dt[, `:=`(sofa_n = as.numeric(NA))]
                
                # Update based on conditions
                # Order of conditions is IMPORTANT
                
                # SOFA = 0
                dt[get(gcs_) == 15, "sofa_n" := 0]
                
                # SOFA = 1
                dt[get(gcs_) <= 14, "sofa_n" := 1]
                
                # SOFA = 2
                dt[get(gcs_) <= 12, "sofa_n" := 2]
                
                # SOFA = 3
                dt[get(gcs_) <=  9, "sofa_n" := 3]
                
                # SOFA = 4
                dt[get(gcs_) <=  5, "sofa_n" := 4]
                
                # - [ ] TODO(2016-05-21):  Now set to NA if patient sedated
                # if (!is.null(rxsed)) {
                #     print(describe(rxsed))
                #     update <- ifelse(rxsed %in% c("True", "TRUE", TRUE), NA, sofa.n)
                #     describe(update)
                #     sofa.n <- ifelse(is.na(update), NA, update)
                #     print(describe(sofa.n))
                # }
                
        } else if (score == "apache ii") {
                
                #  ==============
                #  = APACHE II - GCS =
                #  ==============
                
                # appending _ to var names for readability and to ensure uses scoped version
               
                 # Non-standard evaluation
                pars <- as.list(match.call()[-1])
                
                gcs_ <- as.character(pars$gcs_)
                hours_ <- as.character(pars$hours_)
                id_ <- as.character(pars$id_)
                
                # Set to NA by default (numeric)
                dt[, `:=`(apache_n = as.numeric(NA))]
                
                #apache ii gcs component is based on the lowest score in the first 24 hours 
                #after admission.
                #So, we select the lowest gcs score in the first 24 hours after admission. This is the second pipe.
                #The third pipe replaces values that were recorded as -Inf into NAs. This happenes if there is no gcs score recorded in the first 24 hours after admission.
                
                #Function generates warnings when patientes have no non missing gcs scores in the first 
                #24 hours following admssion. 
                dt[get(hours_) <= 24, "apache_n" := 15 - get(gcs_)][, apache_n := max(apache_n, na.rm =                 T), by = (get(id_))][apache_n == -Inf, apache_n := NA]
                
                
                
        }
        
}

                
      

