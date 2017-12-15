#' @title Generates the APACHE Heart Rate score
#'
#' @description
#' Generates the APACHE Heart Rate score;
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param h_rate_ numeric variable containing the heart rate
#' @param hours_ numeric variable containg the number of hours since ICU admission
#' @param id_ unique patient identifier
#' @param window_ Numerical.Vector delimiting boundaries for time-window.
#' 
#'@examples
#' ddata <- NULL
#' hr <- "Heart rate"
#' ddata$"time" <- sample(seq(1,72,1), 200, replace = T)
#' ddata <- as.data.table(ddata)
#' ddata[, ("site") := sample(c("XX", "ZZ", "YY"), 200, replace = T)]
#' ddata[, ("episode_id") := sample(seq(1,250,1), 200, replace = T)]
#' ddata[, ("Heart rate") := sample(seq(30,120,1), 200, replace = T)]
#' system.time(gen_apache_hr(ddata, window = c(0,24)))
#' ddata[time %between% c(0,24), .N, by = c("site","episode_id", "apache_hr")]
#'
#' @export


gen_apache_hr <- function(dt, h_rate_, hours_, id_, window_ = c(0,24)) {
        #  =============================
        #  = APACHE - Heart Rate =
        #  =============================
        # appending _ to var names for readability and to ensure uses scoped version
        
        # Non-standard evaluation
        pars <- as.list(match.call()[-1])
        
        h_rate_ <- as.character(pars$h_rate_)
        hours_ <- as.character(pars$hours_)
        id_ <- as.character(pars$id_)
       
        
        # library(data.table)
        # data.table changes the object in place unless you use dt1 <- copy(dt)
        # so passing data.tables via function is actually just passing a reference
        
        # Naming  the apache_hr
        apache_hr <- "apache_hr"
        
        # Update based on conditions
        # Order of conditions is IMPORTANT
        
        dt[, (apache_hr) := as.numeric(NA)]
        
        # APACHE = 0
        dt[get(h_rate_) %between% c(70,109), (apache_hr) := 0]
        
        # APACHE = 2
        dt[(get(h_rate_) %between% c(55,69)) | (get(h_rate_) %between% c(110,139)), (apache_hr) := 2]
        
        # APACHE = 3
        dt[(get(h_rate_) %between% c(40,54)) | (get(h_rate_) %between% c(140,179)), (apache_hr) := 3]
        
        # APACHE = 4
        dt[(get(h_rate_) < c(40)) | (get(h_rate_) > c(179)), (apache_hr) := 4]
        
        # Calculate APACHE score for time window
        dt[get(hours_) %between% window_, (apache_hr) := max(apache_hr, na.rm = T), by = get(id_)]             [apache_hr == -Inf, apache_hr := NA]
        
        
}