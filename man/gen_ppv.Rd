% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen.R
\name{gen_ppv}
\alias{gen_ppv}
\title{Various helper functions that GENerate derived variables}
\usage{
gen_ppv(dt, t_ = time, id_ = id, rrate_ppv_)
}
\arguments{
\item{new.col}{name for mortality variable}

\item{old.col}{name for existing mortality variable}
}
\description{
Various helper functions that GENerate derived variables
}
\examples{
# gen_ppv(ddata, time, id, Total_Resp_Rate_Ventil)
# gen_ppv makes a logical vector if positive pressure ventilated
}

