% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/g_events_term_id.R
\name{create_flag_vars}
\alias{create_flag_vars}
\title{create \code{AE} overview flags}
\usage{
create_flag_vars(
  df,
  fatal = AESDTH == "Y",
  serious = AESER == "Y",
  serious_withdrawl = AESER == "Y" & grepl("DRUG WITHDRAWN", AEACN),
  serious_modified = AESER == "Y" & grepl("DRUG (INTERRUPTED|INCREASED|REDUCED)", AEACN),
  serious_related = AESER == "Y" & AEREL == "Y",
  withdrawl = grepl("DRUG WITHDRAWN", AEACN),
  modified = grepl("DRUG (INTERRUPTED|INCREASED|REDUCED)", AEACN),
  related = AEREL == "Y",
  related_withdrawl = AEREL == "Y" & grepl("DRUG WITHDRAWN", AEACN),
  related_modified = AEREL == "Y" & grepl("DRUG (INTERRUPTED|INCREASED|REDUCED)", AEACN),
  ctc35 = AETOXGR \%in\% c("3", "4", "5"),
  ...
)
}
\arguments{
\item{df}{data frame of \code{AE}}

\item{fatal}{\code{AE} with fatal outcome derivation}

\item{serious}{Serious \code{AE} derivation.}

\item{serious_withdrawl}{Serious \code{AE} leading to withdrawal derivation}

\item{serious_modified}{Serious \code{AE} leading to dose modification derivation}

\item{serious_related}{Related Serious \code{AE} derivation}

\item{withdrawl}{\code{AE} leading to withdrawal derivation}

\item{modified}{\code{AE} leading to dose modification derivation}

\item{related}{Related \code{AE} derivation}

\item{related_withdrawl}{Related \code{AE} leading to withdrawal derivation}

\item{related_modified}{Related \code{AE} leading to dose modification derivation}

\item{ctc35}{Grade 3-5 \code{AE} derivation}

\item{...}{named expressions used to generate categories}
}
\description{
create \code{AE} overview flags
}
\details{
in this function, all flags are expressions calls, for simpler usage.
}
\examples{
library(dplyr)

ADAE <- osprey::rADAE

# add additional dummy causality flags
ADAE <- ADAE \%>\%
  mutate(AEREL1 = (AEREL == "Y" & ACTARM == "A: Drug X")) \%>\%
  mutate(AEREL2 = (AEREL == "Y" & ACTARM == "B: Placebo"))
attr(ADAE[["AEREL1"]], "label") <- "AE related to A: Drug X"
attr(ADAE[["AEREL2"]], "label") <- "AE related to B: Placebo"

create_flag_vars(ADAE)
# create other flags
create_flag_vars(ADAE, `AENSER` = AESER != "Y")
# remove flags that are not needed
create_flag_vars(ADAE, fatal = NULL)
}
