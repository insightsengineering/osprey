#' Applies STREAM style filtering to datasets
#'
#' One of slref or anl need to be specified. The conversion from SAS code in filters dataset may not work in all cases.
#' In case of failure a sensible error message should be returned.
#'
#' @param slref The subject level data frame (typically ASL.sas7bdat on BCE)
#' @param anl The analysis data frame (e.g. ATE.sas7bdat on BCE)
#' @param suffix The suffix to apply in quotes (e.g. "ITT_PFSINV")
#' @param slref_keep Variables to keep from slref (e.g. c("REGION", "SEX"))
#' @param usubjid The unique subject identifier variable in quotes (e.g. "USUBJID")
#' @param filters The name of the filters dataset (typically __filters.sas7bdat on BCE)
#'
#' @return \code{dataframe} object
#' @author Iain Bennett
#' @export
#' @examples
#' ASL <- random.cdisc.data::rasl()
#' ATE <- random.cdisc.data::rate(ASL)
#' filters <- as.data.frame(rbind(
#'   c(ID = "IT", FLTTARGET = "SLREF", FLTWHERE = "where 1 eq 1"),
#'   c(ID = "BIO", FLTTARGET = "SLREF", FLTWHERE = "where BMRKR1 ge 4.3"),
#'   c(ID = "M", FLTTARGET = "SLREF", FLTWHERE = "where SEX eq 'M'"),
#'   c(ID = "PFS", FLTTARGET = "ANL", FLTWHERE = "where PARAMCD eq 'PFS'"),
#'   c(ID = "OS", FLTTARGET = "ANL", FLTWHERE = "where PARAMCD eq 'OS'")
#' ))
#' 
#' ANL <- stream_filter(
#'   slref = ASL,
#'   anl = ATE,
#'   suffix = "IT_PFS_BIO",
#'   filters = filters
#' )
stream_filter <- function(slref = NULL, anl = NULL, filters, suffix, slref_keep = NULL, usubjid = "USUBJID") {
  actual_suffix <- NULL

  if (is.null(anl) & is.null(slref)) {
    stop("At least one of anl= or slref= must be provided")
  }

  if (is.null(anl)) {
    anl <- slref
  }

  if (is.null(slref)) {
    slref <- anl
  }

  asl_out <- slref
  anl_out <- anl

  # step 1 get a list of filters

  filters_to_apply <- strsplit(suffix, split = "_", fixed = TRUE) %>%
    unlist()

  n.filters <- length(filters_to_apply)

  for (i in 1:n.filters) {
    this.filter <- filters_to_apply[i]

    # find filter meta data

    this.filter.df <- filter(filters, ID == this.filter) %>%
      unique()

    if (nrow(this.filter.df) == 0) {
      stop(paste("Filter", this.filter, "not found in filters"))
    }

    if (nrow(this.filter.df) > 1) {
      warning(paste("Filter", this.filter, "is duplicated in filters"))
      this.filter.df <- this.filter.df %>%
        slice(1)
    }

    # try and convert where clause from sas to R
    this.sasclause <- this.filter.df$FLTWHERE
    this.rclause <- stream_filter_convwhere(this.sasclause)

    msg1 <- paste("\nSAS code:", this.sasclause, "\nwas converted to\nR code:", this.rclause)

    # what is the target df?

    if (this.filter.df$FLTTARGET == "ANL") {
      this.df <- anl_out
    }

    if (this.filter.df$FLTTARGET == "SLREF") {
      this.df <- asl_out
    }

    # try and apply the filtering
    new.df <- NULL

    new.df <- try(
      filter_(this.df, this.rclause),
      silent = TRUE
    )

    if ("try-error" %in% class(new.df)) {
      # failed - retain original dataset
      warning(paste("\nFilter ID=", this.filter, "was NOT applied.", msg1, "\n Error message:", new.df))
      cat(paste("\nFilter ID=", this.filter, "was NOT applied.", msg1, "\n Error message:", new.df))
      new.df <- this.df
    } else {
      # success
      msg2 <- paste0("\n", nrow(new.df), " of ", nrow(this.df), " observations selected from ", this.filter.df$FLTTARGET)
      cat(paste("\nFilter", this.filter, "applied", msg1, msg2, "\n"))
      if (is.null(actual_suffix)) {
        actual_suffix <- this.filter
      } else {
        actual_suffix <- paste(actual_suffix, this.filter, sep = "_")
      }
    }

    # update the output data sets
    if (this.filter.df$FLTTARGET == "ANL") {
      anl_out <- new.df
    } else if (this.filter.df$FLTTARGET == "SLREF") {
      asl_out <- new.df
    }
  }

  # finished filtering - combine results data
  # what variables to keep from SLREF?
  if (is.null(slref_keep)) {
    slref_keep <- usubjid
  } else {
    slref_keep <- c(slref_keep, usubjid) %>%
      unique()
  }

  # keep these variables only
  asl_out <- transmute_(asl_out, paste(slref_keep, collapse = ","))

  # use inner join to apply both slref and anl restrictions
  rc <- inner_join(asl_out, anl_out, by = usubjid)

  # report out what was applied in case of errors
  actual_suffix <- ifelse(is.null(actual_suffix), " ", actual_suffix)

  if (actual_suffix == suffix) {
    cat(paste0("\nSuffix ", suffix, " was applied"))
  } else {
    cat(paste0("\nNot all filters applied. \n", actual_suffix, " was applied instead of ", suffix))
  }
  cat(paste0("\n", nrow(rc), " of ", nrow(anl), " observations selected from ANL\n"))

  # return the filtered dataset
  return(rc)
}

#' Replicates the use of index function in sas for logic options
#'
#' Assumuption is that use in filters is to only resolve true vs false
#' Primarily for use with stream_filter and releated stream_filter_convwhere functions
#' @param string1 The string to search within - can be a vector
#' @param string2 The string to search for - must have length 1
#'
#' @return \code{boolean} indicator
#' @author Iain Bennett
#' @export
#'
#' @examples
#' AEACN <- c("DRUG MODIFIED", "DRUG STOPPED", "DOSE/DRUG MODIFIED")
#' stream_filter_index(AEACN, "DRUG MODIFIED")
stream_filter_index <- function(string1, string2) {
  rc <- regexpr(string2, string1, fixed = TRUE)
  rc <- ifelse(rc == -1, FALSE, TRUE)
  return(rc)
}


#' Convert SAS code to R code
#'
#' Will convert following sas operators: eq, =, le, lt, ge, gt, index
#' Will convert following logic: and, or, ()
#' Will convert all unquoted values to upper case (assumed to be variable names)
#' All quoted values will be returned with single quotes - may fail if have quotes within quotes
#' @param x a character string of SAS code
#'
#' @return a character string of R code
#' @author Iain Bennett
#' @export
#'
#' @examples
#' 
#' stream_filter_convwhere(x = "where X in (1 2 3 4) and Y gt 4 ")
#' stream_filter_convwhere(x = "where X = \"fred\" and Y gt 4 ")
stream_filter_convwhere <- function(x) {


  # convert double quotes to single quotes. May fail if quoted values exist.
  this.rclause <- gsub("\"", "'", x, fixed = TRUE)

  # convert non quoted values to upper case

  this.rclause.quotes <- strsplit(paste0(" ", this.rclause, " "), split = "'", fixed = TRUE) %>%
    unlist()

  inquotes <- rep(c(0, 1), length.out = length(this.rclause.quotes))

  for (j in 1:length(inquotes)) {

    # try and convert logic outside quotes
    if (inquotes[j] == 0) {
      this.rclause.quotes[j] <- toupper(this.rclause.quotes[j])
      this.rclause.quotes[j] <- gsub("=", "==", this.rclause.quotes[j], fixed = TRUE)
      this.rclause.quotes[j] <- gsub(" EQ ", " == ", this.rclause.quotes[j], fixed = TRUE)
      this.rclause.quotes[j] <- gsub(" NE ", " != ", this.rclause.quotes[j], fixed = TRUE)
      this.rclause.quotes[j] <- gsub(" LE ", " <= ", this.rclause.quotes[j], fixed = TRUE)
      this.rclause.quotes[j] <- gsub(" LT ", " < ", this.rclause.quotes[j], fixed = TRUE)
      this.rclause.quotes[j] <- gsub(" GE ", " >= ", this.rclause.quotes[j], fixed = TRUE)
      this.rclause.quotes[j] <- gsub(" GT ", " > ", this.rclause.quotes[j], fixed = TRUE)
      this.rclause.quotes[j] <- gsub(" AND ", " & ", this.rclause.quotes[j], fixed = TRUE)
      this.rclause.quotes[j] <- gsub(" OR ", " || ", this.rclause.quotes[j], fixed = TRUE)
      this.rclause.quotes[j] <- gsub(" IN ", " %in% c", this.rclause.quotes[j], fixed = TRUE)
      this.rclause.quotes[j] <- gsub("WHERE ", " ", this.rclause.quotes[j], fixed = TRUE)
      this.rclause.quotes[j] <- gsub("INDEX(", " stream_filter_index(", this.rclause.quotes[j], fixed = TRUE)
      this.rclause.quotes[j] <- gsub("UPCASE(", " toupper(", this.rclause.quotes[j], fixed = TRUE)
      this.rclause.quotes[j] <- gsub("DATEPART(", " as.Date(", this.rclause.quotes[j], fixed = TRUE)
    }
  }

  # collapse back to have quoted
  this.rclause <- paste(this.rclause.quotes, collapse = "'")



  # if contains an in  statement need to ensure commas exist

  if (grepl(" %in% c", this.rclause, fixed = TRUE)) {

    # get the clause (assume only 1 per filter...)
    temp1.str <- strsplit(this.rclause, split = " %in% c(", fixed = TRUE) %>%
      unlist()

    if (length(temp1.str) != 2) {
      stop("ERROR - function can't handle multiple IN operators.")
    } else {
      left.str <- temp1.str[1]
      in_right.str <- temp1.str[2]

      # find quoted items bracket
      temp2.str <- strsplit(in_right.str, split = "'", fixed = TRUE) %>%
        unlist()
      inquotes <- rep(c(0, 1), length.out = length(temp2.str))

      # find first not quoted right bracket

      right.idxV <- which(inquotes == 0 & grepl(")", temp2.str, fixed = TRUE))
      right.idxC <- regexpr(")", temp2.str[right.idxV], fixed = TRUE)

      temp3.str <- temp2.str

      temp3.str[right.idxV] <- substr(temp2.str[right.idxV], right.idxC, nchar(temp2.str[right.idxV]))

      right.str <- temp3.str[right.idxV:length(temp3.str)] %>%
        paste(collapse = "'")

      in.idx <- regexpr(right.str, in_right.str, fixed = TRUE) %>%
        as.numeric()

      in.str <- substr(in_right.str, 1, in.idx - 1)

      # now have left.str, in.str and right.str that contain seperate code parts
      # need to check the list of in and remove any commas to later replace between each element
      # first get any unquoted spaces or commas and split these

      temp4.str <- strsplit(in.str, split = "'", fixed = TRUE) %>%
        unlist()
      inquotes <- rep(c(0, 1), length.out = length(temp4.str))

      unquoted <- temp4.str[which(inquotes == 0)]
      quoted.items <- temp4.str[which(inquotes == 1)]

      # seperate any items unqouted
      temp5.str <- strsplit(unquoted, split = ",", fixed = TRUE) %>%
        unlist() %>%
        strsplit(split = " ", fixed = TRUE) %>%
        unlist()

      unquoted.items <- temp5.str[which(!(temp5.str %in% ""))]

      # should now have two vectors of strings
      # unquoted.items and quoted.items
      # first add the quoted items back in quoted.items <- unquoted.items
      if (length(quoted.items) > 0) {
        quoted.items <- paste0("'", quoted.items, "'")
      }

      # now collapse both strings adding commas

      all.items <- c(quoted.items, unquoted.items) %>%
        paste(collapse = " , ")

      # rebuild the complete code piece
      this.rclause <- paste0(left.str, " %in% c(", all.items, right.str)
    }
  }
  return(this.rclause)
}
