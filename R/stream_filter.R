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
#' library(scda)
#' ASL <- synthetic_cdisc_data("latest")$adsl
#' ATE <- synthetic_cdisc_data("latest")$adaette
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

  n_filters <- length(filters_to_apply)

  for (i in 1:n_filters) {
    this_filter <- filters_to_apply[i]

    # find filter meta data

    this_filter_df <- unique(dplyr::filter(filters, .data$ID == this_filter))

    if (nrow(this_filter_df) == 0) {
      stop(paste("Filter", this_filter, "not found in filters"))
    }

    if (nrow(this_filter_df) > 1) {
      warning(paste("Filter", this_filter, "is duplicated in filters"))
      this_filter_df <- slice(this_filter_df, 1)
    }

    # try and convert where clause from sas to R
    this_sasclause <- this_filter_df$FLTWHERE
    this_rclause <- stream_filter_convwhere(this_sasclause)

    msg1 <- paste("\nSAS code:", this_sasclause, "\nwas converted to\nR code:", this_rclause)

    # what is the target df?

    if (this_filter_df$FLTTARGET == "ANL") {
      this_df <- anl_out
    }

    if (this_filter_df$FLTTARGET == "SLREF") {
      this_df <- asl_out
    }

    # try and apply the filtering
    new_df <- NULL

    new_df <- try(
      filter_(this_df, this_rclause),
      silent = TRUE
    )

    if (is(new_df, "try-error")) {
      # failed - retain original dataset
      warning(paste("\nFilter ID=", this_filter, "was NOT applied.", msg1, "\n Error message:", new_df))
      cat(paste("\nFilter ID=", this_filter, "was NOT applied.", msg1, "\n Error message:", new_df))
      new_df <- this_df
    } else {
      # success
      msg2 <- paste0(
        "\n",
        nrow(new_df),
        " of ",
        nrow(this_df),
        " observations selected from ", this_filter_df$FLTTARGET
      )
      cat(paste("\nFilter", this_filter, "applied", msg1, msg2, "\n"))
      actual_suffix <- if (is.null(actual_suffix)) {
        this_filter
      } else {
        paste(actual_suffix, this_filter, sep = "_")
      }
    }

    # update the output data sets
    if (this_filter_df$FLTTARGET == "ANL") {
      anl_out <- new_df
    } else if (this_filter_df$FLTTARGET == "SLREF") {
      asl_out <- new_df
    }
  }

  # finished filtering - combine results data
  # what variables to keep from SLREF?
  slref_keep <- if (is.null(slref_keep)) {
    usubjid
  } else {
    unique(c(slref_keep, usubjid))
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
#' Assumption is that use in filters is to only resolve true vs false
#' Primarily for use with stream_filter and related stream_filter_convwhere functions
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
  this_rclause <- gsub("\"", "'", x, fixed = TRUE)

  # convert non quoted values to upper case

  this_rclause_quotes <- strsplit(paste0(" ", this_rclause, " "), split = "'", fixed = TRUE) %>%
    unlist()

  inquotes <- rep(c(0, 1), length.out = length(this_rclause_quotes))

  for (j in seq_along(inquotes)) {

    # try and convert logic outside quotes
    if (inquotes[j] == 0) {
      this_rclause_quotes[j] <- toupper(this_rclause_quotes[j])
      this_rclause_quotes[j] <- gsub("=", "==", this_rclause_quotes[j], fixed = TRUE)
      this_rclause_quotes[j] <- gsub(" EQ ", " == ", this_rclause_quotes[j], fixed = TRUE)
      this_rclause_quotes[j] <- gsub(" NE ", " != ", this_rclause_quotes[j], fixed = TRUE)
      this_rclause_quotes[j] <- gsub(" LE ", " <= ", this_rclause_quotes[j], fixed = TRUE)
      this_rclause_quotes[j] <- gsub(" LT ", " < ", this_rclause_quotes[j], fixed = TRUE)
      this_rclause_quotes[j] <- gsub(" GE ", " >= ", this_rclause_quotes[j], fixed = TRUE)
      this_rclause_quotes[j] <- gsub(" GT ", " > ", this_rclause_quotes[j], fixed = TRUE)
      this_rclause_quotes[j] <- gsub(" AND ", " & ", this_rclause_quotes[j], fixed = TRUE)
      this_rclause_quotes[j] <- gsub(" OR ", " || ", this_rclause_quotes[j], fixed = TRUE)
      this_rclause_quotes[j] <- gsub(" IN ", " %in% c", this_rclause_quotes[j], fixed = TRUE)
      this_rclause_quotes[j] <- gsub("WHERE ", " ", this_rclause_quotes[j], fixed = TRUE)
      this_rclause_quotes[j] <- gsub("INDEX(", " stream_filter_index(", this_rclause_quotes[j], fixed = TRUE)
      this_rclause_quotes[j] <- gsub("UPCASE(", " toupper(", this_rclause_quotes[j], fixed = TRUE)
      this_rclause_quotes[j] <- gsub("DATEPART(", " as.Date(", this_rclause_quotes[j], fixed = TRUE)
    }
  }

  # collapse back to have quoted
  this_rclause <- paste(this_rclause_quotes, collapse = "'")

  # if contains an in  statement need to ensure commas exist
  if (grepl(" %in% c", this_rclause, fixed = TRUE)) {

    # get the clause (assume only 1 per filter...)
    temp1_str <- strsplit(this_rclause, split = " %in% c(", fixed = TRUE) %>%
      unlist()

    if (length(temp1_str) != 2) {
      stop("ERROR - function can't handle multiple IN operators.")
    } else {
      left_str <- temp1_str[1]
      in_right_str <- temp1_str[2]

      # find quoted items bracket
      temp2_str <- strsplit(in_right_str, split = "'", fixed = TRUE) %>%
        unlist()
      inquotes <- rep(c(0, 1), length.out = length(temp2_str))

      # find first not quoted right bracket

      right_idxv <- which(inquotes == 0 & grepl(")", temp2_str, fixed = TRUE))
      right_idxc <- regexpr(")", temp2_str[right_idxv], fixed = TRUE)

      temp3_str <- temp2_str

      temp3_str[right_idxv] <- substr(temp2_str[right_idxv], right_idxc, nchar(temp2_str[right_idxv]))

      right_str <- temp3_str[right_idxv:length(temp3_str)] %>%
        paste(collapse = "'")

      in_idx <- regexpr(right_str, in_right_str, fixed = TRUE) %>%
        as.numeric()

      in_str <- substr(in_right_str, 1, in_idx - 1)

      # now have left.str, in.str and right.str that contain seperate code parts
      # need to check the list of in and remove any commas to later replace between each element
      # first get any unquoted spaces or commas and split these

      temp4_str <- strsplit(in_str, split = "'", fixed = TRUE) %>%
        unlist()
      inquotes <- rep(c(0, 1), length.out = length(temp4_str))

      unquoted <- temp4_str[which(inquotes == 0)]
      quoted_items <- temp4_str[which(inquotes == 1)]

      # seperate any items unqouted
      temp5_str <- strsplit(unquoted, split = ",", fixed = TRUE) %>%
        unlist() %>%
        strsplit(split = " ", fixed = TRUE) %>%
        unlist()

      unquoted_items <- temp5_str[which(!(temp5_str %in% ""))]

      # should now have two vectors of strings
      # unquoted.items and quoted.items
      # first add the quoted items back in quoted.items <- unquoted.items
      if (length(quoted_items) > 0) {
        quoted_items <- paste0("'", quoted_items, "'")
      }

      # now collapse both strings adding commas

      all_items <- c(quoted_items, unquoted_items) %>%
        paste(collapse = " , ")

      # rebuild the complete code piece
      this_rclause <- paste0(left_str, " %in% c(", all_items, right_str)
    }
  }
  return(this_rclause)
}
