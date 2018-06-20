
#' Create an AE Overview Summary Table (AET01)
#'
#' \code{t_ae_oview} returns adverse events according to STREAM format AET01
#'
#' @param id unique subject identifier variable. If a particular subject has no
#'   adverse event then the subject \code{id} should be listed where
#'   \code{class} and \code{term} should be set to missing (i.e. \code{NA}).
#' @param class system organ class variable.
#' @param term preferred term variable.
#' @param dthfl subject death flag variable.
#' @param dcsreas reason for discontinuation from study variable.
#' @param aesdth results in death variable.
#' @param aeser serious event variable.
#' @param aeacn action taken with study treatment variable.
#' @param arel analysis causality variable.
#' @param aerel causality variable.
#' @param atoxgr analysis toxicity grade variable.
#' @param col_by group variable that will be used for a column header. \code{col_by}
#'  has to be a factor and can not be missing.
#' @param total character string that will be used as a label for a column with
#'  pooled total population, default here is "All Patients", if set to "NONE" then
#'  the "All Patients" column is suppressed.
#'
#'
#' @return \code{rtable} object
#'
#' @export
#'
#' @author Carolyn Zhang
#'
#' @examples
#' library(dplyr)
#' suppressPackageStartupMessages(library(tidyverse))
#' library(rtables)
#' adae <- read_bce("/opt/BIOSTAT/home_ext2/qit3/cdt70194/go39733/libraries/adae.sas7bdat")
#'
#' tbl <- t_ae_oview(
#'    id = adae$USUBJID,
#'    class = adae$AESOC,
#'    term = adae$AEDECOD,
#'    dthfl = adae$DTHFL,
#'    dcsreas = adae$DCSREAS,
#'    aesdth = adae$AESDTH,
#'    aeser = adae$AESER,
#'    aeacn = adae$AEACN,
#'    arel = adae$AREL,
#'    aerel = adae$AEREL,
#'    aetoxgr = adae$AETOXGR,
#'    col_by = factor(adae$ARM),
#'    total="All Patients"
#' )
#'
#' tbl
#'
#' tb2 <- t_ae_oview(
#'    id = adae$USUBJID,
#'    class = adae$AESOC,
#'    term = adae$AEDECOD,
#'    dthfl = adae$DTHFL,
#'    dcsreas = adae$DCSREAS,
#'    aesdth = adae$AESDTH,
#'    aeser = adae$AESER,
#'    aeacn = adae$AEACN,
#'    arel = adae$AREL,
#'    aerel = adae$AEREL,
#'    aetoxgr = adae$AETOXGR,
#'    col_by = factor(adae$ARM),
#'    total="NONE"
#' )
#'
#' tb2
#'
#'

t_ae_oview <- function(id,
                       class,
                       term,
                       dthfl,
                       dcsreas,
                       aesdth,
                       aeser,
                       aeacn,
                       arel,
                       aerel,
                       aetoxgr,
                       col_by,
                       total="All Patients") {

  #check input arguments ---------------------------
  check_col_by(col_by, min_num_levels = 1)

  if (total %in% levels(col_by))
    stop(paste('col_by can not have', total, 'group.'))

  if (any("- Overall -" %in% term))
    stop("'- Overall -' is not a valid term, t_ae_oview reserves it for derivation")
  if (any("All Patients" %in% col_by))
    stop("'All Patients' is not a valid col_by, t_ae_oview derives All Patients column")

  if (any(class == "", na.rm = TRUE))
    stop("empty string is not a valid class, please use NA if data is missing")
  if (any(term == "", na.rm = TRUE))
    stop("empty string is not a valid term, please use NA if data is missing")

  #prepare data ------------------------------------
  df <- data.frame(class = class,
                   term = term,
                   id = id,
                   col_by = col_by,
                   stringsAsFactors = FALSE)

  #includes death flag
  df_d <- data.frame(id = id,
                     dthfl = toupper(dthfl),
                     col_by = col_by,
                     stringsAsFactors = FALSE)

  #includes withdrawn flag
  df_w <- data.frame(id = id,
                     dcsreas = toupper(dcsreas),
                     col_by = col_by,
                     stringsAsFactors = FALSE)

  #includes fatal flag
  df_flags <- data.frame(id = id,
                         aesdth = toupper(aesdth),
                         aeser = toupper(aeser),
                         aeacn = toupper(aeacn),
                         arel = toupper(arel),
                         aerel = toupper(aerel),
                         aetoxgr = toupper(aetoxgr),
                         col_by = col_by,
                         stringsAsFactors = FALSE)

  # adding All Patients
  if(total != "NONE"){
    df <- duplicate_with_var(df, id = paste(df$id, "-", total), col_by = total)
    df_d <- duplicate_with_var(df_d, id = paste(df_d$id, "-", total), col_by = total)
    df_w <- duplicate_with_var(df_w, id = paste(df_w$id, "-", total), col_by = total)
    df_flags <- duplicate_with_var(df_flags, id = paste(df_flags$id, "-", total), col_by = total)
  }


  # total N for column header
  N <- tapply(df$id, df$col_by, function(x) (sum(!duplicated(x))))

  # need to remove any record that is missing class or term
  df <- na.omit(df)
  df_d <- na.omit(df_d)
  df_w <- na.omit(df_w)
  df_flags <- na.omit(df_flags)

  dsm <- c("DRUG INTERRUPTED", "DOSE INCREASED", "DOSE REDUCED")

  #convert Y/N or event tag to 1/0
  df_d$dthfl <- factor(if_else(df_d$dthfl == "Y", 1, 0))
  df_w$dcsreas <- factor(if_else(df_w$dcsreas == "ADVERSE EVENT", 1, 0))
  df_flags$fatal <- factor(if_else(df_flags$aesdth == "Y", 1, 0))
  df_flags$ser  <- factor(if_else(df_flags$aeser == "Y", 1, 0))
  df_flags$serwd <- factor(if_else(df_flags$aeser == "Y" &
                                     df_flags$aeacn == "DRUG WITHDRAWN", 1, 0))
  df_flags$serdsm <- factor(if_else(df_flags$aeser == "Y" &
                                    df_flags$aeacn %in% dsm, 1, 0))
  df_flags$relser <- factor(if_else(df_flags$aeser == "Y" &
                                    df_flags$arel == "Y", 1, 0))
  df_flags$wd  <- factor(if_else(df_flags$aeacn == "DRUG WITHDRAWN", 1, 0))
  df_flags$dsm <- factor(if_else(df_flags$aeacn %in% dsm, 1, 0))
  df_flags$rel <- factor(if_else(df_flags$aerel == "Y", 1, 0))
  df_flags$relwd <- factor(if_else(df_flags$aerel == "Y" &
                                   df_flags$aeacn == "DRUG WITHDRAWN", 1, 0))
  df_flags$reldsm <- factor(if_else(df_flags$arel == "Y" &
                                    df_flags$aeacn %in% dsm, 1, 0))
  df_flags$ctc35 <- factor(if_else(df_flags$aetoxgr %in% c("3", "4", "5"), 1, 0))

  # start tabulating --------------------------------------------------------
  n_cols <- nlevels(col_by)

  #overview information: total patients w/ >= 1 AE, total AE, total death, total withdrawn

  #Overview: total num patients with at least one AE
  df_patients <- list("Total number of patients with at least one AE" = df)
  tbl_overall_patients <- lapply(df_patients, function(df_i) {

    t_helper_tabulate(df_id = df_i,
                      N = N,
                      checkcol = " ",
                      term = "Total number of patients with at least one AE",
                      remove_dupl = TRUE,
                      with_percent = TRUE)

  })

  #Overview: total num of ae's - includes duplicates per patient
  df_ae <- list("Total number of AEs" = df)
  tbl_overall_ae <- lapply(df_ae, function(df_i) {

    t_helper_tabulate(df_id = df_i,
                      N = N,
                      checkcol = " ",
                      term = "Total number of AEs",
                      remove_dupl = FALSE,
                      with_percent = FALSE)

  })

  #Overview: total num of deaths
  df_death <- list("Total number of deaths" = df_d)
  tbl_overall_deaths <- lapply(df_death, function(df_i) {

    t_helper_tabulate(df_id = df_i,
                      N = N,
                      checkcol = "dthfl",
                      term = "Total number of deaths",
                      remove_dupl = TRUE,
                      with_percent = TRUE)

  })

  #Overview: Total number of patients withdrawn from study due to an AE
  df_withdrawn <- list("Total number of patients withdrawn from study due to an AE" = df_w)
  tbl_overall_withdrawn <- lapply(df_withdrawn, function(df_i) {

    t_helper_tabulate(df_id = df_i,
                      N = N,
                      checkcol = "dcsreas",
                      term = "Total number of patients withdrawn from study due to an AE",
                      remove_dupl = TRUE,
                      with_percent = TRUE)

  })

  #Summary table: individual components
  df_ind <- list("AE with fatal outcome" = df_flags,
                 "Serious AE" = df_flags,
                 "Serious AE leading to withdrawal from treatment" = df_flags,
                 "Serious AE leading to dose modification/interruption" = df_flags,
                 "Related Serious AE" = df_flags,
                 "AE leading to withdrawal from treatment" = df_flags,
                 "AE leading to dose modification/interruption" = df_flags,
                 "Related AE" = df_flags,
                 "Related AE leading to withdrawal from treatment" = df_flags,
                 "Related AE leading to dose modification/interruption" = df_flags,
                 "Grade 3-5 AE" = df_flags
  )

  tbl_ind <- mapply(function(df_i, term, c_col) {

    t_helper_tabulate(df_id = df_i,
                      N = N,
                      checkcol = c_col,
                      term = term,
                      remove_dupl = TRUE,
                      with_percent = TRUE)


  },df_ind,  names(df_ind),
  c("fatal", "ser", "serwd", "serdsm", "relser", "wd", "dsm", "rel", "relwd", "reldsm","ctc35"),
  SIMPLIFY = FALSE)

  # put together final table ------------------
  tbls_overview <- c(
    list("Total number of patients with at least one AE" = tbl_overall_patients),
    list("Total number of AEs" = tbl_overall_ae),
    list("Total number of deaths" = tbl_overall_deaths),
    list("Total number of patients withdrawn from study due to an AE" = tbl_overall_withdrawn)
  )

  tbls_ov <- Map(function(tbls_i) {
    lt1 <- Map(shift_label_table_no_grade, tbls_i, names(tbls_i))
    t2 <- do.call(stack_rtables, lt1)
  }, tbls_overview)

  tbls_ind <- c(list("Total number of patients with at least one" = tbl_ind))
  tbls_class <- Map(function(tbls_i, class_i) {
    lt1 <- Map(shift_label_table_no_grade, tbls_i, names(tbls_i))
    t2 <- do.call(stack_rtables, lt1)
    add_ae_class(indent_table(t2, 1), class_i)
  }, tbls_ind, names(tbls_ind))


  tbl_total <- do.call(stack_rtables, tbls_ov)
  tbl_cl <- do.call(stack_rtables, tbls_class)

  tbl <- rbind(tbl_total, tbl_cl)

  tbl

}


#tabulation function for condition checks
t_helper_tabulate <- function(df_id, N, checkcol, term, remove_dupl, with_percent){

  if(remove_dupl){
    df_id <- df_id[!duplicated(df_id$id), ]
  }
  if(checkcol == " "){
    if(with_percent){
      tbl <- rtabulate(
        na.omit(df_id),
        row_by_var = no_by(""),
        col_by_var = "col_by",
        FUN = count_perc_col_N,
        N = N,
        format = "xx (xx.xx%)"
      )
    }
    else{
      tbl <- rtabulate(
        na.omit(df_id),
        row_by_var = no_by(""),
        col_by_var = "col_by",
        FUN = count_col_N,
        N = N,
        format = "xx"
      )
    }
  }
  else{
    tbl <- rtabulate(
      na.omit(df_id),
      row_by_var = checkcol,
      col_by_var = "col_by",
      FUN = count_perc_col_N,
      N = N,
      format = "xx (xx.xx%)"
    )

    if(dim(tbl)[1] > 1){
      tbl <- tbl[dim(tbl)[1]]
    }
    else{
      for(i in 1:dim(tbl)[2]){
        tbl[[1]][[i]] <- rcell(0)
      }
    }

  }

  attr(tbl[[1]], "row.name") <- term

  header(tbl) <- rheader(
    rrowl("", levels(df_id$col_by)),
    rrowl("", unname(N), format = "(N=xx)")
  )
  tbl

}

# checks if there is any case and derives counts, otherwise 0
count_col_N <- function(x_cell, N) {
  N_i <- if (nrow(x_cell) == 0) 0 else N[x_cell$col_by[1]]
  if (N_i > 0) {
    length(x_cell$id) # obtaining the total
  } else {
    rcell(0, format = "xx")
  }
}


#adds row name to rtable
shift_label_table_no_grade <- function(tbl, term) {
  attr(tbl[[1]], "row.name") <- term
  tbl
}




