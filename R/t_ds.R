#' Create a Patient Disposition (DST01) Table
#'
#' \code{t_ae} returns patient disposition according to STREAM format DST01
#'
#' @param id unique subject identifier variable. If a particular subject has no
#'   adverse event then the subject \code{id} should be listed where
#'   \code{class} and \code{term} should be set to missing (i.e. \code{NA}).
#' @param term reason for discontinuation from study.
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
#' # Simple example
#'
#' library(dplyr)
#' library(rtables)
#' library(random.cdisc.data)
#'
#'
#' adae <- read.bce("/opt/BIOSTAT/home/bundfuss/stream_um/str_para2/libraries/adae.sas7bdat")
#'
#' tbl <- t_ds(id = adae$USUBJID,
#'             term = adae$DCSREAS,
#'             col_by = factor(adae$ARM),
#'             total = "All Patients")
#'
#' tbl

t_ds <- function(id, term, col_by, total = "All Patients"){

  #check input arguments ---------------------------
  check_col_by(col_by, min_num_levels = 1)

  if (total %in% levels(col_by))
    stop(paste('col_by can not have', total, 'group.'))

  if (any("All Patients" %in% col_by))
    stop("'All Patients' is not a valid col_by, t_ds derives All Patients column")

  #prepare data ------------------------------------
  df <- data.frame(id = id,
                   term = term,
                   col_by = col_by,
                   stringsAsFactors = FALSE)

  # adding All Patients
  if(total != "NONE"){
    df <- duplicate_with_var(df, id = paste(df$id, "-", total), col_by = total)
  }

  # total N for column header
  N <- tapply(df$id, df$col_by, function(x) (sum(!duplicated(x))))

  # need to remove any record that is missing term
  df <- na.omit(df)

  # convert event tag to 1/0
  df$Compl <- factor(if_else(toupper(df$term) == "", 1, 0))
  df$Disc <- factor(if_else(toupper(df$term) != "", 1, 0))
  df$AE_fatal <- factor(if_else(toupper(df$term) == "AE WITH FATAL OUTCOME", 1, 0))
  df$Death <- factor(if_else(toupper(df$term) == "DEATH", 1, 0))
  df$Lost <- factor(if_else(toupper(df$term) == "LOST TO FOLLOW-UP", 1, 0))
  df$Non_comp <- factor(if_else(toupper(df$term) == "NON-COMPLIANCE WITH STUDY DRUG", 1, 0))
  df$Phys_dec <- factor(if_else(toupper(df$term) == "PHYSICIAN DECISION", 1, 0))
  df$Progr <- factor(if_else(toupper(df$term) == "PROGRESSIVE DISEASE", 1, 0))
  df$Prot <- factor(if_else(toupper(df$term) == "PROTOCOL DEVIATION", 1, 0))
  df$W_pg <- factor(if_else(toupper(df$term) == "WITHDRAWAL BY PARENT/GUARDIAN", 1, 0))
  df$W_sub <- factor(if_else(toupper(df$term) == "WITHDRAWAL BY SUBJECT", 1, 0))


  # start tabulating --------------------------------------------------------
  n_cols <- nlevels(col_by)

  #Overview: total num patients that completed/discontinued study
  df_total_c <- list("Completed study" = df)
  tbl_overall_c <- lapply(df_total_c, function(df_i) {

    t_helper_tabulate(df_id = df_i,
                      N = N,
                      checkcol = "Compl",
                      term = "Completed Study",
                      remove_dupl = TRUE,
                      with_percent = TRUE)

  })
  df_total_d <- list("Discontinued study" = df)
  tbl_overall_d <- lapply(df_total_d, function(df_i) {

    t_helper_tabulate(df_id = df_i,
                      N = N,
                      checkcol = "Disc",
                      term = "Discontinued study",
                      remove_dupl = TRUE,
                      with_percent = TRUE)

  })

  #Summary table: individual components
  df_ind <- list("Adverse event" = df,
                 "Death" = df,
                 "Lost to follow-up" = df,
                 "Non-compliance with study drug" = df,
                 "Physician decision" = df,
                 "Progressive disease" = df,
                 "Protocol Deviation" = df,
                 "Withdrawal by parent/guardian" = df,
                 "Withdrawal by subject" = df
  )

  tbl_ind <- mapply(function(df_i, term, c_col) {

    t_helper_tabulate(df_id = df_i,
                      N = N,
                      checkcol = c_col,
                      term = term,
                      remove_dupl = TRUE,
                      with_percent = TRUE)


  },df_ind,  names(df_ind),
  c("AE_fatal", "Death", "Lost", "Non_comp", "Phys_dec", "Progr", "Prot", "W_pg", "W_sub"),
  SIMPLIFY = FALSE)

  # put together final table ------------------
  tbls_overall = list("Completed study" = tbl_overall_c,
                      "Discontinued study" = tbl_overall_d)
  tbls_ov <- Map(function(tbls_i) {
    lt1 <- Map(shift_label_table_no_grade, tbls_i, names(tbls_i))
    t2 <- do.call(stack_rtables, lt1)
  }, tbls_overall)

  tbls_ind <- c(list(" " = tbl_ind))
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



