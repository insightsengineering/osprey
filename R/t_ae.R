
#' Adverse Events by System Organ Class and Preferred Term
#'
#' \code{t_ae} returns adverse events summary table that corresponds to STREAM template AET02
#'
#' @param class system organ class variable.
#' @param term preferred term variable.
#' @param id unique subject identifier variable. If a particular subject has no
#'   adverse event then the subject \code{id} should be listed where
#'   \code{class} and \code{term} should be set to missing (i.e. \code{NA}).
#' @param col_by group variable that will be used for a column header. \code{col_by}
#'  has to be a factor and can not be missing.
#' @param total character string that will be used as a label for a column with
#'  pooled total population, default here is "All Patients", if set to \code{NULL} then
#'  the "All Patients" column is suppressed.
#'
#' @details this is an equivalent of the STREAM output \code{\%stream_t_summary(templates = aet02)}
#'   (\url{http://bioportal.roche.com/stream_doc/2_05/um/report_outputs_aet02.html})
#'
#'
#' @return \code{rtable} object
#'
#' @export
#'
#' @template author_zhanc107
#'
#' @import tibble
#' @import dplyr
#' @importFrom rtables rrow
#' @examples
#' # Simple example
#' library(dplyr)
#'
#' ASL <- tibble(
#'   USUBJID = paste0("id-", 1:10),
#'   ARM = paste("ARM", LETTERS[rep(c(1, 2), c(3, 7))])
#' )
#'
#'
#' ae_lookup <- tribble(
#'   ~CLASS, ~TERM, ~GRADE,
#'   "cl A", "trm A_1/2", 1,
#'   "cl A", "trm A_2/2", 2,
#'   "cl B", "trm B_1/3", 2,
#'   "cl B", "trm B_2/3", 3,
#'   "cl B", "trm B_3/3", 1,
#'   "cl C", "trm C_1/1", 1
#' )
#'
#' AAE <- cbind(
#'   tibble(
#'     USUBJID = ASL$USUBJID[c(2, 2, 2, 3, 3, 4, 4, 4, 4, 5, 6, 6, 7, 7)]
#'   ),
#'   ae_lookup[c(1, 1, 2, 6, 4, 2, 2, 3, 4, 2, 1, 5, 4, 6), ]
#' )
#'
#' ANL <- left_join(ASL, AAE, by = "USUBJID")
#'
#'
#'
#' tbl <- t_ae(
#'   class = ANL$CLASS,
#'   term = ANL$TERM,
#'   id = ANL$USUBJID,
#'   col_by = factor(ANL$ARM),
#'   total = "All Patients"
#' )
#'
#' tbl
#' # Simple example 2
#'
#' ADSL <- rADSL %>% select(USUBJID, STUDYID, ARM)
#' AAE <- rADAE %>% select(USUBJID, STUDYID, ARM, AEBODSYS, AEDECOD)
#' ANL <- left_join(AAE, ADSL, by = c("USUBJID", "STUDYID", "ARM"))
#'
#'
#' tbl2 <- t_ae(
#'   class = ANL$AEBODSYS,
#'   term = ANL$AEDECOD,
#'   id = ANL$USUBJID,
#'   col_by = factor(ANL$ARM),
#'   total = NULL
#' )
#'
t_ae <- function(class, term, id, col_by, total = "All Patients") {

  # check input arguments ---------------------------
  col_n <- tapply(id, col_by, function(x) sum(!duplicated(x)))
  check_col_by(col_by, col_n, min_num_levels = 1)

  if (any("- Overall -" %in% term)) {
    stop("'- Overall -' is not a valid term, t_ae reserves it for derivation")
  }
  if (any("All Patients" %in% col_by)) {
    stop("'All Patients' is not a valid col_by, t_ae derives All Patients column")
  }

  check_input_length <- c(nrow(data.frame(class)),
                          nrow(data.frame(term)),
                          nrow(data.frame(id)),
                          nrow(data.frame(col_by)))
  check_input_col <- c(ncol(data.frame(class)),
                       ncol(data.frame(term)),
                       ncol(data.frame(id)),
                       ncol(data.frame(col_by)))

  if (length(unique(check_input_length)) > 1) {
    stop("invalid arguments: check that the length of input arguments are identical")
  }
  if (length(unique(check_input_col)) > 1 || unique(check_input_col) != 1) {
    stop("invalid arguments: check that the inputs have a single column")
  }
  if (any(check_input_length == 0) || any(check_input_col == 0)) {
    stop("invalid arguments: check that inputs are not null")
  }

  # prepare data ------------------------------------
  df <- data.frame(
    class = class,
    term = term,
    subjid = id,
    col_by = col_by,
    stringsAsFactors = FALSE
  )

  df <- df %>% dplyr::arrange(class, term)

  df <- df %>% dplyr::mutate(
    class = ifelse(class == "", "No Coding Available", class),
    term = ifelse(term == "", "No Coding Available", term)
  )

  class_label <- attr(class, "label")
  term_label <- attr(term, "label")

  if (is.null(class_label)) class_label <- deparse(substitute(class))
  if (is.null(term_label)) term_label <- deparse(substitute(term))

  # adding All Patients
  if (!is.null(total)) {
    total <- tot_column(total)
    if (total %in% levels(col_by)) {
      stop(paste("col_by can not have", total, "group."))
    }

    df <- duplicate_with_var(df, subjid = paste(df$subjid, "-", total), col_by = total)
  }

  # total N for column header (with All Patients)
  n_total <- tapply(df$subjid, df$col_by, function(x) sum(!duplicated(x)))

  # need to remove extra records that came from subject level data
  # when left join was done. also any record that is missing class or term
  df <- na.omit(df)

  # start tabulating --------------------------------------------------------
  n_cols <- nlevels(col_by)

  # class and term chunks
  l_t_class_terms <- lapply(split(df, df$class), function(df_s_cl) {
    df_s_cl_term <- c(
      list("Total number of patients with at least one adverse event" = df_s_cl),
      split(df_s_cl, df_s_cl$term)
    )

    df_s_cl_num_ae <- list("Total number of events" = df_s_cl)

    # count number of AE - includes duplicates per patient
    l_t_num_ae <- lapply(df_s_cl_num_ae, function(df_i) {
      df_id <- na.omit(data.frame(df_i$subjid, df_i$col_by))
      colnames(df_id) <- c("id", "col_by")

      tbl <- rtabulate(
        df_id,
        row_by = no_by(""),
        col_by = df_id$col_by,
        FUN = count_col_N,
        col_wise_args = list(n_i = n_total),
        format = "xx"
      )

      header(tbl) <- rheader(
        rrowl("", levels(df_id$col_by)),
        rrowl("", unname(n_total), format = "(N=xx)")
      )
      tbl
    })

    l_t_terms <- lapply(df_s_cl_term, function(df_i) {
      df_id <- data.frame(df_i$subjid, df_i$col_by)
      colnames(df_id) <- c("id", "col_by")
      df_id <- df_id[!duplicated(df_id$id), ]


      tbl <- rtabulate(
        na.omit(df_id),
        row_by = no_by(""),
        col_by = df_id$col_by,
        FUN = count_perc_col_N,
        col_wise_args = list(n_i = n_total),
        format = "xx (xx.x%)"
      )

      header(tbl) <- rheader(
        rrowl("", levels(df_id$col_by)),
        rrowl("", unname(n_total), format = "(N=xx)")
      )
      tbl
    })

    l_t_summary <- c(l_t_terms[1], l_t_num_ae)
    l_t_terms <- c(l_t_terms[2:length(l_t_terms)])

    # sort terms by total
    n_total_any <- vapply(l_t_terms, function(tbl) {
      a <- 0
      for (i in c(1:n_cols)) {
        a <- a + tbl[1, i][1]
      }
      a
    }, numeric(1))

    l_t_terms <- l_t_terms[order(-n_total_any, names(l_t_terms), decreasing = FALSE)]


    l_t_terms <- c(l_t_summary, l_t_terms)
  }) #---------------------------end class and term chunk

  # now sort tables
  n_total_overall <- vapply(l_t_class_terms, function(tbl) {
    a <- 0
    for (i in c(1:n_cols)) {
      a <- a + tbl[[1]][1, i][1]
    }
    a
  }, numeric(1))

  l_t_class_terms <- l_t_class_terms[order(-n_total_overall, names(l_t_class_terms), decreasing = FALSE)]

  # Overall: total num patients
  df_patients <- list("Total number of patients with at least one adverse event" = df)
  tbl_overall_patients <- lapply(df_patients, function(df_i) {
    df_id <- data.frame(df_i$subjid, df_i$col_by)
    colnames(df_id) <- c("id", "col_by")
    df_id <- df_id[!duplicated(df_id$id), ]

    tbl <- rtabulate(
      na.omit(df_id),
      row_by = no_by(""),
      col_by = df_id$col_by,
      FUN = count_perc_col_N,
      col_wise_args = list(n_i = n_total),
      format = "xx (xx.x%)"
    )

    header(tbl) <- rheader(
      rrowl("", levels(df_id$col_by)),
      rrowl("", unname(n_total), format = "(N=xx)")
    )
    tbl
  })

  # Overall: total num of events
  df_ae <- list("Overall total number of events" = df)

  # count number of AE - includes duplicates per patient
  tbl_overall_ae <- lapply(df_ae, function(df_i) {
    df_id <- data.frame(df_i$subjid, df_i$col_by)
    colnames(df_id) <- c("id", "col_by")

    tbl <- rtabulate(
      na.omit(df_id),
      row_by = no_by(""),
      col_by = df_id$col_by,
      FUN = count_col_N,
      col_wise_args = list(n_i = n_total),
      format = "xx"
    )

    header(tbl) <- rheader(
      rrowl("", levels(df_id$col_by)),
      rrowl("", unname(n_total), format = "(N=xx)")
    )
    tbl
  })

  tbls_all <- l_t_class_terms

  tbls_overview <- c(
    list("Total number of patients with at least one adverse event" = tbl_overall_patients),
    list("Overall total number of events" = tbl_overall_ae)
  )

  tbls_class <- Map(function(tbls_i, class_i) {
    lt1 <- Map(shift_label_table_no_grade, tbls_i, names(tbls_i))
    t2 <- do.call(stack_rtables_condense, lt1)
    add_ae_class(indent_table(t2, 1), class_i)
  }, tbls_all, names(tbls_all))

  tbls_ov <- Map(function(tbls_i) {
    lt1 <- Map(shift_label_table_no_grade, tbls_i, names(tbls_i))
    t2 <- do.call(stack_rtables_condense, lt1) # nolint
  }, tbls_overview)


  tbl_cl <- do.call(stack_rtables, tbls_class)
  tbl_total <- do.call(stack_rtables, tbls_ov)


  header <- attr(tbl_cl, "header")
  tbl_with_empty_rows <- rtablel(header = header, replicate(1, rrow()))

  tbl <- rbind(tbl_total, tbl_with_empty_rows, tbl_cl)

  attr(attr(tbl, "header")[[1]], "row.name") <- class_label
  attr(attr(tbl, "header")[[2]], "row.name") <- term_label
  attr(attr(tbl, "header")[[2]], "indent") <- 1

  return(tbl)
}
