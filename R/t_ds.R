
#' Patient Disposition Table
#'
#' \code{t_ds} returns patient disposition table that corresponds to STREAM template DST01
#'
#' @param class system organ class variable.
#' @param term reason for discontinuation from study.
#' @param sub dataframe of additional subsets of terms
#' @param id unique subject identifier variable. If a particular subject has no
#'   adverse event then the subject \code{id} should be listed where
#'   \code{class} and \code{term} should be set to missing (i.e. \code{NA}).
#' @param col_by group variable that will be used for a column header. \code{col_by}
#'  has to be a factor and can not be missing.
#' @param total character string that will be used as a label for a column with
#'  pooled total population, default here is "All Patients", if set to \code{NULL} then
#'  the "All Patients" column is suppressed.
#'
#' @details this is an equivalent of the STREAM output \code{\%stream_t_summary(templates = aet01)}
#'   (\url{http://bioportal.roche.com/stream_doc/2_05/um/report_outputs_aet01.html})
#' @return \code{rtable} object
#'
#' @export
#'
#' @template author_zhanc107
#'
#' @examples
#' # Simple example
#' library(dplyr)
#' library(rtables)
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
#' tbl <- t_ds(
#'   class = ANL$CLASS,
#'   term = ANL$TERM,
#'   id = ANL$USUBJID,
#'   col_by = factor(ANL$ARM),
#'   total = "All Patients"
#' )
#'
#' tbl
#'
#' tbl2 <- t_ds(
#'   class = ANL$CLASS,
#'   term = ANL$TERM,
#'   sub = data.frame(GRADE = ANL$GRADE),
#'   id = ANL$USUBJID,
#'   col_by = factor(ANL$ARM),
#'   total = "All Patients"
#' )
#'
#' tbl2
#'
#' # simple example using osprey dummy dataset
#'
#' ANL <- rADSL
#'
#' tbl3 <- t_ds(
#'   class = ANL$EOSSTT,
#'   term = ANL$DCSREAS,
#'   id = ANL$USUBJID,
#'   col_by = factor(ANL$ARM),
#'   total = "All Patients"
#' )
#'
#' tbl3
t_ds <- function(class, term, sub = NULL, id, col_by, total = "All Patients") {

  # check input arguments ---------------------------
  col_n <- tapply(id, col_by, function(x) sum(!duplicated(x)))
  check_col_by(class, col_by_to_matrix(col_by), col_n, min_num_levels = 1)

  if (any("- Overall -" %in% term)) {
    stop("'- Overall -' is not a valid term, t_ae_oview reserves it for derivation")
  }
  if (any("All Patients" %in% col_by)) {
    stop("'All Patients' is not a valid col_by, t_ae_oview derives All Patients column")
  }

  if (!is.null(sub)) {
    check_input_length <- c(
      nrow(data.frame(class)),
      nrow(data.frame(term)),
      nrow(data.frame(id)),
      nrow(data.frame(col_by)),
      nrow(sub)
    )
    check_input_col <- c(
      ncol(data.frame(class)),
      ncol(data.frame(term)),
      ncol(data.frame(id)),
      ncol(data.frame(col_by))
    )
  } else {
    check_input_length <- c(
      nrow(data.frame(class)),
      nrow(data.frame(term)),
      nrow(data.frame(id)),
      nrow(data.frame(col_by))
    )
    check_input_col <- c(
      ncol(data.frame(class)),
      ncol(data.frame(term)),
      ncol(data.frame(id)),
      ncol(data.frame(col_by))
    )
  }

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
    id = id,
    class = class,
    term = term,
    col_by = col_by,
    stringsAsFactors = FALSE
  )

  df <- df %>% dplyr::arrange(class, term)

  if (!is.null(sub)) {
    df <- data.frame(
      id = id,
      class = class,
      term = term,
      sub,
      col_by = col_by,
      stringsAsFactors = FALSE
    )
    df <- df %>% dplyr::arrange(class, term)
  }

  df <- df %>% dplyr::mutate(
    class = ifelse(class == "", "None", as.character(class)),
    term = ifelse(term == "", "None", as.character(term))
  )

  # adding All Patients
  if (!is.null(total)) {
    total <- tot_column(total)

    if (total %in% levels(col_by)) {
      stop(paste("col_by can not have", total, "group."))
    }

    df <- duplicate_with_var(df, id = paste(df$id, "-", total), col_by = total)
  }

  # total N for column header
  n_total <- tapply(df$id, df$col_by, function(x) sum(!duplicated(x)))

  # start tabulating --------------------------------------------------------

  # split class, term, and subterms into lists
  recursive_split <- function(df, name_in, count, max_count) {
    if (nrow(df) == 0) {
      return()
    }
    l_t_comp <- t_helper_tabulate(
      df_id = df,
      n = n_total,
      checkcol = "uniqueid",
      term = name_in,
      remove_dupl = TRUE,
      with_percent = TRUE
    )

    if (count == max_count || (!is.null(nrow(split(df, df[, count])))) && nrow(split(df, df[, count])) == 0) {
      l_t_ov <- t_helper_tabulate(
        df_id = df,
        n = n_total,
        checkcol = "uniqueid",
        term = name_in,
        remove_dupl = TRUE,
        with_percent = TRUE
      )

      l_t_terms <- mapply(function(df_i, term) {
        t_helper_tabulate(
          df_id = df_i,
          n = n_total,
          checkcol = "uniqueid",
          term = term,
          remove_dupl = TRUE,
          with_percent = TRUE
        )
      }, split(df, df[, count]), names(split(df, df[, count])), SIMPLIFY = FALSE)
      l_t_terms <- list(l_t_ov, l_t_terms)
      return(l_t_terms)
    } else {
      out <- mapply(
        recursive_split,
        split(df, df[, count]),
        names(split(df, df[, count])),
        count + 1,
        max_count
      )

      l_out <- list(l_t_comp, out)
      return(l_out)
    }
  }

  # split into lists of lists of subterms
  l_t_class_terms <- mapply(recursive_split,
    df = split(df, df$class),
    name_in = names(split(df, df$class)),
    count = rep(3, length(split(df, df$class))),
    max_count = rep(ncol(df) - 1, length(split(df, df$class))),
    SIMPLIFY = FALSE
  )

  tbls_all <- remove_null(l_t_class_terms)
  tbls_class <- Map(recursive_indent, tbls_all, rep(0, length(tbls_all)))
  tbl <- do.call(stack_rtables_condense, tbls_class)

  # remove NA rows
  index <- numeric(0)
  for (i in seq(1, length(tbl))) {
    if (attr(tbl[[i]], "row.name") == "None") {
      index <- c(index, i)
    }
  }
  if (length(index) > 0) {
    tbl <- tbl[-index, ]
  }
  return(tbl)
}
