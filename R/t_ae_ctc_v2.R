
#' Adverse Events Table by Highest NCI CTCAE Grade
#'
#' \code{t_ae_ctc_v2} returns adverse events sorted by highest NCI (National Cancer
#'  Institute) CTCAE (common terminology criteria for adverse avents) grade. It
#'  corresponds to STREAM template AET01.
#'
#' @param class system organ class variable.
#' @param term preferred term variable.
#' @param id unique subject identifier variable. If a particular subject has no
#'   adverse event then the subject \code{id} should be listed where
#'   \code{class} and \code{term} should be set to missing (i.e. \code{NA}).
#' @param grade grade of adverse event variable.
#' @param col_by group variable that will be used for a column header. \code{col_by}
#'  has to be a factor and can not be missing. See 'Examples'.
#' @param total character string that will be used as a label for a column with
#'  pooled total population, default is "All Patients", if set to \code{NULL} then
#'  the "All Patients" column is suppressed.
#' @param grade_levels ordered values of possible of grades in a form of
#'   \code{x:y}, default is \code{1:5}. This assures a proper fill in for
#'   grades, see 'Details'.
#'
#' @details
#' \code{t_ae_ctc_v2} counts patients according to adverse events (AEs) of greatest
#'  intensity for system organ class (SOC) and overall rows and includes
#'  percentages based on the total number of patients in the column heading
#'  (i.e. "N=nnn"). If the intention is to use patients number from subject level
#'  dataset as N for percentage calculation then adeverse events dataset should
#'  be left joined to subject level dataset and the \code{col_by} variable should
#'  be dropped from adverse events dataset, see the example. Otherwise, N will be
#'  derived using adverse events dataset. At the preferred term (PT) level,
#'  multiple events within a patient of the same PT are counted once using the
#'  greatest intensity reported.
#'
#' \code{t_ae_ctc_v2} removes any non-complete records, e.g. if class or term are
#'  missing. If the intent is to preserve such records, then impute missing
#'  values before using \code{t_ae_ctc_v2}.
#'
#' \code{t_ae_ctc_v2} orders data by "All Patients" column from the most commonly
#'  reported SOC to the least frequent one. Within SOC, it sorts by decreasing
#'  frequency of PT. It brakes ties using SOC/PT names in alphabetical order.
#'
#' \code{t_ae_ctc_v2} fills in \code{col_by} and \code{grade} with \code{0} value
#' in case there was no AEs reported for particular \code{col_by} and/or
#' \code{grade} category. Use \code{grade_levels} to modify the range of existing
#' grades. If data does not have any records with \code{grade} 5 and the intent
#' is to show only grades 1-4 rows then use \code{grade_levels = 1:4}.
#'
#' @details this is an equivalent of the STREAM output \code{\%stream_t_summary(templates = aet04)}
#'   (\url{http://bioportal.roche.com/stream_doc/2_05/um/report_outputs_aet04.html})
#'
#' @export
#'
#' @author Edgar Manukya
#' @author Adrian Waddell
#' @template author_zhanc107
#'
#' @import tibble
#' @import dplyr
#' @examples
#' # Simple example
#' library(tibble)
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
#' \dontrun{
#' tbl <- t_ae_ctc_v2(
#'   class = ANL$CLASS,
#'   term = ANL$TERM,
#'   id = ANL$USUBJID,
#'   grade = ANL$GRADE,
#'   col_by = factor(ANL$ARM),
#'   total = "All Patients",
#'   grade_levels = 1:3
#' )
#' }
#'
#'
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ASL <- radsl(N = 10, seed = 1)
#' AAE <- radae(ASL, seed = 1)
#'
#' ANL <- left_join(AAE, ASL %>% select(USUBJID, STUDYID, ARM), by = c("STUDYID", "USUBJID"))
#'
#'
#' \dontrun{
#' tbl <- with(
#'   ANL,
#'   t_ae_ctc_v2(
#'     class = AEBODSYS,
#'     term = AEDECOD,
#'     id = USUBJID,
#'     grade = AETOXGR,
#'     col_by = factor(ARM),
#'     total = "All Patients",
#'     grade_levels = 1:5
#'   )
#' )
#' }
t_ae_ctc_v2 <- function(class, term, id, grade, col_by, total = "All Patients", grade_levels = 1:5) {

  # check argument validity and consitency ----------------------------------
  check_col_by(col_by, min_num_levels = 1)

  if (any("- Overall -" %in% term)) stop("'- Overall -' is not a valid term, t_ae_ctc_v2 reserves it for derivation")
  if (any("All Patients" %in% col_by)) stop("'All Patients' is not a valid col_by, t_ae_ctc_v2 derives All Patients column")

  # data prep ---------------------------------------------------------------
  df <- data.frame(
    class = class,
    term = term,
    subjid = id,
    gradev = grade,
    col_by = col_by,
    stringsAsFactors = FALSE
  )
  df <- df %>% arrange(class, term)

  df <- df %>% mutate(
    class = ifelse(class == "", NA, class),
    term = ifelse(term == "", NA, term)
  )

  class_label <- attr(class, "label")
  term_label <- attr(term, "label")
  grade_label <- attr(grade, "label")

  if (is.null(class_label)) class_label <- deparse(substitute(class))
  if (is.null(term_label)) term_label <- deparse(substitute(term))
  if (is.null(grade_label)) grade_label <- deparse(substitute(grade))

  if (!is.null(total)) {
    total <- tot_column(total)

    if (total %in% levels(col_by)) {
      stop(paste("col_by can not have", total, "group. t_ae_cts will derive it."))
    }

    # adding All Patients
    df <- duplicate_with_var(df, subjid = paste(df$subjid, "-", total), col_by = total)
  }

  # total N for column header
  N <- tapply(df$subjid, df$col_by, function(x) (sum(!duplicated(x))))

  # need to remove extra records that came from subject level data
  # when left join was done. also any record that is missing class or term
  df <- na.omit(df)

  # start tabulating --------------------------------------------------------
  n_cols <- nlevels(col_by)

  # class and term chunks
  l_t_class_terms <- lapply(split(df, df$class), function(df_s_cl) {
    df_s_cl_term <- c(
      list("- Overall -" = df_s_cl),
      split(df_s_cl, df_s_cl$term)
    )

    l_t_terms <- lapply(df_s_cl_term, function(df_i) {
      t_max_grade_per_id(
        grade = df_i$gradev,
        id = df_i$subjid,
        col_by = df_i$col_by,
        col_N = N,
        grade_levels = grade_levels,
        any_grade = "- Any Grade -"
      )
    })

    # sort terms by total
    N_total_any <- vapply(l_t_terms, function(tbl) {
      a <- 0
      for (i in c(1:n_cols)) {
        a <- a + tbl[1, i][1]
      }
      a
    }, numeric(1))

    l_t_terms <- l_t_terms[order(-N_total_any, names(l_t_terms), decreasing = FALSE)]

    l_t_terms
  })


  # now sort tables
  N_total_overall <- vapply(l_t_class_terms, function(tbl) {
    a <- 0
    for (i in c(1:n_cols)) {
      a <- a + tbl[[1]][1, i][1]
    }
    a
  }, numeric(1))

  l_t_class_terms <- l_t_class_terms[order(-N_total_overall, names(l_t_class_terms), decreasing = FALSE)]


  tbl_overall <- t_max_grade_per_id(
    grade = df$gradev,
    id = df$subjid,
    col_by = df$col_by,
    col_N = N,
    grade_levels = grade_levels,
    any_grade = "- Any Grade -"
  )

  tbls_all <- c(
    list("- Any adverse events -" = list("- Overall -" = tbl_overall)),
    l_t_class_terms
  )

  tbls_class <- Map(function(tbls_i, class_i) {
    lt1 <- Map(shift_label_table, tbls_i, names(tbls_i))
    t2 <- do.call(stack_rtables, lt1)
    add_ae_class(indent_table(t2, 1), class_i)
  }, tbls_all, names(tbls_all))


  tbl <- do.call(stack_rtables, tbls_class)

  attr(attr(tbl, "header")[[1]], "row.name") <- class_label
  attr(attr(tbl, "header")[[2]], "row.name") <- term_label
  attr(attr(tbl, "header")[[2]], "indent") <- 1

  attr(tbl, "header")[[2]][[1]] <- rcell(grade_label)
  attr(tbl, "header")[[1]][[1]] <- rcell(NULL)

  tbl
}
