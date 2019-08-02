
#' AE Overview Summary Table
#'
#' \code{t_ae_oview} returns a summary table of overall AE profile: adverse
#' events, deaths, and withdrawals by trial treatment. It corresponds to STREAM
#' template AET01.
#'
#' @param id unique subject identifier variable. If a particular subject has no
#'  adverse event then the subject \code{id} should be listed where \code{class}
#'  and \code{term} should be set to missing (i.e. \code{NA}).
#' @param class system organ class variable.
#' @param term preferred term variable.
#' @param flags dataframe containing flag variables of all events of interest,
#'  default variables should include \code{dthfl}, \code{dcsreas},
#'  \code{aesdth}, \code{aeser}, \code{aeacn}, \code{aerel}, \code{aetoxgr} to
#'  match STREAM output (lowercase column names). Flags dataframe must include
#'  \code{dthfl} and \code{dcsreas} at minimal.
#' @param display_id vector of strings for which analyses to display possible
#'  values are: fatal ser serwd serdsm relser wd dsm rel relwd reldsm ctc35
#' @param extra_flag dataframe with flag variables to be tabulated. Only records
#'  where values equals to "Y" are counted. Variable names of the dataframe will
#'  be used as the summary table row name.
#' @param col_by group variable that will be used for a column header.
#'  \code{col_by} has to be a factor and can not be missing.
#' @param total character string that will be used as a label for a column with
#'  pooled total population, default here is "All Patients", if set to
#'  \code{NULL} then the "All Patients" column is suppressed.
#'
#' @details this is an equivalent of the STREAM output
#'  \code{\%stream_t_summary(templates = aet01)}
#'  (\url{http://bioportal.roche.com/stream_doc/2_05/um/report_outputs_aet01.html})
#'
#'  The \code{flags} dataframe should be taken from the same dataset and in the
#'  same order as \code{id}, \code{class} and \code{term}. It should only
#'  contain variables needed for the specified flags. All other desired
#'  variables should be passed to the \code{extra_flag} argument, along with the
#'  desired label names (as the variable name of dataframe). Each variable in
#'  \code{extra_flag} will be tabulated to one row in the summary table, where
#'  only "Y" values are counted towards the total.
#'
#' @return \code{rtable} object
#'
#' @export
#'
#' @template author_zhanc107
#' @template author_liaoc10
#' @template author_qit3
#'
#' @examples
#' library(dplyr)
#' 
#' data("rADAE")
#' ANL <- rADAE
#' flag <- data.frame(
#'   dthfl = ANL$DTHFL,
#'   dcsreas = ANL$DCSREAS,
#'   aesdth = ANL$AESDTH,
#'   aeser = ANL$AESER,
#'   aeacn = ANL$AEACN,
#'   aerel = ANL$AEREL,
#'   aetoxgr = ANL$AETOXGR
#' )
#' 
#' extra <- data.frame(
#'   fatal2 = flag$aesdth,
#'   ser2 = flag$aeser
#' )
#' tbl <- t_ae_oview(
#'   id = ANL$USUBJID,
#'   class = ANL$AESOC,
#'   term = ANL$AEDECOD,
#'   flags = flag,
#'   display_id = c(
#'     "fatal", "ser", "serwd", "serdsm", "relser",
#'     "wd", "dsm", "rel", "relwd", "reldsm"
#'   ),
#'   extra_flag = extra,
#'   col_by = factor(ANL$ARM),
#'   total = "All Patients"
#' )
#' 
#' tbl
t_ae_oview <- function(id,
                       class,
                       term,
                       flags,
                       display_id = c(
                         "fatal",
                         "ser",
                         "serwd",
                         "serdsm",
                         "relser",
                         "wd",
                         "dsm",
                         "rel",
                         "relwd",
                         "reldsm",
                         "ctc35"
                       ),
                       extra_flag = NULL,
                       col_by,
                       total = "All Patients") {

  # check input arguments ---------------------------
  check_col_by(col_by, min_num_levels = 1)
  possible_names <- c(
    "dthfl",
    "dcsreas",
    "aesdth",
    "aeser",
    "aeacn",
    "aerel",
    "aetoxgr"
  )

  if (any("- Overall -" %in% term)) {
    stop("'- Overall -' is not a valid term, t_ae_oview reserves it for derivation")
  }
  if (any("All Patients" %in% col_by)) {
    stop("'All Patients' is not a valid col_by, t_ae_oview derives All Patients column")
  }

  if (is.null(flags$dthfl)) {
    stop("invalid arguments: need a dthfl column in the flags parameter")
  }
  if (is.null(flags$dcsreas)) {
    stop("invalid arguments: need a dcsreas column in the flags parameter")
  }
  if (length(display_id) > 11) {
    stop("invalid arguments: the maximum number of defaualt analyses is 11, please add additional analyses to extra_flag")
  }
  if (any(is.element(colnames(flags), possible_names) == FALSE)) {
    stop("invalid arguments: check that the column names in flags matches the expected input")
  }

  # Convert all text to upper case
  flags <- as.data.frame(vapply(flags, toupper, rep(character(1), nrow(flags))))

  if (!is.null(extra_flag)) {
    extra_flag <- as.data.frame(vapply(extra_flag, toupper, rep(character(1), nrow(extra_flag))))
    check_input_length <- c(nrow(data.frame(class)), nrow(data.frame(term)), nrow(data.frame(id)), nrow(data.frame(flags)), nrow(extra_flag))
    check_input_col <- c(ncol(data.frame(class)), ncol(data.frame(term)), ncol(data.frame(id)))
  } else {
    check_input_length <- c(nrow(data.frame(class)), nrow(data.frame(term)), nrow(data.frame(id)), nrow(data.frame(flags)))
    check_input_col <- c(ncol(data.frame(class)), ncol(data.frame(term)), ncol(data.frame(id)))
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
    class = class,
    term = term,
    id = id,
    dthfl = flags$dthfl,
    dcsreas = flags$dcsreas,
    col_by = col_by,
    stringsAsFactors = FALSE
  )

  df$class[df$class == ""] <- "No Coding Available"
  df$term[df$term == ""] <- "No Coding Available"

  df_flags <- data.frame(
    id = id,
    flags,
    col_by = col_by,
    stringsAsFactors = FALSE
  )

  # for alternative calculations
  if (!is.null(extra_flag)) {
    df_extra_flags <- data.frame(
      id = id,
      col_by = col_by,
      extra_flag,
      stringsAsFactors = FALSE
    )
  }

  # adding All Patients
  if (!is.null(total)) {
    total <- tot_column(total)

    if (total %in% levels(col_by)) {
      stop(paste("col_by can not have", total, "group."))
    }

    df <- duplicate_with_var(df, id = paste(df$id, "-", total), col_by = total)
    df_flags <- duplicate_with_var(df_flags, id = paste(df_flags$id, "-", total), col_by = total)

    if (!is.null(extra_flag)) {
      df_extra_flags <- duplicate_with_var(df_extra_flags, id = paste(df_extra_flags$id, "-", total), col_by = total)
    }
  }

  # total N for column header
  N <- tapply(df$id, df$col_by, function(x) (sum(!duplicated(x))))

  # need to remove any record that is missing class or term
  df <- na.omit(df)
  df_flags <- na.omit(df_flags)
  if (!is.null(extra_flag)) {
    df_extra_flags <- na.omit(df_extra_flags)
  }

  dsm <- c("DRUG INTERRUPTED", "DOSE INCREASED", "DOSE REDUCED")

  # convert Y/N or event tag to 1/0
  df$dthfl <- factor(if_else(df$dthfl == "Y", 1, 0))
  df$dcsreas <- factor(if_else(df$dcsreas == "ADVERSE EVENT", 1, 0))

  if ("fatal" %in% display_id) df_flags$fatal <- factor(if_else(df_flags$aesdth == "Y", 1, 0))
  if ("ser" %in% display_id) df_flags$ser <- factor(if_else(df_flags$aeser == "Y", 1, 0))
  if ("serwd" %in% display_id) {
    df_flags$serwd <- factor(if_else(df_flags$aeser == "Y" &
      df_flags$aeacn == "DRUG WITHDRAWN", 1, 0))
  }
  if ("serdsm" %in% display_id) {
    df_flags$serdsm <- factor(if_else(df_flags$aeser == "Y" &
      df_flags$aeacn %in% dsm, 1, 0))
  }
  if ("relser" %in% display_id) {
    df_flags$relser <- factor(if_else(df_flags$aeser == "Y" &
      df_flags$aerel == "Y", 1, 0))
  }
  if ("wd" %in% display_id) df_flags$wd <- factor(if_else(df_flags$aeacn == "DRUG WITHDRAWN", 1, 0))
  if ("dsm" %in% display_id) df_flags$dsm <- factor(if_else(df_flags$aeacn %in% dsm, 1, 0))
  if ("rel" %in% display_id) df_flags$rel <- factor(if_else(df_flags$aerel == "Y", 1, 0))
  if ("relwd" %in% display_id) {
    df_flags$relwd <- factor(if_else(df_flags$aerel == "Y" &
      df_flags$aeacn == "DRUG WITHDRAWN", 1, 0))
  }
  if ("reldsm" %in% display_id) {
    df_flags$reldsm <- factor(if_else(df_flags$aerel == "Y" &
      df_flags$aeacn %in% dsm, 1, 0))
  }
  if ("ctc35" %in% display_id) df_flags$ctc35 <- factor(if_else(df_flags$aetoxgr %in% c("3", "4", "5"), 1, 0))

  # for extra flags
  if (!is.null(extra_flag)) {
    df_conv <- apply(df_extra_flags[-c(1, 2)], 2, FUN = function(b) {
      factor(if_else(b == "Y", 1, 0))
    })
    df_conv <- transform(df_conv, as.numeric())
    df_extra_flags <- data.frame(id = df_extra_flags[, 1], df_conv, col_by = df_extra_flags[, 2])
  }

  # start tabulating --------------------------------------------------------
  n_cols <- nlevels(col_by)

  # Overview: total num patients with at least one AE
  df_patients <- list(
    "Total number of patients with at least one AE" = df,
    "Total number of AEs" = df,
    "Total number of deaths" = df,
    "Total number of patients withdrawn from study due to an AE" = df
  )

  tbl_overall <- mapply(function(df_i, term, c_col) {
    t_helper_tabulate(
      df_id = df_i,
      N = N,
      checkcol = c_col,
      term = term,
      remove_dupl = TRUE,
      with_percent = TRUE
    )
  }, df_patients, names(df_patients), c("uniqueid", "rowcount", "dthfl", "dcsreas"), SIMPLIFY = FALSE)

  # Summary table: individual components
  term_label <- c(
    "AE with fatal outcome",
    "Serious AE",
    "Serious AE leading to withdrawal from treatment",
    "Serious AE leading to dose modification/interruption",
    "Related Serious AE",
    "AE leading to withdrawal from treatment",
    "AE leading to dose modification/interruption",
    "Related AE",
    "Related AE leading to withdrawal from treatment",
    "Related AE leading to dose modification/interruption",
    "Grade 3-5 AE"
  )
  df_ind <- list()
  for (i in 1:length(display_id)) {
    df_ind[[i]] <- df_flags
  }
  names(df_ind) <- term_label[1:length(display_id)] # display_id

  tbl_ind <- mapply(function(df_i, term, c_col) {
    t_helper_tabulate(
      df_id = df_i,
      N = N,
      checkcol = c_col,
      term = term,
      remove_dupl = TRUE,
      with_percent = TRUE
    )
  }, df_ind, names(df_ind), display_id, SIMPLIFY = FALSE)

  # extra flags
  if (!is.null(extra_flag)) {
    df_extra <- list()
    for (i in 1:length(extra_flag)) {
      df_extra[[i]] <- df_extra_flags
    }
    names(df_extra) <- colnames(extra_flag)

    tbl_extra <- mapply(function(df_i, term, c_col) {
      t_helper_tabulate(
        df_id = df_i,
        N = N,
        checkcol = c_col,
        term = term,
        remove_dupl = TRUE,
        with_percent = TRUE
      )
    }, df_extra, names(df_extra), colnames(extra_flag), SIMPLIFY = FALSE)

    tbl_ind <- c(tbl_ind, tbl_extra)
  }

  # put together final table ------------------
  tbls_overview <- c(
    list("Total number of patients with at least one AE" = tbl_overall[1]),
    list("Total number of AEs" = tbl_overall[2]),
    list("Total number of deaths" = tbl_overall[3]),
    list("Total number of patients withdrawn from study due to an AE" = tbl_overall[4])
  )

  tbls_ov <- Map(function(tbls_i) {
    lt1 <- Map(shift_label_table_no_grade, tbls_i, names(tbls_i))
    t2 <- do.call(stack_rtables, lt1)
  }, tbls_overview)

  tbls_ind <- c(list("Total number of patients with at least one" = tbl_ind))
  tbls_class <- Map(function(tbls_i, class_i) {
    lt1 <- Map(shift_label_table_no_grade, tbls_i, names(tbls_i))
    t2 <- do.call(stack_rtables_condense, lt1)
    add_ae_class(indent_table(t2, 1), class_i)
  }, tbls_ind, names(tbls_ind))

  tbl_total <- do.call(stack_rtables_condense, tbls_ov)
  tbl_cl <- do.call(stack_rtables_condense, tbls_class)

  header <- attr(tbl_cl, "header")
  tbl_with_empty_rows <- rtablel(header = header, replicate(1, rrow()))

  tbl <- rbind(tbl_total, tbl_with_empty_rows, tbl_cl)

  tbl
}
