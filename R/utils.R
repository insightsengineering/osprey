# tabulation function for condition checks
#' @importFrom utils getFromNamespace
#' @importFrom rtables rrowl rheader header<- header rcell no_by rtabulate
t_helper_tabulate <- function(df_id, n, checkcol, term, remove_dupl, with_percent) { # nolint
  if (checkcol == "rowcount") {
    tbl <- rtabulate(
      na.omit(df_id),
      row_by = no_by(""),
      col_by = df_id$col_by,
      FUN = nrow,
      format = "xx"
    )
  } else if (checkcol == "uniqueid") {
    if (remove_dupl) {
      df_id <- df_id[!duplicated(df_id$id), ]
    }

    if (with_percent) {
      tbl <- rtabulate(
        na.omit(df_id),
        row_by = no_by(""),
        col_by = df_id$col_by,
        FUN = count_perc_col_N,
        col_wise_args = list(n_i = n),
        format = "xx (xx.xx%)"
      )
    } else {
      tbl <- rtabulate(
        na.omit(df_id),
        row_by = no_by(""),
        col_by = df_id$col_by,
        FUN = count_col_N,
        col_wise_args = list(n_i = n),
        format = "xx"
      )
    }
  } else {
    if (remove_dupl) {
      # sort by checkcol in descending order first
      df_id <- df_id[order(df_id[checkcol], decreasing = TRUE), ]
      df_id <- df_id[!duplicated(df_id$id), ]
    }

    tbl <- rtabulate(
      na.omit(df_id),
      row_by = factor(checkcol),
      col_by = df_id$col_by,
      FUN = count_perc_col_N,
      col_wise_args = list(n_i = n),
      format = "xx (xx.xx%)"
    )

    if (dim(tbl)[1] > 1 | (dim(tbl)[1] == 1 & attributes(tbl[1])$names == "1")) {
      tbl <- tbl[dim(tbl)[1]]
    } else {
      for (i in 1:dim(tbl)[2]) {
        tbl[[1]][[i]] <- rcell(0)
      }
    }
  }

  attr(tbl[[1]], "row.name") <- term

  header(tbl) <- rheader(
    rrowl("", levels(df_id$col_by)),
    rrowl("", unname(n), format = "(N=xx)")
  )
  tbl
}

# checks if there is any case and derives counts, otherwise 0
count_col_N <- function(x_cell, n_i) { # nolint
  if (n_i > 0) {
    length(x_cell$id) # obtaining the total
  } else {
    rcell(0, format = "xx")
  }
}


# adds row name to rtable
shift_label_table_no_grade <- function(tbl, term) {
  attr(tbl[[1]], "row.name") <- term
  tbl
}

# adds row name to rtable
shift_label_table_mod <- function(tbl, term, ind_tbl) {
  attr(tbl[[1]], "row.name") <- term
  indent_table(tbl, ind_tbl)
  tbl
}

# remove null elements from list
remove_null <- function(x) {
  x <- Filter(Negate(is.null), x)
  lapply(x, function(x) {
    if (is.list(x) && class(x) != "rtable") remove_null(x) else x
  })
}

# recursive indent function
recursive_indent <- function(tbl_l, ind_count) {
  if (class(tbl_l) == "rtable") {
    in_t <- list(" " = tbl_l)
    t <- do.call(stack_rtables_condense, in_t)
    for (i in 1:nrow(t)) {
      attr(t[[i]], "indent") <- attr(t[[i]], "indent") + ind_count
    }
    t
  } else if (is.list(tbl_l) && class(tbl_l) != "rtable") {
    odd_ind <- seq(1, length(tbl_l), 2) # nolint
    count <- lapply(tbl_l, function(x) {
      if (class(x) == "rtable") {
        ind_count
      } else {
        ind_count + 1
      }
    })
    count <- unlist(count)
    t0 <- Map(recursive_indent, tbl_l, count)
    tbl <- do.call(stack_rtables_condense, t0) # nolint
  }
}

# arguments for total in tables (AET01, AET02, DST01)
tot_column <- function(choice = c("All Patients")) {
  choice <- match.arg(choice)
  return(choice)
}

#' Stack rtables with rbind
#'
#' @param ... rtbale objects
#' @param nrow_pad number of empty rows between tables in \code{...}
#'
#' @noRd
#'
stack_rtables_condense <- function(..., nrow_pad = 1) {
  tbls <- Filter(Negate(is.null), list(...))

  if (length(tbls) > 0) {

    are <- getFromNamespace("are", pos = "package:rtables")

    if (!are(tbls, "rtable")) stop("not all objects are of type rtable")

    header <- attr(tbls[[1]], "header") # nolint
    Reduce(
      function(x, y) rbind(x, y),
      tbls
    )
  } else {
    list()
  }
}

#' Add Adverse Events class
#'
#' @param tbl (\code{tibble}) Containing the data
#' @param class (\code{character}) Class of adverse events to be added as
#'   an rtable row
#'
#' @importFrom rtables rtable
#' @export
add_ae_class <- function(tbl, class) {
  rbind(
    rtable(header(tbl), rrow(class)),
    tbl
  )
}

#' stack a modified version of a data frame
#'
#' essenially rbind(X,modified(X)). this is useful for example when a total
#' column is needed.
#'
#' @param X a data.frame
#' @param ... key=value pairs, where the key refers to a variable in X and value
#'   is the valueof the variable in modified(X)
#'
#' @importFrom tern var_labels 'var_labels<-'
#' @noRd
#'
#' @examples
#'
#' duplicate_with_var(iris, Species = "Total")
duplicate_with_var <- function(x, ...) { # nolint
  dots <- list(...)
  nms <- names(dots)
  if (length(nms) > 1 && (is.null(nms) || !all(nms %in% names(x)))) {
    stop("not all names in ... are existent or in X")
  }
  x_copy <- x
  vl <- var_labels(x)
  for (var in nms) {
    x_copy[[var]] <- dots[[var]]
  }
  y <- rbind(x, x_copy)
  var_labels(y) <- vl
  y
}
