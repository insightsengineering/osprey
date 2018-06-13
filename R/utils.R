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

check_col_by <- function(x, min_num_levels = 2) {

  if (!is(x, "no_by") && !is.factor(x)) stop("col_by needs to be a factor")
  if (any(is.na(x)) || any(x == '')) stop("col_by can not have any missing data")

  #if (any(table(x)<=0)) stop("data is required for all levels of col_by")

  if (!(min_num_levels == 1 && is(x, "no_by"))) {
    if (length(levels(x)) < min_num_levels) stop("col_by is required to have at least", min_num_levels, "levels")
  }

  TRUE
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
#' @noRd
#'
#' @examples
#'
#' duplicate_with_var(iris, Species = "Total")
#'
duplicate_with_var <- function(X, ...) {
  dots <- list(...)
  nms <- names(dots)
  if (is.null(nms) || !all(nms %in% names(X)))
    stop("not all names in ... are existent or in X")
  X_copy <- X
  vl <- var_labels(X)
  for (var in nms) {
    X_copy[[var]] <- dots[[var]]
  }
  Y <- rbind(X, X_copy)
  var_labels(Y) <- vl
  Y
}

# checks if there is any case and derives counts (percentage), otherwise 0
count_perc_col_N <- function(x_cell, N) {
  N_i <- if (nrow(x_cell) == 0) 0 else N[x_cell$col_by[1]]
  if (N_i > 0) {
    length(x_cell$id) * c(1, 1 / N_i) # obtaining the total and getting percentage
  } else {
    rcell(0, format = "xx")
  }
}

indent_table <- function(x, n) {
  for (i in 1:nrow(x)) {
    attr(x[[i]], "indent") <- attr(x[[i]], "indent") + n
  }
  x
}
