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

#adds row name to rtable
shift_label_table_mod <- function(tbl, term, ind_tbl) {
  attr(tbl[[1]], "row.name") <- term
  indent_tabl(tbl, ind_tbl)
  tbl
}

#remove null elements from list
remove_Null <- function(x) {
  x <- Filter(Negate(is.null), x)
  lapply(x, function(x){ if (is.list(x) && class(x) != "rtable") remove_Null(x) else x})
}

#recursive indent function
recursive_indent <- function(tbl_l, ind_count){
  if (class(tbl_l) == "rtable"){
    in_t <- list(" " = tbl_l)
    t <- do.call(stack_rtables_condense, in_t)
    for (i in 1:nrow(t)) {
      attr(t[[i]], "indent") <- attr(t[[i]], "indent") + ind_count
    }
    t
  } else if(is.list(tbl_l) && class(tbl_l) != "rtable"){
    odd_ind <- seq(1, length(tbl_l), 2)
    count <- lapply(tbl_l, function(x){
      if(class(x) == "rtable")
        ind_count
      else
        ind_count + 1})
    count <- unlist(count)
    t0 <- Map(recursive_indent, tbl_l, count)
    tbl <- do.call(stack_rtables_condense, t0)
  }
}

#arguments for total in tables (AET01, AET02, DST01)
tot_column <- function(choice = c("All Patients", "None")){
  choice <- match.arg(choice)
  return(choice)
}

#' Stack rtables with rbind
#'
#' @param ... rtbale objects
#' @param nrow_pad number of empty rows between tables in \code{...}
#'
#'
#'
stack_rtables_condense <- function(..., nrow_pad = 1) {

  tbls <- Filter(Negate(is.null), list(...))

  if (length(tbls) > 0) {
    if (!rtables:::are(tbls, "rtable")) stop("not all objects are of type rtable")

    header <- attr(tbls[[1]], "header")
    Reduce(
      function(x, y) rbind(x, y),
      tbls
    )

  } else {
    list()
  }
}
