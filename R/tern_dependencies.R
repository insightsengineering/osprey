# import tern functions
check_col_by <- getFromNamespace("check_col_by", "tern")
count_perc_col_N <- getFromNamespace("count_perc_col_N", "tern")
indent_table <- getFromNamespace("indent_table", "tern")
check_same_N <- getFromNamespace("check_same_n", "tern")
to_n <- getFromNamespace("to_n", "tern")
var_relabel <- getFromNamespace("var_relabel", "tern")
t_max_grade_per_id <- getFromNamespace("t_max_grade_per_id", "tern")
shift_label_table <- getFromNamespace("shift_label_table", "tern")


stack_rtables <- function(...) {
  rbind(..., gap = 1)
}
