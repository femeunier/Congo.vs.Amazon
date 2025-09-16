make_lags_by_group <- function(data,
                               group     = c("lon","lat"),
                               order_by  = c("year","month"),
                               vars      = NULL,      # which columns to lag; default = all except keys
                               max_lag   = 6,
                               drop_rows_with_na_lags = TRUE,
                               suffix    = "L") {

  stopifnot(all(group %in% names(data)))
  stopifnot(all(order_by %in% names(data)))

  if (is.null(vars)) {
    vars <- setdiff(names(data), c(group, order_by))
  }

  # list of lag functions L1..Lk
  lag_funs <- setNames(lapply(seq_len(max_lag),
                              function(k) function(x) dplyr::lag(x, k)),
                       paste0(suffix, seq_len(max_lag)))

  out <- data %>%
    group_by(across(all_of(group))) %>%
    arrange(across(all_of(order_by)), .by_group = TRUE) %>%
    mutate(across(all_of(vars), lag_funs, .names = "{.col}_{.fn}")) %>%
    ungroup()

  if (drop_rows_with_na_lags) {
    created_cols <- as.vector(outer(vars, paste0("_", names(lag_funs)), paste0))
    out <- tidyr::drop_na(out, dplyr::all_of(created_cols))
  }

  out
}
