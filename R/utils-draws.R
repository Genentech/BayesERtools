.posterior_draws <- function(fn, mod, newdata, n_draws, seed) {
  set.seed(seed)
  if (inherits(mod, c("stanemax", "stanemaxbin"))) {
    fn(mod, newdata = newdata, ndraws = n_draws)
  } else {
    fn(mod, newdata = newdata, draws = n_draws)
  }
}

.pp_matrix_to_draws_tbl <- function(mat, newdata, col_name) {
  n_draws <- nrow(mat)
  n_obs <- nrow(newdata)
  draw_rows <- expand.grid(
    .draw = seq_len(n_draws),
    .row = seq_len(n_obs)
  )
  newdata |>
    dplyr::mutate(.row = dplyr::row_number()) |>
    dplyr::left_join(
      dplyr::tibble(
        .row = draw_rows$.row,
        .chain = NA_integer_,
        .iteration = NA_integer_,
        .draw = draw_rows$.draw,
        !!col_name := as.vector(mat)
      ),
      by = ".row"
    )
}
