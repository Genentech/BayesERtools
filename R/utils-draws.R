.posterior_draws <- function(fn, mod, newdata, n_draws, seed, ...) {
  set.seed(seed)
  if (inherits(mod, c("stanemax", "stanemaxbin"))) {
    fn(mod, newdata = newdata, ndraws = n_draws)
  } else {
    fn(mod, newdata = newdata, draws = n_draws, ...)
  }
}

# Draws of epred, linpred, and prediction for mixed-effects models
# Returns a list of matrices (n_draws x nrow(newdata))
.sim_draws_lme <- function(ermod, newdata, n_draws, seed, re_type) {
  mod <- extract_mod(ermod)

  if (re_type %in% c("population", "existing_subject")) {
    if (re_type == "existing_subject") check_existing_subjects(ermod, newdata)
    # rstanarm re.form: NA for no random effects, NULL for all random effects
    re_form <- if (re_type == "population") NA else NULL

    draws_fn <- function(fn) {
      .posterior_draws(fn, mod, newdata, n_draws, seed, re.form = re_form)
    }
    return(list(
      epred = draws_fn(rstantools::posterior_epred),
      linpred = draws_fn(rstantools::posterior_linpred),
      prediction = draws_fn(rstantools::posterior_predict)
    ))
  }

  # re_type == "new_subject"
  # rstanarm assigns the same random effects draw to all new levels, so the
  # random effects for new subjects are sampled here from the estimated
  # between-subject distribution, independently for each subject and draw.
  set.seed(seed)
  n_draws_all <- nrow(as.matrix(mod$stanfit))
  idx_draws <- sample.int(n_draws_all, n_draws)

  linpred_pop <-
    rstantools::posterior_linpred(mod, newdata = newdata, re.form = NA)
  linpred_pop <- linpred_pop[idx_draws, , drop = FALSE]

  linpred <- linpred_pop + .sample_re_new_subject(ermod, newdata, idx_draws)

  sigma <- as.matrix(mod, pars = "sigma")[idx_draws, 1]
  # sigma is recycled along rows (draws) of the matrix
  prediction <- linpred +
    matrix(stats::rnorm(length(linpred), 0, sigma), nrow = n_draws)

  # Identity link for linear mixed-effects models
  list(epred = linpred, linpred = linpred, prediction = prediction)
}

# Sample random effects contributions (Z %*% b) for new subjects
# Returns a matrix (length(idx_draws) x nrow(newdata))
.sample_re_new_subject <- function(ermod, newdata, idx_draws) {
  mod <- extract_mod(ermod)
  var_random <- extract_var_random(ermod)
  re_names <- mod$glmod$reTrms$cnms[[var_random]]
  n_re <- length(re_names)

  # Design matrix for the random effects
  mat_z <- vapply(re_names, function(.x) {
    if (.x == "(Intercept)") rep(1, nrow(newdata)) else newdata[[.x]]
  }, numeric(nrow(newdata)))
  mat_z <- matrix(mat_z, nrow = nrow(newdata))

  # Subject index for each row of newdata
  id_subj <- as.character(newdata[[var_random]])
  idx_subj <- match(id_subj, unique(id_subj))
  n_subj <- length(unique(id_subj))

  # Posterior draws of the covariance matrix elements
  draws_sigma <- as.matrix(mod)[idx_draws, , drop = FALSE]
  get_sigma_col <- function(i, j) {
    # rstanarm stores the lower triangle only, e.g. Sigma[ID:CONC,(Intercept)]
    row_col <- if (i >= j) c(i, j) else c(j, i)
    draws_sigma[, paste0(
      "Sigma[", var_random, ":", re_names[row_col[1]], ",",
      re_names[row_col[2]], "]"
    )]
  }
  l_sigma_cols <- list()
  for (i in seq_len(n_re)) {
    for (j in seq_len(n_re)) {
      l_sigma_cols[[paste(i, j)]] <- get_sigma_col(i, j)
    }
  }

  re_contrib <- vapply(seq_along(idx_draws), function(s) {
    mat_sigma <- matrix(
      vapply(l_sigma_cols, function(.x) .x[s], numeric(1)),
      nrow = n_re, byrow = TRUE
    )
    mat_b <- matrix(stats::rnorm(n_subj * n_re), nrow = n_subj) %*%
      mat_sqrt_psd(mat_sigma)
    rowSums(mat_z * mat_b[idx_subj, , drop = FALSE])
  }, numeric(nrow(newdata)))

  t(matrix(re_contrib, nrow = nrow(newdata)))
}

# Matrix square root R such that t(R) %*% R = x, for positive semi-definite x
mat_sqrt_psd <- function(x) {
  out <- tryCatch(chol(x), error = function(e) NULL)
  if (!is.null(out)) {
    return(out)
  }
  eig <- eigen(x, symmetric = TRUE)
  t(eig$vectors %*% diag(sqrt(pmax(eig$values, 0)), nrow = nrow(x)))
}

check_existing_subjects <- function(ermod, newdata) {
  var_random <- extract_var_random(ermod)
  id_fitted <-
    levels(extract_mod(ermod)$glmod$reTrms$flist[[var_random]])
  id_new <- unique(as.character(newdata[[var_random]]))
  id_missing <- setdiff(id_new, id_fitted)
  if (length(id_missing) > 0) {
    stop(
      "`re_type = \"existing_subject\"` requires all `", var_random,
      "` values in newdata to exist in the data used for model development. ",
      "Not found: ", paste(utils::head(id_missing, 10), collapse = ", "),
      if (length(id_missing) > 10) ", ...",
      "\nUse `re_type = \"new_subject\"` to simulate new subjects."
    )
  }
  invisible()
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
