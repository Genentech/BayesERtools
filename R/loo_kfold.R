#' Efficient approximate leave-one-out cross-validation (LOO)
#'
#' See [loo::loo()] for details.
#'
#' @name loo
#' @param x An object of class `ermod`
#' @param ... Additional arguments passed to `loo::loo()`
#' @importFrom loo loo
#'
#' @return An object of class `loo`
#' @export
loo::loo

#' @rdname loo
#' @export
loo.ermod <- function(x, ...) {
  loo::loo(x$mod, ...)
}

#' @rdname loo
#' @export
loo.ermod_emax <- function(x, ...) {
  rlang::check_installed("digest")

  out <- loo::loo(x$mod$stanfit, ...)

  y <- x$mod$standata$response
  attributes(y) <- NULL
  attr(out, "yhash") <- digest::sha1(y)

  return(out)
}

#' @rdname loo
#' @export
loo.ermod_bin_emax <- function(x, ...) {
  rlang::check_installed("digest")

  out <- loo::loo(x$mod$stanfit, ...)

  y <- x$mod$standata$response
  attributes(y) <- NULL
  attr(out, "yhash") <- digest::sha1(y)

  return(out)
}

#' Run k-fold cross-validation
#'
#' This function performs k-fold cross-validation using the appropriate model
#' development function based on the class of the `ermod` object.
#' It is internally used by [eval_ermod()]. The output is compatible with
#' `loo` ecosystem, e.g. it can be used for [loo::loo_compare()] function.
#' See [loo::kfold()] for details.
#'
#' @rdname kfold
#' @param x An `ermod` object containing the model and data.
#' @param k The number of folds for cross-validation. Default is 5.
#' For mixed-effects models (`ermod_lme`), folds are formed by the random
#' effect grouping variable (e.g. subject), so that all observations from
#' the same subject are held out together. Holdout predictions (`d_sim`) are
#' population-level predictions (random effects set to zero), and the
#' pointwise log-likelihood of the held-out observations is evaluated with
#' random effects drawn from the estimated between-subject distribution.
#' @param newdata Optional new dataset to use instead of the original data.
#' Default is NULL.
#' @param seed Random seed for reproducibility. Default is NULL.
#' @param ... Currently not used.
#'
#' @return [kfold()] returns `kfold_ermod` class object containing the fitted
#' models and holdout predictions for each fold.
#'
#' @examplesIf BayesERtools:::.if_run_ex_eval_mod()
#' \donttest{
#' data(d_sim_binom_cov_hgly2)
#'
#' ermod_bin <- dev_ermod_bin(
#'   data = d_sim_binom_cov_hgly2,
#'   var_resp = "AEFLAG",
#'   var_exposure = "AUCss_1000",
#'   var_cov = "BHBA1C_5",
#'   # Settings to make the example run faster
#'   chains = 2,
#'   iter = 1000
#' )
#'
#' cv_results <- kfold(ermod_bin, k = 3, seed = 123)
#'
#' print(cv_results)
#' }
#'
#' @export
kfold.ermod <- function(x, k = 5, newdata = NULL, seed = NULL, ...) {
  rlang::check_installed("digest")

  ermod <- x

  # Set seed for reproducibility
  if (!is.null(seed)) {
    rng_state_old <- .Random.seed
    on.exit(assign(".Random.seed", rng_state_old, envir = .GlobalEnv))
    set.seed(seed)
  }

  # Use newdata if provided, otherwise use data from ermod object
  data <- if (!is.null(newdata)) newdata else ermod$data

  # Convert non-numeric columns to factors
  # Need to do this first, otherwise there can be discrepancies in categorical
  # levels between training and testing data
  data <- data |>
    dplyr::mutate(dplyr::across(dplyr::where(~ !is.numeric(.)), as.factor)) |>
    dplyr::mutate(.row_orig = dplyr::row_number())

  # Determine the model development function based on the class of the ermod
  if (inherits(ermod, "ermod_lme")) {
    # Also used for ermod_cqt; the derived covariate columns are already
    # present in the data stored in the ermod object
    model_dev_fun <- dev_ermod_lme
  } else if (inherits(ermod, "ermod_emax")) {
    model_dev_fun <- dev_ermod_emax
  } else if (inherits(ermod, "ermod_bin_emax")) {
    model_dev_fun <- dev_ermod_bin_emax
  } else if (inherits(ermod, "ermod_lin")) {
    model_dev_fun <- dev_ermod_lin
  } else if (inherits(ermod, "ermod_bin")) {
    model_dev_fun <- dev_ermod_bin
  } else {
    stop("Unsupported model class")
  }

  # Create k-fold cross-validation splits manually
  if (inherits(ermod, "ermod_lme")) {
    # Split by subject for mixed-effects models
    id_subj <- as.character(data[[ermod$var_random]])
    id_unique <- unique(id_subj)
    if (length(id_unique) < k) {
      stop(
        "Number of unique `", ermod$var_random, "` values (",
        length(id_unique), ") is smaller than k (", k, ")."
      )
    }
    fold_ids_subj <- sample(rep(1:k, length.out = length(id_unique)))
    fold_ids <- fold_ids_subj[match(id_subj, id_unique)]
  } else {
    fold_ids <- sample(rep(1:k, length.out = nrow(data)))
  }

  fit_and_sim <- function(fold_id) {
    train_data <- data[fold_ids != fold_id, ]
    test_data <- data[fold_ids == fold_id, ]

    dev_args <- c(
      list(
        data = train_data,
        var_resp = ermod$var_resp,
        var_exposure = ermod$var_exposure
      ),
      if (inherits(ermod, "ermod_lme")) {
        list(var_cov = ermod$var_cov, var_random = ermod$var_random)
      } else if (inherits(ermod, "ermod_lin") || inherits(ermod, "ermod_bin")) {
        list(var_cov = ermod$var_cov)
      } else {
        list(l_var_cov = ermod$l_var_cov)
      },
      ermod$input_args
    )

    ermod_k <- do.call(model_dev_fun, dev_args)
    ersim_k <- sim_er(ermod_k, newdata = test_data)

    d_truth_k <- test_data |>
      dplyr::select(.row = .row_orig, truth = !!rlang::sym(ermod$var_resp)) |>
      dplyr::mutate(fold_id = fold_id)

    d_sim_k <- ersim_k |>
      dplyr::ungroup() |>
      dplyr::select(.row = .row_orig, .draw, pred = .epred) |>
      dplyr::mutate(fold_id = fold_id)

    loglik_k <- rstanarm::log_lik(extract_mod(ermod_k), newdata = test_data)

    list(
      ermod = ermod_k,
      d_truth = d_truth_k,
      d_sim = d_sim_k,
      test_id = which(fold_ids == fold_id),
      loglik = loglik_k
    )
  }

  results <- purrr::map(1:k, fit_and_sim)
  results <- purrr::list_transpose(results)
  results$l_ermod <- results$ermod
  results$ermod <- NULL

  results$d_truth <- dplyr::bind_rows(results$d_truth) |>
    dplyr::arrange(.row) |>
    dplyr::group_by(fold_id)
  results$d_sim <- dplyr::bind_rows(results$d_sim) |>
    dplyr::arrange(.row) |>
    dplyr::group_by(fold_id, .row)

  results$k <- k

  loglik <- do.call(cbind, results$loglik)
  elpds_unord <- apply(loglik, 2, logmeanexp)
  order_test_id <- order(unlist(results$test_id))
  elpds <- elpds_unord[order_test_id]

  # for computing effective number of parameters
  loglik_orig <- rstanarm::log_lik(extract_mod(ermod))
  lpds <- apply(loglik_orig, 2, logmeanexp)
  ps <- lpds - elpds

  pointwise <- cbind(elpd_kfold = elpds, p_kfold = ps, kfoldic = -2 * elpds)
  est <- colSums(pointwise)
  se_est <- sqrt(nrow(data) * apply(pointwise, 2, stats::var))

  estimates <- cbind(Estimate = est, SE = se_est)
  rownames(estimates) <- colnames(pointwise)

  results$estimates <- estimates
  results$pointwise <- pointwise

  y <- extract_data(ermod)[[extract_var_resp(ermod)]]
  attributes(y) <- NULL
  attr(results, "yhash") <- digest::sha1(y)

  results$loglik <- NULL
  class(results) <- c("kfold_ermod", "kfold", "loo")
  return(results)
}

logmeanexp <- function(x) {
  xmax <- which.max(x)
  log1p(sum(exp(x[-xmax] - x[xmax]))) + x[xmax] - log(length(x))
}

#' @name kfold
#' @importFrom loo kfold
#' @export
loo::kfold

#' @export
print.kfold_ermod <- function(x, ...) {
  class(x) <- c("kfold", "loo") # Needed for print to work

  cli::cli({
    cli::cli_h1("k-fold Cross-Validation for ermod object")
    cli::cli_alert_info(paste(
      "Number of folds: ", x$k
    ))
    cli::cli_h2("Structure of the object")
    cli::cli_li("$l_ermod: list of ermod objects")
    cli::cli_li("$d_truth: data frame with true response values")
    cli::cli_li("$d_sim: data frame with holdout predictions")
    cli::cli_h2("elpd (used in `loo` package)")
    print(x) |>
      utils::capture.output() |>
      cli::cli_code()
  })
}


#' @export
#' @name kfold
#' @param kfold_ermod An object of class `kfold_ermod` from [kfold()]
#' @return [extract_kfold_loo()] returns  `c("kfold", "loo")` class object
#' that works well with `loo` ecosystem
extract_kfold_loo <- function(kfold_ermod) {
  class(kfold_ermod) <- c("kfold", "loo")
  return(kfold_ermod)
}
