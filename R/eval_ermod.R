#' Evaluate exposure-response model prediction performance
#'
#' This function evaluates the performance of an exposure-response model
#' using various metrics.
#'
#' @export
#' @param ermod An object of class `ermod`.
#' @param eval_type A character string specifying the evaluation dataset.
#' Options are:
#'   - `training`: Use the training dataset.
#'   - `test`: Use a new dataset for evaluation.
#'   - `kfold`: Perform k-fold cross-validation (uses `newdata` if provided,
#'     otherwise uses the training dataset).
#' @param newdata A data frame containing new data for evaluation when
#' `eval_type` is set to `test` or `kfold`.
#' @param summary_method A character string specifying how to summarize the
#' simulation draws. Default is `median`.
#' @param k The number of folds for cross-validation. Default is 5.
#' @param seed_kfold Random seed for k-fold cross-validation.
#'
#' @return A tibble with calculated performance metrics, such as AUROC or
#' RMSE, depending on the model type.
#'
#' @examplesIf BayesERtools:::.if_run_ex_eval_mod()
#' \donttest{
#' data(d_sim_binom_cov_hgly2)
#' d_split <- rsample::initial_split(d_sim_binom_cov_hgly2)
#' d_train <- rsample::training(d_split)
#' d_test <- rsample::testing(d_split)
#'
#' ermod_bin <- dev_ermod_bin(
#'   data = d_train,
#'   var_resp = "AEFLAG",
#'   var_exposure = "AUCss_1000",
#'   var_cov = "BHBA1C_5",
#'   # Settings to make the example run faster
#'   chains = 2,
#'   iter = 1000
#' )
#'
#' metrics_training <- eval_ermod(ermod_bin, eval_type = "training")
#' metrics_test <- eval_ermod(ermod_bin, eval_type = "test", newdata = d_test)
#' metrics_kfold <- eval_ermod(ermod_bin, eval_type = "kfold", k = 3)
#'
#' print(metrics_training)
#' print(metrics_test)
#' print(metrics_kfold)
#' }
#'
eval_ermod <- function(
    ermod, eval_type = c("training", "kfold", "test"),
    newdata = NULL, summary_method = c("median", "mean"),
    k = 5, seed_kfold = NULL) {
  rlang::check_installed("yardstick")

  eval_type <- match.arg(eval_type)
  summary_method <- match.arg(summary_method)

  var_resp <- ermod$var_resp

  # Set the data to use for evaluation
  if (is.null(newdata)) {
    if (eval_type == "test") {
      stop("For 'test' evaluation, please provide the 'newdata' parameter.")
    }
    data_to_use <- ermod$data
  } else {
    if (eval_type == "training") {
      warning("The 'newdata' parameter is ignored for 'training' evaluation.")
    }
    data_to_use <- newdata
  }

  if (eval_type == "kfold") {
    cv_results <- kfold(
      ermod,
      newdata = data_to_use, k = k, seed = seed_kfold
    )

    d_sim <- cv_results$d_sim
    d_truth <- cv_results$d_truth |>
      dplyr::ungroup() |>
      dplyr::select(-fold_id)
  } else {
    ersim <- sim_er(
      ermod = ermod,
      newdata = data_to_use,
      output_type = "draws"
    )

    d_sim <- ersim |>
      dplyr::ungroup() |>
      dplyr::select(.row, .draw, pred = .epred) |>
      dplyr::group_by(.row)

    d_truth <- data_to_use |>
      dplyr::mutate(.row = dplyr::row_number()) |>
      dplyr::select(.row, truth = !!rlang::sym(var_resp))
  }

  # Summarize the predictions
  fn_summary <- if (summary_method == "median") stats::median else mean

  d_sim_summary <- d_sim |>
    dplyr::summarize(
      pred = fn_summary(pred),
      .groups = "drop_last"
    )

  # Join with actual response values
  results <-
    dplyr::left_join(d_sim_summary, d_truth, by = ".row")

  if (ermod$endpoint_type == "binary") {
    results <- results |>
      dplyr::mutate(
        truth = factor(truth, levels = c(1, 0))
      )

    class_metrics <- yardstick::metric_set(
      yardstick::roc_auc, yardstick::mn_log_loss
    )
  } else {
    class_metrics <- yardstick::metric_set(
      yardstick::rmse, yardstick::rsq, yardstick::rsq_trad
    )
  }

  metrics <- class_metrics(results, truth, pred)

  return(metrics)
}

.if_run_ex_eval_mod <- function() {
  requireNamespace("rsample", quietly = TRUE) &&
    requireNamespace("yardstick", quietly = TRUE) &&
    requireNamespace("digest", quietly = TRUE) &&
    rlang::is_installed("rstanemax", version = "0.1.9")
}
