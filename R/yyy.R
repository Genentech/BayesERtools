# Set globalVariables to minimize R CMD check notes


if (getRversion() >= "2.15.1") {
  # General
  utils::globalVariables(c(
    ".", "n", ":=", ".epred", ".linpred", ".prediction", ".draw", ".response",
    ".id_exposure"
  ))

  # ggplot
  utils::globalVariables(c(
    ".data", ".lower", ".upper", ".exposure", ".exp_metric",
    ".epred.lower", ".epred.upper", ".linpred.lower", ".linpred.upper",
    ".prediction.lower", ".prediction.upper"
  ))

  # coveff
  utils::globalVariables(c(
    "var_name", "var_order", "value_label", "value_cont", "value_cat",
    "value_order", "value_annot", "value_order", "var_label",
    "var_value_index_num", ".response_diff",
    "is_covariate", "is_ref_value", "show_ref_value",
    "is_ref", ".linpred_ref", ".delta_linpred", ".is_ref_value",
    ".row_id", "df_one_row", "df_one_row_new", ".odds_ratio",
    "Odds ratio", "95% CI"
  ))

  # rstanemax
  utils::globalVariables(c(
    "mcmcid", "respHat", "response", ".upper", ".exposure", "exp_metric"
  ))

  # eval_ermod and cv
  utils::globalVariables(c(
    "truth", "pred", "fold_id", ".row_orig"
  ))

  # sim data gen
  utils::globalVariables(c(
    "exposure", "cnt_a", "cnt_b", "cnt_c", "bin_d", "bin_pred", "bin_prob"
  ))
}


#' Sample simulated data for exposure-response with binary endpoint.
#'
#' @name d_sim_binom_cov
#' @format A data frame with columns:
#' \describe{
#' \item{ID}{Subject ID}
#' \item{AETYPE}{Adverse event type: hgly2 (Gr2+ hyperglycemia),
#' dr2 (Gr2+ Diarrhea), ae_covsel_test (hypothetical AE for covariate
#' selection function test)}
#' \item{AEFLAG}{Adverse event flag: 0 - no event, 1 - event}
#' \item{Dose_mg}{Dose in mg: 200, 400}
#' \item{AUCss}{Steady-state area under the curve}
#' \item{Cmaxss}{Steady-state maximum (peak) concentration}
#' \item{Cminss}{Steady-state minimum (trough) concentration}
#' \item{BAGE}{Baseline age in years}
#' \item{BWT}{Baseline weight in kg}
#' \item{BGLUC}{Baseline glucose in mmol/L}
#' \item{BHBA1C}{Baseline HbA1c in percentage}
#' \item{RACE}{Race: White, Black, Asian}
#' \item{VISC}{Visceral disease: No, Yes}
#' \item{AUCss_1000}{AUCss/1000}
#' \item{BAGE_10}{BAGE/10}
#' \item{BWT_10}{BWT/10}
#' \item{BHBA1C_5}{BHBA1C/5}
#' }
#' @details
#'
#' This simulated dataset is very loosely inspired by ER analysis of
#' ipatasertib by Kotani (2022) at:
#'
#' https://doi.org/10.1007/s00280-022-04488-2
#'
#' You can find the data generating code in the package source code,
#' under `data-raw/d_sim_binom_cov.R`.
#'
#' d_sim_binom_cov_hgly2 is a subset of this dataset with only hgly2 AE type
#' and some columns added for testing.
#'
#' @examples
#' d_sim_binom_cov
#' d_sim_binom_cov_hgly2
"d_sim_binom_cov"

#' @rdname d_sim_binom_cov
"d_sim_binom_cov_hgly2"


#' Sample simulated data for exposure-response with continuous endpoint
#' using linear model.
#'
#' @format A data frame with columns:
#' \describe{
#' \item{ID}{Subject ID}
#' \item{AUCss}{Steady-state area under the curve}
#' \item{Cmaxss}{Steady-state maximum (peak) concentration}
#' \item{BAGE}{Baseline age in years}
#' \item{SEX}{M or F}
#' \item{response}{Response}
#' }
#' @details
#'
#' True model is defined as `0.5 * AUCss + 0.5 * BAGE + 5 * SEX`, with
#' variability added with standard deviation of 10.
#' You can find the data generating code in the package source code,
#' under `data-raw/d_sim_lin.R`.
#'
#' @examples
#' d_sim_lin
"d_sim_lin"



#' Sample simulated data for Emax exposure-response models with covariates.
#'
#' @name d_sim_emax
#' @format A data frame with columns:
#' \describe{
#' \item{dose}{Nominal dose, units not specified}
#' \item{exposure_1}{Exposure value, units and metric not specified}
#' \item{exposure_2}{Exposure value, units and metric not specified, but different from exposure_1}
#' \item{response_1}{Continuous response value (units not specified)}
#' \item{response_2}{Binary response value (group labels not specified)}
#' \item{cnt_a}{Continuous valued covariate}
#' \item{cnt_b}{Continuous valued covariate}
#' \item{cnt_c}{Continuous valued covariate}
#' \item{bin_d}{Binary valued covariate}
#' \item{bin_e}{Binary valued covariate}
#' }
#' @details
#'
#' This simulated dataset is entirely synthetic. It is a generic data set that can be used
#' to illustrate Emax modeling. It contains variables corresponding to dose and exposure,
#' and includes both a continuous response variable and a binary response variable. Three
#' continuous valued covariates are included, along with two binary covariates.
#'
#' You can find the data generating code in the package source code,
#' under `data-raw/d_sim_emax.R`.
#'
#' @examples
#' d_sim_emax
"d_sim_emax"
