# Generate data for concentration-QTc (C-QT) analysis
#
# Hypothetical single ascending dose (SAD) study with placebo:
# - 6 dose cohorts (25, 50, 100, 200, 400, 800 mg), each with 6 subjects on
#   active drug and 2 subjects on placebo (48 subjects in total)
# - ECGs (triplicate-averaged QTcF) at pre-dose (baseline) and 10 post-dose
#   nominal time points, time-matched with PK sampling
#
# PK (parent): 1-compartment model with first-order absorption
# PK (metabolite): formed from the parent, 1-compartment elimination
# QTc: linear direct effect of the parent concentration, following the
# structure of the pre-specified linear mixed-effects model in
# Garnett et al. (2018) J Pharmacokinet Pharmacodyn 45:383-397
# (correction: J Pharmacokinet Pharmacodyn 45:399,
# doi:10.1007/s10928-017-9565-6):
#
# dQTcF_ij = (theta_0 + eta_0,i) + theta_trt * TRT_i +
#            (theta_conc + eta_conc,i) * CONC_ij + diurnal(t_j) +
#            theta_bl * (QTcF_baseline_i - 400) + eps_ij

set.seed(1234)

# Study design ----------------------------------------------------------------
doses <- c(25, 50, 100, 200, 400, 800) # mg
n_active <- 6
n_placebo <- 2
times <- c(0.5, 1, 1.5, 2, 3, 4, 6, 8, 12, 24) # h post-dose

d_subj <-
  tidyr::expand_grid(
    COHORT = seq_along(doses),
    ARM = c(rep("Active", n_active), rep("Placebo", n_placebo))
  ) |>
  dplyr::mutate(
    ID = dplyr::row_number(),
    DOSE = dplyr::if_else(ARM == "Active", doses[COHORT], 0),
    TRT = ARM
  ) |>
  dplyr::select(ID, COHORT, DOSE, TRT)

n_subj <- nrow(d_subj)

# Subject-level parameters ----------------------------------------------------
## PK: typical values and between-subject variability (log-normal)
d_subj <-
  d_subj |>
  dplyr::mutate(
    KA = 1.2 * exp(rnorm(n_subj, 0, 0.3)), # 1/h
    CL = 20 * exp(rnorm(n_subj, 0, 0.3)), # L/h
    V = 150 * exp(rnorm(n_subj, 0, 0.2)), # L
    KM = 0.1 * exp(rnorm(n_subj, 0, 0.2)), # 1/h, metabolite elimination
    VM = 100, # L, metabolite volume
    FM = 0.3 # fraction metabolized to the metabolite
  )

## QTc: baseline and random effects
theta_0 <- -1 # ms, intercept (placebo, at reference time point)
theta_trt <- 0.5 # ms, treatment-specific intercept
theta_conc <- 2 # ms per ug/mL, concentration-QTc slope
theta_bl <- -0.15 # effect of baseline QTcF (regression to the mean)
sd_eta_0 <- 5 # ms, between-subject SD of the intercept
sd_eta_conc <- 0.6 # ms per ug/mL, between-subject SD of the slope
sd_eps <- 5 # ms, residual SD

d_subj <-
  d_subj |>
  dplyr::mutate(
    QTCFBL = rnorm(n_subj, 400, 12),
    ETA_0 = rnorm(n_subj, 0, sd_eta_0),
    ETA_CONC = rnorm(n_subj, 0, sd_eta_conc)
  )

# Diurnal variation of QTcF relative to the pre-dose baseline (dosing ~8 AM)
diurnal <- function(t) 3 * sin(2 * pi * (t + 2) / 24) - 3 * sin(2 * pi * 2 / 24)

# Concentration-time profiles ---------------------------------------------------
# Parent: amount-based Bateman function, dose in mg and V in L -> mg/L = ug/mL
conc_parent <- function(t, dose, ka, cl, v) {
  k <- cl / v
  dose * ka / (v * (ka - k)) * (exp(-k * t) - exp(-ka * t))
}

# Metabolite: gut -> parent -> metabolite linear chain
conc_metab <- function(t, dose, ka, cl, v, km, vm, fm) {
  k <- cl / v
  am <- fm * dose * ka * k * (
    exp(-ka * t) / ((k - ka) * (km - ka)) +
      exp(-k * t) / ((ka - k) * (km - k)) +
      exp(-km * t) / ((ka - km) * (k - km))
  )
  am / vm
}

lloq <- 1 # ng/mL

d_sim_cqt <-
  tidyr::expand_grid(d_subj, NTIME = times) |>
  dplyr::mutate(
    # True concentrations (ug/mL)
    CP_TRUE = conc_parent(NTIME, DOSE, KA, CL, V),
    CM_TRUE = conc_metab(NTIME, DOSE, KA, CL, V, KM, VM, FM),
    # Observed concentrations (ng/mL), 15% proportional residual error,
    # values below the LLOQ set to 0
    CONC = CP_TRUE * 1000 * exp(rnorm(dplyr::n(), 0, 0.15)),
    MCONC = CM_TRUE * 1000 * exp(rnorm(dplyr::n(), 0, 0.15)),
    CONC = dplyr::if_else(CONC < lloq, 0, round(CONC, 1)),
    MCONC = dplyr::if_else(MCONC < lloq, 0, round(MCONC, 1)),
    CONC_1000 = CONC / 1000,
    MCONC_1000 = MCONC / 1000,
    # QTcF change from baseline, driven by the true parent concentration
    DQTCF = theta_0 + ETA_0 + theta_trt * (TRT == "Active") +
      (theta_conc + ETA_CONC) * CP_TRUE + diurnal(NTIME) +
      theta_bl * (QTCFBL - 400) + rnorm(dplyr::n(), 0, sd_eps),
    QTCFBL = round(QTCFBL, 1),
    QTCF = round(QTCFBL + DQTCF, 1),
    DQTCF = round(QTCF - QTCFBL, 1)
  ) |>
  dplyr::select(
    ID, COHORT, DOSE, TRT, NTIME, CONC, CONC_1000, MCONC, MCONC_1000,
    QTCFBL, QTCF, DQTCF
  )

readr::write_csv(d_sim_cqt, "data-raw/d_sim_cqt.csv")
usethis::use_data(d_sim_cqt, overwrite = TRUE)
