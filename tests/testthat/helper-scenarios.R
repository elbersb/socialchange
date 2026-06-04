# Helper functions for creating test scenarios
# These extract reusable components from vignettes to avoid code duplication

library(data.table)

# Population builders ---------------------------------------------------------

#' Build simple age-stratified population (ages 20-39)
#' Used in Scenario 1 (simple simulation)
build_simple_population <- function() {
  data.table(
    age = c(20:39),
    n = c(rep(100, 10), seq.int(100, 10, by = -10))
  )
}

#' Build population split by gender
#' Used in Scenario 2 (gender covariate)
build_gendered_population <- function() {
  data.table(
    age = c(20:39, 20:39),
    gender = c(rep("f", 20), rep("m", 20)),
    n = c(
      c(rep(50, 10), seq.int(50, 5, by = -5)),
      c(rep(50, 10), seq.int(50, 5, by = -5))
    )
  )
}

#' Build population with smoking status and differential mortality
#' Used in Scenario 5 (smoking)
build_smoking_population <- function() {
  data.table(
    age = c(20:39, 20:39),
    smoking = c(rep("smoker", 20), rep("nonsmoker", 20)),
    n = c(
      c(rep(90, 10), seq.int(90, 0, by = -12), 0, 0),
      c(rep(10, 10), seq.int(10, 1, by = -1))
    )
  )
}

# Outcome function factories --------------------------------------------------

#' Create age-only outcome function
#' Support increases linearly with age (pure age effect)
#' At age 20: outcome = 0; At age 40: outcome = 1
make_age_only_outcome <- function() {
  function(data, time) {
    data[, (age - 20) * 1 / 20]
  }
}

#' Create time-only outcome function for gendered population
#' Support increases with time (pure intraindividual change)
#' Gender baseline: f=0.2, m=0.4; increases by 0.1 per period
make_time_only_outcome <- function() {
  function(data, time) {
    data[, ifelse(gender == "f", 0.2, 0.4) + time / 10]
  }
}

#' Create cohort-only outcome function for gendered population
#' Support based on birth cohort (pure population turnover)
#' Cohort = time - age + 20
make_cohort_only_outcome <- function() {
  function(data, time) {
    data[, ifelse(gender == "f", 0.2, 0.4) + 0.7516 + (time - age + 20) / 10]
  }
}

#' Create gender-specific age outcome function
#' One gender fixed at 0.3758, other gender increases with age
make_gender_age_outcome <- function() {
  function(data, time) {
    data[, ifelse(gender == "f", 0.3758, (age - 20) * 1 / 20)]
  }
}

#' Create static smoking outcome function
#' Smokers: 0.9 support; Non-smokers: 0.6 support
make_smoking_static_outcome <- function() {
  function(data, time) {
    data[, ifelse(smoking == "smoker", 0.9, 0.6)]
  }
}

#' Create time-varying smoking outcome function
#' Smokers decline at 0.05/period; Non-smokers at 0.1/period
make_smoking_time_outcome <- function() {
  function(data, time) {
    data[, ifelse(smoking == "smoker", 0.9 - time * 0.05, 0.6 - time * 0.1)]
  }
}

# Population dynamics factories -----------------------------------------------

#' Create stable mortality function for simple population
#' Ages 30+: 10 deaths per period; Ages <30: 0 deaths
make_stable_mortality_simple <- function() {
  function(data, period) {
    data[, ifelse(age >= 30, 10, 0)]
  }
}

#' Create stable coming-of-age function for simple population
#' Adds 100 individuals at age 20 each period
make_stable_coming_of_age_simple <- function() {
  function(data, period) {
    data.table(age = 20, n = 100)
  }
}

#' Create stable mortality function for gendered population
#' Ages 30+: 5 deaths per period per gender; Ages <30: 0 deaths
make_stable_mortality_gendered <- function() {
  function(data, period) {
    data[, ifelse(age >= 30, 5, 0)]
  }
}

#' Create stable coming-of-age function for gendered population
#' Adds 50 f and 50 m individuals at age 20 each period
make_stable_coming_of_age_gendered <- function() {
  function(data, period) {
    data.table(age = c(20, 20), gender = c("f", "m"), n = c(50, 50))
  }
}

#' Create differential mortality function for smoking population
#' Smokers age 30+: up to 12 deaths; Non-smokers age 30+: 1 death
make_smoking_mortality <- function() {
  function(data, period) {
    data[, fcase(
      age >= 30 & smoking == "smoker", pmin(12, n),
      age >= 30 & smoking == "nonsmoker", 1,
      default = 0
    )]
  }
}

#' Create stable coming-of-age function for smoking population
#' Adds 90 smokers and 10 non-smokers at age 20 each period
make_smoking_coming_of_age_static <- function() {
  function(data, period) {
    data.table(age = c(20, 20), smoking = c("smoker", "nonsmoker"), n = c(90, 10))
  }
}

#' Create time-varying coming-of-age function for smoking population
#' Proportion of non-smokers increases 15 per period (from 10 to 85)
make_smoking_coming_of_age_varying <- function() {
  function(data, period) {
    data.table(
      age = c(20, 20),
      smoking = c("smoker", "nonsmoker"),
      n = c(90 - period * 15, 10 + period * 15)
    )
  }
}

#' Create transitions function for smoking population
#' 15% of smokers become non-smokers each period
make_smoking_transitions <- function() {
  function(data, time) {
    n_smokers <- data[smoking == "smoker", sum(n)]
    data.table(smoking = "smoker", to_smoking = "nonsmoker", n = round(n_smokers * 0.15))
  }
}
