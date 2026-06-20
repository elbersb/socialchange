#' GSS attitudes toward homosexual sex relations
#'
#' A subset of the General Social Survey (GSS) containing respondents with
#' valid responses to the \code{homosex} question, covering survey years
#' 1973--2018 and birth cohorts 1892--1995. Oversample designs (samples 4, 5,
#' and 7) are excluded. The outcome variable has been rescaled from its
#' original 1--4 coding to a 0--1 scale. Prepared from the
#' \href{https://cran.r-project.org/package=gssr}{gssr} package.
#'
#' @format A data.table with 36,494 rows and 19 variables:
#' \describe{
#'   \item{id}{Respondent ID number.}
#'   \item{year}{GSS survey year.}
#'   \item{wtssall}{Survey weight.}
#'   \item{sample}{Sampling frame and method code.}
#'   \item{vstrat}{Variance stratum (NA for many years).}
#'   \item{vpsu}{Variance primary sampling unit (NA for many years).}
#'   \item{homosex}{Attitude toward sexual relations between two adults of the
#'     same sex, rescaled to 0--1 (0 = always wrong, 1 = not wrong at all).
#'     Original 4-point scale: 1 = Always Wrong, 2 = Almost Always Wrong,
#'     3 = Sometimes Wrong, 4 = Not Wrong at All.}
#'   \item{age}{Age of respondent at time of interview.}
#'   \item{cohort}{Birth year, computed as \code{year - age}.}
#'   \item{sex}{Sex of respondent (\code{"male"} or \code{"female"}).}
#'   \item{educ}{Highest year of school completed.}
#'   \item{marital}{Marital status (1 = Married, 2 = Widowed, 3 = Divorced,
#'     4 = Separated, 5 = Never Married).}
#'   \item{race}{Race of respondent (\code{"white"}, \code{"black"}, or
#'     \code{"other"}).}
#'   \item{region}{Region of interview (1--9, Census divisions).}
#'   \item{born}{Whether respondent was born in the United States
#'     (1 = Yes, 2 = No; NA for many years).}
#'   \item{physhlth}{Days of poor physical health in past 30 days
#'     (NA for many years).}
#'   \item{compuse}{Whether respondent uses a computer
#'     (1 = Yes, 2 = No; NA for many years).}
#'   \item{relig16}{Religion in which respondent was raised
#'     (1 = Protestant, 2 = Catholic, 3 = Jewish, 4 = None, etc.).}
#'   \item{pray}{Frequency of prayer (NA for many years).}
#' }
#' @source Smith, Tom W., Davern, Michael, Freese, Jeremy, and Morgan,
#'   Stephen L. General Social Surveys, 1972--2018. NORC, Chicago.
#'   Accessed via the \href{https://cran.r-project.org/package=gssr}{gssr}
#'   R package.
"gss_homosex"

#' GSS racial attitudes of white Americans
#'
#' A subset of the General Social Survey (GSS) containing white respondents
#' with valid responses to racial attitude questions, covering survey years
#' 1972, 1976, 1980, and 1984. The outcome variable \code{rac} is a composite
#' scale built from four items (RACDIN, RACSEG, RACPUSH, RACMAR), each
#' negated and z-scored so that higher values indicate more tolerant attitudes.
#' Up to two missing items per respondent are imputed by rescaling the
#' available items. Prepared from the
#' \href{https://cran.r-project.org/package=gssr}{gssr} package.
#'
#' @format A data.table with 7 variables:
#' \describe{
#'   \item{rac}{Composite racial tolerance scale (higher = more tolerant).
#'     Built from four z-scored items (each negated), then shifted by +6 to
#'     give positive values.}
#'   \item{year}{GSS survey year (1972, 1976, 1980, or 1984).}
#'   \item{age}{Age of respondent at time of interview.}
#'   \item{cohort}{Birth year, computed as \code{year - age}.}
#'   \item{region}{Region of interview (1--9, Census divisions).}
#'   \item{sex}{Sex of respondent (1 = Male, 2 = Female).}
#'   \item{wtssall}{Survey weight.}
#' }
#' @details The four items comprising the scale are:
#' \describe{
#'   \item{RACDIN}{Whether Black Americans are welcome for dinner.}
#'   \item{RACSEG}{Whether whites have the right to keep Black Americans out
#'     of their neighborhoods.}
#'   \item{RACPUSH}{Whether Black Americans should not push themselves where
#'     they are not wanted.}
#'   \item{RACMAR}{Whether there should be laws against Black-white marriage.}
#' }
#' @source Smith, Tom W., Davern, Michael, Freese, Jeremy, and Morgan,
#'   Stephen L. General Social Surveys, 1972--2018. NORC, Chicago.
#'   Accessed via the \href{https://cran.r-project.org/package=gssr}{gssr}
#'   R package.
"gss_rac"

#' UN World Population Prospects population data
#'
#' Total population estimates and projections by location and year from the
#' UN World Population Prospects (WPP), covering 1950--2100. Includes world
#' regions and individual countries. Prepared via the
#' \href{https://github.com/PPgp/tidywpp}{tidywpp} package.
#'
#' @format A data frame with 43,186 rows and 3 variables:
#' \describe{
#'   \item{Location}{Name of the country or region (character).}
#'   \item{Time}{Year (numeric), ranging from 1950 to 2100.}
#'   \item{PopTotal}{Total population in thousands (numeric).}
#' }
#' @source United Nations, Department of Economic and Social Affairs,
#'   Population Division. World Population Prospects 2022.
#'   \url{https://population.un.org/wpp/}
"wpp_data"

#' US population by age and sex, 1973--2016
#'
#' United States population by single year of age and sex, from the UN World
#' Population Prospects (WPP) 2022, for every year from 1973 to 2016 (the span
#' of the \code{\link{gss_homosex}} survey waves). Ages run from 21 to 89, with
#' 89 representing "89 or older" to match the GSS age top-code. Counts are in
#' thousands and reflect the true US age/sex structure; they are intended for
#' use as the \code{population} frame in \code{\link{decompose_aggregated}},
#' where only the relative cell structure matters (rescale per period as needed).
#' Prepared via the \href{https://github.com/PPgp/tidywpp}{tidywpp} package.
#'
#' @format A data.table with 6,072 rows and 4 variables:
#' \describe{
#'   \item{period}{Year (numeric), 1973--2016.}
#'   \item{age}{Single year of age (21--89; 89 = "89 or older").}
#'   \item{sex}{Sex (\code{"male"} or \code{"female"}), matching
#'     \code{gss_homosex$sex}.}
#'   \item{n}{Population in thousands (numeric).}
#' }
#' @source United Nations, Department of Economic and Social Affairs,
#'   Population Division. World Population Prospects 2022.
#'   \url{https://population.un.org/wpp/}
"wpp_us"

#' US mortality rates, 1933--2019
#'
#' Mortality rates by sex and age. Used in examples for
#' \code{\link{decompose_aggregated}}.
#'
#' @format A data frame with 15,660 rows and 4 variables:
#' \describe{
#'   \item{year}{Year.}
#'   \item{age}{Age.}
#'   \item{sex}{Sex, either 'male' or 'female'.}
#'   \item{death_rate}{Central death rate m(x): deaths divided by exposure (HMD `Mx_1x1`).
#'     Not a probability -- it can exceed 1 at the oldest ages.}
#' }
#' @source HMD. Human Mortality Database.
#'  Max Planck Institute for Demographic Research (Germany),
#'  University of California, Berkeley (USA),
#'  and French Institute for Demographic Studies (France). Available at www.mortality.org.
"mortality_us"

#' EU membership events
#'
#' Dates of entry into and exit from the European Union (and its predecessor,
#' the European Coal and Steel Community / European Economic Community) for
#' all 28 member states as of 2020. Used in examples for
#' \code{\link{decompose_events}}.
#'
#' @format A data frame with 29 rows and 3 variables:
#' \describe{
#'   \item{country}{Country name (character).}
#'   \item{date}{Date of the membership event in \code{"YYYY-MM-DD"} format
#'     (character).}
#'   \item{event_type}{Type of event: \code{"initial"} for the six founding
#'     members of the ECSC in 1952, \code{"entry"} for subsequent accessions,
#'     or \code{"exit"} for departures (United Kingdom, 2020-01-31).}
#' }
#' @source Wikipedia contributors. "Member state of the European Union."
#'   Wikipedia, The Free Encyclopedia.
"eu_membership"
