#' @title evalRTPF: Compare real-time probabilistic forecasts
#' @name evalRTPF-package
#' @aliases evalRTPF
#' @docType package
#'
#' Implements the **delta test** and related tools from Yeh, Rice, and Dubin
#' for comparing two continuously updated probabilistic forecasts (e.g. in-game
#' win probabilities) under squared (Brier-type) loss. Core steps: estimate
#' pointwise mean loss difference and its variance ([`calc_L_s2()`]), form the
#' test statistic [`calc_Z()`], estimate leading eigenvalues of the associated
#' covariance [`calc_eig()`], and obtain Monte Carlo *p*-values and quantiles
#' [`calc_pval()`]. [`df_gen()`] reproduces the simulation designs used in the
#' paper; [`plot_pcb()`] draws naive pointwise confidence bands.
#'
#' The companion replication repository (NBA data workflow, calibration
#' surfaces, and additional plots) is separate from this package; this package
#' exports the statistical building blocks used there.
#'
#' @references
#' Yeh, C.-K., Rice, G., & Dubin, J. A. (2022). Evaluating real-time
#' probabilistic forecasts with application to National Basketball Association
#' outcome prediction. *The American Statistician*, *76*, 214--223.
#' \doi{10.1080/00031305.2021.1967781}
#'
#' Preprint: \url{https://arxiv.org/abs/2010.00781} (PDF:
#' \url{https://arxiv.org/pdf/2010.00781.pdf}).
#'
#' @keywords internal
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import purrr
#' @importFrom rlist list.rbind
#' @importFrom stats ecdf rchisq approx qnorm pnorm runif
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
