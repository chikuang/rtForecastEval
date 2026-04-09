#' Monte Carlo p-value and quantiles for the delta test
#'
#' Given leading eigenvalues of the covariance operator used in the delta test
#' (from [`calc_eig()`]) and the observed statistic \(Z\) (from [`calc_Z()`]),
#' draws a Monte Carlo sample from the weighted sum of chi-square(1) variables
#' and returns the right-tail *p*-value and quantiles of the null distribution
#' (as in the paper’s replication code).
#'
#' @param Z Observed test statistic from [`calc_Z()`].
#' @param eig Numeric vector of leading eigenvalues (length `n_eig`).
#' @param quan Probabilities for which to return quantiles of the simulated null
#'   statistic (e.g. `c(0.90, 0.95, 0.99)`).
#' @param n_MC Number of Monte Carlo draws (default `5000`).
#'
#' @return A list with `p_val` (one-sided p-value) and `quantile` (named
#'   vector of quantiles at `quan`).
#'
#' @references
#' Yeh, C.-K., Rice, G., & Dubin, J. A. (2022). *The American Statistician*,
#' *76*, 214--223. \doi{10.1080/00031305.2021.1967781}
#'
#' Preprint: \url{https://arxiv.org/abs/2010.00781}
#'
#' @export
calc_pval <- function(Z, eig, quan, n_MC = 5000) {
  set.seed(520)
  n_eig <- length(eig)
  MC <- sapply(1:n_MC, function(x) {
    crossprod(eig, stats::rchisq(n_eig, df = 1))
  })

  list(
    p_val = 1 - stats::ecdf(MC)(Z),
    quantile = stats::quantile(MC, quan)
  )
}
