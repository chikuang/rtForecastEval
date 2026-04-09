#' Pointwise mean loss difference and variance
#'
#' For each time grid point, estimates the pointwise difference in expected
#' squared (Brier) loss between two probabilistic forecasts, and the variance
#' factor used for inference (Yeh, Rice, and Dubin, 2022). With default loss
#' `L(x,y) = (x-y)^2`, the function computes the mean over games of
#' `L(Y, phat_1) - L(Y, phat_2)` at each time, and the influence-style terms
#' `si` used to form sigma-squared over four, matching the paper replication
#' code.
#'
#' @param df A data frame with one row per (game × time) in long format.
#' @param pA,pB Column names for the two forecast vectors (probabilities between
#'   0 and 1).
#' @param Y Column name for the binary outcome (0/1).
#' @param grid Column name for the normalized time grid between 0 and 1.
#' @param L Loss function; default is squared error, \(L(x,y)=(x-y)^2\).
#'
#' @return A tibble with one row per `grid` value and columns `L` (mean loss
#'   difference), `sigma2` (variance factor), and `n` (number of rows).
#'
#' @references
#' Yeh, C.-K., Rice, G., & Dubin, J. A. (2022). *The American Statistician*,
#' *76*, 214--223. \doi{10.1080/00031305.2021.1967781}
#'
#' Preprint: \url{https://arxiv.org/abs/2010.00781}
#'
#' @export
calc_L_s2 <- function(df, pA = "phat_1", pB = "phat_2", Y = "Y",
                      grid = "grid", L = function(x, y) (x - y)^2) {
  df %>% mutate(si = L(1, !!sym(pA)) - L(0, !!sym(pA)) -
                   (L(1, !!sym(pB)) - L(0, !!sym(pB)))) %>%
    group_by(!!sym(grid)) %>%
    summarise(
      L = mean(L(!!sym(Y), !!sym(pA)) - L(!!sym(Y), !!sym(pB))),
      sigma2 = mean(si ^ 2) / 4,
      n = n(),
      .groups = "drop"
    )
}
