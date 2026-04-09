#' Delta test statistic for comparing two forecasting methods
#'
#' Computes the global test statistic Z for comparing two real-time forecasters
#' under squared loss: Z equals (n_game / n_samp) times the sum over grid
#' points of the squared pointwise mean loss differences, matching the
#' implementation in the paper replication code (utility.R).
#'
#' @param df Data frame containing forecasts, outcomes, and `grid`.
#' @param pA,pB Names of columns with the two probability forecasts.
#' @param Y Name of the binary outcome column.
#' @param grid Name of the column with normalized times between 0 and 1.
#' @param nsamp Number of distinct time points (length of the grid).
#' @param ngame Number of independent replicates (e.g. games).
#' @param L Loss function; default is squared error.
#'
#' @return A single numeric value of the test statistic Z.
#'
#' @references
#' Yeh, C.-K., Rice, G., & Dubin, J. A. (2022). *The American Statistician*,
#' *76*, 214--223. \doi{10.1080/00031305.2021.1967781}
#'
#' Preprint: \url{https://arxiv.org/abs/2010.00781}
#'
#' @export
calc_Z <- function(df, pA = "phat_1", pB = "phat_2", Y = "Y", grid = "grid",
                   nsamp, ngame,
                   L = function(x, y) (x - y)^2) {
  df %>% dplyr::select(!!sym(grid), !!sym(Y), !!sym(pA), !!sym(pB)) %>%
    group_by(!!sym(grid)) %>%
    summarise(delta_n = mean(L(!!sym(pA), !!sym(Y)) - L(!!sym(pB), !!sym(Y))), .groups = "drop") %>%
    summarise(sum(delta_n^2) / nsamp * ngame) %>% pull()
}
