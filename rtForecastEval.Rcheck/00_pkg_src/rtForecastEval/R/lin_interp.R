#' Linear interpolation of a forecast trajectory onto a grid
#'
#' Maps a vector of probabilities `prob` observed at times `grid` onto an
#' equally spaced grid of length `length(grid)` between 0 and 1 using linear
#' interpolation (`stats::approx`). Useful when aligning ESPN or model outputs
#' to the common grid used in the NBA analysis (see replication `pre_process` /
#' `utility.R`).
#'
#' @param prob Numeric vector of forecast probabilities.
#' @param grid Numeric vector of time points (same length as `prob`) between 0
#'   and 1.
#' @param outcome Scalar binary outcome \(Y\) to attach to every interpolated row.
#'
#' @return A tibble with columns `phat_approx`, `grid`, and `Y`.
#'
#' @references
#' Yeh, C.-K., Rice, G., & Dubin, J. A. (2022). *The American Statistician*,
#' *76*, 214--223. \doi{10.1080/00031305.2021.1967781}
#'
#' Preprint: \url{https://arxiv.org/abs/2010.00781}
#'
#' @export
lin_interp <- function(prob, grid, outcome) {
  ngrid <- length(grid)
  my_grid <- seq(0, 1, 1 / ngrid)
  df <- tibble(prob = prob, grid = grid)

  df %>%
    summarise(
      phat_approx = list(approx(grid, prob, my_grid, method = "linear")$y),
      grid = list(my_grid),
      Y = outcome
    ) %>%
    tidyr::unnest(cols = c("phat_approx", "grid"))
}
