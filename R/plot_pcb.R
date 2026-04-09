#' Naive pointwise confidence band for mean loss difference
#'
#' Plots the pointwise mean loss difference from [`calc_L_s2()`] (column `L`)
#' against time, with a normal-theory band using `sigma2` and the point sample
#' size `n`. This is the **naive** \(t\)-style band from the paper; global
#' inference uses [`calc_Z()`] / [`calc_pval()`] instead.
#'
#' @param df Output of [`calc_L_s2()`] (or compatible tibble with `grid`, `L`,
#'   `sigma2`, `n`).
#' @param grid Name of the x-axis column (default `"grid"`).
#' @param L Name of the pointwise mean difference column (default `"L"`).
#' @param var Name of the variance column (default `"sigma2"`).
#'
#' @return A **ggplot2** object.
#'
#' @references
#' Yeh, C.-K., Rice, G., & Dubin, J. A. (2022). *The American Statistician*,
#' *76*, 214--223. \doi{10.1080/00031305.2021.1967781}
#'
#' Preprint: \url{https://arxiv.org/abs/2010.00781}
#'
#' @export
plot_pcb <- function(df, grid = "grid", L = "L", var = "sigma2") {
  df %>% ggplot(aes(!!sym(grid), !!sym(L))) + geom_line() +
    geom_ribbon(aes(
      ymax = !!sym(L) + stats::qnorm(0.975) * sqrt(!!sym(var)) / sqrt(n),
      ymin = !!sym(L) + stats::qnorm(0.025) * sqrt(!!sym(var)) / sqrt(n)
    ),
    alpha = 0.2, col = "red"
    ) +
    geom_hline(yintercept = 0, colour = "blue", linewidth = 1.25)
}
