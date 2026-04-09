#' Naive pointwise confidence band for mean loss difference
#'
#' Plots the pointwise mean loss difference from [`calc_L_s2()`] (column `L`)
#' against time, with a normal-theory band using `sigma2` and the point sample
#' size `n`. This is the **naive** \(t\)-style band from the paper; global
#' inference uses [`calc_Z()`] / [`calc_pval()`] instead.
#'
#' This plot summarizes **relative skill** (loss difference) over time — not a
#' classical **calibration** plot (forecast vs observed event rate). For a
#' simple reliability-style view from the same long-format data, see the package
#' vignette example that bins `phat` and plots mean `Y` vs mean forecast.
#'
#' @param df Output of [`calc_L_s2()`] (or compatible tibble with `grid`, `L`,
#'   `sigma2`, `n`).
#' @param grid Name of the x-axis column (default `"grid"`).
#' @param L Name of the pointwise mean difference column (default `"L"`).
#' @param var Name of the variance column (default `"sigma2"`).
#' @param title,subtitle,caption Passed to [ggplot2::labs()]. Defaults describe
#'   the plot when `NULL` (some labels are omitted if `NULL`).
#' @param xlab,ylab Axis labels (see [ggplot2::labs()]).
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
plot_pcb <- function(df, grid = "grid", L = "L", var = "sigma2",
                     title = "Pointwise mean loss difference (A vs B)",
                     subtitle = "Naive normal band (95%); use calc_pval() for global test",
                     caption = NULL,
                     xlab = "Normalized time (grid)",
                     ylab = "Mean squared loss difference") {
  df %>%
    ggplot(aes(!!sym(grid), !!sym(L))) +
    geom_ribbon(
      aes(
        ymax = !!sym(L) + stats::qnorm(0.975) * sqrt(!!sym(var)) / sqrt(!!sym("n")),
        ymin = !!sym(L) + stats::qnorm(0.025) * sqrt(!!sym(var)) / sqrt(!!sym("n"))
      ),
      alpha = 0.22,
      fill = "steelblue"
    ) +
    geom_line(color = "steelblue", linewidth = 0.9) +
    geom_hline(yintercept = 0, colour = "grey35", linewidth = 0.6, linetype = "dashed") +
    labs(title = title, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold")
    )
}
