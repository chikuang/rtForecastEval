#' Leading eigenvalues for the delta test covariance
#'
#' Forms the empirical covariance matrix of either centered or non-centered
#' forecast differences across time points (see `diff_cent` vs `diff_non_cent`
#' in the paper), and returns the leading eigenvalues scaled by one over
#' `nsamp`, for use with [`calc_pval()`]. This matches the construction in the
#' replication `utility.R` (`calc_p_val` / `eigs_sym`).
#'
#' @param df Data frame containing `grid` and a column `diff_cent` or
#'   `diff_non_cent` (vector differences of forecasts across games, aligned
#'   within each time point).
#' @param n_eig Number of leading eigenvalues to extract (dimension D in the
#'   paper).
#' @param ngame Number of independent games (used for scaling inner products).
#' @param nsamp Number of time grid points.
#' @param grid Name of the time grid column.
#' @param cent If `TRUE`, use `diff_cent` (centered differences); if `FALSE`,
#'   use `diff_non_cent`.
#'
#' @return A numeric vector of length `n_eig` of eigenvalues (descending).
#'
#' @references
#' Yeh, C.-K., Rice, G., & Dubin, J. A. (2022). *The American Statistician*,
#' *76*, 214--223. \doi{10.1080/00031305.2021.1967781}
#'
#' Preprint: \url{https://arxiv.org/abs/2010.00781}
#'
#' @export
calc_eig <- function(df, n_eig = 10, ngame, nsamp, grid = "grid", cent = FALSE) {
  diff <- ifelse(cent, "diff_cent", "diff_non_cent")
  df_list <- df %>% dplyr::select(!!sym(grid), !!sym(diff)) %>%
    group_split(!!sym(grid), .keep = FALSE)
  df_vec <- lapply(seq_along(df_list), function(x) {
    df_list[[x]] %>% unlist() %>% as.vector()
  })
  rm(df_list)
  eigV <- lapply(1:nsamp, function(i) {
    sapply(1:nsamp, function(j) {
      as.numeric(df_vec[[i]] %*% df_vec[[j]] / ngame)
    })
  }) %>% rlist::list.rbind() %>% {
    RSpectra::eigs_sym(A = (.), k = n_eig, which = "LM",
                       opts = list(retvec = FALSE))$values
  } %>%
    {
      (.)/nsamp
    }

  eigV
}
