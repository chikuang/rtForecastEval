#' Simulate real-time probabilistic forecasts (paper designs)
#'
#' Generates replicated “games” and two competing forecast trajectories on a
#' uniform grid from 0 to 1, following the simulation constructions used in
#' Yeh, Rice, and Dubin (2022) (see `sanity_generator` / simulation scripts in
#' the replication repository). Ornstein–Uhlenbeck (`type = "OU"`) or Brownian
#' motion (`type = "BM"`) noise can drive the latent processes; outputs include
#' binary outcomes `Y` and forecast probabilities `phat_A`, `phat_B` suitable
#' for [`calc_Z()`], [`calc_eig()`], etc.
#'
#' Requires the **sde** package (Suggests) for Brownian paths.
#'
#' @param N Number of interior time steps (grid has `N+1` points from 0 to 1).
#' @param Ngame Number of independent replicates (games).
#' @param type `"OU"` (default) or `"BM"` for the innovation process used in the
#'   bivariate construction.
#' @param a,b Constants controlling the drift of the latent trajectory (see
#'   replication code).
#'
#' @return A tibble with columns including `game`, `grid`, `Y`, `phat_A`,
#'   `phat_B`, and latent `W1`, `W2`, etc., in long format.
#'
#' @references
#' Yeh, C.-K., Rice, G., & Dubin, J. A. (2022). *The American Statistician*,
#' *76*, 214--223. \doi{10.1080/00031305.2021.1967781}
#'
#' Preprint: \url{https://arxiv.org/abs/2010.00781}
#'
#' @importFrom MASS mvrnorm
#' @export
df_gen <- function(N, Ngame, type = c("OU", "BM"), a = 1.0, b = 0.27) {
  if (!is.numeric(N) || length(N) != 1L || N < 1L) {
    stop("`N` must be a positive integer (number of interior time steps).", call. = FALSE)
  }
  if (!is.numeric(Ngame) || length(Ngame) != 1L || Ngame < 1L) {
    stop("`Ngame` must be a positive integer (number of replicates).", call. = FALSE)
  }
  if (!requireNamespace("sde", quietly = TRUE)) {
    stop("Package \"sde\" is required for df_gen(). Install with install.packages(\"sde\").", call. = FALSE)
  }
  type <- match.arg(type)
  N <- as.integer(N)
  Ngame <- as.integer(Ngame)

  uu <- a * runif(Ngame, -1, +1) + b
  tt <- seq(0, 1, 1 / N)
  xt <- sapply(seq_len(Ngame), function(g) {
    wt <- sde::BM(x = 0, t0 = 0, T = 1, N = N) %>% as.numeric()
    uu[g] * tt + wt
  })

  Yn <- ifelse(xt[N + 1, ] > 0, 1, 0)

  if (type == "OU") {
    sim_OU <- function(n_time, n_rep) {
      sig <- 1
      tj <- seq(0, 1, 1 / n_time)
      mu <- rep(0, length(tj))
      etj <- exp(tj)
      CovXt <- sig^2 * exp(-0.5 * outer(tj, tj, `+`)) * outer(etj, etj, pmin)
      mvrnorm(n = n_rep, mu = mu, Sigma = CovXt) %>% t()
    }

    w1 <- sim_OU(n_time = N, n_rep = Ngame)
    w2 <- sim_OU(n_time = N, n_rep = Ngame)
  } else {
    w1 <- sapply(seq_len(Ngame), function(g) {
      sde::BM(x = 0, t0 = 0, T = 1, N = N) %>% as.numeric()
    })
    w2 <- sapply(seq_len(Ngame), function(g) {
      sde::BM(x = 0, t0 = 0, T = 1, N = N) %>% as.numeric()
    })
  }
  lapply(1:(N + 1), function(k) {
    tibble(game = 1:Ngame, u = uu, Xt = xt[k, ],
           grid = (k - 1) / N, Y = Yn,
           W1 = w1[k, ], W2 = w2[k, ]) # each sample point
  }) %>% list.rbind() %>% mutate(signXt = sign(Xt),
                                 phat_A = pnorm(u * (1 - grid) + Xt + W1,
                                                sd = sqrt(1 - grid)),
                                 phat_B = pnorm(u * (1 - grid) + Xt + W2,
                                                sd = sqrt(1 - grid)))
}
