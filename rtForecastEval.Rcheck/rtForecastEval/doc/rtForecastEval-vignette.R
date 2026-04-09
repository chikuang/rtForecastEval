## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(digits = 3)

## ----workflow-schematic, echo=FALSE, fig.width=8, fig.height=4, fig.cap="Typical workflow: global test (top) and pointwise loss plot (bottom). Arrows are logical dependencies, not strict call order."----
library(ggplot2)

box <- function(x, y, w, h, label) {
  data.frame(
    xmin = x - w / 2, xmax = x + w / 2,
    ymin = y - h / 2, ymax = y + h / 2,
    label = label, x = x, y = y
  )
}

top <- rbind(
  box(1.2, 5, 1.1, 0.55, "Data\n(df_gen or yours)"),
  box(3.1, 5, 1.0, 0.55, "Long format +\ncentered diffs"),
  box(4.9, 5.45, 0.85, 0.45, "calc_Z"),
  box(4.9, 4.55, 0.85, 0.45, "calc_eig"),
  box(6.8, 5, 0.95, 0.55, "calc_pval")
)
bot <- rbind(
  box(3.1, 2.6, 1.0, 0.55, "calc_L_s2"),
  box(5.0, 2.6, 0.85, 0.55, "plot_pcb")
)
rects <- rbind(top, bot)

seg <- data.frame(
  x = c(1.75, 2.65, 4.45, 4.45, 5.35, 5.35, 3.1, 3.65),
  y = c(5, 5, 5.45, 4.55, 5.45, 4.55, 4.45, 2.6),
  xend = c(2.6, 3.6, 5.35, 5.35, 6.35, 6.35, 3.1, 4.55),
  yend = c(5, 5, 5.45, 4.55, 5, 5, 3.15, 2.6)
)

ggplot() +
  geom_rect(
    data = rects,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    fill = "grey96", color = "grey35", size = 0.35
  ) +
  geom_text(data = rects, aes(x = x, y = y, label = label), size = 3) +
  geom_segment(
    data = seg,
    aes(x = x, y = y, xend = xend, yend = yend),
    arrow = grid::arrow(length = grid::unit(0.12, "cm"), type = "closed"),
    size = 0.35,
    color = "grey25"
  ) +
  annotate("text", x = 4.9, y = 5.95, label = "Global delta test (chi-square approx.)", size = 3.2, fontface = "italic") +
  annotate("text", x = 4.0, y = 1.85, label = "Pointwise mean loss + naive band", size = 3.2, fontface = "italic") +
  coord_cartesian(xlim = c(0.3, 7.5), ylim = c(1.5, 6.2), clip = "off") +
  theme_void() +
  theme(plot.margin = grid::unit(c(12, 8, 8, 8), "pt"))

## ----load, message = FALSE, include = FALSE-----------------------------------
require(dplyr)
require(tidyr)
require(gridExtra)
require(RSpectra)
require(rlist)

## ----include = FALSE----------------------------------------------------------
library(rtForecastEval)

## -----------------------------------------------------------------------------
library(ggplot2)
library(tibble)
library(MASS)
nsamp <- 100 # number of in-game events
ngame <- 100 # number of games

#' Parameter for generating the eigenvalues, and p-values
D <- 10 # Number of eigenvalues to keep
N_MC <- 5000 # for simulating the p-value
L <- function(x, y) {
  return((x - y) ^ 2)
}

# Data generation ---------------------------------------------------------
df_equ <- df_gen(N = nsamp, Ngame = ngame) %>%
  group_by(grid) %>%
  mutate(
    p_bar_12 = mean(phat_A - phat_B),
    diff_non_cent = phat_A - phat_B,
    diff_cent = phat_A - phat_B - p_bar_12
  ) %>% ungroup()

# Apply our test (explicit construction, as in utility.R) ----------------

Z <- df_equ %>% group_by(grid) %>%
  summarise(delta_n = mean(L(phat_A, Y) - L(phat_B, Y))) %>%
  {
    sum((.)$delta_n^2) / nsamp * ngame
  }

temp <- df_equ %>% group_split(grid, .keep = FALSE)

eigV_hat <- lapply(1:nsamp, function(i) {
  sapply(1:nsamp, function(j) {
    as.numeric(temp[[i]]$diff_non_cent %*% temp[[j]]$diff_non_cent / ngame)
  })
}) %>% list.rbind() %>% {
  RSpectra::eigs_sym(
    A = (.),
    k = D,
    which = "LM",
    opts = list(retvec = FALSE)
  )$values
} %>%
  {
    (.)/nsamp
  }

eigV_til <- lapply(1:nsamp, function(i) {
  sapply(1:nsamp, function(j) {
    as.numeric(temp[[i]]$diff_cent %*% temp[[j]]$diff_cent / ngame)
  })
}) %>% list.rbind() %>% {
  RSpectra::eigs_sym(
    A = (.),
    k = D,
    which = "LM",
    opts = list(retvec = FALSE)
  )$values
} %>%
  {
    (.)/nsamp
  }

MC_hat <- sapply(1:N_MC, function(x) {
  crossprod(eigV_hat, rchisq(D, df = 1))
})

q_90_hat <- quantile(MC_hat, 0.90)
q_95_hat <- quantile(MC_hat, 0.95)
q_99_hat <- quantile(MC_hat, 0.99)

MC_til <- sapply(1:N_MC, function(x) {
  crossprod(eigV_til, rchisq(D, df = 1))
})

q_90_til <- quantile(MC_til, 0.90)
q_95_til <- quantile(MC_til, 0.95)
q_99_til <- quantile(MC_til, 0.99)

p_hat <- 1 - ecdf(MC_hat)(Z)

tibble(
  type = c("non-center", "center"),
  Z = rep(Z, 2),
  "pval" = c(p_hat, p_hat),
  "90%" = c(q_90_hat, q_90_til),
  "95%" = c(q_95_hat, q_95_til),
  "99%" = c(q_99_hat, q_99_til)
)

## ----function wrappers, fig.width = 7, fig.height = 4.2, fig.cap = "Pointwise mean loss difference (A vs B) with naive normal band — simulation setting. This is a skill curve, not a calibration diagram."----
to_center <- FALSE

ZZ <- calc_Z(df = df_equ, pA = "phat_A", pB = "phat_B", Y = "Y", nsamp = nsamp, ngame = ngame)
eigg <- calc_eig(
  df = df_equ, n_eig = D, ngame = ngame,
  nsamp = nsamp, grid = "grid", cent = to_center
)
oh <- calc_pval(ZZ, eig = eigg, quan = c(0.90, 0.95, 0.99), n_MC = N_MC)

temp <- calc_L_s2(df = df_equ, pA = "phat_A", pB = "phat_B")

plot_pcb(df = temp)

tibble(
  type = ifelse(to_center, "center", "non-center"),
  Z = ZZ,
  pval = oh$p_val,
  "90%" = oh$quantile[1],
  "95%" = oh$quantile[2],
  "99%" = oh$quantile[3]
)

## ----calibration-reliability, fig.width = 7.5, fig.height = 4, fig.cap = "Binned reliability diagram at a fixed grid (closest to 0.5): mean outcome vs mean forecast for A and B. Points near the diagonal indicate better marginal calibration at that time."----
g_mid <- df_equ %>%
  distinct(grid) %>%
  slice_min(order_by = abs(grid - 0.5), n = 1) %>%
  pull(grid)

slice_t <- df_equ %>%
  filter(grid == g_mid) %>%
  transmute(
    game,
    Y,
    phat_A,
    phat_B
  )

rel_long <- slice_t %>%
  tidyr::pivot_longer(c(phat_A, phat_B), names_to = "forecaster", values_to = "phat") %>%
  mutate(forecaster = ifelse(forecaster == "phat_A", "Forecaster A", "Forecaster B"))

rel_binned <- rel_long %>%
  group_by(forecaster) %>%
  mutate(bin = ntile(phat, 10)) %>%
  group_by(forecaster, bin) %>%
  summarise(
    mean_forecast = mean(phat),
    mean_outcome = mean(Y),
    n_games = dplyr::n(),
    .groups = "drop"
  )

ggplot(rel_binned, aes(mean_forecast, mean_outcome)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey45") +
  geom_point(aes(size = n_games), alpha = 0.9, color = "darkred") +
  facet_wrap(~forecaster, nrow = 1) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1), ratio = 1) +
  labs(
    title = "Binned reliability (calibration) at one grid",
    subtitle = paste0("Grid = ", signif(g_mid, 3), " (closest to 0.5); 10 quantile bins per forecaster"),
    x = "Mean forecast in bin",
    y = "Mean outcome (Y) in bin",
    size = "Games"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(face = "bold")
  )

