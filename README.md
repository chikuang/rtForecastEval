# evalRTPF: EVALuating Real-Time Probabilistic Forecast

April 9, 2026

<!-- badges: start -->

[![R-CMD-check](https://github.com/chikuang/evalRTPF/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/chikuang/evalRTPF/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

------------------------------------------------------------------------

## Authors

**Chi-Kuang Yeh** (Georgia State University)
[![ORCID](https://img.shields.io/badge/ORCID-0000--0001--7057--2096-A6CE39?logo=orcid.png)](https://orcid.org/0000-0001-7057-2096)

**Gregory Rice** (University of Waterloo)

**Joel A. Dubin** (University of Waterloo)

------------------------------------------------------------------------

## Description

**evalRTPF** is an R implementation of the methodology in [Yeh, Rice,
and Dubin (2022)](https://doi.org/10.1080/00031305.2021.1967781)
(preprint [arXiv:2010.00781](https://arxiv.org/abs/2010.00781),
[PDF](https://arxiv.org/pdf/2010.00781.pdf)). The paper develops tools
to evaluate **continuously updated** probabilistic forecasts:
calibration summaries, **skill** (relative performance of two
forecasters) via a global **delta test** in (L^2\[0,1\]) under squared
(Brier-type) loss, and simple **pointwise** inference for the mean loss
difference over time.

This package provides the statistical building blocks: pointwise loss
and variance (`calc_L_s2()`), the delta test statistic (`calc_Z()`),
eigenvalues for the chi-square approximation (`calc_eig()`), Monte Carlo
*p*-values (`calc_pval()`), simulation designs (`df_gen()`),
interpolation on a time grid (`lin_interp()`), and a naive pointwise
band plot (`plot_pcb()`).

Full replication code for the NBA application (data preparation,
calibration **surface** plots, competitor models, etc.) lives in a
separate analysis repository and is not bundled here.

## Installation

Currently evalRTPF is only available in R. We plan to develop Python,
Julia and/or Matlab versions in the future.

``` r
devtools::install_github("chikuang/evalRTPF")
```

## Examples

Minimal simulation (needs the **sde** package:
`install.packages("sde")`). After
`devtools::install_github("chikuang/evalRTPF")` or
`devtools::load_all()` from a clone:

``` r
library(dplyr)
library(evalRTPF)

set.seed(1)
nsamp <- 40   # time grid size (N in df_gen → nsamp + 1 points)
ngame <- 50   # number of independent “games”
D <- 8        # number of leading eigenvalues
N_MC <- 2000  # Monte Carlo draws for p-value

df_equ <- df_gen(N = nsamp, Ngame = ngame) |>
  group_by(grid) |>
  mutate(
    p_bar_12 = mean(phat_A - phat_B),
    diff_non_cent = phat_A - phat_B,
    diff_cent = phat_A - phat_B - p_bar_12
  ) |>
  ungroup()

ZZ <- calc_Z(df_equ, "phat_A", "phat_B", "Y", nsamp = nsamp, ngame = ngame)
eig <- calc_eig(df_equ, n_eig = D, ngame = ngame, nsamp = nsamp, cent = FALSE)
out <- calc_pval(ZZ, eig, quan = c(0.9, 0.95, 0.99), n_MC = N_MC)

Ltab <- calc_L_s2(df_equ, "phat_A", "phat_B")
# plot_pcb(Ltab)  # needs library(ggplot2)

c(Z = ZZ, p_value = out$p_val, out$quantile)
```

For the full workflow (larger `nsamp` / `ngame`, centered eigenvalues,
etc.), see `vignette("evalRTPF-vignette")`.

## TODO

- [ ] Add examples with graphical illustrations
- [ ] Add detailed descriptions
- [ ] Speed-up with RCPP components
- [ ] Upload to CRAN

## References

- Yeh, C.-K., Rice, G. & Dubin, J. A. (2022). [Evaluating real-time
  probabilistic forecasts with application to National Basketball
  Association outcome
  prediction](https://www.tandfonline.com/doi/full/10.1080/00031305.2021.1967781).
  *The American Statistician*, *76*, 214–223. DOI
  [10.1080/00031305.2021.1967781](https://doi.org/10.1080/00031305.2021.1967781).
- Preprint: [arXiv:2010.00781](https://arxiv.org/abs/2010.00781)
  ([PDF](https://arxiv.org/pdf/2010.00781.pdf)).
