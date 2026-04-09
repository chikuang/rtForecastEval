#!/usr/bin/env Rscript
# Render README.qmd -> README.md and fix Pandoc's "``` mermaid" fence (GitHub needs "```mermaid").
if (nzchar(Sys.which("quarto"))) {
  status <- system2("quarto", c("render", "README.qmd"), env = character())
  if (!identical(as.integer(status), 0L)) stop("quarto render failed", call. = FALSE)
} else if (requireNamespace("quarto", quietly = TRUE)) {
  quarto::quarto_render("README.qmd")
} else {
  stop("Install Quarto CLI (https://quarto.org) or R package quarto.", call. = FALSE)
}
p <- "README.md"
lines <- readLines(p)
lines <- sub("^``` mermaid$", "```mermaid", lines)
writeLines(lines, p)
message("Wrote ", normalizePath(p, winslash = "/"))
