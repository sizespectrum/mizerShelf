plot_diff <- function(x1, x2) {
    wc <- compare(x1, x2, max_diffs = Inf)
    if (length(wc)) {
        barplot(rel_diff(x1, x2), names.arg = 1:length(x1), horiz = TRUE)
    }
    wc
}

rel_diff <- function(x1, x2) {
    r <- abs((x1 - x2) / (x1 + x2))
    r[is.nan(r)] <- 0
    r
}

l <- function(w) {
    (w / sp$a) ^ (1 / sp$b)
}

w <- function(l) {
    sp$a * l ^ sp$b
}
