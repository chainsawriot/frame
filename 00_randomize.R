require(tidyverse)
set.seed(1011212189)
frames <- tibble::tibble(docid = 1:100, topic = sample(rep(c("Ukraine", "Corona", "Climate", "Tech", "Joker"), 20)), frame = replicate(100, sample(c("Conflict", "Human Interest", "Economic Consequences", "Morality", "Responsibility"), 1)), writer = sample(rep(1:4, 25)))

frames$writer

n <- c(0, 0, 0, 0)
res <- c()

for (i in seq_along(frames$writer)) {
    redraw <- TRUE
    while(redraw) {
        candidate <- sample(setdiff(1:4, frames$writer[i]), 1)
        if (n[candidate] >= 25) {
            redraw <- TRUE
        } else {
            redraw <- FALSE
            n[candidate] <- n[candidate] + 1
            res[i] <- candidate
        }
    }
}

frames$editor <- res

rio::export(frames, here::here("data", "frames.csv"))

