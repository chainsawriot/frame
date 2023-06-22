set.seed(1212121)
source("lib.R")

## notes:
## in the original implementation, it says nothing about how many topics to train in the LDA. Here, we use k_factor to
## control the number of topics. k_factor of 2 means training 5 (k) * 2 = 10.

conditions <- expand.grid(words = c("none", "stem", "lemma"),
                          stopwords = c(TRUE, FALSE),
                          trim = c(TRUE, FALSE),
                          alpha = c(0.01, 0.05, 0.1, 0.2, 0.5, 1.0),
                          k_factor = c(2, 3, 4))
generic_sim("ANTMN", experiment_antmn, conditions = conditions)

## conditions2 <- conditions

## conditions2$normal_tokens <- rep(list(normal_tokens), nrow(conditions))
## conditions2$lemma_tokens <- rep(list(lemma_tokens), nrow(conditions))
## conditions2$frame_corpus <- rep(list(frame_corpus), nrow(conditions))


## set.seed(1212121)
## res <- purrr::pmap(conditions2, experiment_antmn, .progress = TRUE)

## ANTMN <- conditions
## ANTMN$res <- res
## saveRDS(tibble::tibble(ANTMN), ipath("ANTMN.RDS"))
