set.seed(1212121)
source("lib.R")

rio::import(here::here("data", "Frame Corpus.xlsx")) %>% tibble::as_tibble() -> frame_df

frame_corpus <- corpus(x = frame_df$Content, docnames = frame_df$docid, docvars = data.frame(frame = frame_df$frame))

normal_tokens <- readRDS(ipath("normal_tokens.RDS"))
lemma_tokens <- readRDS(ipath("lemma_tokens.RDS"))

## notes:
## in the original implementation, it says nothing about how many topics to train in the LDA. Here, we use k_factor to
## control the number of topics. k_factor of 2 means training 5 (k) * 2 = 10.

conditions <- expand.grid(words = c("none", "stem", "lemma"),
                          stopwords = c(TRUE, FALSE),
                          trim = c(TRUE, FALSE),
                          alpha = c(0.01, 0.05, 0.1, 0.2, 0.5, 1.0),
                          k_factor = c(2, 3, 4))

res <- list()
for(i in 1:nrow(conditions)) {
    print(i)
    res[[i]] <- experiment_antmn(words = conditions$words[i], stopwords = conditions$stopwords[i], trim = conditions$trim[i], alpha = conditions$alpha[i], k_factor = conditions$k_factor[i], normal_tokens, lemma_tokens, frame_corpus)
}

ANTMN <- conditions
ANTMN$res <- res
saveRDS(tibble::tibble(ANTMN), ipath("ANTMN.RDS"))
