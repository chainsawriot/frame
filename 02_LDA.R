set.seed(1212121)
source("lib.R")
rio::import(here::here("data", "Frame Corpus.xlsx")) %>% tibble::as_tibble() -> frame_df

frame_corpus <- corpus(x = frame_df$Content, docnames = frame_df$docid, docvars = data.frame(frame = frame_df$frame))

normal_tokens <- readRDS(ipath("normal_tokens.RDS"))
lemma_tokens <- readRDS(ipath("lemma_tokens.RDS"))

conditions <- expand.grid(words = c("none", "stem", "lemma"), stopwords = c(TRUE, FALSE), trim = c(TRUE, FALSE), alpha = c(0.01, 0.05, 0.1, 0.2, 0.5, 1.0))

res <- list()
for(i in seq_len(nrow(conditions))) {
    print(i)
    res[[i]] <- experiment_lda(conditions$words[i], conditions$stopwords[i], conditions$trim[i], conditions$alpha[i],
                               normal_tokens, lemma_tokens, frame_corpus)
}

##purrr::map_dbl(res, max)
LDA <- conditions
LDA$res <- res
saveRDS(tibble::tibble(LDA), ipath("LDA.RDS"))
