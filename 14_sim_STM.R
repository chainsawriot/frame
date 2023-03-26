set.seed(1212121)
source("lib.R")

conditions <- expand.grid(words = c("none", "stem", "lemma"),
                          stopwords = c(TRUE, FALSE),
                          trim = c(TRUE, FALSE),
                          alpha = c(0.01, 0.05, 0.1, 0.2, 0.5, 1.0))

for (n in ns) {
    lemma_tokens <- readRDS(ipath(paste0("lemma_tokens_sim", n, ".RDS")))
    frame_corpus <- readRDS(ipath(paste0("frame_corpus_sim", n, ".RDS")))
    frame_corpus %>% tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_separators = TRUE, remove_symbols = TRUE, split_hyphens = TRUE) %>% tokens_tolower() -> normal_tokens
    res <- list()
    for(i in seq_len(nrow(conditions))) {
        print(i)
        res[[i]] <- experiment_stm(conditions$words[i], conditions$stopwords[i], conditions$trim[i], conditions$alpha[i], normal_tokens, lemma_tokens, frame_corpus)
    }
    STM <- conditions
    STM$res <- res
    saveRDS(tibble::tibble(STM), ipath(paste0("STM_sim", n, ".RDS")))
}
