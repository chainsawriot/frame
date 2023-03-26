set.seed(1212121)
source("lib.R")

conditions <- expand.grid(words = c("none", "stem", "lemma"), stopwords = c(TRUE, FALSE), trim = c(TRUE, FALSE))

for (n in ns) {
    lemma_tokens <- readRDS(ipath(paste0("lemma_tokens_sim", n, ".RDS")))
    frame_corpus <- readRDS(ipath(paste0("frame_corpus_sim", n, ".RDS")))
    frame_corpus %>% tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_separators = TRUE, remove_symbols = TRUE, split_hyphens = TRUE) %>% tokens_tolower() -> normal_tokens
    res <- list()
    for(i in seq_len(nrow(conditions))) {
        print(i)
        res[[i]] <- experiment_pca(conditions$words[i], conditions$stopwords[i], conditions$trim[i], normal_tokens, lemma_tokens, frame_corpus)
    }
    PCA <- conditions
    PCA$res <- res
    saveRDS(tibble::tibble(PCA), ipath(paste0("PCA_sim", n, ".RDS")))
}
