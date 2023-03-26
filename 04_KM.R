set.seed(1212121)
source("lib.R")

rio::import(here::here("data", "Frame Corpus.xlsx")) %>% tibble::as_tibble() -> frame_df

frame_corpus <- corpus(x = frame_df$Content, docnames = frame_df$docid, docvars = data.frame(frame = frame_df$frame))

normal_tokens <- readRDS(ipath("normal_tokens.RDS"))
lemma_tokens <- readRDS(ipath("lemma_tokens.RDS"))

conditions <- expand.grid(words = c("none", "stem", "lemma"), stopwords = c(TRUE, FALSE), trim = c(TRUE, FALSE))

res <- list()
for(i in 1:nrow(conditions)) {
    print(i)
    res[[i]] <- experiment_km(conditions$words[i], conditions$stopwords[i], conditions$trim[i], normal_tokens, lemma_tokens, frame_corpus)
}

KM <- conditions
KM$res <- res
saveRDS(tibble::tibble(KM), ipath("KM.RDS"))
