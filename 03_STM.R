require(quanteda)
require(stm)
require(combinat)
suppressPackageStartupMessages(require(here))
ipath <- function(fname) {
    here::here("intermediate", fname)
}

set.seed(1212121)
rio::import(here::here("data", "Frame Corpus.xlsx")) %>% tibble::as_tibble() -> frame_df

frame_corpus <- corpus(x = frame_df$Content, docnames = frame_df$docid, docvars = data.frame(frame = frame_df$frame))

normal_tokens <- readRDS(ipath("normal_tokens.RDS"))
lemma_tokens <- readRDS(ipath("lemma_tokens.RDS"))

conditions <- expand.grid(words = c("none", "stem", "lemma"), stopwords = c(TRUE, FALSE), trim = c(TRUE, FALSE), alpha = c(0.01, 0.05, 0.1, 0.2, 0.5, 1.0))

experiment2 <- function(words, stopwords, trim, alpha) {
    if (words == "none") {
        x <- dfm(normal_tokens)
    } else if (words == "stem") {
        normal_tokens %>% tokens_wordstem("en") %>% dfm -> x
    } else if (words == "lemma") {
        x <- dfm(lemma_tokens)
    }
    if (stopwords) {
        x <- x %>% dfm_remove(stopwords("en"))
    }
    if (trim) {
        x <- x %>% dfm_trim(min_termfreq = 0.05, max_termfreq = 0.99, termfreq_type = "quantile")
    }
    x_stm <- convert(x, to = "stm")
    model.stm <- suppressMessages(stm(x_stm$documents, x_stm$vocab, K = 5, data = x_stm$meta, init.type = "Spectral", control = list(alpha = alpha)))
    topic_stm <- apply(model.stm$theta, 1, which.max)
    possible_frames <- unique(frame_df$frame)
    perm15 <- permn(c(1,2,3,4,5))
    return(purrr::map_dbl(1:120, ~ sum(diag(table(frame_df$frame, possible_frames[match(topic_stm, perm15[[.]])]))) / 100))
}

res <- list()
for(i in 1:72) {
    print(i)
    res[[i]] <- experiment2(conditions$words[i], conditions$stopwords[i], conditions$trim[i], conditions$alpha[i])
}

##purrr::map_dbl(res, max)
STM <- conditions
STM$res <- res
saveRDS(tibble::tibble(STM), ipath("STM.RDS"))
