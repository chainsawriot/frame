set.seed(1212121)
source("lib.R")

conditions <- expand.grid(words = c("none", "stem", "lemma"), stopwords = c(TRUE, FALSE), trim = c(TRUE, FALSE), alpha = c(0.01, 0.05, 0.1, 0.2, 0.5, 1.0))

generic_sim("LDA", experiment_lda, conditions = conditions)

## rio::import(here::here("data", "Frame Corpus.xlsx")) %>% tibble::as_tibble() -> frame_df

## frame_corpus <- corpus(x = frame_df$Content, docnames = frame_df$docid, docvars = data.frame(frame = frame_df$frame))

## normal_tokens <- readRDS(ipath("normal_tokens.RDS"))
## lemma_tokens <- readRDS(ipath("lemma_tokens.RDS"))

## conditions2 <- conditions

## conditions2$normal_tokens <- rep(list(normal_tokens), nrow(conditions))
## conditions2$lemma_tokens <- rep(list(lemma_tokens), nrow(conditions))
## conditions2$frame_corpus <- rep(list(frame_corpus), nrow(conditions))

## set.seed(1212121)
## res <- purrr::pmap(conditions2, experiment_lda, .progress = TRUE)

## all.equal(res, readRDS(ipath("LDA.RDS"))$res)
