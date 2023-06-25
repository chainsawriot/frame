set.seed(1212121)
source("lib.R")

generic_sim("PCA", experiment_pca)

## rio::import(here::here("data", "Frame Corpus.xlsx")) %>% tibble::as_tibble() -> frame_df

## frame_corpus <- corpus(x = frame_df$Content, docnames = frame_df$docid, docvars = data.frame(frame = frame_df$frame))

## normal_tokens <- readRDS(ipath("normal_tokens.RDS"))
## lemma_tokens <- readRDS(ipath("lemma_tokens.RDS"))

## conditions <- expand.grid(words = c("none", "stem", "lemma"), stopwords = c(TRUE, FALSE), trim = c(TRUE, FALSE))

## conditions2 <- conditions

## conditions2$normal_tokens <- rep(list(normal_tokens), nrow(conditions))
## conditions2$lemma_tokens <- rep(list(lemma_tokens), nrow(conditions))
## conditions2$frame_corpus <- rep(list(frame_corpus), nrow(conditions))

## set.seed(1212121)
## res <- purrr::pmap(conditions2, experiment_pca, .progress = TRUE)

## PCA <- conditions
## PCA$res <- res
## saveRDS(tibble::tibble(PCA), ipath("PCA.RDS"))
