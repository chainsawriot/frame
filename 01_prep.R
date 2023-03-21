require(quanteda)
require(spacyr)
## spacyr::spacy_install()
suppressPackageStartupMessages(require(here))

ipath <- function(fname) {
    here::here("intermediate", fname)
}

set.seed(1212121)
rio::import(here::here("data", "Frame Corpus.xlsx")) %>% tibble::as_tibble() -> frame_df

frame_corpus <- corpus(x = frame_df$Content, docnames = frame_df$docid, docvars = data.frame(frame = frame_df$frame))

frame_corpus %>% tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_separators = TRUE, remove_symbols = TRUE, split_hyphens = TRUE) %>% tokens_tolower() -> normal_tokens
saveRDS(normal_tokens, ipath("normal_tokens.RDS"))

as.tokens(spacy_parse(frame_corpus), use_lemma = TRUE) %>% tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_separators = TRUE, remove_symbols = TRUE, split_hyphens = TRUE) %>% tokens_tolower() -> lemma_tokens

saveRDS(lemma_tokens, ipath("intermediate", "lemma_tokens.RDS"))
saveRDS(frame_df, ipath("intermediate", "frame_df.RDS"))

require(seededlda)
require(combinat)

conditions <- expand.grid(words = c("none", "stem", "lemma"), stopwords = c(TRUE, FALSE), trim = c(TRUE, FALSE), alpha = c(0.01, 0.05, 0.1, 0.2, 0.5, 1.0))

experiment <- function(words, stopwords, trim, alpha) {
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
    tmod_lda <- textmodel_lda(x, k = 5, alpha = alpha)
    topic_lda <- apply(tmod_lda$theta, 1, which.max)
    possible_frames <- unique(frame_df$frame)
    perm15 <- permn(c(1,2,3,4,5))
    return(purrr::map_dbl(1:120, ~ sum(diag(table(frame_df$frame, possible_frames[match(topic_lda, perm15[[.]])]))) / 100))
}

res <- list()
for(i in seq_len(nrow(conditions))) {
    print(i)
    res[[i]] <- experiment(conditions$words[i], conditions$stopwords[i], conditions$trim[i], conditions$alpha[i])
}

##purrr::map_dbl(res, max)
LDA <- conditions
LDA$res <- res
saveRDS(tibble::tibble(LDA), ipath("LDA.RDS"))

require(stm)
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

experiment3 <- function(words, stopwords, trim) {
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
    km_model <- kmeans(as.matrix(dfm_tfidf(x)), 5)
    topic_km <- km_model$cluster
    possible_frames <- unique(frame_df$frame)
    perm15 <- permn(c(1,2,3,4,5))
    return(purrr::map_dbl(1:120, ~ sum(diag(table(frame_df$frame, possible_frames[match(topic_km, perm15[[.]])]))) / 100))
}

conditions <- expand.grid(words = c("none", "stem", "lemma"), stopwords = c(TRUE, FALSE), trim = c(TRUE, FALSE))

res <- list()
for(i in 1:nrow(conditions)) {
    print(i)
    res[[i]] <- experiment3(conditions$words[i], conditions$stopwords[i], conditions$trim[i])
}


KM <- conditions
KM$res <- res
saveRDS(tibble::tibble(KM), ipath("KM.RDS"))

experiment4 <- function(words, stopwords, trim) {
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
    pr_x <- prcomp(as.matrix(dfm_tfidf(x)))
    topic_pca <- apply(pr_x$x[,1:5], 1, which.max)
    possible_frames <- unique(frame_df$frame)
    perm15 <- permn(c(1,2,3,4,5))
    return(purrr::map_dbl(1:120, ~ sum(diag(table(frame_df$frame, possible_frames[match(topic_pca, perm15[[.]])]))) / 100))
}

conditions <- expand.grid(words = c("none", "stem", "lemma"), stopwords = c(TRUE, FALSE), trim = c(TRUE, FALSE))

res <- list()
for(i in 1:nrow(conditions)) {
    print(i)
    res[[i]] <- experiment4(conditions$words[i], conditions$stopwords[i], conditions$trim[i])
}

PCA <- conditions
PCA$res <- res
saveRDS(tibble::tibble(PCA), ipath("PCA.RDS"))


require(igraph)

conditions <- expand.grid(words = c("none", "stem", "lemma"), stopwords = c(TRUE, FALSE), trim = c(TRUE, FALSE), alpha = c(0.01, 0.05, 0.1, 0.2, 0.5, 1.0), k_factor = c(2, 3, 4))

experiment5 <- function(words, stopwords, trim, alpha, k_factor) {
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
    big_k <- 5 * k_factor
    tmod_lda <- textmodel_lda(x, k = big_k, alpha = alpha)
    theta <- tmod_lda$theta
    colnames(theta) <- seq_len(ncol(theta))
    mycosine <- lsa::cosine(as.matrix(theta))
    colnames(mycosine) <- colnames(theta)
    rownames(mycosine) <- colnames(theta)
    topmodnet <- graph.adjacency(mycosine, mode="undirected",
                                 weighted=TRUE, diag = FALSE, add.colnames="label")
    newg <- topmodnet
    mywalktrap <- cluster_walktrap(newg)
    reduced_trap <- cut_at(mywalktrap, 5)
    ## topic_lda <- apply(theta, 1, which.max)
    ## topic_antmn <- reduced_trap[topic_lda]
    theta_sum <- list()
    for (j in 1:5) {
        theta_sum[[j]] <- apply(theta[,which(reduced_trap == j), drop = FALSE], 1, sum)
    }
    antmn_theta <- matrix(c(theta_sum[[1]], theta_sum[[2]], theta_sum[[3]], theta_sum[[4]], theta_sum[[5]]), nrow = 100, byrow = FALSE)
    topic_antmn <- apply(antmn_theta, 1, which.max)
    possible_frames <- unique(frame_df$frame)
    perm15 <- permn(c(1,2,3,4,5))
    return(purrr::map_dbl(1:120, ~ sum(diag(table(frame_df$frame, possible_frames[match(topic_antmn, perm15[[.]])]))) / 100))
}

res <- list()
for(i in 1:nrow(conditions)) {
    print(i)
    res[[i]] <- experiment5(words = conditions$words[i], stopwords = conditions$stopwords[i], trim = conditions$trim[i], alpha = conditions$alpha[i], k_factor = conditions$k_factor[i])
}

ANTMN <- conditions
ANTMN$res <- res
saveRDS(tibble::tibble(ANTMN), ipath("ANTMN.RDS"))
