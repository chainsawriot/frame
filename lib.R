library(quanteda)
require(seededlda)
require(combinat)
suppressPackageStartupMessages(require(stm))
require(igraph)
suppressPackageStartupMessages(require(keyATM))
suppressPackageStartupMessages(require(here))

ipath <- function(fname) {
    here::here("intermediate", fname)
}

spath <- function(fname) {
    here::here("intermediate/sim", fname)
}

.match_topics <- function(topics, frame_corpus) {
    possible_frames <- unique(docvars(frame_corpus)$frame)
    perm15 <- permn(seq_len(length(possible_frames)))
    return(purrr::map_dbl(seq_len(length(perm15)),
                          ~ sum(diag(table(docvars(frame_corpus)$frame,
                                           possible_frames[match(topics, perm15[[.]])]))) / ndoc(frame_corpus)))
}

.gen_dfm <- function(words, stopwords, trim, normal_tokens, lemma_tokens) {
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
    return(x)
}

.get_hat_y <- function(p, p_is_hat_y) {
    if (isFALSE(p_is_hat_y)) {
        return(apply(p, 1, which.max))
    }
    p
}

.return_match <- function(match = TRUE, p, frame_corpus, p_is_hat_y = FALSE) {
    if (isTRUE(match)) {
        hat_y <- .get_hat_y(p = p, p_is_hat_y = p_is_hat_y)
        return(.match_topics(hat_y, frame_corpus))
    }
    return(p)
}

experiment_lda <- function(words, stopwords, trim, alpha, normal_tokens, lemma_tokens, frame_corpus, k = 5, match = TRUE) {
    x <- .gen_dfm(words, stopwords, trim, normal_tokens, lemma_tokens)
    p <- textmodel_lda(x, k = k, alpha = alpha)$theta
    .return_match(match, p, frame_corpus)
}

experiment_stm <- function(words, stopwords, trim, alpha, normal_tokens, lemma_tokens, frame_corpus, k = 5, match = TRUE) {
    x <- .gen_dfm(words, stopwords, trim, normal_tokens, lemma_tokens)
    x_stm <- convert(x, to = "stm")
    p <- suppressMessages(stm(x_stm$documents, x_stm$vocab, K = k, data = x_stm$meta, init.type = "Spectral", control = list(alpha = alpha)))$theta
    .return_match(match, p, frame_corpus)
}

experiment_km <- function(words, stopwords, trim, normal_tokens, lemma_tokens, frame_corpus, k = 5, match = TRUE) {
    x <- .gen_dfm(words, stopwords, trim, normal_tokens, lemma_tokens)
    km_model <- kmeans(as.matrix(dfm_tfidf(x)), k)
    hat_y <- km_model$cluster
    .return_match(match, hat_y, frame_corpus, p_is_hat_y = TRUE)
}

experiment_pca <- function(words, stopwords, trim, normal_tokens, lemma_tokens, frame_corpus, k = 5, match = TRUE) {
    x <- .gen_dfm(words, stopwords, trim, normal_tokens, lemma_tokens)
    p <- prcomp(as.matrix(dfm_tfidf(x)))$x[,seq_len(k)]
    .return_match(match, p, frame_corpus)
}

experiment_antmn <- function(words, stopwords, trim, alpha, k_factor, normal_tokens, lemma_tokens, frame_corpus, k = 5, match = TRUE) {
    x <- .gen_dfm(words, stopwords, trim, normal_tokens, lemma_tokens)
    big_k <- k * k_factor
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
    reduced_trap <- cut_at(mywalktrap, k)
    ## topic_lda <- apply(theta, 1, which.max)
    ## topic_antmn <- reduced_trap[topic_lda]
    theta_sum <- list()
    for (j in seq_len(k)) {
        theta_sum[[j]] <- apply(theta[,which(reduced_trap == j), drop = FALSE], 1, sum)
    }
    p <- matrix(unlist(theta_sum), nrow = ndoc(frame_corpus), byrow = FALSE)
    .return_match(match, p, frame_corpus)
}

## k in these cases is controlled by the dictionary

experiment_seeded <- function(words, stopwords, trim, alpha, expert, normal_tokens, lemma_tokens, frame_corpus, exp1, exp2, exp3, match = TRUE) {
    x <- .gen_dfm(words, stopwords, trim, normal_tokens, lemma_tokens)
    p <- textmodel_seededlda(x, dictionary = list(exp1, exp2, exp3)[[expert]], alpha = alpha, valuetype = "glob")$theta
    .return_match(match, p, frame_corpus)
}

experiment_keyatm <- function(words, stopwords, trim, alpha, expert, normal_tokens, lemma_tokens, frame_corpus, exp1, exp2, exp3, match = TRUE) {
    x <- .gen_dfm(words, stopwords, trim, normal_tokens, lemma_tokens)
    keyATMdoc <- keyATM_read(x)
    kw <- read_keywords(file = NULL, dictionary = list(exp1, exp2, exp3)[[expert]], keyATMdoc)
    p <- keyATM(keyATMdoc, model = "base", keywords = kw, no_keyword_topics = 0)$theta
    .return_match(match, p, frame_corpus)
}

generic_sim <- function(prefix, experiment_fun, conditions = NULL, .progress = TRUE, seed = 1212121, p_is_hat_y = FALSE) {
    if (is.null(conditions)) {
        conditions <- expand.grid(words = c("none", "stem", "lemma"), stopwords = c(TRUE, FALSE), trim = c(TRUE, FALSE))
    }
    frame_df <- rio::import(here::here("data", "Frame Corpus.xlsx")) %>% tibble::as_tibble()
    frame_corpus <- corpus(x = frame_df$Content, docnames = frame_df$docid, docvars = data.frame(frame = frame_df$frame))
    normal_tokens <- readRDS(ipath("normal_tokens.RDS"))
    lemma_tokens <- readRDS(ipath("lemma_tokens.RDS"))
    set.seed(seed)
    p <- purrr::pmap(conditions, experiment_fun, .progress = TRUE, match = FALSE,
                         normal_tokens = normal_tokens, lemma_tokens = lemma_tokens,
                         frame_corpus = frame_corpus)
    saveRDS(p, ipath(paste0(prefix, "_p.RDS")))
    hat_y <- purrr::map(p, .get_hat_y, p_is_hat_y = p_is_hat_y)
    res <- purrr::map(hat_y, .match_topics, frame_corpus = frame_corpus)
    output <- conditions
    output$res <- res
    saveRDS(tibble::tibble(output), ipath(paste0(prefix, ".RDS")))
}

ns <- c(500, 1000, 2000)
