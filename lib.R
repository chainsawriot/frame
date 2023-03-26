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

experiment_lda <- function(words, stopwords, trim, alpha, normal_tokens, lemma_tokens, frame_corpus) {
    x <- .gen_dfm(words, stopwords, trim, normal_tokens, lemma_tokens)
    tmod_lda <- textmodel_lda(x, k = 5, alpha = alpha)
    topic_lda <- apply(tmod_lda$theta, 1, which.max)
    .match_topics(topic_lda, frame_corpus)
}

experiment_stm <- function(words, stopwords, trim, alpha, normal_tokens, lemma_tokens, frame_corpus) {
    x <- .gen_dfm(words, stopwords, trim, normal_tokens, lemma_tokens)
    x_stm <- convert(x, to = "stm")
    model.stm <- suppressMessages(stm(x_stm$documents, x_stm$vocab, K = 5, data = x_stm$meta, init.type = "Spectral", control = list(alpha = alpha)))
    topic_stm <- apply(model.stm$theta, 1, which.max)
    .match_topics(topic_stm, frame_corpus)    
}

experiment_km <- function(words, stopwords, trim, normal_tokens, lemma_tokens, frame_corpus) {
    x <- .gen_dfm(words, stopwords, trim, normal_tokens, lemma_tokens)
    km_model <- kmeans(as.matrix(dfm_tfidf(x)), 5)
    topic_km <- km_model$cluster
    .match_topics(topic_km, frame_corpus)
}

experiment_pca <- function(words, stopwords, trim, normal_tokens, lemma_tokens, frame_corpus) {
    x <- .gen_dfm(words, stopwords, trim, normal_tokens, lemma_tokens)
    pr_x <- prcomp(as.matrix(dfm_tfidf(x)))
    topic_pca <- apply(pr_x$x[,1:5], 1, which.max)
    .match_topics(topic_pca, frame_corpus)
}

experiment_antmn <- function(words, stopwords, trim, alpha, k_factor, normal_tokens, lemma_tokens, frame_corpus) {
    x <- .gen_dfm(words, stopwords, trim, normal_tokens, lemma_tokens)
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
    antmn_theta <- matrix(c(theta_sum[[1]], theta_sum[[2]], theta_sum[[3]], theta_sum[[4]], theta_sum[[5]]), nrow = ndoc(frame_corpus), byrow = FALSE)
    topic_antmn <- apply(antmn_theta, 1, which.max)
    .match_topics(topic_antmn, frame_corpus)
}

experiment_seeded <- function(words, stopwords, trim, alpha, expert, normal_tokens, lemma_tokens, frame_corpus, exp1, exp2, exp3) {
    x <- .gen_dfm(words, stopwords, trim, normal_tokens, lemma_tokens)
    tmod_lda <- textmodel_seededlda(x, dictionary = list(exp1, exp2, exp3)[[expert]], alpha = alpha, valuetype = "glob")
    topic_lda <- apply(tmod_lda$theta, 1, which.max)
    .match_topics(topic_lda, frame_corpus)
}

experiment_keyatm <- function(words, stopwords, trim, alpha, expert, normal_tokens, lemma_tokens, frame_corpus, exp1, exp2, exp3) {
    x <- .gen_dfm(words, stopwords, trim, normal_tokens, lemma_tokens)
    keyATMdoc <- keyATM_read(x)
    kw <- read_keywords(file = NULL, dictionary = list(exp1, exp2, exp3)[[expert]], keyATMdoc)
    tmod_ka <- keyATM(keyATMdoc, model = "base", keywords = kw, no_keyword_topics = 0)
    topic_ka <- apply(tmod_ka$theta, 1, which.max)
    .match_topics(topic_ka, frame_corpus)
}

ns <- c(300, 500, 1000, 2000)
