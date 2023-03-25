require(quanteda)
require(seededlda)
require(combinat)
require(igraph)

suppressPackageStartupMessages(require(here))
ipath <- function(fname) {
    here::here("intermediate", fname)
}

set.seed(1212121)
rio::import(here::here("data", "Frame Corpus.xlsx")) %>% tibble::as_tibble() -> frame_df

frame_corpus <- corpus(x = frame_df$Content, docnames = frame_df$docid, docvars = data.frame(frame = frame_df$frame))

normal_tokens <- readRDS(ipath("normal_tokens.RDS"))
lemma_tokens <- readRDS(ipath("lemma_tokens.RDS"))

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
