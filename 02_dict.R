require(quanteda)
require(seededlda)
require(combinat)
require(quanteda)
set.seed(1212121)

resp <- c("cause","blame","guilt*","solution","necessary","insufficient","adequate","inadequate","useful","improv*","exacerbat*","pragmati*","*effective","*efficien*","avert","mistake","fail*","solve","accus*")

human <- c("civilian","affect*","pain*","suffer*","happy","family","love","mother","child","people","hurt","private","friend*","personal","casualt*")

conflict <- c("war","struggl*","fight*","conflict*","crime","assault*","battle*","violent","clash*","winner","loser","attack*","disagreement","challenge","defeat","victory")

moral <- c("moral*","immoral*","justice","injustice","fair*","unfair*","humane","inhumane","exploit*","oppress*","*tolerance","freedom","liberty","equality","equitable","compassion*","heartless","loyal","disloyal","rights","sacred","sin")

econ <- c("success","succeed","yield","gain","benefit","loss*","inflation","economy","*employment","growth","collapse","financial","cost","depreciation","devaluation","bubble","currency ","exchange")

exp1 <- dictionary(list(resp = resp, human = human, conflict = conflict, moral = moral, econ = econ))

resp2 <- c("responsib*", "blam*", "solution*")
human2 <- c("personal*", "affect*")
conflict2 <- c("conflict*", "winner*", "loser*", "disagree*", "crisis*", "crises*")
moral2 <- c("god*", "moral*", "values*")
econ2 <- c("loss*", "damage*", "consequence*", "gain*", "cost*", "percent*")

exp2 <- dictionary(list(resp = resp2, human = human2, conflict = conflict2, moral = moral2, econ = econ2))

exp3 <- dictionary(list(resp = union(resp, resp2), human = union(human, human2), conflict = union(conflict, conflict2), moral = union(moral, moral2), econ = union(econ, econ2)))

normal_tokens <- readRDS("normal_tokens.RDS")

lemma_tokens <- readRDS("lemma_tokens.RDS")
frame_df <- readRDS("frame_df.RDS")

conditions <- expand.grid(words = c("none", "stem", "lemma"), stopwords = c(TRUE, FALSE), trim = c(TRUE, FALSE), alpha = c(0.01, 0.05, 0.1, 0.2, 0.5, 1.0), expert = c(1, 2, 3))

experiment_s1 <- function(words, stopwords, trim, alpha, expert) {
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
    tmod_lda <- textmodel_seededlda(x, dictionary = list(exp1, exp2, exp3)[[expert]], alpha = alpha, valuetype = "glob")
    topic_lda <- apply(tmod_lda$theta, 1, which.max)
    possible_frames <- unique(frame_df$frame)
    perm15 <- permn(c(1,2,3,4,5))
    return(purrr::map_dbl(seq_len(length(permn(c(1,2,3,4,5)))), ~ sum(diag(table(frame_df$frame, possible_frames[match(topic_lda, perm15[[.]])]))) / 100))
}

res <- list()
for(i in seq_len(nrow(conditions))) {
    print(i)
    res[[i]] <- experiment_s1(conditions$words[i], conditions$stopwords[i], conditions$trim[i], conditions$alpha[i], conditions$expert[i])
}

SEEDED <- conditions
SEEDED$res <- res
saveRDS(tibble::tibble(SEEDED), "SEEDED.RDS")

require(keyATM)

experiment_s2 <- function(words, stopwords, trim, alpha, expert) {
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
    keyATMdoc <- keyATM_read(x)
    kw <- read_keywords(file = NULL, dictionary = list(exp1, exp2, exp3)[[expert]], keyATMdoc)
    tmod_ka <- keyATM(keyATMdoc, model = "base", keywords = kw, no_keyword_topics = 0)
    topic_ka <- apply(tmod_ka$theta, 1, which.max)
    possible_frames <- unique(frame_df$frame)
    perm15 <- permn(c(1,2,3,4,5))
    return(purrr::map_dbl(seq_len(length(permn(c(1,2,3,4,5)))), ~ sum(diag(table(frame_df$frame, possible_frames[match(topic_ka, perm15[[.]])]))) / 100))
}

res <- list()
for(i in seq_len(nrow(conditions))) {
    print(i)
    res[[i]] <- experiment_s2(conditions$words[i], conditions$stopwords[i], conditions$trim[i], conditions$alpha[i], conditions$expert[i])
}

KEYATM <- conditions
KEYATM$res <- res
saveRDS(tibble::tibble(KEYATM), "KEYATM.RDS")
