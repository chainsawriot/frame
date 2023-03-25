require(quanteda)
#require(spacyr)
require(tidyverse)
set.seed(1212121)
rio::import(here::here("data", "Frame Corpus.xlsx")) %>% tibble::as_tibble() -> frame_df

ipath <- function(fname) {
    here::here("intermediate", fname)
}

fpath <- function(fname) {
    here::here("figure", fname)
}

frame_corpus <- corpus(x = frame_df$Content, docnames = frame_df$docid, docvars = data.frame(frame = frame_df$frame))

expert1 <- rio::import(here::here("data", "coding PM.xlsx")) %>% select(-2, -1)
expert2 <- rio::import(here::here("data", "coding RF.xlsx")) %>% select(-docid, -Content)

colnames(expert1) <- str_extract(colnames(expert1), "^[A-Z][0-9]")
colnames(expert2) <- str_extract(colnames(expert2), "^[A-Z][0-9]")

table(conf = expert2$F2, frame = expert2$F1)

expert1_frame <- c("Responsibility", "Human Interest", "Conflict", "Morality", "Consequences")[1 + expert1$F1]
expert2_frame <- c("Responsibility", "Human Interest", "Conflict", "Morality", "Consequences")[1 + expert2$F1]


e1_correct <- expert1$F2[frame_df$frame == expert1_frame]

t.test(expert1$F2[e1_correct], expert1$F2[!e1_correct])

e2_correct <- expert2$F2[frame_df$frame == expert2_frame]
t.test(expert2$F2[e2_correct], expert2$F2[!e2_correct])



mean(expert1$F2[frame_df$frame == expert1_frame])
mean(expert1$F2[frame_df$frame != expert1_frame])


mean(expert1$F2)
mean(expert2$F2)

confdata <- data.frame(correct = c(frame_df$frame == expert1_frame, frame_df$frame == expert2_frame), exp = as.factor(c(rep(0, 100), rep(1, 100))), conf = c(expert1$F2, expert2$F2))

require(brms)
weaklyinformative_prior <- c(prior_string("normal(0, 1)", class = "b"), prior_string("normal(0, 1)", class = "Intercept"))

set.seed(12333)
mod <- brm(correct~conf + exp + (1 | exp), data = confdata, control = list(adapt_delta = 0.97), core = 6, prior = weaklyinformative_prior, bernoulli(link = "logit"))
summary(mod)
saveRDS(mod, here::here("intermediate/conf_mod.RDS"))

## plot(conditional_effects(mod, prob = 0.89, plot = FALSE), plot = FALSE) [[1]] +  xlab("Confidence") + ylab("Correction rate") + theme_minimal()
