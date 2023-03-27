source("lib.R")
require(brms)
require(tidyverse)
sreadRDS <- function(x, n) {
    readRDS(here::here("intermediate/sim", paste0(x, "_sim", n, ".RDS")))
}

for (n in ns) {
    bind_rows(mutate(sreadRDS("KM", n), method = "km"),
              mutate(sreadRDS("PCA", n), method = "pca"),
              mutate(sreadRDS("LDA", n), method = "lda"),
              mutate(sreadRDS("STM", n), method = "stm"),
              mutate(sreadRDS("ANTMN", n), method = "antmn"),
              mutate(sreadRDS("SEEDED", n), method = "seededlda"),
              mutate(sreadRDS("KEYATM", n), method = "keyATM")) %>%
        mutate(maxp = map_dbl(res, max)) %>% mutate(method = factor(method)) %>% mutate(method = fct_relevel(method, "km")) -> all_uni
    human <- readRDS(here::here("intermediate", "human_accuracy.RDS")) %>% mutate(method = "gold")

    all_uni %>% bind_rows(human) %>% mutate(method_type = case_when(method == "gold" ~ 0,
                                                                    method %in% c("keyATM", "seededlda") ~ 1,
                                                                    TRUE ~ 2)) %>% mutate(method_type = factor(method_type, levels = c(0,1,2), labels = c("Gold", "Semisupervised", "Automatic"))) -> all_uni
    weaklyinformative_prior <- c(prior_string("normal(0, 1)", class = "b"), prior_string("normal(0, 1)", class = "Intercept"))
    set.seed(12111111)
    mod <- brm(maxp~method_type+ (1 | method), data = all_uni, control = list(adapt_delta = 0.99, max_treedepth = 15), core = 6, prior = weaklyinformative_prior)
    saveRDS(mod, here::here("intermediate/sim", paste0("brms_mod_sim", n, ".RDS")))
}
