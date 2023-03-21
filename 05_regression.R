require(tidyverse)

ireadRDS <- function(fname) {
    readRDS(here::here("intermediate", fname))
}

bind_rows(mutate(ireadRDS("KM.RDS"), method = "km"),
          mutate(ireadRDS("PCA.RDS"), method = "pca"),
          mutate(ireadRDS("LDA.RDS"), method = "lda"),
          mutate(ireadRDS("STM.RDS"), method = "stm"),
          mutate(ireadRDS("ANTMN.RDS"), method = "antmn"),
          mutate(ireadRDS("SEEDED.RDS"), method = "seededlda"),
          mutate(ireadRDS("KEYATM.RDS"), method = "keyATM")) %>%
    mutate(maxp = map_dbl(res, max)) %>% mutate(method = factor(method)) %>% mutate(method = fct_relevel(method, "km")) -> all_uni

human <- ireadRDS("human_accuracy.RDS") %>% mutate(method = "gold")

all_uni %>% bind_rows(human) %>% mutate(method_type = case_when(method == "gold" ~ 0,
                           method %in% c("keyATM", "seededlda") ~ 1,
                           TRUE ~ 2)) %>% mutate(method_type = factor(method_type, levels = c(0,1,2), labels = c("Gold", "Semisupervised", "Automatic"))) -> all_uni

require(brms)
weaklyinformative_prior <- c(prior_string("normal(0, 1)", class = "b"), prior_string("normal(0, 1)", class = "Intercept"))

set.seed(12)
mod <- brm(maxp~method_type+ (1 | method), data = all_uni, control = list(adapt_delta = 0.9), core = 6, prior = weaklyinformative_prior)
saveRDS(mod, here::here("intermediate", "brms_mod.RDS"))

## set.seed(1211)

## all_uni %>% filter(method_type != "Gold") %>% brm(maxp~method_type+ (1 | method), data = .,control = list(adapt_delta = 0.9), core = 6, prior = weaklyinformative_prior) -> mod2
## saveRDS(mod2, here::here("intermediate", "brms_mod2.RDS"))
