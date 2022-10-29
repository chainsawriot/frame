require(tidyverse)

bind_rows(mutate(readRDS("KM.RDS"), method = "km"), mutate(readRDS("PCA.RDS"), method = "pca"), mutate(readRDS("LDA.RDS"), method = "lda"), mutate(readRDS("STM.RDS"), method = "stm"), mutate(readRDS("ANTMN.RDS"), method = "antmn"), mutate(readRDS("SEEDED.RDS"), method = "seededlda"), mutate(readRDS("KEYATM.RDS"), method = "keyATM")) %>% mutate(maxp = map_dbl(res, max)) %>% mutate(method = factor(method)) %>% mutate(method = fct_relevel(method, "km")) -> all_uni

human <- readRDS("human_accuracy.RDS") %>% mutate(method = "gold")

all_uni %>% bind_rows(human) %>% mutate(method_type = case_when(method == "gold" ~ 0,
                           method %in% c("keyATM", "seededlda") ~ 1,
                           TRUE ~ 2)) %>% mutate(method_type = factor(method_type, levels = c(0,1,2), labels = c("Gold", "Semisupervised", "Automatic"))) -> all_uni

require(brms)
weaklyinformative_prior <- c(prior_string("normal(0, 1)", class = "b"), prior_string("normal(0, 1)", class = "Intercept"))

mod <- brm(maxp~method_type+ (1 | method), data = all_uni, control = list(adapt_delta = 0.9), core = 6, prior = weaklyinformative_prior)
saveRDS(mod, "brms_mod.RDS")

all_uni %>% filter(method_type != "Gold") %>% brm(maxp~method_type+ (1 | method), data = .,control = list(adapt_delta = 0.9), core = 6, prior = weaklyinformative_prior) -> mod2
saveRDS(mod2, "brms_mod2.RDS")
