require(tidyverse)

readRDS("KM.RDS") %>% mutate(maxp = map_dbl(res, max)) %>% rowwise() %>% mutate(desc = paste0(words, ifelse(stopwords, ", sw", ""), ifelse(trim, ", s/d", "")), se = sqrt((maxp * (1 - maxp)) / 100), upper = maxp + (1.96 * se), lower = maxp - (1.96 * se), method = "K-Means") %>% arrange(maxp) %>% ungroup %>% select(method, desc, maxp, lower, upper) %>% ggplot(aes(x = maxp, y = reorder(desc, maxp), xmin = lower, xmax = upper)) + geom_pointrange() + xlim(0, 1) + geom_vline(aes(xintercept = 0.2, alpha = 0.8), linetype = "dashed") + geom_vline(aes(xintercept = 0.3, alpha = 0.8), linetype = "dashed") + labs(title = "K-Means (tfidf)") + xlab("CCR") + ylab("Treatment") + theme(legend.position = "none") -> km_gg

ggsave("km_gg.png", km_gg)

readRDS("PCA.RDS") %>% mutate(maxp = map_dbl(res, max)) %>% rowwise() %>% mutate(desc = paste0(words, ifelse(stopwords, ", sw", ""), ifelse(trim, ", s/d", "")), se = sqrt((maxp * (1 - maxp)) / 100), upper = maxp + (1.96 * se), lower = maxp - (1.96 * se), method = "PCA") %>% arrange(maxp) %>% ungroup %>% select(method, desc, maxp, lower, upper) %>% ggplot(aes(x = maxp, y = reorder(desc, maxp), xmin = lower, xmax = upper)) + geom_pointrange() + xlim(0, 1) + geom_vline(aes(xintercept = 0.2, alpha = 0.8), linetype = "dashed") + geom_vline(aes(xintercept = 0.3, alpha = 0.8), linetype = "dashed") + xlab("CCR") + ylab("Treatment")+ labs(title = "PCA (tfidf)") + theme(legend.position = "none") -> pca_gg

ggsave("pca_gg.png", pca_gg)

readRDS("LDA.RDS") %>% mutate(maxp = map_dbl(res, max)) %>% rowwise() %>% mutate(desc = paste0(words, ifelse(stopwords, ", sw", ""), ifelse(trim, ", s/d", ""), ", α: ", alpha), se = sqrt((maxp * (1 - maxp)) / 100), upper = maxp + (1.96 * se), lower = maxp - (1.96 * se), method = "LDA") %>% arrange(maxp) %>% ungroup %>% select(method, desc, maxp, lower, upper) %>% ggplot(aes(x = maxp, y = reorder(desc, maxp), xmin = lower, xmax = upper)) + geom_pointrange() + xlim(0, 1) + geom_vline(aes(xintercept = 0.2, alpha = 0.8), linetype = "dashed") + geom_vline(aes(xintercept = 0.3, alpha = 0.8), linetype = "dashed") + xlab("CCR") + ylab("Treatment")+ labs(title = "LDA") + theme(legend.position = "none") -> lda_gg

ggsave("lda_gg.png", lda_gg)

readRDS("STM.RDS") %>% mutate(maxp = map_dbl(res, max)) %>% rowwise() %>% mutate(desc = paste0(words, ifelse(stopwords, ", sw", ""), ifelse(trim, ", s/d", ""), ", α: ", alpha), se = sqrt((maxp * (1 - maxp)) / 100), upper = maxp + (1.96 * se), lower = maxp - (1.96 * se), method = "STM") %>% arrange(maxp) %>% ungroup %>% select(method, desc, maxp, lower, upper) %>% ggplot(aes(x = maxp, y = reorder(desc, maxp), xmin = lower, xmax = upper)) + geom_pointrange() + xlim(0, 1) + geom_vline(aes(xintercept = 0.2, alpha = 0.8), linetype = "dashed") + geom_vline(aes(xintercept = 0.3, alpha = 0.8), linetype = "dashed") + xlab("CCR") + ylab("Treatment")+ labs(title = "STM") + theme(legend.position = "none") -> stm_gg

ggsave("stm_gg.png", stm_gg)


readRDS("ANTMN.RDS") %>% mutate(maxp = map_dbl(res, max)) %>% rowwise() %>% mutate(desc = paste0(words, ifelse(stopwords, ", sw", ""), ifelse(trim, ", s/d", ""), ", α: ", alpha, ", kf: ", k_factor), se = sqrt((maxp * (1 - maxp)) / 100), upper = maxp + (1.96 * se), lower = maxp - (1.96 * se), method = "ANTMN") %>% arrange(maxp) %>% ungroup %>% select(method, desc, maxp, lower, upper) %>% ggplot(aes(x = maxp, y = reorder(desc, maxp), xmin = lower, xmax = upper)) + geom_pointrange() + xlim(0, 1) + geom_vline(aes(xintercept = 0.2, alpha = 0.8), linetype = "dashed")+ geom_vline(aes(xintercept = 0.3, alpha = 0.8), linetype = "dashed") + xlab("CCR") + ylab("Treatment")+ labs(title = "ANTMN") + theme(legend.position = "none") -> antmn_gg

ggsave("antmn_gg.png", antmn_gg, height = 21)

readRDS("SEEDED.RDS") %>% mutate(maxp = map_dbl(res, max)) %>% rowwise() %>% mutate(desc = paste0(words, ifelse(stopwords, ", sw", ""), ifelse(trim, ", s/d", ""), ", α: ", alpha, ", exp", expert), se = sqrt((maxp * (1 - maxp)) / 100), upper = maxp + (1.96 * se), lower = maxp - (1.96 * se), method = "LDA") %>% arrange(maxp) %>% ungroup %>% select(method, desc, maxp, lower, upper) %>% ggplot(aes(x = maxp, y = reorder(desc, maxp), xmin = lower, xmax = upper)) + geom_pointrange() + xlim(0, 1) + geom_vline(aes(xintercept = 0.2, alpha = 0.8), linetype = "dashed") + geom_vline(aes(xintercept = 0.3, alpha = 0.8), linetype = "dashed")+ xlab("CCR") + ylab("Treatment")+ labs(title = "Seeded-LDA") + theme(legend.position = "none") -> seeded_gg

ggsave("seeded_gg.png", seeded_gg, height = 21)

readRDS("KEYATM.RDS") %>% mutate(maxp = map_dbl(res, max)) %>% rowwise() %>% mutate(desc = paste0(words, ifelse(stopwords, ", sw", ""), ifelse(trim, ", s/d", ""), ", α: ", alpha, ", exp", expert), se = sqrt((maxp * (1 - maxp)) / 100), upper = maxp + (1.96 * se), lower = maxp - (1.96 * se), method = "LDA") %>% arrange(maxp) %>% ungroup %>% select(method, desc, maxp, lower, upper) %>% ggplot(aes(x = maxp, y = reorder(desc, maxp), xmin = lower, xmax = upper)) + geom_pointrange() + xlim(0, 1) + geom_vline(aes(xintercept = 0.2, alpha = 0.8), linetype = "dashed") + geom_vline(aes(xintercept = 0.3, alpha = 0.8), linetype = "dashed") + xlab("CCR") + ylab("Treatment")+ labs(title = "keyATM") + theme(legend.position = "none") -> keyatm_gg

ggsave("keyatm_gg.png", keyatm_gg, height = 21)

bind_rows(mutate(readRDS("KM.RDS"), method = "km"), mutate(readRDS("PCA.RDS"), method = "pca"), mutate(readRDS("LDA.RDS"), method = "lda"), mutate(readRDS("STM.RDS"), method = "stm"), mutate(readRDS("ANTMN.RDS"), method = "antmn"), mutate(readRDS("SEEDED.RDS"), method = "seededlda"), mutate(readRDS("KEYATM.RDS"), method = "keyATM")) %>% mutate(maxp = as.integer(map_dbl(res, max) * 100)) %>% mutate(method = factor(method)) %>% mutate(method = fct_relevel(method, "km")) -> all_uni

require(brms)
set.seed(123)
explore <- brm(maxp~words+stopwords+trim+method, data = all_uni, family = negbinomial())

all_uni %>% filter(method %in% c("keyATM", "seededlda")) %>% brm(maxp~words+stopwords+trim+method+as.factor(expert), data = ., family = negbinomial()) -> semi_methods
