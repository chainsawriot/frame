set.seed(1212121)
source("lib.R")
require(tidyverse)
require(ggridges)
frame_df <- rio::import(here::here("data", "Frame Corpus.xlsx")) %>% tibble::as_tibble()
frame_corpus <- corpus(x = frame_df$Content, docnames = frame_df$docid, docvars = data.frame(frame = frame_df$frame))

.gen_no_m_res <- function(prefix, p_is_hat_y = FALSE) {
    p <- readRDS(ipath(paste0(prefix, "_p.RDS")))
    hat_y <- purrr::map(p, .get_hat_y, p_is_hat_y = p_is_hat_y)
    purrr::map(hat_y, .match_topics_no_moral, frame_corpus = frame_corpus)
}

prefix <- "KM"
mutate(readRDS(ipath(paste0(prefix, ".RDS"))), no_m_res = .gen_no_m_res(prefix, p_is_hat_y = TRUE)) %>% mutate(maxp = purrr::map_dbl(res, max), maxp2 = purrr::map_dbl(no_m_res, max)) %>% rowwise() %>% mutate(desc = paste0(words, ifelse(stopwords, ", sw", ""), ifelse(trim, ", s/d", ""))) %>% select(desc, maxp, maxp2) %>% pivot_longer(!desc, names_to = "x", values_to = "ccr") %>%
    ggplot(aes(y = reorder(desc, ccr), x = ccr, color = x)) +
    geom_point() + xlim(0, 1) + geom_vline(aes(xintercept = 0.2, alpha = 0.5), linetype = "dashed") + geom_vline(aes(xintercept = 0.3, alpha = 0.5), linetype = "dashed") + xlab(expression(CCR[max])) + ylab("Treatment") + scale_color_brewer(palette="Dark2") +  theme_minimal() + theme(legend.position = "none")

prefix <- "PCA"
mutate(readRDS(ipath(paste0(prefix, ".RDS"))), no_m_res = .gen_no_m_res(prefix, p_is_hat_y = FALSE)) %>% mutate(maxp = purrr::map_dbl(res, max), maxp2 = purrr::map_dbl(no_m_res, max)) %>% rowwise() %>% mutate(desc = paste0(words, ifelse(stopwords, ", sw", ""), ifelse(trim, ", s/d", ""))) %>% select(desc, maxp, maxp2) %>% pivot_longer(!desc, names_to = "x", values_to = "ccr") %>% ggplot(aes(y = reorder(desc, ccr), x = ccr, color = x)) + geom_point() + xlim(0, 1) + geom_vline(aes(xintercept = 0.2, alpha = 0.5), linetype = "dashed") + geom_vline(aes(xintercept = 0.3, alpha = 0.5), linetype = "dashed") + xlab(expression(CCR[max])) + ylab("Treatment") + scale_color_brewer(palette="Dark2") +  theme_minimal() + theme(legend.position = "none")


prefix <- "LDA"
mutate(readRDS(ipath(paste0(prefix, ".RDS"))), no_m_res = .gen_no_m_res(prefix))  %>% mutate(maxp = purrr::map_dbl(res, max), maxp2 = purrr::map_dbl(no_m_res, max)) %>% rowwise() %>% mutate(desc = paste0(words, ifelse(stopwords, ", sw", ""), ifelse(trim, ", s/d", ""), ", Alpha: ", alpha)) %>% select(desc, maxp, maxp2) %>% pivot_longer(!desc, names_to = "x", values_to = "ccr") %>% ggplot(aes(y = reorder(desc, ccr), x = ccr, color = x)) + geom_point() + xlim(0, 1) + geom_vline(aes(xintercept = 0.2, alpha = 0.5), linetype = "dashed") + geom_vline(aes(xintercept = 0.3, alpha = 0.5), linetype = "dashed") + xlab(expression(CCR[max])) + ylab("Treatment") + scale_color_brewer(palette="Dark2") +  theme_minimal() + theme(legend.position = "none")

prefix <- "STM"
mutate(readRDS(ipath(paste0(prefix, ".RDS"))), no_m_res = .gen_no_m_res(prefix))  %>% mutate(maxp = purrr::map_dbl(res, max), maxp2 = purrr::map_dbl(no_m_res, max)) %>% rowwise() %>% mutate(desc = paste0(words, ifelse(stopwords, ", sw", ""), ifelse(trim, ", s/d", ""), ", Alpha: ", alpha)) %>% select(desc, maxp, maxp2) %>% pivot_longer(!desc, names_to = "x", values_to = "ccr") %>% ggplot(aes(y = reorder(desc, ccr), x = ccr, color = x)) + geom_point() + xlim(0, 1) + geom_vline(aes(xintercept = 0.2, alpha = 0.5), linetype = "dashed") + geom_vline(aes(xintercept = 0.3, alpha = 0.5), linetype = "dashed") + xlab(expression(CCR[max])) + ylab("Treatment") + scale_color_brewer(palette="Dark2") +  theme_minimal() + theme(legend.position = "none")

prefix <- "ANTMN"
mutate(readRDS(ipath(paste0(prefix, ".RDS"))), no_m_res = .gen_no_m_res(prefix, p_is_hat_y = FALSE)) %>% mutate(maxp = purrr::map_dbl(res, max), maxp2 = purrr::map_dbl(no_m_res, max)) %>% rowwise() %>% mutate(desc = paste0(words, ifelse(stopwords, ", sw", ""), ifelse(trim, ", s/d", ""), ", Alpha: ", alpha, ", kf: ", k_factor)) %>% select(desc, maxp, maxp2) %>% pivot_longer(!desc, names_to = "x", values_to = "ccr") %>% ggplot(aes(y = reorder(desc, ccr), x = ccr, color = x)) + geom_point() + xlim(0, 1) + geom_vline(aes(xintercept = 0.2, alpha = 0.5), linetype = "dashed") + geom_vline(aes(xintercept = 0.3, alpha = 0.5), linetype = "dashed") + xlab(expression(CCR[max])) + ylab("Treatment") + scale_color_brewer(palette="Dark2") +  theme_minimal() + theme(legend.position = "none")

prefix <- "KEYATM"
mutate(readRDS(ipath(paste0(prefix, ".RDS"))), no_m_res = .gen_no_m_res(prefix, p_is_hat_y = FALSE)) %>% mutate(maxp = purrr::map_dbl(res, max), maxp2 = purrr::map_dbl(no_m_res, max)) %>% rowwise() %>% mutate(desc = paste0(words, ifelse(stopwords, ", sw", ""), ifelse(trim, ", s/d", ""), ", alpha: ", alpha, ", exp", expert)) %>% select(desc, maxp, maxp2) %>% pivot_longer(!desc, names_to = "x", values_to = "ccr") %>% ggplot(aes(y = reorder(desc, ccr), x = ccr, color = x)) + geom_point() + xlim(0, 1) + geom_vline(aes(xintercept = 0.2, alpha = 0.5), linetype = "dashed") + geom_vline(aes(xintercept = 0.3, alpha = 0.5), linetype = "dashed") + xlab(expression(CCR[max])) + ylab("Treatment") + scale_color_brewer(palette="Dark2") +  theme_minimal() + theme(legend.position = "none")

prefix <- "SEEDED"
mutate(readRDS(ipath(paste0(prefix, ".RDS"))), no_m_res = .gen_no_m_res(prefix, p_is_hat_y = FALSE)) %>% mutate(maxp = purrr::map_dbl(res, max), maxp2 = purrr::map_dbl(no_m_res, max)) %>% rowwise() %>% mutate(desc = paste0(words, ifelse(stopwords, ", sw", ""), ifelse(trim, ", s/d", ""), ", alpha: ", alpha, ", exp", expert)) %>% select(desc, maxp, maxp2) %>% pivot_longer(!desc, names_to = "x", values_to = "ccr") %>% ggplot(aes(y = reorder(desc, ccr), x = ccr, color = x)) + geom_point() + xlim(0, 1) + geom_vline(aes(xintercept = 0.2, alpha = 0.5), linetype = "dashed") + geom_vline(aes(xintercept = 0.3, alpha = 0.5), linetype = "dashed") + xlab(expression(CCR[max])) + ylab("Treatment") + scale_color_brewer(palette="Dark2") +  theme_minimal() + theme(legend.position = "none")


## mutate(desc = reorder(desc, ccr)) %>%  uncount(3000) %>% mutate(value = rbinom(n(), 100, ccr) / 100) %>% ggplot(aes(x = value, y = desc, fill = x)) + geom_density_ridges(alpha = 0.5, linetype = "blank") + xlim(0, 1) + geom_vline(aes(xintercept = 0.2, alpha = 0.5), linetype = "dashed") + geom_vline(aes(xintercept = 0.3, alpha = 0.5), linetype = "dashed") + xlab(expression(CCR[max])) + ylab("Treatment") + scale_color_brewer(palette="Dark2") +  theme_minimal() + theme(legend.position = "none")

bind_rows(ireadRDS("expert_accuracy_k4.RDS"), ireadRDS("human_accuracy_k4.RDS")) %>% mutate(expert = c(rep(TRUE, 7), rep(FALSE, 5))) %>% pull(maxp) -> k4_p

bind_rows(ireadRDS("expert_accuracy.RDS"), ireadRDS("human_accuracy.RDS")) %>% mutate(expert = c(rep(TRUE, 7), rep(FALSE, 5))) %>% mutate(maxp2 = k4_p) %>% select(desc, maxp, maxp2) %>% pivot_longer(!desc, names_to = "x", values_to = "ccr") %>% mutate(desc = reorder(desc, ccr)) %>% ggplot(aes(y = reorder(desc, ccr), x = ccr, color = x)) + geom_point() + xlim(0, 1) + geom_vline(aes(xintercept = 0.2, alpha = 0.5), linetype = "dashed") + geom_vline(aes(xintercept = 0.3, alpha = 0.5), linetype = "dashed") + geom_vline(aes(xintercept = 0.25, alpha = 0.5), linetype = "dashed", col = "red") + geom_vline(aes(xintercept = 0.29, alpha = 0.5), linetype = "dashed", col = "red") + xlab(expression(CCR[max])) + ylab("Treatment") + scale_color_brewer(palette="Dark2") +  theme_minimal() + theme(legend.position = "none")
