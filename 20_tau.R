set.seed(1212121)
source("lib.R")
require(tidyverse)
frame_df <- rio::import(here::here("data", "Frame Corpus.xlsx")) %>% tibble::as_tibble()

frame_corpus <- corpus(x = frame_df$Content, docnames = frame_df$docid, docvars = data.frame(frame = frame_df$frame))

expert1 <- rio::import(here::here("data", "coding PM.xlsx")) %>% select(-2, -1)
expert2 <- rio::import(here::here("data", "coding RF.xlsx")) %>% select(-docid, -Content)

colnames(expert1) <- str_extract(colnames(expert1), "^[A-Z][0-9]")
colnames(expert2) <- str_extract(colnames(expert2), "^[A-Z][0-9]")

expert1 %>% mutate(avga = (A1+A2+A3+A4+A5), avgb = (B1+B2+B3+B4), avgc = (C1+ C2+ C3+C4), avgd = (D1+ D2+ D3), avge = (E1+ E2+ E3)) %>% select(starts_with("avg")) -> expert1j

expert2 %>% mutate(avga = (A1+A2+A3+A4+A5), avgb = (B1+B2+B3+B4), avgc = (C1+ C2+ C3+C4), avgd = (D1+ D2+ D3), avge = (E1+ E2+ E3)) %>% select(starts_with("avg")) %>% rename(zavga = `avga`, zavgb = `avgb`, zavgc = `avgc`, zavgd = `avgd`, zavge = `avge`) -> expert2j

bind_cols(expert1j, expert2j) %>% mutate(scorea = (avga + zavga) / 10, scoreb = (avgb + zavgb) / 8, scorec = (avgc + zavgc) / 8, scored = (avgd + zavgd) / 6, scoree = (avge + zavge) / 6) %>% select(starts_with("score")) -> bothexpert

expert_vector <- c(as.matrix(bothexpert))

gt <- as.numeric(factor(docvars(frame_corpus, "frame"), levels = c("Responsibility",  "Human Interest", "Conflict", "Morality", "Economic Consequences")))

gt_matrix <- matrix(rep(0, 500), ncol = 5)

for (i in seq_along(gt)) {
    gt_matrix[i, gt[i]] <- 1
}

gt_vector <- c(gt_matrix)

purrr::walk(c("PCA", "LDA", "STM", "ANTMN", "SEEDED", "KEYATM"), .gen_tau, reference_y = expert_vector, .progress = TRUE) 

purrr::walk(c("PCA", "LDA", "STM", "ANTMN", "SEEDED", "KEYATM"), .gen_tau, reference_y = gt_vector, ending = "_tau_gt.RDS", .progress = TRUE) 
