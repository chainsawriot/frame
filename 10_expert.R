source("lib.R")

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

expert1_frame <- c("Responsibility", "Human Interest", "Conflict", "Morality", "Economic Consequences")[1 + expert1$F1]
expert2_frame <- c("Responsibility", "Human Interest", "Conflict", "Morality", "Economic Consequences")[1 + expert2$F1]

table(c(frame_df$frame, frame_df$frame), c(expert1_frame == frame_df$frame, expert2_frame == frame_df$frame)) %>% as.data.frame() %>% group_by(Var1) %>% mutate(t = sum(Freq), p = Freq / t) %>% ungroup %>% filter(Var2 == "TRUE") %>% select(Var1, p) %>% arrange(p) %>% rename("Frame (Ground Truth)" = `Var1`, "Overall" = `p`) -> overallp

table(frame_df$frame, expert1_frame == frame_df$frame)%>% as.data.frame() %>% group_by(Var1) %>% mutate(t = sum(Freq), p = Freq / t) %>% ungroup %>% filter(Var2 == "TRUE") %>% select(Var1, p) %>% arrange(p) %>% rename("Frame (Ground Truth)" = `Var1`, "Expert A" = `p`) -> expert1p

table(frame_df$frame, expert2_frame == frame_df$frame)%>% as.data.frame() %>% group_by(Var1) %>% mutate(t = sum(Freq), p = Freq / t) %>% ungroup %>% filter(Var2 == "TRUE") %>% select(Var1, p) %>% arrange(p) %>% rename("Frame (Ground Truth)" = `Var1`, "Expert B" = `p`) -> expert2p

suppressMessages(overallp %>% left_join(expert1p) %>% left_join(expert2p)) -> overalltable

knitr::kable(overalltable, digits = 2)

expert1 %>% mutate(avga = (A1+A2+A3+A4+A5)/5, avgb = (B1+B2+B3+B4)/4, avgc = (C1+ C2+ C3+C4)/4, avgd = (D1+ D2+ D3)/3, avge = (E1+ E2+ E3) / 3) %>% select(starts_with("avg")) %>% rowwise() %>% mutate(maxx = which.max(c(avga, avgb, avgc, avgd, avge))) %>% pull(maxx) -> expert1_avg

oneexpert <- max(.match_topics(expert1_avg, frame_corpus))

expert1 %>% mutate_all(~ . != 0) %>% mutate(avga = (A1+A2+A3+A4+A5)/5, avgb = (B1+B2+B3+B4)/4, avgc = (C1+ C2+ C3+C4)/4, avgd = (D1+ D2+ D3)/3, avge = (E1+ E2+ E3) / 3) %>% select(starts_with("avg")) %>% rowwise() %>% mutate(maxx = which.max(c(avga, avgb, avgc, avgd, avge))) %>% pull(maxx) -> expert1_b

oneexpertb <- max(.match_topics(expert1_b, frame_corpus))

varm <- psych::principal(select(expert1, -F1, -F2), nfactors = 5, scores = TRUE)
expert1_varm <- apply(varm$scores, 1, which.max)

oneexpert_varimax <- max(.match_topics(expert1_varm, frame_corpus))

expert1 %>% mutate(avga = (A1+A2+A3+A4+A5), avgb = (B1+B2+B3+B4), avgc = (C1+ C2+ C3+C4), avgd = (D1+ D2+ D3), avge = (E1+ E2+ E3)) %>% select(starts_with("avg")) -> expert1j

expert2 %>% mutate(avga = (A1+A2+A3+A4+A5), avgb = (B1+B2+B3+B4), avgc = (C1+ C2+ C3+C4), avgd = (D1+ D2+ D3), avge = (E1+ E2+ E3)) %>% select(starts_with("avg")) %>% rename(zavga = `avga`, zavgb = `avgb`, zavgc = `avgc`, zavgd = `avgd`, zavge = `avge`) -> expert2j

bind_cols(expert1j, expert2j) %>% mutate(scorea = (avga + zavga) / 10, scoreb = (avgb + zavgb) / 8, scorec = (avgc + zavgc) / 8, scored = (avgd + zavgd) / 6, scoree = (avge + zavge) / 6) %>% select(starts_with("score")) %>% rowwise() %>% mutate(maxx = which.max(c(scorea, scoreb, scorec, scored, scoree))) %>% pull(maxx) -> bothexpert

twoexperts <- max(.match_topics(bothexpert, frame_corpus))

varm <- psych::principal(bind_cols(select(expert1, -F1, -F2), select(expert2, -F1, -F2)), nfactors = 5, scores = TRUE)
b_varm <- apply(varm$scores, 1, which.max)

twoexperts_varimax <- max(.match_topics(b_varm, frame_corpus))

expert1_excl <- max(.match_topics(expert1$F1+1, frame_corpus))
expert2_excl <- max(.match_topics(expert2$F1+1, frame_corpus))

tibble(desc = c("1 expert, avg", "1 expert, avg, binary", "1 expert, varimax", "Expert A, Exclusionary", "Expert B, Exclusionary",  "2 experts, avg", "2 experts, varimax"), maxp = c(oneexpert, oneexpertb, oneexpert_varimax, expert1_excl, expert2_excl, twoexperts, twoexperts_varimax)) -> expert_accuracy

saveRDS(expert_accuracy, ipath("expert_accuracy.RDS"))

## "Boasting"
## No improvement to add four together

## hjhuman <- rio::import(here::here("data", "coding HJ.xlsx")) %>% select(-docid, -Content)

## zohuman <- rio::import(here::here("data", "coding ZO (corrected).xlsx")) %>%  select(-docid, -Content)

## colnames(hjhuman) <- str_extract(colnames(hjhuman), "^[A-Z][0-9]")
## colnames(zohuman) <- str_extract(colnames(zohuman), "^[A-Z][0-9]")

## varm4 <- psych::principal(bind_cols(select(expert1, -F1, -F2), select(expert2, -F1, -F2), hjhuman, zohuman), nfactors = 5, scores = TRUE)
## varm4 <- apply(varm4$scores, 1, which.max)

## perm15 <- combinat::permn(c(1,2,3,4,5))
## purrr::map_dbl(1:120, ~ sum(diag(table(frame_df$frame, possible_frames[match(varm4, perm15[[.]])]))) / 100) %>% max

