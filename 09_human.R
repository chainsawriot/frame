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

hjhuman <- rio::import(here::here("data", "coding HJ.xlsx")) %>% select(-docid, -Content)

##zohuman <- rio::import(here::here("data", "coding ZO.xlsx")) %>%  select(-docid, -Content)
## temp fix
## zohuman[29,] <- hjhuman[29,]
## zohuman[99,1] <- hjhuman[99,1]
## zohuman[79, 15] <- as.character(hjhuman[79, 15])
## zohuman[,15] <- as.numeric(zohuman[,15])

zohuman <- rio::import(here::here("data", "coding ZO (corrected).xlsx")) %>%  select(-docid, -Content)

colnames(hjhuman) <- str_extract(colnames(hjhuman), "^[A-Z][0-9]")
colnames(zohuman) <- str_extract(colnames(zohuman), "^[A-Z][0-9]")

dist_mat <- dist(hjhuman)

hierar_cl <- hclust(dist_mat, method = "average")
fit <- cutree(hierar_cl, k = 5)

hjhuman %>% mutate(avga = (A1+A2+A3+A4+A5)/5, avgb = (B1+B2+B3+B4)/4, avgc = (C1+ C2+ C3+C4)/4, avgd = (D1+ D2+ D3)/3, avge = (E1+ E2+ E3) / 3) %>% select(starts_with("avg")) %>% rowwise() %>% mutate(maxx = which.max(c(avga, avgb, avgc, avgd, avge))) %>% pull(maxx) -> hj

table(hj, frame_df$frame)

possible_frames <- unique(frame_df$frame)

perm15 <- combinat::permn(c(1,2,3,4,5))
purrr::map_dbl(1:120, ~ sum(diag(table(frame_df$frame, possible_frames[match(hj, perm15[[.]])]))) / 100) %>% max -> onecoder

## shoehorn
topic_vector <- as.numeric(as.factor(frame_df$topic))
purrr::map_dbl(1:120, ~ sum(diag(table(frame_df$frame, possible_frames[match(topic_vector, perm15[[.]])]))) / 100) %>% max


hjhuman %>% mutate_all(~ . != 0) %>% mutate(avga = (A1+A2+A3+A4+A5)/5, avgb = (B1+B2+B3+B4)/4, avgc = (C1+ C2+ C3+C4)/4, avgd = (D1+ D2+ D3)/3, avge = (E1+ E2+ E3) / 3) %>% select(starts_with("avg")) %>% rowwise() %>% mutate(maxx = which.max(c(avga, avgb, avgc, avgd, avge))) %>% pull(maxx) -> hjb

perm15 <- combinat::permn(c(1,2,3,4,5))
purrr::map_dbl(1:120, ~ sum(diag(table(frame_df$frame, possible_frames[match(hjb, perm15[[.]])]))) / 100) %>% max -> onecoderb

possible_frames

table(c("Responsibility", "Human Interest", "Conflict", "Morality", "Economic Consequences")[hj], frame_df$frame)

##str(varimax(loadings(factanal(~., 5, data = human)), normalize = FALSE))


varm <- psych::principal(hjhuman, nfactors = 5, scores = TRUE)

hj_varm <- apply(varm$scores, 1, which.max)

purrr::map_dbl(1:120, ~ sum(diag(table(frame_df$frame, possible_frames[match(hj_varm, perm15[[.]])]))) / 100) %>% max -> onecoder_varimax

hjhuman %>% mutate(avga = (A1+A2+A3+A4+A5), avgb = (B1+B2+B3+B4), avgc = (C1+ C2+ C3+C4), avgd = (D1+ D2+ D3), avge = (E1+ E2+ E3)) %>% select(starts_with("avg")) -> thj

zohuman %>% mutate(avga = (A1+A2+A3+A4+A5), avgb = (B1+B2+B3+B4), avgc = (C1+ C2+ C3+C4), avgd = (D1+ D2+ D3), avge = (E1+ E2+ E3)) %>% select(starts_with("avg")) %>% rename(zavga = `avga`, zavgb = `avgb`, zavgc = `avgc`, zavgd = `avgd`, zavge = `avge`) -> zoj

bind_cols(thj, zoj) %>% mutate(scorea = (avga + zavga) / 10, scoreb = (avgb + zavgb) / 8, scorec = (avgc + zavgc) / 8, scored = (avgd + zavgd) / 6, scoree = (avge + zavge) / 6) %>% select(starts_with("score")) %>% rowwise() %>% mutate(maxx = which.max(c(scorea, scoreb, scorec, scored, scoree))) %>% pull(maxx) -> bothhuman

table(bothhuman, frame_df$frame)

possible_frames <- unique(frame_df$frame)

perm15 <- combinat::permn(c(1,2,3,4,5))
purrr::map_dbl(1:120, ~ sum(diag(table(frame_df$frame, possible_frames[match(bothhuman, perm15[[.]])]))) / 100) %>% max -> twocoders

bind_cols(thj, zoj) %>% mutate(scorea = (avga + zavga) / 10, scoreb = (avgb + zavgb) / 8, scorec = (avgc + zavgc) / 8, scored = (avgd + zavgd) / 6, scoree = (avge + zavge) / 6) %>% select(starts_with("score"))

    varm <- psych::principal(bind_cols(hjhuman, zohuman), nfactors = 5, scores = TRUE)
b_varm <- apply(varm$scores, 1, which.max)


purrr::map_dbl(1:120, ~ sum(diag(table(frame_df$frame, possible_frames[match(b_varm, perm15[[.]])]))) / 100) %>% max -> twocoder_varimax

as_tibble(varm$scores) %>% mutate(frame = frame_df$frame) %>% group_by(frame) %>% summarise_if(is.numeric, mean)

tibble(desc = c("1 coder, avg", "1 coder, avg, binary", "1 coder, varimax", "two coders, avg", "two coders, varimax"), maxp = c(onecoder, onecoderb, onecoder_varimax, twocoders, twocoder_varimax)) -> human_accuracy

saveRDS(human_accuracy, ipath("human_accuracy.RDS"))

## human_accuracy %>% mutate(se = sqrt((maxp * (1 - maxp)) / 100), upper = maxp + (1.96 * se), lower = maxp - (1.96 * se)) %>% arrange(maxp) %>% ggplot(aes(x = maxp, y = reorder(desc, maxp), xmin = lower, xmax = upper)) + geom_pointrange() + xlim(0, 1) + geom_vline(aes(xintercept = 0.2, alpha = 0.5), linetype = "dashed") + geom_vline(aes(xintercept = 0.3, alpha = 0.5), linetype = "dashed") + xlab(expression(CCR[max])) + ylab("Treatment")+ labs(title = "Gold Standard") + theme_minimal() + theme(legend.position = "none") -> human_gg

## ggsave(fpath("human_gg.png"), human_gg)
