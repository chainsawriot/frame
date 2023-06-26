require(tidyverse)
require(ggridges)

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

all_uni$recommend <- FALSE

all_uni[all_uni$method == "antmn" & all_uni$words == "none" & all_uni$trim & all_uni$stopwords,]$recommend <- TRUE


all_uni %>% mutate(method = fct_relevel(method, "gold", "seededlda", "keyATM")) %>% ggplot(aes(x = maxp, y = method, fill = method_type)) + geom_density_ridges(alpha = 0.5, panel_scaling = TRUE, linetype = "blank") + geom_density_ridges(aes(point_color = recommend), jittered_points = TRUE, position = position_points_jitter(width = 0.05, height = 0), point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0, panel_scaling = TRUE, linetype = "blank") + geom_vline(aes(xintercept = 0.3, alpha = 0.5), linetype = "dashed") + geom_vline(aes(xintercept = 0.2, alpha = 0.5), linetype = "dashed") + xlim(c(0, 0.7)) + xlab(expression(CCR[max])) + ylab("Treatment") + theme_minimal() + scale_discrete_manual("point_color", values = c("#B0B0B0", "#FF0000"), guide = "none") + theme(legend.position = "none") 

all_uni %>% filter(method == "antmn") %>% ggplot(aes(x = maxp, y = method, fill = recommend)) + geom_density_ridges(alpha = 0.5, panel_scaling = TRUE, linetype = "blank") + geom_density_ridges(aes(point_color = recommend), jittered_points = TRUE, position = position_points_jitter(width = 0.05, height = 0), point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0, panel_scaling = TRUE, linetype = "blank") + geom_vline(aes(xintercept = 0.3, alpha = 0.5), linetype = "dashed") + geom_vline(aes(xintercept = 0.2, alpha = 0.5), linetype = "dashed") + xlim(c(0, 0.7)) + xlab(expression(CCR[max])) + ylab("Treatment") + theme_minimal() + scale_discrete_manual("point_color", values = c("#B0B0B0", "#FF0000"), guide = "none") + theme(legend.position = "none") + scale_fill_brewer(palette="Dark2") +  theme_minimal() + theme(legend.position = "none")



## geom_density_ridges(stat = "binline", jittered_points = TRUE,position = position_points_jitter(width = 0.05, height = 0), point_shape = 'X', point_size = 3, point_alpha = 1, alpha = 0.7)

set.seed(12121)
bind_rows(ireadRDS("expert_accuracy.RDS"), ireadRDS("human_accuracy.RDS")) %>% mutate(expert = c(rep(TRUE, 7), rep(FALSE, 5))) %>% arrange(maxp) %>% mutate(desc = reorder(desc, maxp)) %>% uncount(2000) %>% mutate(value = rbinom(n(), 100, maxp) / 100) %>% ggplot(aes(x = value, y = desc, fill = expert)) + geom_density_ridges(alpha = 0.5, linetype = "blank") + xlim(0, 1) + geom_vline(aes(xintercept = 0.2, alpha = 0.5), linetype = "dashed") + geom_vline(aes(xintercept = 0.3, alpha = 0.5), linetype = "dashed") + xlab(expression(CCR[max])) + ylab("Treatment") + scale_fill_brewer(palette="Dark2") +  theme_minimal() + theme(legend.position = "none")

set.seed(12121)
bind_rows(ireadRDS("expert_accuracy.RDS"), ireadRDS("human_accuracy.RDS")) %>% mutate(expert = c(rep(TRUE, 7), rep(FALSE, 5))) %>% arrange(maxp) %>% mutate(desc = reorder(desc, maxp)) %>% uncount(15000) %>% mutate(value = rbinom(n(), 100, maxp) / 100) %>% ggplot(aes(x = value, y = desc, fill = expert)) + geom_density_ridges(alpha = 0.5, linetype = "blank") + xlim(0, 1) + geom_vline(aes(xintercept = 0.2, alpha = 0.5), linetype = "dashed") + geom_vline(aes(xintercept = 0.3, alpha = 0.5), linetype = "dashed") + xlab(expression(CCR[max])) + ylab("Treatment") + scale_fill_brewer(palette="Dark2") +  theme_minimal() + theme(legend.position = "none")
