source("lib.R")

prefix <- "PCA"

mutate(ireadRDS(paste0(prefix, ".RDS")), tau = purrr::map_dbl(ireadRDS(paste0(prefix, "_tau.RDS")), max), tau_gt  = purrr::map_dbl(ireadRDS(paste0(prefix, "_tau_gt.RDS")), max)) %>% rowwise() %>% mutate(desc = paste0(words, ifelse(stopwords, ", sw", ""), ifelse(trim, ", s/d", ""))) %>% select(desc, tau, tau_gt) %>% ungroup() %>% pivot_longer(!desc, names_to = "x", values_to = "tau") %>% ggplot(aes(y = fct_reorder2(desc, x, tau, .desc = FALSE), x = tau, color = x)) + geom_point() + xlim(0, 0.5) + theme_minimal() + xlab("r") + ylab("Treatment") + theme(legend.position = "none") 

prefix <- "STM"

mutate(ireadRDS(paste0(prefix, ".RDS")), tau = purrr::map_dbl(ireadRDS(paste0(prefix, "_tau.RDS")), max), tau_gt  = purrr::map_dbl(ireadRDS(paste0(prefix, "_tau_gt.RDS")), max)) %>% rowwise() %>% mutate(desc = paste0(words, ifelse(stopwords, ", sw", ""), ifelse(trim, ", s/d", ""), ", Alpha: ", alpha)) %>% select(desc, tau, tau_gt) %>% pivot_longer(!desc, names_to = "x", values_to = "tau") %>% ggplot(aes(y = fct_reorder2(desc, x, tau, .desc = FALSE), x = tau, color = x)) + geom_point() + xlim(0, 0.5) + theme_minimal() + xlab("r") + ylab("Treatment") + theme(legend.position = "none")

prefix <- "LDA"

mutate(ireadRDS(paste0(prefix, ".RDS")), tau = purrr::map_dbl(ireadRDS(paste0(prefix, "_tau.RDS")), max), tau_gt  = purrr::map_dbl(ireadRDS(paste0(prefix, "_tau_gt.RDS")), max)) %>% rowwise() %>% mutate(desc = paste0(words, ifelse(stopwords, ", sw", ""), ifelse(trim, ", s/d", ""), ", Alpha: ", alpha)) %>% select(desc, tau, tau_gt) %>% pivot_longer(!desc, names_to = "x", values_to = "tau") %>% ggplot(aes(y = fct_reorder2(desc, x, tau, .desc = FALSE), x = tau, color = x)) + geom_point() + xlim(0, 0.5) + theme_minimal() + xlab("r") + ylab("Treatment") + theme(legend.position = "none") 

prefix <- "ANTMN"

mutate(ireadRDS(paste0(prefix, ".RDS")), tau = purrr::map_dbl(ireadRDS(paste0(prefix, "_tau.RDS")), max), tau_gt  = purrr::map_dbl(ireadRDS(paste0(prefix, "_tau_gt.RDS")), max)) %>% rowwise() %>% mutate(desc = paste0(words, ifelse(stopwords, ", sw", ""), ifelse(trim, ", s/d", ""), ", Alpha: ", alpha, ", kf: ", k_factor)) %>% select(desc, tau, tau_gt) %>% pivot_longer(!desc, names_to = "x", values_to = "tau") %>% ggplot(aes(y = fct_reorder2(desc, x, tau, .desc = FALSE), x = tau, color = x)) + geom_point() + xlim(0, 0.5) + theme_minimal() + xlab("r") + ylab("Treatment")+ theme(legend.position = "none") 

prefix <- "SEEDED"

mutate(ireadRDS(paste0(prefix, ".RDS")), tau = purrr::map_dbl(ireadRDS(paste0(prefix, "_tau.RDS")), max), tau_gt  = purrr::map_dbl(ireadRDS(paste0(prefix, "_tau_gt.RDS")), max)) %>% rowwise() %>% mutate(desc = paste0(words, ifelse(stopwords, ", sw", ""), ifelse(trim, ", s/d", ""), ", alpha: ", alpha, ", exp", expert)) %>% select(desc, tau, tau_gt) %>% pivot_longer(!desc, names_to = "x", values_to = "tau") %>% ggplot(aes(y = fct_reorder2(desc, x, tau, .desc = FALSE), x = tau, color = x)) + geom_point() + xlim(0, 0.5) + theme_minimal() + xlab("r") + ylab("Treatment")+ theme(legend.position = "none") 

prefix <- "KEYATM"

mutate(ireadRDS(paste0(prefix, ".RDS")), tau = purrr::map_dbl(ireadRDS(paste0(prefix, "_tau.RDS")), max), tau_gt  = purrr::map_dbl(ireadRDS(paste0(prefix, "_tau_gt.RDS")), max)) %>% rowwise() %>% mutate(desc = paste0(words, ifelse(stopwords, ", sw", ""), ifelse(trim, ", s/d", ""), ", alpha: ", alpha, ", exp", expert)) %>% select(desc, tau, tau_gt) %>% pivot_longer(!desc, names_to = "x", values_to = "tau") %>% ggplot(aes(y = fct_reorder2(desc, x, tau, .desc = FALSE), x = tau, color = x)) + geom_point() + xlim(0, 0.5) + theme_minimal() + xlab("r") + ylab("Treatment")+ theme(legend.position = "none") 
