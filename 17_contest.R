require(tidyverse)
require(ggridges)

rio::import(here::here("data", "frame_contestation_coded.xlsx")) %>% tibble::as_tibble() -> contest

colnames(contest) <- c("docid", "topic", "writer", "editor", "Content", "frame", "expert1", "expert2", "expert1a", "expert2a", "expert1b", "expert2b")

##contest %>% mutate(a_avg = (expert1a + expert2a) / 2) %>% ggplot(aes(x = a_avg, y = frame)) + geom_density_ridges(alpha = 0.5, panel_scaling = TRUE, linetype = "blank")

contest %>% select(frame, expert1a, expert2a) %>% pivot_longer(!frame, names_to = "expert", values_to = "score")  %>% mutate(expert = recode(expert, expert1a = "Expert A", expert2a = "Expert B")) %>% ggplot(aes(x = score, y = frame)) + geom_density_ridges(alpha = 0.5, panel_scaling = FALSE, linetype = "blank") + facet_wrap(vars(expert)) + xlab("Score (Do you agree with the ground truth frame of this article?)") + theme_minimal() 


