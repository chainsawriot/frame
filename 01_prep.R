require(quanteda)
##require(quanteda.textmodels)
require(seededlda)
##require(spacyr) ## no longer working, use udpipe instead
## spacyr::spacy_install()
require(udpipe)
##dl <- udpipe_download_model(language = "english-gum", model_dir = here::here("data"))
require(combinat)
require(stm)
require(igraph)

suppressPackageStartupMessages(require(here))

udmodel_english <- udpipe_load_model(file = here::here("data", "english-gum-ud-2.5-191206.udpipe"))

ipath <- function(fname) {
    here::here("intermediate", fname)
}

set.seed(1212121)
rio::import(here::here("data", "Frame Corpus.xlsx")) %>% tibble::as_tibble() -> frame_df

frame_corpus <- corpus(x = frame_df$Content, docnames = frame_df$docid, docvars = data.frame(frame = frame_df$frame))

frame_corpus %>% tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_separators = TRUE, remove_symbols = TRUE, split_hyphens = TRUE) %>% tokens_tolower() -> normal_tokens
saveRDS(normal_tokens, ipath("normal_tokens.RDS"))

##as.tokens(spacy_parse(frame_corpus), use_lemma = TRUE) %>% tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_separators = TRUE, remove_symbols = TRUE, split_hyphens = TRUE) %>% tokens_tolower() -> lemma_tokens


lemmas <- udpipe(as.character(frame_corpus), udmodel_english)
lemmas$doc_id <- factor(lemmas$doc_id, levels = 1:100)

as.tokens(split(lemmas$lemma, lemmas$doc_id)) %>% tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_separators = TRUE, remove_symbols = TRUE, split_hyphens = TRUE) %>% tokens_tolower() -> lemma_tokens

saveRDS(lemma_tokens, ipath("lemma_tokens.RDS"))
saveRDS(frame_df, ipath("frame_df.RDS"))
