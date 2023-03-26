require(quanteda)
require(udpipe)
require(tokenizers)
source("lib.R")
suppressPackageStartupMessages(require(here))

ipath <- function(fname) {
    here::here("intermediate", fname)
}

rio::import(here::here("data", "Frame Corpus.xlsx")) %>% tibble::as_tibble() -> frame_df

set.seed(1212121)

.scramble <- function(content) {
    tokenized_content <- tokenize_sentences(content)[[1]]
    paste0(sample(tokenized_content, length(tokenized_content), replace = TRUE), collapse = "\n")
}

.gen_boot <- function(frame_df, n = 1000) {
    docid <- sample(frame_df$docid, n, replace = TRUE)
    frame <- frame_df$frame[docid]
    original_content <- frame_df$Content[docid]
    scrambled_content <- vapply(original_content, .scramble, character(1), USE.NAMES = FALSE)
    frame_corpus <- corpus(x = scrambled_content, docnames = seq(1, n), docvars = data.frame(frame = frame, original_docid = docid))
    udmodel_english <- udpipe_load_model(file = here::here("data", "english-gum-ud-2.5-191206.udpipe"))
    lemmas <- udpipe(as.character(frame_corpus), udmodel_english)
    lemmas$doc_id <- factor(lemmas$doc_id, levels = seq(1, n))
    as.tokens(split(lemmas$lemma, lemmas$doc_id)) %>% tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_separators = TRUE, remove_symbols = TRUE, split_hyphens = TRUE) %>% tokens_tolower() -> lemma_tokens
    saveRDS(lemma_tokens, ipath(paste0("lemma_tokens_sim", n, ".RDS")))
    saveRDS(frame_corpus, ipath(paste0("frame_corpus_sim", n, ".RDS")))
}

for (x in ns) {
    .gen_boot(frame_df, n = x)
}
