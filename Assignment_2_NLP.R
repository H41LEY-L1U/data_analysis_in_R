library(syuzhet)
abc2 <- get_text_as_string(file.choose())
abc2

abc2_sent <- get_sentences(abc2, fix_curly_quotes = TRUE, as_vector = TRUE)
abc2_sent

library(spacyr)
spacy_initialize(model = "en_core_web_sm")

doc1 <- abc2_sent
doc1

parsed_doc <- spacy_parse(doc1, tag = TRUE)
parsed_doc

#1 Histogram of document or sentence lengths
doc_table <- table(list(parsed_doc$doc_id))
doc_table

hist(doc_table)
hist(doc_table, breaks = length(doc_table),xlab='Sent_length', main="Histogram of Sentence Length")

doc_sent_table <- table(list(parsed_doc$doc_id, parsed_doc$sentence_id))
doc_sent_table

doc_sent_df2 <- data.frame(doc_sent_table)
nrow(doc_sent_df2)

doc_sent_df2_nozeroes <- subset(doc_sent_df2, Freq > 1)
nrow(doc_sent_df2_nozeroes)

hist(doc_sent_df2_nozeroes$Freq, breaks = 500)
hist(doc_sent_df2_nozeroes$Freq, breaks = nrow(doc_sent_df2_nozeroes))

#2 Table of POS
pos_table <- table(parsed_doc$pos)
pos_table

nounCnt <- tapply(parsed_doc$pos == "NOUN", parsed_doc$doc_id, sum)
nounCnt
verbCnt <- tapply(parsed_doc$pos == "VERB", parsed_doc$doc_id, sum)
adjCnt <- tapply(parsed_doc$pos == "ADJ", parsed_doc$doc_id, sum)
pronCnt <- tapply(parsed_doc$pos == "PRON", parsed_doc$doc_id, sum)

#3 Plot sentence distribution of POS tags
plot(nounCnt+pronCnt, adjCnt, pch=19, cex=2, col=rgb(0,0,1,0.02))
plot(nounCnt+pronCnt, verbCnt, pch=19, cex=2, col=rgb(0,0,1,0.02))

#4 Top 25 noun lemmas
nrow(parsed_doc)

parsed_doc_nopunct <- subset(parsed_doc, pos != "PUNCT")
nrow(parsed_doc_nopunct)

lemmas_table <- table(parsed_doc_nopunct$lemma)
lemmas_table
lemmas_table <- sort(lemmas_table, decreasing = TRUE)
head(lemmas_table, 50)

library(stopwords)

lemmas_table_nostops <- table(parsed_doc_nopunct$lemma[!parsed_doc_nopunct$lemma %in% stopwords::stopwords("en", source = "stopwords-iso")])
lemmas_table_nostops <- sort(lemmas_table_nostops, decreasing = TRUE)
new_lemmas_table <- lemmas_table_nostops[-c(1,3,4,5,10,17,21,29,30)]
head(new_lemmas_table,25)

#5 Locations in text

ner_entities <- entity_extract(parsed_doc)
ner_entities

ner_entities_all <- entity_extract(parsed_doc, type = "all")
ner_entities_all

ner_entities_all[ner_entities_all$entity_type == "LOC",]
GPE <- ner_entities_all[ner_entities_all$entity_type == "GPE",]
unique(GPE$entity)
ppl <- ner_entities_all[ner_entities_all$entity_type == "PERSON",]
unique(ppl$entity)


library("quanteda")
library("quanteda.textplots")

stopWords <- stopwords("en")
docS <- doc1
rm_words <- function(string, words) {
  stopifnot(is.character(string), is.character(words))
  spltted <- strsplit(string, " ", fixed = TRUE) # fixed = TRUE for speedup
  vapply(spltted, function(x) paste(x[!tolower(x) %in% words], collapse = " "), character(1))
}
docS <- rm_words(docS, tm::stopwords("en"))

dfm_lm <- corpus_subset(corpus(docS)) %>% 
  dfm(remove = stopwords('english'), remove_punct = TRUE) %>%
  dfm_trim(min_termfreq = 10, verbose = FALSE)

set.seed(120)
textplot_wordcloud(dfm_lm)

summary(corpus(docS))

dfm(corpus(docS) %>% tokens())
