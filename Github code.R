#######Github Twitter code#######
#Load libraries#
library(dplyr)
library(stringr)
library(quanteda)
library(stm)

#####Loading German Twitter data#####
#Do string search for relevant key words to minimize the size of the dataset
migration_DE <- migration_Twitter_noRT %>%
  dplyr::filter(Language == 'de') %>%
  dplyr::filter(str_detect(Text, 'EU|European Union|Frontex|frontex|eu|Europäische Union|europäische union|
                           europäische kommission|Europäische Kommmission|Europäische Parlament|
                           Europäische Rat|europäische rat'))

######Do preprocessing using Quanteda#####
#Create corpus
corpus_migration_DE <- corpus(migration_DE, text_field = "Text")

#Tokenize, remove German stopwords and ensure that umlauts are maintained
toks_migration_DE <- tokens(corpus_migration_DE, remove_punct = TRUE, remove_numbers = TRUE) %>% 
  tokens_remove(pattern = stopwords(language = 'de')) %>% 
  tokens_keep(pattern = "^[\\p{script=Latn}]+$", valuetype = "regex")

#Create bigrams and Quanteda DFM
tokens_bigram_migration_DE <- tokens_ngrams(toks_migration_DE, 2)
dfmat_migration_DE <- dfm(tokens_bigram_migration_DE)

#Convert from Quanteda DFM to STM format
stm_migration_de <- convert(dfmat_migration_expert_DE, to = "stm")

#######Run STM model########

#Run search for optimal number of K
searchK_migration_DE <- searchK(stm_migration_de$documents, stm_migration_de$vocab,
                                K = c(10,20,30,40,50,60,70,80,90,100), data = stm_migration_DE$meta)

plot(searchK_migration_DE)

#Run STM
stm_migration_DE_model <- stm(stm_migration_de$documents, stm_migration_de$vocab, 
                        K = 40, data = stm_migration_DE$meta)

plot(stm_migration_DE_model)



