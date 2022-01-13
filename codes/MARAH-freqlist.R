library(corplingr)
leipzig_path <- collogetr::leipzig_corpus_path

# freqlist of ANGER terms
MARAH <- corplingr::freqlist_leipzig_each("(?<!-)\\b(ke|a)?marah(an)?(ku|mu|nya)?\\b(?!-+)", leipzig_path)
MARAH_sum <- corplingr::freqlist_leipzig_summarise(MARAH, match)
MARAH_sum
save(MARAH, MARAH_sum, file = "data/leipzig-MARAH-wlist.RData")


# freqlist of ANGER terms (NON-MARAH)
NON_MARAH <- corplingr::freqlist_leipzig_each("(?<!-)\\b(ke)?(geram|kesal|murka|berang|gusar)(an)?(ku|mu|nya)?\\b(?!-+)", leipzig_path)
NON_MARAH_sum <- corplingr::freqlist_leipzig_summarise(NON_MARAH, match)
NON_MARAH_sum
save(NON_MARAH, NON_MARAH_sum, file = "data/leipzig-NON-MARAH-wlist.RData")


# Load the prepared data

load(file = "data/leipzig-NON-MARAH-wlist.RData")
load(file = "data/leipzig-MARAH-wlist.RData")
library(tidyverse)

# remove the PERSONAL PRONOUN suffixes

MARAH_sum_lemma <- MARAH_sum %>% 
  mutate(lemma = str_replace_all(match, "(nya|ku|mu)$", "")) %>% 
  group_by(lemma) %>% 
  summarise(n = sum(n), .groups = "drop") %>% 
  arrange() %>% 
  filter(lemma %in% c("amarah", "kemarahan", "marah"))

NON_MARAH_sum_lemma <- NON_MARAH_sum %>% 
  mutate(lemma = str_replace_all(match, "(nya|ku|mu)$", "")) %>% 
  group_by(lemma) %>% 
  summarise(n = sum(n), .groups = "drop") %>% 
  arrange() %>% 
  filter(lemma %in% c("geram", "berang", "gusar", "kegusaran", "keberangan", "kegeraman", "kekesalan", "kemurkaan", "keberangan", "kesal", "murka"))

# compare frequency of the terms
ALL_MARAH_sum_lemma <- bind_rows(MARAH_sum_lemma, NON_MARAH_sum_lemma)
ke_an <- ALL_MARAH_sum_lemma %>% 
  filter(str_detect(lemma, "^ke.+an$")) %>% 
  filter(str_detect(lemma, "kesal", negate = TRUE)) %>% 
  arrange(desc(n)) %>% 
  mutate(perc = round(n/sum(n) * 100, 2))
ke_an_count <- ke_an$n
names(ke_an_count) <- ke_an$lemma
ke_an_chisq <- chisq.test(ke_an_count)
ke_an_chisq_residuals <- chisq.test(ke_an_count)$residuals

stems <- ALL_MARAH_sum_lemma %>% 
  filter(str_detect(lemma, "^ke.+an$", negate = TRUE)) %>% 
  filter(str_detect(lemma, "kesal", negate = TRUE)) %>% 
  mutate(lemma = if_else(str_detect(lemma, "marah"), "(a)marah", lemma)) %>%
  group_by(lemma) %>% 
  summarise(n = sum(n), .groups = "drop") %>% 
  arrange(desc(n)) %>% 
  mutate(perc = round(n/sum(n) * 100, 2))
stems_count <- stems$n
names(stems_count) <- stems$lemma
stems_chisq <- chisq.test(stems_count)
stems_chisq_residuals <- chisq.test(stems_count)$residuals