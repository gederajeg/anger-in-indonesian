library(tidyverse)

# read the type-based, lexical-approach dataset
marah_typebased <- read_tsv("data/lexical-approach-main.txt")

# filter out irrelevant and duplicated observation
marah_typebased <- marah_typebased %>% 
  filter(RELEVANCE %in% c("metaphor", "metonymy"))

# split the metaphor and metonymy observation
metaphor_typebased <- marah_typebased %>% 
  filter(RELEVANCE == "metaphor")

metonymy_typebased <- marah_typebased %>% 
  filter(RELEVANCE == "metonymy")

# type-based METAPHOR analysis ===========
## count the number of types per metaphor
metaphor_typebased_n_type <- metaphor_typebased %>% 
  count(CM_BROADER, sort = TRUE, name = "n_type") %>% 
  mutate(n_perc_type = round(n_type/sum(n_type) * 100, digits = 2)) %>% 
  rename(metaphor = CM_BROADER)
# metaphor_typebased_n_type

## count the number of mappings per metaphor
metaphor_typebased_n_mapping <- metaphor_typebased %>% 
  select(CM_BROADER, starts_with("MAP")) %>% 
  pivot_longer(-CM_BROADER, names_to = "MAPPING_ID", values_to = "MAPPING") %>% 
  filter(!is.na(MAPPING)) %>% 
  group_by(CM_BROADER) %>% 
  summarise(n_mapping = n_distinct(MAPPING), .groups = "drop") %>% 
  arrange(desc(n_mapping)) %>% 
  mutate(n_perc_mapping = round(n_mapping/sum(n_mapping) * 100, digits = 2)) %>% 
  rename(metaphor = CM_BROADER)
# metaphor_typebased_n_mapping

## join the type and mapping tables
metaphor_typebased_salience <- metaphor_typebased_n_type %>% 
  left_join(metaphor_typebased_n_mapping, by = "metaphor") %>% 
  mutate(aggregate = n_perc_type + n_perc_mapping) %>% 
  arrange(desc(aggregate))

# type-based METONYMY analysis ===========
metonymy_typebased_salience <- metonymy_typebased %>% 
  count(CM_BROADER, sort = TRUE, name = "n_type") %>% 
  mutate(n_perc_type = round(n_type/sum(n_type) * 100, digits = 2)) %>% 
  rename(metonymy = CM_BROADER)
# metonymy_typebased_salience