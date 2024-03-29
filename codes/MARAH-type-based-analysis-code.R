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

source("codes/MARAH-type-based-mapping-edits.R")

# type-based METAPHOR analysis ===========
## count the number of types per metaphor
metaphor_typebased_n_type <- metaphor_typebased %>% 
  count(CM_BROADER, sort = TRUE, name = "n_type") %>% 
  mutate(n_perc_type = round(n_type/sum(n_type) * 100, digits = 1)) %>% 
  rename(metaphor = CM_BROADER)
# metaphor_typebased_n_type

## count the number of mappings per metaphor
metaphor_typebased_mapping <- metaphor_typebased %>% 
  select(CM_BROADER, starts_with("MAP"), matches("^(LU|PATTERN|SFRAME|CM_ROLEMAPPING|CITATIONS|ENGLISH_TRANSLATION|LU_GLOSS)$"))  %>%
  pivot_longer(cols = starts_with("MAP"), names_to = "MAPPING_ID", values_to = "MAPPING") %>% 
  filter(!is.na(MAPPING)) %>% 
  mutate(status = "")


metaphor_typebased_n_mapping <- metaphor_typebased %>% 
  select(CM_BROADER, starts_with("MAP")) %>% 
  pivot_longer(-CM_BROADER, names_to = "MAPPING_ID", values_to = "MAPPING") %>% 
  filter(!is.na(MAPPING)) %>% 
  group_by(CM_BROADER) %>% 
  summarise(n_mapping = n_distinct(MAPPING), .groups = "drop") %>% 
  arrange(desc(n_mapping)) %>% 
  mutate(n_perc_mapping = round(n_mapping/sum(n_mapping) * 100, digits = 1)) %>% 
  rename(metaphor = CM_BROADER)
# metaphor_typebased_n_mapping

## join the type and mapping tables
metaphor_typebased_salience <- metaphor_typebased_n_type %>% 
  left_join(metaphor_typebased_n_mapping, by = "metaphor") %>% 
  mutate(aggregate = n_perc_type + n_perc_mapping) %>% 
  arrange(desc(aggregate))

# for printing in the MS Word
metaphor_typebased_salience_print <- metaphor_typebased_salience %>% 
  mutate(metaphor = str_replace(metaphor, "^anger is (a\\s)?", ""),
         metaphor = str_replace(metaphor, "^\\(cause of\\) anger is ", ""),
         metaphor = str_replace(metaphor, "^(heated fluid|pressurised substance) in a container$", "\\1"),
         metaphor = str_replace(metaphor, "^(intensity) of anger( is .+)$", "\\1\\2"),
         metaphor = str_replace(metaphor, "^(degree of control) for anger( is .+)$", "\\1\\2"),
         metaphor = str_replace(metaphor, "(?<=intensity is )object (quantity)$", "\\1"),
         metaphor = str_replace(metaphor, "^substance in a container$", "contained substance"),
         metaphor = paste("[", metaphor, "]{.smallcaps}", sep = "")) %>% 
  rename(`Metaphorical source domains` = metaphor,
         `Types:` = n_type,
         `% of all types` = n_perc_type,
         `No. of metaphorical mappings` = n_mapping,
         `% of metaphorical mappings` = n_perc_mapping,
         Aggregate = aggregate)
metaphor_typebased_salience_total <- metaphor_typebased_salience_print %>% 
  summarise(across(where(is.numeric), ~sum(.))) %>% 
  mutate(across(where(is.numeric), ~round(.)),
         `Metaphorical source domains` = "**TOTAL**",
         Aggregate = replace(Aggregate, `Metaphorical source domains` == "**TOTAL**", NA)) %>% 
  select(`Metaphorical source domains`, everything())
metaphor_typebased_salience_print <- metaphor_typebased_salience_print %>% 
  bind_rows(metaphor_typebased_salience_total)


# type-based METONYMY analysis ===========
metonymy_typebased_salience <- metonymy_typebased %>% 
  count(CM_BROADER, sort = TRUE, name = "n_type") %>% 
  mutate(n_perc_type = round(n_type/sum(n_type) * 100, digits = 1)) %>% 
  rename(metonymy = CM_BROADER)
# metonymy_typebased_salience