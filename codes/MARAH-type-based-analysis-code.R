library(tidyverse)

get_mappings <- function(mapping_df, metaphor_regex) {
  interim_df <- distinct(select(arrange(filter(mapping_df, str_detect(CM_BROADER, metaphor_regex)), MAPPING_ID), CM_BROADER, MAPPING))
  interim_df <- mutate(interim_df, MAPPING2 = str_replace_all(MAPPING, "-", " "), MAPPING2 = str_replace_all(MAPPING2, "_", " <- "), MAPPING2 = str_replace_all(MAPPING2, " is ", " <- "))
  return(interim_df)
}
get_salience_stats <- function(salience_df, col_names, metaphor_regex) {
  salience_df_colnames <- colnames(salience_df)
  interim_df <- filter(salience_df, str_detect(metaphor, metaphor_regex))
  return(interim_df[[salience_df_colnames[salience_df_colnames == col_names]]])
}

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
  mutate(metaphor = str_replace(metaphor, "^anger is ", ""),
         metaphor = str_replace(metaphor, "^\\(cause of\\) anger is ", ""),
         # metaphor = str_c('<span style="font-variant:small-caps;">', metaphor, '</span>', sep = ""),
         metaphor = paste("[", metaphor, "]{.smallcaps}", sep = "")) %>% 
  rename(`Metaphorical source domains` = metaphor,
         `No. of types of linguistic expression` = n_type,
         `% of all types of conceptual metaphor` = n_perc_type,
         `No. of metaphorical mappings` = n_mapping,
         `% of all types of metaphorical mappings` = n_perc_mapping,
         Aggregate = aggregate)
metaphor_typebased_salience_total <- metaphor_typebased_salience_print %>% 
  summarise(across(where(is.numeric), ~sum(.))) %>% 
  mutate(`Metaphorical source domains` = "**TOTAL**") %>% 
  select(`Metaphorical source domains`, everything()) %>% 
  mutate(Aggregate = replace(Aggregate, `Metaphorical source domains` == "**TOTAL**", NA))
metaphor_salience_print <- metaphor_typebased_salience_print %>% 
  bind_rows(metaphor_typebased_salience_total)


# type-based METONYMY analysis ===========
metonymy_typebased_salience <- metonymy_typebased %>% 
  count(CM_BROADER, sort = TRUE, name = "n_type") %>% 
  mutate(n_perc_type = round(n_type/sum(n_type) * 100, digits = 1)) %>% 
  rename(metonymy = CM_BROADER)
# metonymy_typebased_salience