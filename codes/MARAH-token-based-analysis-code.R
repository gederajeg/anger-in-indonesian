library(tidyverse)

# read the metaphor data
marah0 <- read_tsv("data/token-based-approach-main.txt")

marah <- marah0 %>% 
  filter(USE == "metaphor", !LU_POS %in% c("adj", "prepo"))

# count the token frequency of each metaphor
n_metaphor <- marah %>% 
  count(CM_BROADER, sort = TRUE, name = "n_token") %>% 
  mutate(n_perc_token = round(n_token/sum(n_token) * 100, digits = 1)) %>% 
  arrange(desc(n_perc_token))
# n_metaphor

joining_column <- colnames(n_metaphor)[1]

# count the number of mappings by metaphor
marah_mapping_df <- marah %>% 
  select(CM_BROADER, starts_with("MAP"), matches("^(MP|PATTERN|SFRAME|CM_ROLEMAPPING)$"))  %>%
  pivot_longer(cols = starts_with("MAP"), names_to = "MAPPING_ID", values_to = "MAPPING") %>% 
  filter(!is.na(MAPPING))

source("codes/MARAH-mapping-edits-code.R")

n_mapping <- marah_mapping_df %>% 
  group_by(CM_BROADER) %>% 
  summarise(n_mapping = n_distinct(MAPPING), .groups = "drop") %>% 
  arrange(desc(n_mapping)) %>% 
  mutate(n_perc_mapping = round(n_mapping/sum(n_mapping) * 100, digits = 1))
# n_mapping

# count the number of metaphorical lexical units/patterns by metaphor
n_lu <- marah %>% 
  group_by(CM_BROADER) %>% 
  summarise(n_lu = n_distinct(MP), .groups = "drop") %>% 
  # summarise(n_lu = n_distinct(LU), .groups = "drop") %>% 
  mutate(n_perc_lu = round(n_lu/sum(n_lu) * 100, digits = 1)) %>% 
  arrange(desc(n_perc_lu))
# n_lu

# combining the data frame from the three measures (token, type, and mapping numbers) and calculate the salience score
metaphor_salience <- n_metaphor %>% 
  left_join(n_mapping, by = joining_column) %>% 
  left_join(n_lu, by = joining_column) %>% 
  mutate(aggregate = n_perc_token + n_perc_mapping + n_perc_lu) %>% 
  arrange(desc(aggregate))
metaphor_salience %>% slice_head(n = 10)

# for printing in the MS Word
metaphor_salience_print <- metaphor_salience %>% 
  mutate(CM_BROADER = str_replace(CM_BROADER, "^anger is ", ""),
         CM_BROADER = str_replace(CM_BROADER, "^\\(cause of\\) anger is ", ""),
         CM_BROADER = paste("[", CM_BROADER, "]{.smallcaps}", sep = "")) %>%
  select(CM_BROADER, n_token, n_perc_token, n_lu, n_perc_lu, n_mapping, n_perc_mapping, aggregate) %>% 
  rename(`Metaphorical source domains` = CM_BROADER,
         `No of all tokens` = n_token,
         `% of all tokens` = n_perc_token,
         `No. of types of linguistic expression` = n_lu,
         `% of all types of conceptual metaphor` = n_perc_lu,
         `No. of metaphorical mappings` = n_mapping,
         `% of all types of metaphorical mappings` = n_perc_mapping,
         Aggregate = aggregate)
metaphor_salience_total <- metaphor_salience_print %>% 
  summarise(across(where(is.numeric), ~sum(.))) %>% 
  mutate(`Metaphorical source domains` = "**TOTAL**") %>% 
  select(`Metaphorical source domains`, everything()) %>% 
  mutate(Aggregate = replace(Aggregate, `Metaphorical source domains` == "**TOTAL**", NA))
metaphor_salience_print <- metaphor_salience_print %>% 
  bind_rows(metaphor_salience_total)


# read the metonymy data
marah_metonymy <- read_tsv("data/token-based-approach-main.txt") %>% 
  filter(USE == "metonymy")

# metonymic salience analysis
marah_metonymy_salience <- marah_metonymy %>% 
  group_by(METONYMY) %>% 
  summarise(n_token = n(), 
            n_type = n_distinct(PATTERN),
            .groups = "drop") %>% 
  mutate(n_perc_token = round(n_token/sum(n_token) * 100, digits = 1),
         n_perc_type = round(n_type/sum(n_type) * 100, digits = 1),
         aggregate = n_perc_token + n_perc_type) %>% 
  arrange(desc(n_token))
