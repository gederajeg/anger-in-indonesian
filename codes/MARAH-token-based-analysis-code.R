library(tidyverse)

get_metaphor_mapping_n_lu <- function(mapping_lu_stats_df, mapping_regex, mapping_var = "MAPPING", pull_lu = TRUE) {
  mapping_var <- rlang::sym(mapping_var)
  if (pull_lu) {
    # return(pull(filter(mapping_lu_stats_df, str_detect(MAPPING, mapping_regex)), n_lu))
    return(pull(filter(mapping_lu_stats_df, str_detect(!!mapping_var, mapping_regex)), n_lu))
  } else {
    # return(pull(filter(mapping_lu_stats_df, str_detect(MAPPING, mapping_regex)), MAPPING))
    return(pull(filter(mapping_lu_stats_df, str_detect(!!mapping_var, mapping_regex)), !!mapping_var))
  }
}

get_metaphor_mapping_tokenfreq <- function(mapping_sourcedomain_token_df, mapping_regex, lu_var = "LU", gloss_var = "LU_GLOSS", mapping_var = "MAPPING", key_mapping = FALSE, lu_output = FALSE) {
  
  lu_var <- rlang::sym(lu_var)
  gloss_var <- rlang::sym(gloss_var)
  mapping_var <- rlang::sym(mapping_var)
  
  if (key_mapping == FALSE) {
    if (!lu_output) {
      # return(sum(count(filter(mapping_sourcedomain_token_df, status == "no", str_detect(MAPPING, mapping_regex)), LU, LU_GLOSS)$n))
      return(sum(count(filter(mapping_sourcedomain_token_df, status == "no", str_detect(!!mapping_var, mapping_regex)), !!lu_var, !!gloss_var)$n))
    } else {
      # return(arrange(count(filter(mapping_sourcedomain_token_df, status == "no", str_detect(MAPPING, mapping_regex)), LU, LU_GLOSS), desc(n)))
      return(arrange(count(filter(mapping_sourcedomain_token_df, status == "no", str_detect(!!mapping_var, mapping_regex)), !!lu_var, !!gloss_var), desc(n)))
    }
    
  } else {
    return(distinct(filter(mapping_sourcedomain_token_df, status == "key")))
  }
}

get_lu_gloss_n_printed <- function(mapping_sourcedomain_token_df, mapping_regex, lu_var = "LU", gloss_var = "LU_GLOSS", mapping_var = "MAPPING", key_mapping = FALSE) {
  lu_var <- rlang::sym(lu_var)
  gloss_var <- rlang::sym(gloss_var)
  mapping_var <- rlang::sym(mapping_var)
  
  mapping_sourcedomain_token_df %>% 
    filter(status == 'no') %>% 
    # filter(str_detect(MAPPING, mapping_regex)) %>% 
    filter(str_detect(!!mapping_var, mapping_regex)) %>% 
    # count(LU, LU_GLOSS) %>% 
    count(!!lu_var, !!gloss_var) %>% 
    # mutate(lu_gloss_n = paste("*", str_replace_all(LU, "<\\/?w>", ""), "* '", LU_GLOSS, "' (", n, ")", sep = "")) %>% 
    mutate(lu_gloss_n = paste("*", str_replace_all(!!lu_var, "<\\/?w>", ""), "* '", !!gloss_var, "' (", n, ")", sep = "")) %>% 
    arrange(desc(n)) %>% 
    pull(lu_gloss_n) %>% 
    paste(collapse = "; ") %>% 
    return()
}

get_metaphor_mapping_stat_typefreq <- function(mapping_sourcedomain_token_df, lu_var = "LU", mapping_var = "MAPPING") {
  lu_var <- rlang::sym(lu_var)
  mapping_var <- rlang::sym(mapping_var)
  mapping_sourcedomain_token_df %>% 
    filter(status == 'no') %>% 
    group_by(!!mapping_var) %>% 
    summarise(n_lu = n_distinct(!!lu_var)) %>% 
    arrange(desc(n_lu)) %>% 
    return()
}

# read the metaphor data
marah0 <- read_tsv("data/token-based-approach-main.txt")

marah <- marah0 %>% 
  filter(USE == "metaphor", !LU_POS %in% c("adj", "prepo"))

source("codes/MARAH-mapping-edits-code-SUBSTANCE.R")

# count the token frequency of each metaphor
n_metaphor <- marah %>% 
  count(CM_BROADER, sort = TRUE, name = "n_token") %>% 
  mutate(n_perc_token = round(n_token/sum(n_token) * 100, digits = 1)) %>% 
  arrange(desc(n_perc_token))
# n_metaphor

joining_column <- colnames(n_metaphor)[1]

# count the number of mappings by metaphor
metaphor_tokenbased_mapping <- marah %>% 
  select(CM_BROADER, starts_with("MAP"), matches("^(MP|LU|LU_GLOSS|PATTERN|SFRAME|CM_ROLEMAPPING)$"))  %>%
  pivot_longer(cols = starts_with("MAP"), names_to = "MAPPING_ID", values_to = "MAPPING") %>% 
  filter(!is.na(MAPPING))

source("codes/MARAH-mapping-edits-code.R")

n_mapping <- metaphor_tokenbased_mapping %>% 
  group_by(CM_BROADER) %>% 
  summarise(n_mapping = n_distinct(MAPPING), .groups = "drop") %>% 
  arrange(desc(n_mapping)) %>% 
  mutate(n_perc_mapping = round(n_mapping/sum(n_mapping) * 100, digits = 1))
# n_mapping

# count the number of metaphorical lexical units/patterns by metaphor
n_lu <- marah %>% 
  group_by(CM_BROADER) %>% 
  # summarise(n_lu = n_distinct(MP), .groups = "drop") %>% 
  summarise(n_lu = n_distinct(LU), .groups = "drop") %>%
  mutate(n_perc_lu = round(n_lu/sum(n_lu) * 100, digits = 2)) %>% 
  arrange(desc(n_perc_lu))
# n_lu

# combining the data frame from the three measures (token, type, and mapping numbers) and calculate the salience score
metaphor_salience <- n_metaphor %>% 
  left_join(n_mapping, by = joining_column) %>% 
  left_join(n_lu, by = joining_column) %>% 
  mutate(aggregate = n_perc_token + n_perc_mapping + n_perc_lu) %>% 
  arrange(desc(aggregate)) %>% 
  rename(metaphor = CM_BROADER,
         n_type = n_lu,
         n_perc_type = n_perc_lu)
metaphor_salience %>% slice_head(n = 10)

# for printing in the MS Word
metaphor_salience_print <- metaphor_salience %>% 
  mutate(metaphor = str_replace(metaphor, "^anger is (a\\s)?", ""),
         metaphor = str_replace(metaphor, "^\\(cause of\\) anger is ", ""),
         metaphor = str_replace(metaphor, "^(heated fluid|pressurised substance) in a container$", "\\1"),
         metaphor = str_replace(metaphor, "^(intensity) of anger( is .+)$", "\\1\\2"),
         metaphor = str_replace(metaphor, "^(degree of control) for anger( is .+)$", "\\1\\2"),
         metaphor = str_replace(metaphor, "(?<=intensity is )object (quantity)$", "\\1"),
         metaphor = str_replace(metaphor, "^substance in a container$", "contained substance"),
         metaphor = paste("[", metaphor, "]{.smallcaps}", sep = "")) %>%
  select(metaphor, n_token, n_perc_token, n_type, n_perc_type, n_mapping, n_perc_mapping, aggregate) %>% 
  rename(`Metaphorical source domains` = metaphor,
         `No of all tokens` = n_token,
         `% of all tokens` = n_perc_token,
         `No. of types of linguistic expression` = n_type,
         `% of all types of linguistic expression` = n_perc_type,
         `No. of metaphorical mappings` = n_mapping,
         `% of all types of metaphorical mappings` = n_perc_mapping,
         Aggregate = aggregate)
metaphor_salience_total <- metaphor_salience_print %>% 
  summarise(across(where(is.numeric), ~sum(.))) %>% 
  mutate(across(where(is.numeric), ~round(.)),
         `Metaphorical source domains` = "**TOTAL**",
         Aggregate = replace(Aggregate, `Metaphorical source domains` == "**TOTAL**", NA)) %>% 
  select(`Metaphorical source domains`, everything())
metaphor_salience_print <- metaphor_salience_print %>% 
  mutate(`% of all types of linguistic expression` = round(`% of all types of linguistic expression`, digits = 1),
         Aggregate = round(Aggregate, digits = 1)) %>% 
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
