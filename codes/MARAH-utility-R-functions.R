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

get_metaphor_salience_rank <- function(salience_df, metaphor_regex) {
  interim_df <- as.data.frame(salience_df)
  return(rownames(subset(interim_df, grepl(metaphor_regex, metaphor, perl = TRUE))))
}

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

get_metaphor_mapping_tokenfreq <- function(mapping_sourcedomain_token_df, mapping_regex, lu_var = "MP", gloss_var = "MP_GLOSS", mapping_var = "MAPPING", key_mapping = FALSE, lu_output = FALSE) {
  
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

get_lu_gloss_n_printed <- function(mapping_sourcedomain_token_df, mapping_regex, lu_var = "MP", gloss_var = "MP_GLOSS", mapping_var = "MAPPING", key_mapping = FALSE, metaphor_data = TRUE) {
  lu_var <- rlang::sym(lu_var)
  gloss_var <- rlang::sym(gloss_var)
  mapping_var <- rlang::sym(mapping_var)
  
  if (metaphor_data == TRUE) { # printing for metaphor
    
    mapping_sourcedomain_token_df %>% 
      filter(status == 'no') %>% 
      # filter(str_detect(MAPPING, mapping_regex)) %>% 
      filter(str_detect(!!mapping_var, mapping_regex)) %>% 
      # count(LU, LU_GLOSS) %>% 
      count(!!lu_var, !!gloss_var) %>% 
      mutate(!!lu_var := str_replace_all(!!lu_var, "\\b(node)\\b", "[marah]{.smallcaps}")) %>%
      mutate(!!lu_var := gsub("([[:upper:]]+)", "[\\L\\1]{.smallcaps}", !!lu_var, perl = TRUE)) %>%
      mutate(!!gloss_var := gsub("([[:upper:]]+)", "[\\L\\1]{.smallcaps}", !!gloss_var, perl = TRUE)) %>%
      # mutate(lu_gloss_n = paste("*", str_replace_all(LU, "<\\/?w>", ""), "* '", LU_GLOSS, "' (", n, ")", sep = "")) %>% 
      mutate(lu_gloss_n = paste("*", str_replace_all(!!lu_var, "<\\/?w>", "__"), "* '", !!gloss_var, "' (", n, ")", sep = "")) %>% 
      arrange(desc(n)) %>% 
      pull(lu_gloss_n) %>% 
      paste(collapse = "; ") %>% 
      return()
    
  } else if (metaphor_data == FALSE) { # printing for metonymy
    
    mapping_sourcedomain_token_df %>% 
      filter(str_detect(!!mapping_var, mapping_regex)) %>% 
      count(!!lu_var, !!gloss_var) %>% 
      mutate(!!lu_var := str_replace_all(!!lu_var, "\\b(node)\\b", "[marah]{.smallcaps}")) %>%
      mutate(!!lu_var := gsub("([[:upper:]]+)", "[\\L\\1]{.smallcaps}", !!lu_var, perl = TRUE)) %>%
      mutate(!!gloss_var := gsub("([[:upper:]]+)", "[\\L\\1]{.smallcaps}", !!gloss_var, perl = TRUE)) %>%
      mutate(lu_gloss_n = paste("*", str_replace_all(!!lu_var, "<\\/?w>", "__"), "* '", !!gloss_var, "' (", n, ")", sep = "")) %>% 
      arrange(desc(n)) %>% 
      pull(lu_gloss_n) %>% 
      paste(collapse = "; ") %>% 
      return()
    
  }
}

get_metaphor_mapping_stat_typefreq <- function(mapping_sourcedomain_token_df, lu_var = "MP", mapping_var = "MAPPING") {
  lu_var <- rlang::sym(lu_var)
  mapping_var <- rlang::sym(mapping_var)
  mapping_sourcedomain_token_df %>% 
    filter(status == 'no') %>% 
    group_by(!!mapping_var) %>% 
    summarise(n_lu = n_distinct(!!lu_var),
              n_token = n()) %>% 
    mutate(ttr_mapping = n_lu/n_token,
           ttr_mapping_perc = round(ttr_mapping * 100, digits = 1)) %>% 
    arrange(desc(n_lu)) %>% 
    return()
}

get_metaphor_mapping_ttr <- function(mapping_sourcedomain_token_stats, mapping_regex = NULL, ttr_perc = TRUE, mapping_var = "MAPPING") {
  
  if (is.null(mapping_regex)) {
    stop("Specify the regex of the mapping!\n")
  }
  
  mapping_var <- rlang::sym(mapping_var)
  
  if (ttr_perc) {
    stats_type <- rlang::sym("ttr_mapping_perc")
    mapping_sourcedomain_token_stats %>% 
      filter(str_detect(!!mapping_var, mapping_regex)) %>% 
      pull(!!stats_type) %>% 
      return()
  } else {
    stats_type <- rlang::sym("ttr_mapping")
    mapping_sourcedomain_token_stats %>% 
      filter(str_detect(!!mapping_var, mapping_regex)) %>% 
      pull(!!stats_type) %>% 
      round(digits = 2)
    return()
  }
}

get_key_mappings <- function(mapping_sourcedomain_token_df, mapping_var = "MAPPING", status_var = "status") {
  mapping_var <- rlang::sym(mapping_var)
  status_var <- rlang::sym(status_var)
  interim_mapping <- unique(pull(filter(mapping_sourcedomain_token_df, !!status_var == "key"), !!mapping_var))
  interim_mapping <- gsub("-", " ", interim_mapping, perl = TRUE)
  interim_mapping <- gsub("(_|\\sis\\s)", " <- ", interim_mapping, perl = TRUE)
  if (length(interim_mapping) > 1) {
    return(paste(interim_mapping, collapse = "; "))
  } else {
    return(interim_mapping)
  }
}

# helper function for determining the p-value to print in the text
pval_print <- function(pval) {
  
  if (pval < 0.05 & pval > 0.01) {
    pv <- " < 0.05"
  } else if (pval < 0.01 & pval > 0.001) {
    pv <- " < 0.01"
  } else if (pval < 0.001) {
    pv <- " < 0.001"
  } else if (pval > 0.05 & pval < 0.1) {
    pv <- " > 0.05"
  } else if (pval > 0.1) {
    pv <- " > 0.1"
  }
  
  return(pv)
  
}
