# code for editing the metaphorical mapping dataset on the fly in R

marah_mapping_df <- marah_mapping_df %>% 
  mutate(MAPPING = replace(MAPPING, 
                           MAPPING %in% c("non-experiencer_fighter2", "experiencer_fighter2") & CM_BROADER == "anger is adversary", 
                           "experiencer-or-other-states_fighter2"),
         MAPPING = replace(MAPPING,
                           CM_BROADER %in% c("anger is substance in a container", "anger is pressurised substance in a container") & 
                             MAPPING %in% c("expressing anger is content going out", "expression of anger is expelling object"),
                           "expressing anger is expelling/making contents out"),
         MAPPING = replace(MAPPING,
                           CM_BROADER %in% c("anger is substance in a container", "anger is pressurised substance in a container") & 
                             MAPPING %in% c("increased intensity is increased pressure", "intense anger is pressing substance in a container"),
                           "increased intensity is increased pressure of the substance to the container"),
         MAPPING = replace(MAPPING,
                           CM_BROADER == "anger is substance in a container" & 
                             MAPPING %in% c("high intensity of anger is a filled-up container", "intensity_fullness-degree", "intensity_level-of-contained-entity"),
                           "increased intensity is increased pressure and fullness of the substance to the container"),
         MAPPING = replace(MAPPING,
                           CM_BROADER == "anger is fire" & 
                             MAPPING %in% c("causing anger is igniting fire (i.e. set on fire)"),
                           "cause-of-anger_cause-of-fire")
         )