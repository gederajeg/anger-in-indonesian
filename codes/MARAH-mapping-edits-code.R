# code for editing the metaphorical mapping of the token-based dataset on the fly in R

metaphor_tokenbased_mapping <- metaphor_tokenbased_mapping %>% 
  mutate(MAPPING = replace(MAPPING, 
                           MAPPING %in% c("non-experiencer_fighter2", "experiencer_fighter2") & 
                             CM_BROADER == "anger is adversary", 
                           "experiencer-or-other-states_fighter2"),
         MAPPING = replace(MAPPING,
                           CM_BROADER %in% c("anger is substance in a container", 
                                             "anger is pressurised substance in a container") &
                             MAPPING %in% c("expressing anger is content going out", 
                                            "expression of anger is expelling object"),
                           "expressing anger is expelling/making contents out"),
         MAPPING = replace(MAPPING,
                           CM_BROADER %in% c("anger is substance in a container", 
                                             "anger is pressurised substance in a container") & 
                             MAPPING %in% c("increased intensity is increased pressure", 
                                            "intense anger is pressing substance in a container"),
                           "increased intensity is increased pressure of the substance to the container"),
         MAPPING = replace(MAPPING,
                           CM_BROADER == "anger is substance in a container" & 
                             MAPPING %in% c("high intensity of anger is a filled-up container", 
                                            "intensity_fullness-degree", "intensity_level-of-contained-entity"),
                           "increased intensity is increased fullness of the substance in the container"),
         # MAPPING = replace(MAPPING,
         #                   CM_BROADER == "anger is fire" & 
         #                     MAPPING %in% c("causing anger is igniting fire (i.e. set on fire)"),
         #                   "cause-of-anger_cause-of-fire"),
         MAPPING = replace(MAPPING,
                           CM_BROADER == "anger is substance in a container" &
                             MAPPING == "controlling anger is making the substance harden/sedimented",
                           "regulating anger is making the substance hardened/sedimented"),
         MAPPING = replace(MAPPING,
                           CM_BROADER == "anger is substance in a container" &
                             MAPPING %in% c("expressing anger is releasing/channeling the liquid (at others)", 
                                            "expressing anger is expelling/making contents out"),
                           "expressing anger is expelling/releasing/channeling substance out (at others)"),
         MAPPING = replace(MAPPING,
                           CM_BROADER == "anger is natural force" & MAPPING == "anger_destroyer",
                           "anger_(destructive-)natural-force"),
         # pressurised substance mapping
         MAPPING = replace(MAPPING,
                           CM_BROADER == "anger is pressurised substance in a container" &
                             MAPPING %in% c("loss of control over anger is explosion of container", 
                                            "intentional loss of control over anger is causing explosion of container"),
                           "(intentional) loss of control over anger is (causing) explosion of container")
         )

# adversary metaphor (token-based dataset)
metaphor_tokenbased_mapping <- metaphor_tokenbased_mapping |> 
  mutate(MAPPING = replace(MAPPING, 
                           MAPPING_ID == "MAP3" & MP %in% c("<w>kompromi dengan</w> node", "<w>konfrontasi terhadap</w> node"),
                           "attempt to control anger is combating opponent (e.g., fighting, confronting)"))