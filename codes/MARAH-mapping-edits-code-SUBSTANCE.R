# SUBSTANCE-related metaphors =======
marah <- marah %>% 
  mutate(CM_BROADER = replace(CM_BROADER, CM_ROLEMAPPING == "anger is (un)mixed substance", "anger is (un)mixed substance"),
         MAP1 = replace(MAP1, CM_ROLEMAPPING == "anger is (un)mixed substance", "anger_substance"),
         CM_BROADER = replace(CM_BROADER, CM_BROADER %in% c("anger is (heated) fluid in a container", 
                                                            "anger is substance in a container") &
                                CM_ROLEMAPPING %in% c("anger is contained liquid", 
                                                      "anger is contained entity", 
                                                      "anger is located entity", 
                                                      "anger is moved object"), 
                              "anger is substance in a container"),
         CM_BROADER = replace(CM_BROADER, 
                              CM_ROLEMAPPING == "anger is pressing entity in a container", 
                              "anger is pressurised substance in a container"),
         MAP1 = replace(MAP1, CM_BROADER == "anger is substance in a container" & 
                          CM_ROLEMAPPING %in% c("anger is contained liquid", 
                                                "anger is contained entity", 
                                                "anger is located entity", 
                                                "anger is moved object"), 
                        "anger_located-substance/entity"),
         MAP2 = replace(MAP2, CM_BROADER == "anger is substance in a container" & 
                          CM_ROLEMAPPING %in% c("anger is contained liquid", 
                                                "anger is contained entity", 
                                                "anger is located entity", 
                                                "anger is moved object") & 
                          MAP2 %in% c("intensity_liquid-level", "intensity_fullness-degree"), 
                        "intensity_substance-fullness-level"),
         MAP3 = replace(MAP3, CM_BROADER == "anger is substance in a container" & 
                          CM_ROLEMAPPING %in% c("anger is contained liquid", 
                                                "anger is contained entity", 
                                                "anger is located entity", 
                                                "anger is moved object") & 
                          MAP3 %in% c("intensity_liquid-level", "intensity_fullness-degree"), 
                        "intensity_substance-fullness-level"),
         CM_BROADER = replace(CM_BROADER, CM_ROLEMAPPING == "anger is heated fluid in a container", "anger is heated fluid in a container"),
         
         ## node <w>mengandung</w> STATE
         ROLEMAPPING_TD_IN_SFRAME = replace(ROLEMAPPING_TD_IN_SFRAME, MP == "node <w>mengandung</w> STATE", "substance"),
         CM_ROLEMAPPING = replace(CM_ROLEMAPPING, MP == "node <w>mengandung</w> STATE", "anger is (un)mixed substance"),
         CM_BROADER = replace(CM_BROADER, MP == "node <w>mengandung</w> STATE", "anger is (un)mixed substance"),
         MAP1 = replace(MAP1, MP == "node <w>mengandung</w> STATE", "anger_substance"),
         MAP2 = replace(MAP2, MP == "node <w>mengandung</w> STATE", "intensity_mixed-substance"),
         
         ## source frame of SUBMERGED ENTITY
         SFRAME = replace(SFRAME, SFRAME == "cause upward motion", "caused upward motion")
         )

# OTHER METAPHORS ========
marah <- marah %>% 
  mutate(CM_BROADER = replace(CM_BROADER, CM_BROADER == "anger is captive (dangerous) animal", "anger is fierce, captive animal"),
         CM_BROADER = replace(CM_BROADER, CM_BROADER == "anger is confining/impeding entity", "anger is confinement/impediment"),
         CM_BROADER = replace(CM_BROADER, CM_BROADER == "anger is (destructive) natural force", "anger is natural force"),
         CM_BROADER = replace(CM_BROADER, 
                              str_detect(CM_BROADER, "sence of object in some location"), 
                              "anger is (ab/pre)sence of an object"),
         CM_BROADER = replace(CM_BROADER, str_detect(CM_BROADER, "is physical harm"), "anger is physical contact/harm"),
         CM_BROADER = replace(CM_BROADER, CM_BROADER == "anger is sleeping entity", "anger is a sleeping organism"))
