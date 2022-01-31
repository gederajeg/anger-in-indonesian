# EDIT THE SUBSTANCE-related metaphors
marah <- marah %>% 
  mutate(CM_BROADER = replace(CM_BROADER, CM_ROLEMAPPING == "anger is (un)mixed substance", "anger is (un)mixed substance"),
         MAP1 = replace(MAP1, CM_ROLEMAPPING == "anger is (un)mixed substance", "anger_substance"),
         CM_BROADER = replace(CM_BROADER, CM_BROADER %in% c("anger is (heated) fluid in a container", "anger is substance in a container") & CM_ROLEMAPPING %in% c("anger is contained liquid", "anger is contained entity", "anger is located entity", "anger is moved object"), "anger is substance in a container"),
         CM_BROADER = replace(CM_BROADER, CM_ROLEMAPPING == "anger is pressing entity in a container", "anger is pressurised substance in a container"),
         MAP1 = replace(MAP1, CM_BROADER == "anger is substance in a container" & CM_ROLEMAPPING %in% c("anger is contained liquid", "anger is contained entity", "anger is located entity", "anger is moved object"), "anger_located-substance/entity"),
         MAP2 = replace(MAP2, CM_BROADER == "anger is substance in a container" & CM_ROLEMAPPING %in% c("anger is contained liquid", "anger is contained entity", "anger is located entity", "anger is moved object") & MAP2 %in% c("intensity_liquid-level", "intensity_fullness-degree"), "intensity_substance-fullness-level"),
         MAP3 = replace(MAP3, CM_BROADER == "anger is substance in a container" & CM_ROLEMAPPING %in% c("anger is contained liquid", "anger is contained entity", "anger is located entity", "anger is moved object") & MAP3 %in% c("intensity_liquid-level", "intensity_fullness-degree"), "intensity_substance-fullness-level"),
         CM_BROADER = replace(CM_BROADER, CM_ROLEMAPPING == "anger is heated fluid in a container", "anger is heated fluid in a container"))

# EDIT OTHER METAPHORS
marah <- marah %>% 
  mutate(CM_BROADER = replace(CM_BROADER, CM_BROADER == "anger is captive (dangerous) animal", "anger is (fierce) captive animal"))
