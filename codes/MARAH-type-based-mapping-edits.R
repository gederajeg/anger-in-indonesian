metaphor_typebased <- metaphor_typebased %>% 
  mutate(CM_BROADER = replace(CM_BROADER, CM_BROADER %in% c("anger is (heated) fluid in a container", "anger is substance in a container") & SFRAME %in% c("fluid containment", "liquid", "mixture", "containment"), "anger is fluid in a container"),
         CM_BROADER = replace(CM_BROADER, CM_BROADER %in% c("anger is (heated) fluid in a container", "anger is substance in a container") & SFRAME %in% c("pressure in a container"), "anger is pressurised substance in a container"),
         MAP_1 = replace(MAP_1, CM_BROADER %in% c("anger is fluid in a container", "anger is pressurised substance in a container"), "anger_contents/located-entity"),
         MAP_2 = replace(MAP_2, CM_BROADER == "anger is fluid in a container" & MAP_2 == "anger-level_fluid-heat-level", "anger-level_substance-fullness-level"),
         CM_BROADER = replace(CM_BROADER, CM_BROADER == "anger is fluid in a container", "anger is substance in a container"),
         CM_BROADER = replace(CM_BROADER, CM_BROADER == "anger is (heated) fluid in a container", "anger is heated fluid in a container"),
         CM_BROADER = replace(CM_BROADER, CM_BROADER == "anger is (destructive) natural force", "anger is natural force"),
         CM_BROADER = replace(CM_BROADER, CM_BROADER == "anger is fierce (captive) animal", "anger is fierce, captive animal"))