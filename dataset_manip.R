dataset_clean <- function(dataset){

  c1 <- which(names(dataset) == 'SITUATION.COURANTE')
  
  dataset_current <- 
    dataset[,c(1,2,3,c1:(c1+5))] %>% 
    as_tibble() 

  names(dataset_current) = c(
    'adm0_name',
    'adm1_name',
    'adm2_name',
    'phase',
    'phase1perc',
    'phase2perc',
    'phase3perc',
    'phase4perc', 
    'phase5perc'
  )
  
  dataset_current <- dataset_current %>% 
    mutate(across(phase:phase5perc, as.numeric))

  dataset_current <- dataset_current %>% mutate(
    rule1 = if_else(phase5perc > 0,"rule: phase5perc > 0 suggestion: check phase 5","ok"),
    rule2 = if_else(phase4perc > 0.05,"phase4perc > 5 - check phase 4","ok"),
    rule3 = if_else(phase3perc > 0.35, "phase3perc > 35 - check phase 3", "ok"),
    rule4 = if_else(phase4perc > phase3perc, "phase4perc > phase3perc - check phase 4", "ok"),
    rule5 = if_else(phase2perc < (phase3perc +phase5perc +phase5perc),"phase2perc < (phase3perc +phase5perc +phase5perc) - Check population in phase 3 and above", "ok"),
    rule6 = if_else(phase == 4 & (phase4perc +phase5perc) < 0.2, "phase == 4 & (phase4perc +phase5perc) < 20) - Check population in phase 4 or review classification", "ok"),
    rule7 = if_else((phase == 4 & between((phase4perc +phase5perc),0.2,0.21)),"phase == 4 & (phase4perc +phase5perc) = 20 | 21 - close to limits","ok"),
    rule8 = if_else(phase == 3 & (phase3perc +phase4perc +phase5perc) < 0.2, "phase == 3 & (phase3perc +phase4perc +phase5perc) < 20) - Check population in phase 3 or review classification", "ok"),
    rule9 = if_else((phase == 3 & between((phase3perc +phase4perc +phase5perc),0.2,0.21)),"phase == 3 & (phase3perc +phase4perc +phase5perc) = 20 | 21 - close to limits","ok"),
    rule10 = if_else(phase == 2 & (phase2perc +phase3perc +phase4perc +phase5perc) < 0.2, "phase == 2 & (phase2perc +phase3perc +phase4perc +phase5perc) < 20) - Check population in phase 3 or review classification", "ok"),
    rule11 = if_else((phase == 2 & between((phase2perc +phase3perc +phase4perc +phase5perc),0.2,0.21)),"phase == 2 & (phase3perc +phase4perc +phase5perc) = 20 | 21 - close to limits","ok"),
    rule12 = if_else(between(phase1perc +phase2perc +phase3perc +phase4perc +phase5perc,0.999,1.001),"ok","phase1perc +phase2perc +phase3perc +phase4perc +phase5perc != 100% - check total"),
    rule13 = if_else((phase == 2 & phase3perc > 0.5)| (phase == 2 & phase4perc > 0), "phase == 2 & phase3perc > 0.5)| (phase == 2 & phase4perc > 0) - Check population in phase 3 and above", "ok"),
    rule14 = if_else((phase == 1 & phase3perc > 0) | (phase == 1 & phase4perc > 0), "phase == 1 & phase3perc > 0)| (phase == 1 & phase4perc > 0) - Check population in phase 3 and above", "ok"))
  
  #figure out a smarter way of doing this with filter across 
  dataset_current <- dataset_current %>% filter(rule1 != "ok" | rule2 != "ok" | rule3 != "ok"| rule4 != "ok"| rule5 != "ok"| rule6 != "ok"| rule7 != "ok" | rule8 != "ok" | rule9 != "ok" | rule10 != "ok" | rule11 != "ok" | rule12 != "ok" | rule13 != "ok" | rule14 != "ok")
  
  return(dataset_current)
}