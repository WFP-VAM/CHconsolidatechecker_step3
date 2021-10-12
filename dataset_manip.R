
count_ok_fun <- function(x){
  if(x == 'ok'){ return(1)} #code stops here if TRUE
  return(0)
}

dataset_clean <- function(dataset, sheet_type){
  
  #column 8 is the start of the current phase information - selecting by column number because of english/french versions
  
  if(sheet_type == 'current'){
    print("Selecting current Data")
    c1 <-  8
  }else if(sheet_type == 'projected'){
    c1 <- 20
    print("Selecting projected Data")
  }

  
  #including the first 3 columns which contain geographic info, last 5 columns contain - remove any total field
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
  
  #dont include any totals or info where the geo info is missing
  dataset_current <- dataset_current %>% filter(!is.na(adm0_name))
  
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
  
  #filter out only observations that have an error
  
  dataset_current <- dataset_current %>%
    rowwise() %>% #for each row
    mutate(count_ok = sum(across(rule1:rule14,count_ok_fun))) %>% 
    filter(count_ok < 14) %>% 
    select(-count_ok)
  
  return(dataset_current)
}

