# This function calculates harvest rates and exploitation rates using fishery handle data
# Written by Tim Sippel - WDFW

calc_ER_HR<-function(FRAM_object, CR_data_object){
  if (!"scales" %in% installed.packages()) install.packages("scales")
  library("scales")
  #source("Functions/read_FRAM_data.R")
  
  # Calculate proportion of unmarked fish that are LCN for ER discount of unmarked fish above Lewis River (ie. Z4-5, and sport above Lewis) 
  # Early
  LCN_early_above_Lewis<-sum(filter(CR_data$LCN_abundance, Run_type == "Early")$Abundance, na.rm=T)
  BON_early_unmarked<-FRAM_Upriver_Esc_Unmarked %>% filter(Source == 'Esc_To_Bonn') %>% pull(Early_UnMarked)
  Prop_early_LCN_above_Lewis<-LCN_early_above_Lewis/sum(c(LCN_early_above_Lewis, BON_early_unmarked))
  
  # Late
  LCN_late_above_Lewis<-sum(filter(CR_data$LCN_abundance, Run_type == "Late")$Abundance, na.rm=T)
  BON_late_unmarked<-FRAM_Upriver_Esc_Unmarked %>% filter(Source == 'Esc_To_Bonn') %>% pull(Late_UnMarked)
  Prop_late_LCN_above_Lewis<-LCN_late_above_Lewis/sum(c(LCN_late_above_Lewis, BON_late_unmarked))
  
  CR_data_object_calcs<-CR_data_object$fishery_data
  
  # Calculate Columbia River fishery harvest rates (HR) on hatchery fish by clip-status (Adipose clipped/unclipped) and run-type (Early/Late) #
  CR_data_object_calcs<-mutate(CR_data_object_calcs, 
                    HR_marked_early = Kept_clipped_adults_early / 
                      filter(FRAM, 
                             Source == "In-river", Abundance_Harvest == "Abundance",
                             Run_type == "Early", Production_type == "Hatchery",
                             Mark_status == 'Marked')$Value,
                    HR_marked_late = Kept_clipped_adults_late / 
                      filter(FRAM, 
                             Source == "In-river", Abundance_Harvest == "Abundance",
                             Run_type == "Late", Production_type == "Hatchery",
                             Mark_status == 'Marked')$Value,
                    HR_unmarked_early = Kept_unclipped_adults_early / 
                      filter(FRAM, 
                             Source == "In-river", Abundance_Harvest == "Abundance",
                             Run_type == "Early", Production_type == "Hatchery",
                             Mark_status == 'UnMarked')$Value,
                    HR_unmarked_late = Kept_unclipped_adults_late / 
                      filter(FRAM, 
                             Source == "In-river", Abundance_Harvest == "Abundance",
                             Run_type == "Late", Production_type == "Hatchery",
                             Mark_status == 'UnMarked')$Value) 
    
    # Sport fisheries below the Lewis River ####
    # Buoy 10; Tongue Pt. - Warrior Rk.; LCR Tributaries - WA (Zone 1-3); LCR Tributaries - OR (Zone 1-3)
    ER_Sport_Below_Lewis <- CR_data_object_calcs %>% 
      filter(Fishery_area == "Buoy 10" |
              Fishery_area == "Tongue Pt. - Warrior Rk." |
              Fishery_area == "LCR Tributaries - WA (Zone 1-3)" |
              Fishery_area == "LCR Tributaries - OR (Zone 1-3)") %>% 
    # Early  
    mutate(ER_early = rowSums(cbind(Kept_unclipped_adults_early, Released_mortality_adults_early), na.rm = T) /
             filter(FRAM,
                    Source == "Ocean", Abundance_Harvest == "Abundance",
                    Run_type == "Early", Production_type == "Hatchery",
                    Mark_status == 'UnMarked')$Value) %>% 
    # Late
    mutate(ER_late = rowSums(cbind(Kept_unclipped_adults_late, Released_mortality_adults_late), na.rm = T) /
             filter(FRAM,
                    Source == "Ocean", Abundance_Harvest == "Abundance",
                    Run_type == "Late", Production_type == "Hatchery",
                    Mark_status == 'UnMarked')$Value) #%>% 
  
    
    # Youngs Bay - SAFE ####
    ER_Youngs_Bay<-CR_data_object_calcs %>% 
      filter(Fishery_area == "Youngs Bay") %>%
    # Early
    mutate(ER_early = rowSums(cbind(Kept_unclipped_adults_early, Released_mortality_adults_early), na.rm = T) *
             (1-filter(LUT_fishery_areas, Fishery_area == "Youngs Bay")$Prop_unmarked_local) /
             filter(FRAM,
                    Source == "Ocean", Abundance_Harvest == "Abundance",
                    Run_type == "Early", Production_type == "Hatchery",
                    Mark_status == 'UnMarked')$Value) %>%
    # Late
    mutate(ER_late = rowSums(cbind(Kept_unclipped_adults_late, Released_mortality_adults_late), na.rm = T) *
                        (1-filter(LUT_fishery_areas, Fishery_area == "Youngs Bay")$Prop_unmarked_local) /
             filter(FRAM,
                    Source == "Ocean", Abundance_Harvest == "Abundance",
                    Run_type == "Late", Production_type == "Hatchery",
                    Mark_status == 'UnMarked')$Value) 
    
  # Tongue Pt. & S. Channel - SAFE ####
    ER_TonguePt_SChannel<-CR_data_object_calcs %>% 
      filter(Fishery_area == "Tongue Pt. & S. Channel") %>%
    # Early
    mutate(ER_early = rowSums(cbind(Kept_unclipped_adults_early, Released_mortality_adults_early), na.rm = T) *
             (1-filter(LUT_fishery_areas, Fishery_area == "Tongue Pt. & S. Channel")$Prop_unmarked_local) /
             filter(FRAM,
                    Source == "Ocean", Abundance_Harvest == "Abundance",
                    Run_type == "Early", Production_type == "Hatchery",
                    Mark_status == 'UnMarked')$Value) %>%
    # Late
    mutate(ER_late = rowSums(cbind(Kept_unclipped_adults_late, Released_mortality_adults_late), na.rm = T) *
             (1-filter(LUT_fishery_areas, Fishery_area == "Tongue Pt. & S. Channel")$Prop_unmarked_local) /
             filter(FRAM,
                    Source == "Ocean", Abundance_Harvest == "Abundance",
                    Run_type == "Late", Production_type == "Hatchery",
                    Mark_status == 'UnMarked')$Value) 
  
  # Blind & Knappa Slough - SAFE ####
    ER_Blind_Knappa_Slough<-CR_data_object_calcs %>% 
      filter(Fishery_area == "Blind & Knappa Slough") %>%
    # Early
    mutate(ER_early = rowSums(cbind(Kept_unclipped_adults_early, Released_mortality_adults_early), na.rm = T) *
             (1-filter(LUT_fishery_areas, Fishery_area == "Blind & Knappa Slough")$Prop_unmarked_local) /
             filter(FRAM,
                    Source == "Ocean", Abundance_Harvest == "Abundance",
                    Run_type == "Early", Production_type == "Hatchery",
                    Mark_status == 'UnMarked')$Value) %>%
    # Late
    mutate(ER_late = rowSums(cbind(Kept_unclipped_adults_late, Released_mortality_adults_late), na.rm = T) *
             (1-filter(LUT_fishery_areas, Fishery_area == "Blind & Knappa Slough")$Prop_unmarked_local) /
             filter(FRAM,
                    Source == "Ocean", Abundance_Harvest == "Abundance",
                    Run_type == "Late", Production_type == "Hatchery",
                    Mark_status == 'UnMarked')$Value) 
  
  # Deep River - SAFE ####
    ER_DeepRiver<-CR_data_object_calcs %>% 
      filter(Fishery_area == "Deep River") %>%
    # Early
    mutate(ER_early = rowSums(cbind(Kept_unclipped_adults_early, Released_mortality_adults_early), na.rm = T) *
             (1-filter(LUT_fishery_areas, Fishery_area == "Deep River")$Prop_unmarked_local) /
             filter(FRAM,
                    Source == "Ocean", Abundance_Harvest == "Abundance",
                    Run_type == "Early", Production_type == "Hatchery",
                    Mark_status == 'UnMarked')$Value) %>%
    # Late
    mutate(ER_late = rowSums(cbind(Kept_unclipped_adults_late, Released_mortality_adults_late), na.rm = T) *
             (1-filter(LUT_fishery_areas, Fishery_area == "Deep River")$Prop_unmarked_local) /
             filter(FRAM,
                    Source == "Ocean", Abundance_Harvest == "Abundance",
                    Run_type == "Late", Production_type == "Hatchery",
                    Mark_status == 'UnMarked')$Value) 

    # Zone 1-3 ####
    ER_Z13 <- CR_data_object_calcs %>%
      filter(Fishery_area == "Zone 1-3") %>%
    # Early
    mutate(ER_early = rowSums(cbind(Kept_unclipped_adults_early, Released_mortality_adults_early), na.rm = T) /
           filter(FRAM,
                  Source == "Ocean", Abundance_Harvest == "Abundance",
                  Run_type == "Early", Production_type == "Hatchery",
                  Mark_status == 'UnMarked')$Value *

           (filter(FRAM,
                  Source == "Ocean", Abundance_Harvest == "Abundance",
                  Run_type == "Early", Production_type == "Wild")$Value /

              sum(filter(FRAM,
                         Source == "Ocean", Abundance_Harvest == "Abundance",
                         Run_type == "Early", 
                         Mark_status == 'UnMarked')$Value))) %>%

    # Late
    mutate(ER_late = rowSums(cbind(Kept_unclipped_adults_late, Released_mortality_adults_late), na.rm = T) /
           filter(FRAM,
                  Source == "Ocean", Abundance_Harvest == "Abundance",
                  Run_type == "Late", Production_type == "Hatchery",
                  Mark_status == 'UnMarked')$Value *

           (filter(FRAM,
                   Source == "Ocean", Abundance_Harvest == "Abundance",
                   Run_type == "Late", Production_type == "Wild",
                   Mark_status == 'UnMarked')$Value /

              sum(filter(FRAM,
                         Source == "Ocean", Abundance_Harvest == "Abundance",
                         Run_type == "Late", 
                         Mark_status == 'UnMarked')$Value)))
  
  # Fishery Areas above the Lewis River #### 
  # Zone 4-5; Warrior Rk. - Bonneville; LCR Tributaries - WA (Zone 4-5); LCR Tributaries - OR (Zone 4-5)
  ER_AboveLewis <- CR_data_object_calcs %>%
    filter(Fishery_area == "Zone 4-5" | 
             Fishery_area == "Warrior Rk. - Bonneville" |
             Fishery_area == "LCR Tributaries - WA (Zone 4-5)" |
             Fishery_area == "LCR Tributaries - OR (Zone 4-5)") %>%
    rowwise() %>% 
  # Early
  mutate(ER_early = sum(c(Kept_unclipped_adults_early, Released_mortality_adults_early), na.rm = T) *
           Prop_early_LCN_above_Lewis /
            filter(FRAM,
              Source == "Ocean", Abundance_Harvest == "Abundance",
              Run_type == "Early", Production_type == "Wild") %>% pull(Value)) %>%
  # Late
  mutate(ER_late = sum(c(Kept_unclipped_adults_late, Released_mortality_adults_late), na.rm = T) *
            Prop_late_LCN_above_Lewis /
            filter(FRAM,
              Source == "Ocean", Abundance_Harvest == "Abundance",
              Run_type == "Late", Production_type == "Wild") %>% pull(Value))
  
CR_inriver_calcs<-rbind(ER_Sport_Below_Lewis, ER_Youngs_Bay, ER_TonguePt_SChannel, ER_Blind_Knappa_Slough, ER_DeepRiver, 
                        ER_Z13, ER_AboveLewis)
return(CR_inriver_calcs)
}


  
