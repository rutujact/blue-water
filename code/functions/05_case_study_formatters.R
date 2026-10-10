# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 05_case_study_formatters.R
# Purpose: Case-study-specific data manipulations for Hemingway et al. (202X).
# Keeps the core toolkit generic by isolating localized assumptions.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Format Classical Sankey Budget for Agrarian Scale
#' 
#' @description Reallocates excess farm-level grasslands to grazed areas exclusively 
#' for the 1950s village-level Sankey representation, while leaving 2022 grasslands 
#' as "unused". # These "Grassland" areas are areas that are not grazed in a given year n (but could be in the year n+1). The biomass requirement for the animals has already been calculated and matches the area under "Grazed area". The "Grasslands" area represents the grassland in excess, which we reallocate to the production system at village level, but not at farm level. So these were not included as "Grasslands" area at the farm level, as it is not needed to meet the dry matter/biomass requirements of the livestock. The adjustment/reallocation is only made at village level so that, in the figure at village level, all grassland areas appear as grazed and no "unused" land appear. This differs from the 2022 figure, where some grassland areas were never grazed and therefore appeared as unused land. 
#' @param ps_volumetric_summary The generic volumetric output table.
#' @return A dataframe formatted for the generic get_d3_sankey() function.
format_agrarian_sankey_data <- function(ps_volumetric_summary) {
  ps_volumetric_summary %>%
    dplyr::mutate(
      grazed = dplyr::if_else(time_period == "1950", grazed + grasslands, grazed), 
      grasslands = dplyr::if_else(time_period == "1950", 0, grasslands)
    ) %>%
    dplyr::rename(unused = grasslands)
}

#' Correct Local Indian Crop Season Names
#' 
#' @description Re-labels crop seasons to correctly match Indian agricultural 
#' seasons (Rabi, Kharif, Zaid) based on their phenological start dates (DOY).
#' @param kc_df The processed kc.doy.crop.all dataframe.
#' @return A dataframe with corrected crop names for localized plotting.
rename_local_indian_seasons <- function(kc_df) {
  kc_df %>%
    dplyr::mutate(
      crop = dplyr::case_when(
        crop == "Okra Summer" ~ "Okra Winter",
        crop == "Okra Winter" ~ "Okra Summer",
        crop == "Groundnut Summer" ~ "Groundnut Winter",
        crop == "Tomato Summer" ~ "Tomato Winter",
        crop == "Tomato Winter" ~ "Tomato Monsoon",
        crop == "Rice Short Summer" ~ "Rice Short Winter",
        TRUE ~ crop
      )
    )
}

#' Harmonize Crop System Names
#' 
#' @description Detects spelling or formatting mismatches (e.g., "Sweet lime" vs "Sweet Lime") 
#' in the area mapping dataset and automatically corrects them to match the 
#' master crop system definitions, preventing silent data drops during merges.
#' @param area_df Dataframe containing the production system acreage mapping.
#' @param master_df Dataframe containing the master crop system definitions (e.g., crop.systems).
#' @return A dataframe with corrected crop.system names.
#' @export
harmonize_crop_system_names <- function(area_df, master_df) {
  valid_cs <- unique(master_df$crop.system)
  input_cs <- unique(area_df$crop.system)
  mismatched <- setdiff(input_cs, valid_cs)
  
  if (length(mismatched) > 0) {
    message("Note: Found spelling mismatches in crop system area mapping. Auto-correcting to match master list.")
    
    # Create a robust string cleaner (strips all spaces, punctuation, and makes lowercase)
    robust_clean <- function(x) stringr::str_remove_all(tolower(x), "[^a-z0-9]")
    valid_cs_clean <- robust_clean(valid_cs)
    
    area_df <- area_df %>%
      dplyr::mutate(
        temp_clean = robust_clean(crop.system),
        crop.system = purrr::map2_chr(crop.system, temp_clean, function(original, cleaned) {
          match_idx <- match(cleaned, valid_cs_clean)
          if (!is.na(match_idx)) {
            return(valid_cs[match_idx]) # Replace with perfect master spelling
          } else {
            return(original) # Leave alone if no match found at all
          }
        })
      ) %>%
      dplyr::select(-temp_clean)
  }
  
  return(area_df)
}