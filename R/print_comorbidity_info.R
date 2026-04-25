#' Print Comorbidity Index Reference Table
#'
#' Displays a formatted reference table of comorbidity components and their
#' scoring weights for the Charlson or Elixhauser index, along with key
#' citations.
#'
#' @param method One of `"charlson"` (default) or `"elixhauser"`.
#'
#' @return `invisible(NULL)`. Called for its side effects.
#' @export
#'
#' @examples
#' \dontrun{
#' print_comorbidity_info("charlson")
#' print_comorbidity_info("elixhauser")
#' }
print_comorbidity_info <- function(method = c("charlson", "elixhauser")) {

  method <- match.arg(method)

  #--- Build Charlson reference table ---
  if (method == "charlson") {

    weights <- c(
      mi = 1, chf = 1, pvd = 1, cevd = 1, dementia = 1, cpd = 1,
      rheumd = 1, pud = 1, mld = 1, diab = 1, diabwc = 2, hp = 2,
      rend = 2, canc = 2, msld = 3, metacanc = 6, aids = 6
    )

    full_names <- c(
      mi       = "Myocardial Infarction",
      chf      = "Congestive Heart Failure",
      pvd      = "Peripheral Vascular Disease",
      cevd     = "Cerebrovascular Disease",
      dementia = "Dementia",
      cpd      = "Chronic Pulmonary Disease",
      rheumd   = "Rheumatic Disease",
      pud      = "Peptic Ulcer Disease",
      mld      = "Mild Liver Disease",
      diab     = "Diabetes without Complications",
      diabwc   = "Diabetes with Chronic Complications",
      hp       = "Hemiplegia or Paraplegia",
      rend     = "Renal Disease",
      canc     = "Any Malignancy (except skin cancer)",
      msld     = "Moderate or Severe Liver Disease",
      metacanc = "Metastatic Solid Tumor",
      aids     = "AIDS/HIV"
    )

    ref_table <- data.frame(
      comorbidity   = full_names[names(weights)],
      abbreviation  = names(weights),
      weight        = weights,
      row.names     = NULL,
      stringsAsFactors = FALSE
    )

    print(knitr::kable(ref_table, caption = "Charlson Comorbidity Index"))

    cli::cli_h3("References")
    cli::cli_ul(c(
      "Charlson et al. (1987), J Chronic Dis 40(5):373-383",
      "Quan et al. (2005), Medical Care 43(11):1130-1139"
    ))

  } else {

    #--- Build Elixhauser reference table ---
    full_names <- c(
      congestive_heart_failure        = "Congestive Heart Failure",
      cardiac_arrhythmias             = "Cardiac Arrhythmias",
      valvular_disease                = "Valvular Disease",
      pulmonary_circulation_disorders = "Pulmonary Circulation Disorders",
      peripheral_vascular_disorders   = "Peripheral Vascular Disorders",
      hypertension_uncomplicated      = "Hypertension, Uncomplicated",
      hypertension_complicated        = "Hypertension, Complicated",
      paralysis                       = "Paralysis",
      other_neurological              = "Other Neurological Disorders",
      chronic_pulmonary               = "Chronic Pulmonary Disease",
      diabetes_uncomplicated          = "Diabetes, Uncomplicated",
      diabetes_complicated            = "Diabetes, Complicated",
      hypothyroidism                  = "Hypothyroidism",
      renal_failure                   = "Renal Failure",
      liver_disease                   = "Liver Disease",
      peptic_ulcer_disease            = "Peptic Ulcer Disease",
      aids                            = "AIDS/HIV",
      lymphoma                        = "Lymphoma",
      metastatic_cancer               = "Metastatic Cancer",
      solid_tumor_without_metastasis  = "Solid Tumor without Metastasis",
      rheumatoid_arthritis            = "Rheumatoid Arthritis",
      coagulation_defects             = "Coagulopathy",
      obesity                         = "Obesity",
      weight_loss                     = "Weight Loss",
      fluid_and_electrolyte_disorders = "Fluid and Electrolyte Disorders",
      blood_loss_anemia               = "Blood Loss Anemia",
      deficiency_anemias              = "Deficiency Anemias",
      alcohol_abuse                   = "Alcohol Abuse",
      drug_abuse                      = "Drug Abuse",
      psychoses                       = "Psychoses",
      depression                      = "Depression"
    )

    weights <- c(
      congestive_heart_failure        =  7,
      cardiac_arrhythmias             =  5,
      valvular_disease                =  6,
      pulmonary_circulation_disorders =  6,
      peripheral_vascular_disorders   =  2,
      hypertension_uncomplicated      = -1,
      hypertension_complicated        =  0,
      paralysis                       =  5,
      other_neurological              =  2,
      chronic_pulmonary               =  3,
      diabetes_uncomplicated          =  0,
      diabetes_complicated            =  2,
      hypothyroidism                  = -2,
      renal_failure                   =  7,
      liver_disease                   = 11,
      peptic_ulcer_disease            =  0,
      aids                            =  0,
      lymphoma                        =  9,
      metastatic_cancer               = 14,
      solid_tumor_without_metastasis  =  9,
      rheumatoid_arthritis            =  0,
      coagulation_defects             =  7,
      obesity                         = -5,
      weight_loss                     =  6,
      fluid_and_electrolyte_disorders =  7,
      blood_loss_anemia               =  3,
      deficiency_anemias              =  0,
      alcohol_abuse                   =  0,
      drug_abuse                      =  0,
      psychoses                       = -1,
      depression                      = -3
    )

    ref_table <- data.frame(
      comorbidity  = full_names[names(weights)],
      abbreviation = names(weights),
      weight       = weights,
      row.names    = NULL,
      stringsAsFactors = FALSE
    )

    print(knitr::kable(ref_table, caption = "Elixhauser Comorbidity Index"))

    cli::cli_h3("References")
    cli::cli_ul(c(
      "Elixhauser et al. (1998), Medical Care 36(1):8-27",
      "van Walraven et al. (2009), Medical Care 47(1):626-633"
    ))
  }

  invisible(NULL)
}
