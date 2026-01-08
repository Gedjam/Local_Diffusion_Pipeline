# Functions to get paths to package data

#' Helper function to get FA template path
#' @param normative_baseline_space String indicating whether the normative space is
#' "MNI" or "UCL" (default = "MNI").
preprocess_get_fa_template_2mm_path <- function(normative_baseline_space = "MNI") {

  if(normative_baseline_space == "UCL"){
      template_file <- file.path("Templates", "UCL_MultiModal_CNNP_Template_FA.nii.gz")
  } else if(normative_baseline_space == "MNI"){
      template_file <- file.path("Templates", "MNI_FA_2mm.nii.gz")
  } else {
      stop("normative_baseline_space must be either 'MNI' or 'UCL'")
  }

  cat("FA template file:", template_file, "\n")
  return(system.file("extdata", template_file, package = "ADvizR"))
}

#' Helper function to get MD template path
#' @param normative_baseline_space String indicating whether the normative space is
#' "MNI" or "UCL" (default = "MNI").
preprocess_get_md_template_2mm_path <- function(normative_baseline_space = "MNI") {

  if(normative_baseline_space == "UCL"){
      template_file <- file.path("Templates", "UCL_MultiModal_CNNP_Template_MD.nii.gz")
  } else if(normative_baseline_space == "MNI"){
      template_file <- file.path("Templates", "MNI_MD_2mm.nii.gz")
  } else {
      stop("normative_baseline_space must be either 'MNI' or 'UCL'")
  }

  cat("MD template file:", template_file, "\n")
  return(system.file("extdata", template_file, package = "ADvizR"))
}

#' Helper function to get atlas paths (atlas and ROI names)
get_atlas_paths <- function() {
    atlas_path <- system.file(
        file.path("extdata", "Atlas", "60", "myaparc_60wm5max_Dil_with_Subcortical_Regions_2mm.nii.gz"),
        package = "ADvizR"
    )
    roi_path <- system.file(
        file.path("extdata", "Atlas", "60", "myaparc_60_with_subcortical_ROI_labels.txt"), 
        package = "ADvizR"
    )
    return(list("atlas_path" = atlas_path, "roi_path" = roi_path))
}

#' Helper function to get path to control data means
#' @param normative_baseline_space String indicating whether the normative space is
#' "MNI" or "UCL" (default = "MNI").
get_control_baseline_means <- function(normative_baseline_space = "MNI") {
  
  if(normative_baseline_space == "UCL"){

    control_means_path <- system.file(
        file.path("extdata", "Control_Baseline", "old_eddy", "control_NODDI_means.RData"),
        package = "ADvizR"
    )  

    } else if(normative_baseline_space == "MNI"){

    control_means_path <- system.file(
        file.path("extdata", "Control_Baseline", "control_NODDI_means.RData"),
        package = "ADvizR"
    )  

    } else {
      stop("normative_baseline_space must be either 'MNI' or 'UCL'")
    }
    cat("Control means file:", control_means_path, "\n")
    return(control_means_path)
}

#' Helper function to get path to control data standard deviations
#' @param normative_baseline_space String indicating whether the normative space is
#' "MNI" or "UCL" (default = "MNI").
get_control_baseline_sds <- function(normative_baseline_space = "MNI") {

  if(normative_baseline_space == "UCL"){

    control_sds_path <- system.file(
        file.path("extdata", "Control_Baseline", "old_eddy", "control_NODDI_sds.RData"),
        package = "ADvizR"
    )  

    } else if(normative_baseline_space == "MNI"){

    control_sds_path <- system.file(
        file.path("extdata", "Control_Baseline", "control_NODDI_sds.RData"),
        package = "ADvizR"
    )  

    } else {
      stop("normative_baseline_space must be either 'MNI' or 'UCL'")
    }
    cat("Control standard deviations file:", control_sds_path, "\n")

    return(control_sds_path)
}

#' Helper function to get paths for UCL to MNI transformation
get_ucl_to_mni_transformation_paths <- function() {
    warped <- system.file(
        file.path("extdata", "Templates", "UCL_2_MNI152_Warped.nii.gz"),
        package = "ADvizR"
    )
    warp <- system.file(
        file.path("extdata", "Templates", "UCL_2_MNI152_1Warp.nii.gz"), 
        package = "ADvizR"
    )
    genericaffine <- system.file(
        file.path("extdata", "Templates", "UCL_2_MNI152_0GenericAffine.mat"),
        package = "ADvizR"
    )

    return(list(
        "warped" = warped,
        "warp" = warp,
        "genericaffine" = genericaffine
    ))
}