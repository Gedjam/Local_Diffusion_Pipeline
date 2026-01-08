#' Run full diffusion abnormalities pipeline.
#'
#' Calculates diffussion abnormalities from L1, L2, and L3 images, registers to MNI
#' space, and converts abnormalities to NIFTI R object.
#'
#' @param l1_input_nifti NIFTI (.nii.gz) file of L1 tensor values,
#' registered to the template (UCL) space.
#' @param l2_input_nifti NIFTI (.nii.gz) file of L2 tensor values,
#' registered to the template (UCL) space.
#' @param l3_input_nifti NIFTI (.nii.gz) file of L3 tensor values,
#' registered to the template (UCL) space.
#' @param neuro_output_dir String, path to directory for saving outputs for registration
#' to MNI space; directory will be created if does not exist.
#' @param normative_baseline_space String indicating whether the normative space is
#' "MNI" or "UCL" (default = "MNI").
#' @return A NIFTI R object of abnormality values in standard (MNI-152) space
#' @export
#'
calculate_and_output_abnormalities <- function(l1_input_nifti,
                                               l2_input_nifti,
                                               l3_input_nifti,
                                               neuro_output_dir,
                                               normative_baseline_space = "MNI") {    

  # Create output directory for abnormalities niftis if does not already exist
  dir.create(neuro_output_dir, showWarnings = FALSE)

  # Set up subdirectory for temporary (intermediate) outputs
  tmp_dir <- "tmp"         
  dir.create(file.path(neuro_output_dir, tmp_dir), showWarnings = FALSE)                          

  # Set up logging
  logout <- file.path(neuro_output_dir, tmp_dir, "R_logs.txt")
  logout_f <- file(logout, open = "wt")
  sink(logout_f, type="output")                                          
  sink(logout_f, type="message") 

  #### Step 1: Read in tensor values for subject ####

  # Set required diffusion metrics
  required_diffusion_metrics <- c("L1", "L2", "L3")

  # Set the subject ID manually here
  # this should be a user input on the website
  subject_id <- "Test_ID"

  # Read in first diffusion metric
  # this is also to get number of voxels for the size of the tibble
  subject_diffusion_data_l1 <- oro.nifti::readNIfTI(l1_input_nifti)

  # Create tibble for metrics
  subject_data <- dplyr::as_tibble(
    matrix(
      nrow = length(c(subject_diffusion_data_l1)),
      ncol = length(required_diffusion_metrics) + 2
    ),
    .name_repair = ~ c("ID", "VoxelID", required_diffusion_metrics)
  )

  # Add to tibble
  subject_data[, "ID"] <- subject_id
  subject_data[, "VoxelID"] <- dplyr::as_tibble(
    1:prod(methods::slot(subject_diffusion_data_l1, "dim_")[c(2, 3, 4)])
  )
  subject_data[, "L1"] <- c(subject_diffusion_data_l1)

  # Read in second diffusion metric and add to tibble
  subject_diffusion_data_l2 <- oro.nifti::readNIfTI(l2_input_nifti)
  subject_data[, "L2"] <- c(subject_diffusion_data_l2)
  rm(subject_diffusion_data_l2)

  # Read in second diffusion metric and add to tibble
  subject_diffusion_data_l3 <- oro.nifti::readNIfTI(l3_input_nifti)
  subject_data[, "L3"] <- c(subject_diffusion_data_l3)
  rm(subject_diffusion_data_l3)


  # Compute MD for data
  cat("Computing mean diffusivity\n")
  subject_data <- compute_all_md(subject_data)
  cat("Finished computing mean diffusivity\n")



  #### Step 2: Calculate abnormalities by comparison to a control cohort ####

  # Control cohort mean and std. dev. values have been pre-computed

  # Load pre-computed control data
  load("/Users/ged/Documents/Helping/dMRI_Pipeline_Local/R_Files/control_NODDI_means.RData")
  assign("control_means", control_NODDI_means)
  rm(control_NODDI_means)
  load("/Users/ged/Documents/Helping/dMRI_Pipeline_Local/R_Files/control_NODDI_sds.RData")
  assign("control_sds", control_NODDI_sds)
  rm(control_NODDI_sds)

  # Select only MD
  control_means <- control_means |> dplyr::select(VoxelID, MD)
  control_sds <- control_sds |> dplyr::select(VoxelID, MD)

  # Compute abnormalities (in template space)
  subject_abnormalities <- compute_abnormalities(
    subject_md_data = subject_data,
    control_means = control_means,
    control_sds = control_sds,
    absolute_abnormalities = TRUE
  )
  cat("Abnormalities computed\n")

  #### Step 3: Transform abnormalities to MNI space and remove abnormalities not in grey or white matter ####

  # Convert abnormalities to nifti and save
  subject_abnormalities_template_nifti <- output_abnormalities_to_nifti(
    abnormality_data_frame = subject_abnormalities,
    nifti_template = subject_diffusion_data_l1,
    abnormality_column = "MD_zscore"
  )
  abnormalities_template_file <- file.path(neuro_output_dir, "intermediate", "abnormalities_template") # file for abnormalities (template space)

  if(normative_baseline_space == "MNI") {

    cat("Abnormalities already in MNI space, no need to transform...")
    abnormalities_mni_file <- file.path(neuro_output_dir, tmp_dir, "abnormalities_mni_before_pTFCE") # file for transformed abnormalities (MNI space)
      oro.nifti::writeNIfTI(
      subject_abnormalities_template_nifti,
      filename = abnormalities_mni_file
    )

  } else if(normative_baseline_space == "UCL") {

    # Transform
    cat("Transforming abnormalities to MNI space...")
    abnormalities_mni_file <- file.path(neuro_output_dir, tmp_dir, "abnormalities_mni_before_pTFCE")
    transform_from_ucl_to_mni_space(abnormalities_template_file, abnormalities_mni_file)
    cat(
      "Transformed abnormalities to MNI space: ",
      abnormalities_mni_file,
      "\n"
    )

  } else {

    stop("normative_baseline_space must be either 'MNI' or 'UCL'")

  }

  # Load transformed abnormalities and update abnormalities tibble with correct values
  subject_abnormalities_mni_data <- oro.nifti::readNIfTI(abnormalities_mni_file)
  subject_abnormalities[, "MD_zscore"] <- c(subject_abnormalities_mni_data)

  cat("Updated abnormalities in tibble to MNI space\n")

  # Remove abnormalities not in grey or white matter
  # Get WM parcellation to derive WM mask - use 5mm of inwards dilation
  #atlas_and_roi_paths <- get_atlas_paths()
  subject_abnormalities <- set_voxels_outside_of_grey_and_white_matter_to_na(
    subject_abnormalities,
    "/Users/ged/Documents/Helping/dMRI_Pipeline_Local/R_Files/myaparc_60wm5max_Dil_with_Subcortical_Regions_2mm.nii.gz",
    "/Users/ged/Documents/Helping/dMRI_Pipeline_Local/R_Files/myaparc_60_with_subcortical_ROI_labels.txt"
  )

  cat("Removed abnormalities not in grey or white matter\n")

  #### Step 4: Apply probabilistic threshold free clustering enhancement ####

  # Transformation before we apply pTFCE
  # it needs input as z-scores. Will transform back later
  subject_abnormalities$MD_zscore[subject_abnormalities$MD_zscore == 0] <- 0.0001 # ensures no 0 values
  md_scaled <- scale(log(abs(subject_abnormalities$MD_zscore)))
  subject_abnormalities$MD_zscore_abs_std <- md_scaled[, 1]

  # Get mask for pTFCE
  ptfce_mask <- subject_abnormalities |>
    dplyr::mutate(Mask = dplyr::case_when(
      stringr::str_detect(
        ROI, "Vent|Brain-Stem|choroid|CSF|Cerebellum",
        negate = TRUE
      ) ~ 1,
      stringr::str_detect(
        ROI, "Vent|Brain-Stem|choroid|CSF|Cerebellum"
      ) ~ 0
    ))
  ptfce_mask <- array(ptfce_mask$Mask, dim = dim(subject_diffusion_data_l1))
  mask <- subject_diffusion_data_l1
  methods::slot(mask, ".Data") <- ptfce_mask


  # MD - Transform to array for pTFCE
  subject_abnormalities_md_array <-
    array(subject_abnormalities$MD_zscore_abs_std,
      dim = dim(subject_diffusion_data_l1)
    )
  subject_md_nifti <- subject_diffusion_data_l1
  methods::slot(subject_md_nifti, ".Data") <- subject_abnormalities_md_array

  cat("\nmax abnormality before ptfce: ", max(subject_abnormalities_md_array, na.rm=TRUE))


  library(oro.nifti) # hack to use pTFCE outside of package set-up
  # Apply pTFCE - will ignore anything below 0 but that corresponds to small MD abnormality so OK #nolint
  # Used tryCatch since some subjects could fail when Direction was applied
  ptfce_md <- tryCatch(
    pTFCE::ptfce(subject_md_nifti, mask),
    error = function(e) NA
  )


  # If there were any NaNs before pTFCE
  # then ensure that they are still NaNs afterwards
  if (any(is.na(ptfce_md))) {
    # Add to original data frame
    subject_abnormalities$MD_zscore_abs_std_pTFCE <- NA
  } else {
    # Add to original data frame
    subject_abnormalities$MD_zscore_abs_std_pTFCE <-
      c(methods::slot(ptfce_md$Z, ".Data"))

    # Set original MD NAs to NAs in pTFCE column
    subject_abnormalities[is.na(subject_abnormalities$MD_zscore), ]$MD_zscore_abs_std_pTFCE <- NA # nolint
  }

  cat("\nmax abnormality after ptfce (NaNs added): ", max(subject_abnormalities$MD_zscore_abs_std_pTFCE, na.rm=TRUE))


  # Transform back
  subject_abnormalities$MD_pTFCE <-
    exp(subject_abnormalities$MD_zscore_abs_std_pTFCE *
          attr(md_scaled, "scaled:scale") +
          attr(md_scaled, "scaled:center"))

  cat("Applied pTFCE\n")

  cat("\nmax abnormality (final)): ", max(subject_abnormalities$MD_pTFCE, na.rm=TRUE))


  #### Step 5: Output abnormalities for visualisation ####

  subject_abnormalities_nifti <- output_abnormalities_to_nifti(
    abnormality_data_frame = subject_abnormalities,
    nifti_template = subject_diffusion_data_l1,
    abnormality_column = "MD_pTFCE"
  )
  cat("Abnormalities formatted as NIFTI\n")

  abnormalities_final_file <- file.path(neuro_output_dir, "abnormalities_final") # 
  oro.nifti::writeNIfTI(
    subject_abnormalities_nifti,
    filename = abnormalities_final_file
  )

  #### Added Step 6: Via Ged
  clusters <- get_significant_clusters(subject_abnormalities_nifti,
                                      abnormality_threshold = 3,
                                      max_number_of_clusters = 20)
  cat("Saving clusters NIFTI file…\n")
  oro.nifti::writeNIfTI(clusters,
                        filename = file.path(
                          neuro_output_dir,
                          paste0("abnormality_clusters_binarised")
                        ))
  cat("Saved clusters NIFTI file\n")

  sink()

}



#' Transform NIFTI image from UCL (template) to MNI (standard) space.
#'
#' Uses ANTs neuroimaging software.
#'
#' @param input_nifti_file String, path to input NIFTI (.nii.gz) file in template (UCL)
#' space.
#' @param output_nifti_file String, path for NIFTI (.nii.gz) file registered to MNI
#' space that is created by this function.
#'
transform_from_ucl_to_mni_space <- function(input_nifti_file, output_nifti_file) {

  # get files
  transform_paths <- get_ucl_to_mni_transformation_paths()

  # transform
  command <- paste0(
    "antsApplyTransforms -d 3 -i ", input_nifti_file,
    " -r ", transform_paths$warped,
    " -o ", output_nifti_file,
    " -t ", transform_paths$warp,
    " -t ", transform_paths$genericaffine
  )
  system_with_error_handling(command)
}

