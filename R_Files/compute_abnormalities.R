#' Compute abnormalities for mean diffusivity (MD)
#'
#' @param subject_md_data A data frame, with columns including 'ID', 'VoxelID'
#' and 'MD', where each row is a unique voxel.
#' @param control_means A data frame, with columns including 'VoxelID'
#' and 'MD', where each row is a unique voxel. The 'MD' column contains the
#' normative mean MD value, derived from a healthy cohort.
#' @param control_sds A data frame, with columns including 'VoxelID'
#' and 'MD', where each row is a unique voxel. The 'MD' column contains the
#' normative std. dev. MD value, derived from a healthy cohort.
#' @param absolute_abnormalities A boolean, which is TRUE by default. When TRUE,
#' abnormalities output will be absolute z-score values. When FALSE,
#' abnormalities output will be z-scores.
#'
#' @return A data frame with columns including 'ID', 'VoxelID', 'MD_zscore',
#' 'ROI' and 'ROI_ID', where each row is a unique voxel.
#' @export
#'
#' @examples
#' compute_abnormalities(
#'   subject_md_data = data.frame(
#'     ID = rep("subject_ID", 3),
#'     VoxelID = seq(1:3),
#'     MD = c(0.35, 0.43, 0.29)
#'   ),
#'   control_means = data.frame(
#'     VoxelID = seq(1:3),
#'     MD = c(0.32, 0.41, 0.21)
#'   ),
#'   control_sds = data.frame(
#'     VoxelID = seq(1:3),
#'     MD = c(0.02, 0.01, 0.02)
#'   ),
#'   absolute_abnormalities = TRUE
#' )
compute_abnormalities <- function(subject_md_data,
                                  control_means,
                                  control_sds,
                                  absolute_abnormalities = TRUE) {
  # Copy subject data in a new data frame for abnormalities
  subject_abnormalities <- subject_md_data

  # Calculate subject abnormalities
  subject_abnormalities$MD_zscore <-
    (subject_abnormalities$MD - control_means$MD) / control_sds$MD

  # If 'absolute_abnormalities' is TRUE then get absolute value
  if (isTRUE(absolute_abnormalities)) {
    subject_abnormalities$MD_zscore <- abs(subject_abnormalities$MD_zscore)
  }

  # Select data to output
  subject_abnormalities <- subject_abnormalities |>
    dplyr::select(ID, VoxelID, MD_zscore) # ROI, ROI_ID)

  return(subject_abnormalities)
}
