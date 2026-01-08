#' Set voxels outside of grey and white matter to NAs in subject abnormalities data
#' frame.
#'
#' @param subject_abnormalities A data frame with subject abnormalities; must have
#' columns VoxelID and MD_zscore.
#' @param atlas_path Path to atlas NIFTI file.
#' @param roi_path Path to text file with ROI labels for the atlas.
#'
#' @return Amended subject_abnormalities data frame with voxels outside of grey and
#' white matter set to NA.
#'
set_voxels_outside_of_grey_and_white_matter_to_na <- function(
    subject_abnormalities,
    atlas_path,
    roi_path
) {

    # Load atlas
    atlas_tibble <- load_atlas_as_tibble(atlas_path, roi_path)

    # Join abnormalities with atlas
    subject_abnormalities <- join_to_atlas(
      data_frame = subject_abnormalities,
      atlas = atlas_tibble,
      joining_column = "VoxelID"
    )

    # Set abnormalities in non-ROIs to NA
    subject_abnormalities <- set_voxels_to_na(
      data_frame = subject_abnormalities,
      column_values_to_set_as_na = "MD_zscore",
      column_region = "ROI",
      regions_to_set_as_na = c(
        "Vent", "Brain-Stem",
        "choroid", "CSF",
        "Cerebellum", "unknown"
      )
    )

    return(subject_abnormalities)
}

#' Create tibble of atlas data
#'
#' @param atlas_path Path to atlas NIFTI file.
#' @param roi_path Path to text file with ROI labels for the atlas.
#'
#' @return Amended subject_abnormalities data frame with voxels outside of grey and
#' white matter set to NA.
#' @export
load_atlas_as_tibble <- function(atlas_path, roi_path) {

    # load atlas
    atlas <- oro.nifti::readNIfTI(atlas_path) # nolint

    # convert to array
    atlas_array <- methods::slot(atlas, ".Data")

    # convert to tibble
    atlas_tibble <- dplyr::tibble(
      VoxelID = seq_len(length(atlas_array)),
      ROI_ID = c(atlas_array)
    )

    # add ROI labels
    roi_labels <- readr::read_delim(roi_path, col_names = FALSE) |> # nolint
      dplyr::rename(ROI_ID = X1, ROI = X2)
    atlas_tibble <- atlas_tibble |> dplyr::left_join(roi_labels)

    return(atlas_tibble)
}

#' Joins data frame to a given atlas.
#'
#' @param data_frame A data frame, where each row represents a unique voxel.
#' @param atlas An atlas (parcellation scheme), specifying to which ROI
#' each voxel is assigned.
#' @param joining_column A string, the column name on which to join.
#'
#' @return A data frame, where each row represents a unique voxel, with a
#' column indicating the ROI assignment of each voxel.
#' @export
#'
#' @examples
#' join_to_atlas(
#'   data_frame = data.frame(
#'     ID = rep("subject_ID", 3),
#'     VoxelID = seq(1:3),
#'     MD = c(0.35, 0.43, 0.29)
#'   ),
#'   atlas = data.frame(
#'     VoxelID = seq(1:3),
#'     ROI = c("Right-Hippocampus", "Right-Hippocampus", "Left-Hippocampus"),
#'     ROI_ID = c(53, 53, 17)
#'   ),
#'   joining_column = "VoxelID"
#' )
join_to_atlas <- function(data_frame,
                          atlas,
                          joining_column = "VoxelID") {
  # Join with atlas
  data_frame <- data_frame |>
    dplyr::left_join(atlas, by = joining_column)

  return(data_frame)
}

#' Sets voxels assigned to given ROIs as NA
#'
#' @param data_frame A data frame, where each row represents a unique voxel.
#' @param column_values_to_set_as_na A string, the column name of values to
#' (conditionally) set as NA.
#' @param column_region A string, the column name of regions.
#' @param regions_to_set_as_na A vector of strings of region names. Voxels
#' assigned region names containing these strings will be set to NA.
#'
#' @return A data frame, where each row represents a unique voxel.
#' @importFrom rlang :=
#' @export
#'
#' @examples
#' set_voxels_to_na(
#'   data_frame = data.frame(
#'     ID = rep("subject_ID", 3),
#'     VoxelID = seq(1:3),
#'     MD = c(0.35, 0.43, 0.29),
#'     ROI = c("Right-Hippocampus", "Brain-Stem", "Left-Hippocampus")
#'   ),
#'   column_values_to_set_as_na = "MD",
#'   column_region = "ROI",
#'   regions_to_set_as_na = c(
#'     "Vent", "Brain-Stem",
#'     "choroid", "CSF",
#'     "Cerebellum", "unknown"
#'   )
#' )
set_voxels_to_na <- function(data_frame,
                             column_values_to_set_as_na = "MD",
                             column_region = "ROI",
                             regions_to_set_as_na = NULL
                             ) {

  # Get default regions to set as na
  if (is.null(regions_to_set_as_na)) {
    regions_to_set_as_na <- get_roi_strings_to_exclude()
  }
  # Set abnormalities in non-ROIs to NA
  data_frame <- data_frame |>
    dplyr::mutate(!!dplyr::sym(column_values_to_set_as_na) := dplyr::case_when(
      stringr::str_detect(!!dplyr::sym(column_region),
        paste0(regions_to_set_as_na, collapse = "|"),
        negate = TRUE
      ) ~
        !!dplyr::sym(column_values_to_set_as_na),
      stringr::str_detect(
        !!dplyr::sym(column_region),
        paste0(regions_to_set_as_na, collapse = "|")
      ) ~
        NA,
      is.na(!!dplyr::sym(column_region)) ~ NA
    ))

  return(data_frame)
}

#' Helper function to get strings for excluding ROIs from analysis.
#' Should be used to remove ROIs using partial, not exact, matches (e.g., using
#' stringr::str_detect).
#' Note: this list is designed to work with the
#' myaparc_60wm5max_Dil_with_Subcortical_Regions_2mm.nii.gz atlas - list may neec
#' to be amended for other atlases.
#'
#' @return List of strings for excluding ROIs
get_roi_strings_to_exclude <- function() {
  # for "myaparc_60wm5max_Dil_with_Subcortical_Regions_2mm.nii.gz"
  return(c("Vent", "Brain-Stem", "choroid", "CSF", "Cerebellum", "unknown"))
}
