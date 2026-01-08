# Utility printing function (missing in original code) 
header_cat <- function(text, level = "h2") { 
  cat("\n==============================\n") 
  cat(text, "\n") 
  cat("==============================\n\n") 
}

#' Function to calculate initial colour bar value for website
#'
#' @param subject_abnormalities_nifti NIFTI R object output from pipeline
#'
#' @return A double (numeric value) to be used as the initial colour bar value
#' for website
#' @export
#'
get_initial_colourbar_value <- function(subject_abnormalities_nifti){

  header_cat("COMPUTING INITIAL COLOURBAR VALUE", "h2")

  # Get a vector of voxel-wise abnormality values
  subject_abnormalities_vector <- c(subject_abnormalities_nifti)

  # Remove NAs (or zeros) that were (primarily) assigned to voxels outside of the brain
  subject_abnormalities_vector <- subject_abnormalities_vector[!is.na(subject_abnormalities_vector)]
  subject_abnormalities_vector <- subject_abnormalities_vector[subject_abnormalities_vector != 0]

  # Set 99.99th percentile as the initial colour bar value
  # Figure derived from manual testing
  initial_colour_bar_value <- unname(
    stats::quantile(
      subject_abnormalities_vector,
      probs = 0.9999
    )
  )

  cat("Computed initial colourbar value:", initial_colour_bar_value, "\n")

  # Return value
  return(initial_colour_bar_value)

}

# Functions for computing and analysing abnormality clusters -------------------

#' Function to compute (significant) clusters and return a nifti of clusters
#'
#' @param subject_abnormalities_nifti NIFTI R object output from pipeline
#' @param abnormality_threshold A double (numeric value) to be used as the
#' threshold for creating clusters, set to 3 by default
#' @param max_number_of_clusters A double (numeric value), set to 9 be default.
#' The maximum number of abnormality clusters visualised. Smaller (in size)
#' clusters will be set to 0.
#'
#' @return NIFTI R object containing substantial clusters, where 1 is
#' the largest cluster, 2 is the second largest (if it exists) etc.
#' @export
#'
get_significant_clusters <- function(subject_abnormalities_nifti,
                                     abnormality_threshold = 3,
                                     max_number_of_clusters = 9){

  header_cat("COMPUTING ABNORMALITY CLUSTERS", "h2")

  #Get dimensions of nifti
  subject_abnormalities_array <- methods::slot(subject_abnormalities_nifti, ".Data")

  #Threshold
  subject_abnormalities_array_thresholded <- ifelse(subject_abnormalities_array > abnormality_threshold,
                                                    1,
                                                    0)

  #Set up shape kernel for clusters (from mmand package)
  #Find connected components uses a 3x3x3 box to detemine connections
  k <- mmand::shapeKernel(c(3,3,3), type="box")

  #Set models for determining important clusters
  model_ranking = list(Cluster_Count ~ 1, 1~1)  # two intercept-only segments

  #Get cluster components
  subject_abnormalities_array_components <- mmand::components(
    subject_abnormalities_array_thresholded,
    k)

  #Get cluster information and counts
  cluster_tibble <- dplyr::tibble(Cluster_Number = names(table(subject_abnormalities_array_components)),
                                   Cluster_Count = as.numeric(table(subject_abnormalities_array_components)),
                                   Cluster_Rank = dplyr::row_number(dplyr::desc(Cluster_Count)),
                                   Cluster_Proportion = Cluster_Count / sum(Cluster_Count))

  #Get number of substantial clusters, using change point analysis and a gamma prior
  number_substantial_clusters <- tryCatch(summary(mcp::mcp(model_ranking,
                                                           data = cluster_tibble,
                                                           par_x = "Cluster_Rank",
                                                           prior = list(cp_1 = "dgamma(3, 6)"))) |>
                                            dplyr::filter(name == "cp_1") |>
                                            dplyr::select(mean) |>
                                            dplyr::pull() |>
                                            round(), error=function(e) nrow(cluster_tibble))

  #If abnormalities - add which cluster they were in
  substantial_clusters <- cluster_tibble |>
    dplyr::arrange(Cluster_Rank) |>
    dplyr::slice_head(n = number_substantial_clusters)

  # Create a named vector for mapping cluster numbers to ranks
  cluster_rank_map <- stats::setNames(substantial_clusters$Cluster_Rank, substantial_clusters$Cluster_Number)

  # Replace array values with ranks, setting unmatched clusters to NA
  subject_substantial_clusters_array <- array(ifelse(
    is.na(subject_abnormalities_array_components), NA,
    cluster_rank_map[as.character(subject_abnormalities_array_components)]
  ), dim = dim(subject_abnormalities_array_components))

  #Set all NAs to zeros for visualisation
  subject_substantial_clusters_array[is.na(subject_substantial_clusters_array)] <- 0

  #Display no more clusters than max_number_of_clusters (default = 9)
  subject_substantial_clusters_array[subject_substantial_clusters_array > max_number_of_clusters] <- 0

  #Create nifti copy
  substantial_clusters_nifti <- subject_abnormalities_nifti

  # Update template data with abnormalities
  methods::slot(substantial_clusters_nifti, ".Data") <-
    subject_substantial_clusters_array

  cat("Finished computing abnormality clusters\n")

  #Return value
  return(substantial_clusters_nifti)

}

#' Compute ROI and abnormality info for each cluster.
#'
#' Outputs include
#'    ROIs in each cluster
#'    For ROIs in a cluster, the proportion and volume of the ROI in the cluster
#'    Size of each cluster (in terms of number of voxels and mm3)
#'    Max abnormality in each cluster
#'    Location of the max abnormality in terms of (x, y, z) voxel coordinates
#'
#' @param clusters_nifti Subject abnormality clusters as a NIFTI R object
#' (computed from get_significant_clusters)
#' @param abnormalities_nifti Subject abnormalities as a NIFTI R object
#' @param atlas_path Path to atlas NIFTI file.
#' @param roi_path Path to text file with ROI labels for the atlas.
#' @param roi_strings_to_exclude (optional) List of strings for identifying ROIs
#' to exclude from the analysis (ROIs containing these strings are excluded)
#' @param sort_rois_by (optional, default "propInCluster") String, how ROIs within
#' each cluster should be ordered. Options are by proportion in cluster
#' ("propInCluster") or by volume in cluster ("volumeInCluster")
#' @param convert_to_json (optional, default TRUE) Boolean, whether to convert
#' tibble results to JSON
#' @param pretty_json (optional, default FALSE) Boolean, whether JSON should be
#' in "pretty" format with indentation and new lines
#'
#' @return Named list with two elements: cluster_data (a summary of results by
#' cluster) and roi_prop_in_cluster (output from function
#' compute_proportion_of_roi_in_each_cluster, see that function for a
#' description).
#'
#' cluster_data has one entry per abnormality cluster (no cluster results are
#' excluded), with variables
#'    clusterId: voxel abnormality cluster (0 = no cluster)
#'    abnormality: voxel abnormality
#'    x: voxel x coordinate
#'    y: voxel y coordinate
#'    z: voxel z coordinate
#'    nVoxels: size of cluster (number of voxels)
#'    volumeMm3: size of the cluster (volume, mm3)
#'    nRois: number of ROIs in the cluster
#'    roisInCluster: list of names of ROIs in the cluster
#'    roisPropInCluster: list of proportion of each ROI that is in the cluster
#'    roisVolumeMm3InCluster: list of the volume of each ROI (in mm3) that is
#'      in the cluster.
#'
#' If convert_to_json is TRUE, this data is converted to JSON format; otherwise,
#' the data is a tibble.
#'
#' @export
#'
compute_cluster_roi_and_abnormality_data <- function(
    clusters_nifti,
    abnormalities_nifti,
    atlas_path,
    roi_path,
    roi_strings_to_exclude = NULL,
    sort_rois_by = "propInCluster",
    convert_to_json = TRUE,
    pretty_json = FALSE
) {

  header_cat("COMPUTING ROI AND ABNORMALITY INFO FOR ABNORMALITY CLUSTERS", "h2")

  # Set default ROI strings to exclude if not specified
  if (is.null(roi_strings_to_exclude)) {
    roi_strings_to_exclude <- get_roi_strings_to_exclude()
  }

  # Create tibble for abnormalities and clustering results
  clusters_tibble <- make_clusters_and_abnormalities_tibble(
    clusters_nifti, abnormalities_nifti
  )

  # Compute size of each cluster
  clusters_n_voxels <- compute_number_of_voxels_per_cluster(clusters_tibble)

  # Get max abnormality in each cluster and the location of the max abnormality
  cluster_max_abnormalities <- compute_max_abnormality_per_cluster(clusters_tibble)

  # Compute proportion of each ROI in each cluster
  roi_prop_in_cluster <- compute_proportion_of_roi_in_each_cluster(
    clusters_tibble,
    atlas_path,
    roi_path,
    roi_strings_to_exclude = roi_strings_to_exclude,
    sort_rois_by = sort_rois_by
  )

  # TODO: consider adding nice labels for ROIs for report

  # Create tibble with all cluster data (by cluster)

  # ROI info for each cluster as lists
  roi_lists <- roi_prop_in_cluster |>
    dplyr::select(clusterId, roi, propInCluster, volumeInCluster) |>

    # Lists of ROIs and their metadata, by cluster
    dplyr::group_by(clusterId) |>
    dplyr::summarise(
      nRois = dplyr::n(), # also add number of ROIs in each cluster
      roisInCluster = list(roi),
      roisPropInCluster = list(propInCluster),
      roisVolumeMm3InCluster = list(volumeInCluster)
    )

  # Combine cluster data
  cluster_data <- dplyr::full_join(
    cluster_max_abnormalities, clusters_n_voxels, by = "clusterId"
  )
  cluster_data <- cluster_data |> # Also compute volume of each cluster
    dplyr::mutate(volumeMm3 = nVoxels * 8)
  cluster_data <- dplyr::full_join(cluster_data, roi_lists, by = "clusterId")

  # Add ROIs and tidy
  cluster_data <- cluster_data |>
    dplyr::relocate(clusterId) |>   # Move clusterId to start
    dplyr::select(-voxelId)         # Remove voxel ID

  # Convert outputs to JSON (optional)
  if (convert_to_json) {
    cluster_data <- jsonlite::toJSON(
      cluster_data, dataframe = "rows", digits = 10, pretty=pretty_json
    )

    # Will also need full table of ROI proportions for some visualisations
    roi_prop_in_cluster <- jsonlite::toJSON(
      roi_prop_in_cluster, dataframe = "rows", digits = 10, pretty=pretty_json
    )
  }

  cat("Finished computing ROI and abnormality info for abnormality clusters\n")

  return(list(
    cluster_data = cluster_data,
    roi_prop_in_cluster = roi_prop_in_cluster
  ))

}


#' Create tibble with abnormality and abnormality cluster of each voxel.
#'
#' @param clusters_nifti Subject abnormality clusters as a NIFTI R object
#' (computed from get_significant_clusters)
#' @param abnormalities_nifti Subject abnormalities as a NIFTI R object
#'
#' @return A tibble of subject abnormalities and clusters with variables
#'    voxelId: voxel ID (unique numeric identifier for voxel)
#'    abnormality: voxel abnormality
#'    clusterId: voxel abnormality cluster (0 = no cluster)
#'    x: voxel x coordinate
#'    y: voxel y coordinate
#'    z: voxel z coordinate
#'
#' @export
#'
make_clusters_and_abnormalities_tibble <- function(
    clusters_nifti,
    abnormalities_nifti
) {

  # Initialise tibble
  clusters_tibble <- dplyr::as_tibble(
    matrix(
      nrow = length(clusters_nifti),
      ncol = 3
    ),
    .name_repair = ~ c("voxelId", "abnormality", "clusterId")
  )

  # Add voxel IDs and original coordinates (needed for determining cluster locations)
  dims <- methods::slot(clusters_nifti, "dim_")[c(2, 3, 4)]
  coords <- expand.grid(
    x = seq_len(dims[1]),
    y = seq_len(dims[2]),
    z = seq_len(dims[3])
  )
  clusters_tibble[, "voxelId"] <- seq_len(nrow(coords))
  clusters_tibble <- dplyr::bind_cols(clusters_tibble, coords)

  # Add abnormalities and cluster IDs to tibble
  clusters_tibble[, "abnormality"] <- c(abnormalities_nifti)
  clusters_tibble[, "clusterId"] <- c(clusters_nifti)

  return(clusters_tibble)

}

#' Compute size of each abnormality cluster.
#' Uses tibble created by make_clusters_and_abnormalities_tibble (with clusterId
#' variable) as input.
#'
#' @param clusters_tibble Tibble of abnormality and cluster info for each voxel
#' (created by function make_clusters_and_abnormalities_tibble)
#'
#' @return Tibble of the number of voxels in each cluster with variables
#'    clusterId: ID of cluster
#'    nVoxels: number of voxels in the specified cluster
#' The number of rows is equal to the number of clusters, and clusters are
#' ordered numerically from 1 to n clusters. No cluster (clusterId = 0) results
#' are not included in the output.
#'
compute_number_of_voxels_per_cluster <- function(clusters_tibble) {

  clusters_n_voxels <- clusters_tibble |>
    dplyr::filter(clusterId > 0) |> # remove no cluster (id = 0)
    dplyr::count(clusterId) |>
    dplyr::rename(nVoxels = n) |>
    dplyr::arrange(clusterId) # sort by cluster

  return(clusters_n_voxels)

}

#' Find the max abnormality in each cluster and the location of that value.
#'
#' @param clusters_tibble Tibble of abnormality and cluster info for each voxel
#' (created by function make_clusters_and_abnormalities_tibble)
#'
#' @return Tibble of the max abnormality in each cluster and the location of
#' that value, with variables
#'    voxelId: ID of the voxel with the max abnormality in the specified cluster
#'    abnormality: abnormality value of that voxel
#'    clusterId: ID of cluster
#'    x: x coordinate of the voxel (in standard MNI space)
#'    y: y coordinate of the voxel (in standard MNI space)
#'    z: z coordinate of the voxel (in standard MNI space)
#' The number of rows is equal to the number of clusters, and clusters are
#' ordered numerically from 1 to n clusters. No cluster (clusterId = 0) results
#' are not included in the output.
#'
compute_max_abnormality_per_cluster <- function(clusters_tibble) {

  cluster_max_abnormalities <- clusters_tibble |>
    dplyr::filter(clusterId > 0) |> # remove no cluster (id = 0)
    dplyr::group_by(clusterId) |>
    dplyr::slice_max(order_by = abnormality, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::arrange(clusterId)

  return(cluster_max_abnormalities)
}

#' Compute proportion of each region of interest (ROI) in each abnormality
#' cluster.
#'
#' @param clusters_tibble Tibble of abnormality and cluster info for each voxel
#' (created by function make_clusters_and_abnormalities_tibble)
#' @param atlas_path Path to atlas NIFTI file.
#' @param roi_path Path to text file with ROI labels for the atlas.
#' @param roi_strings_to_exclude (optional) List of strings for identifying ROIs
#' to exclude from the analysis (ROIs containing these strings are excluded)
#' @param sort_rois_by (optional, default "propInCluster") String, how ROIs within
#' each cluster should be ordered. Options are by proportion in cluster
#' ("propInCluster") or by volume in cluster ("volumeInCluster")
#'
#' @return Tibble providing the proportion and volume of each ROI in each
#' abnormality cluster. Only non-zero proportions are included. Variables:
#'    roiId: ID of each ROI (from atlas)
#'    roi: name of each ROI (from atlas)
#'    clusterId: numeric ID of each cluster
#'    nVoxels: number of voxels that the ROI has in the specified cluster
#'    propInCluster: proportion of the ROI that is in the specified cluster
#'    volumeInCluster: volume of the ROI that is in the specified cluster (mm3)
#' The tibble is sorted first by clusterId (ascending), then by the sort_rois_by
#' variable (descending), which is propInCluster by default.
#' No cluster (clusterId == 0) info is excluded from the tibble.
#'
compute_proportion_of_roi_in_each_cluster <- function(
    clusters_tibble,
    atlas_path,
    roi_path,
    roi_strings_to_exclude = NULL,
    sort_rois_by = "propInCluster"
) {

  # Set default ROI strings to exclude if not specified
  if (is.null(roi_strings_to_exclude)) {
    roi_strings_to_exclude <- get_roi_strings_to_exclude()
  }

  # Check that sort_rois_by is allowed value
  allowed_sort_rois_by <- c("propInCluster", "volumeInCluster")
  if (!sort_rois_by %in% allowed_sort_rois_by) {
    stop(
      paste(
        "Invalid input for sort_rois_by. Valid options are ",
        paste(allowed_sort_rois_by, collapse = ", ")
      )
    )
  }

  # Add atlas info to clusters tibble
  atlas_tibble <- load_atlas_as_tibble(atlas_path, roi_path)
  atlas_tibble <- atlas_tibble |>
    dplyr::rename(
      voxelId = VoxelID,
      roiId = ROI_ID,
      roi = ROI
    )

  clusters_with_rois_tibble <- clusters_tibble |>
    dplyr::left_join(atlas_tibble) |>

    # Remove no ROI voxels
    dplyr::filter(roiId > 0) |>

    # Remove ROIs that don't want to analyse (e.g., ventricles)
    dplyr::filter(
      stringr::str_detect(roi, paste0(roi_strings_to_exclude, collapse = "|"), negate = TRUE)
    )

  # Compute proportion and volume of each ROI in each cluster
  roi_prop_in_cluster <- clusters_with_rois_tibble |>

    # First get counts of each cluster/ROI combination
    dplyr::group_by(roiId, roi, clusterId) |>
    dplyr::summarise(nVoxels = dplyr::n()) |>
    dplyr::ungroup() |>

    # Compute proportions using counts for each ROI
    dplyr::group_by(roiId, roi) |>
    dplyr::mutate(propInCluster = nVoxels / sum(nVoxels)) |>
    dplyr::ungroup() |>

    # Also compute volume in cluster (mm3)
    dplyr::mutate(volumeInCluster = nVoxels * 8) |>

    # Arrange by cluster, then specified variable (e.g., propInCluster by default), descending
    dplyr::arrange(clusterId, dplyr::desc(!!dplyr::sym(sort_rois_by))) |>

    # Remove data on "cluster" 0 (i.e., no cluster)
    dplyr::filter(clusterId > 0)

  return(roi_prop_in_cluster)
}

#' Apply inverse transformation back to native (subject) space of abnormalities.
#'
#' @param input_dir A string, the relative or absolute path to the top-level directory
#' containing the registration outputs directory.
#' @param output_dir A string, the relative or absolute path to the top-level directory
#' for the native space abnormalities to be saved.
#' @param filename_native A string, the filename to use for the native space
#' abnormalities when they are saved in output_dir. Name should end in ".nii.gz".
#' @param registration_output_subdir A string, the subdirectory in input_dir that
#' contains the files created by the registration step preprocess_apply_registration
#' @param native_tensor_map_subdir A string, the subdirectory in input_dir that
#' contains the tensor maps in native space.
#' @return A string giving the full path (including filename) of abnormalities
#' in native (subject) space. 
#' @export
#'
transform_from_template_to_native_space <- function(input_dir,
                                               output_dir,
                                               filename_native = "abnormalities_native.nii.gz",
                                               registration_output_subdir = "09_registration_output",
                                               native_tensor_map_subdir = "07_tensor_metrics") {

  header_cat("TRANSFORMING ABNORMALITIES FROM TEMPLATE TO NATIVE SPACE", "h2")

  # Get file paths
  inverse_transform_paths <- list(genericaffine = file.path(input_dir, registration_output_subdir, "0GenericAffine.mat"),
                                  inversewarp = file.path(input_dir, registration_output_subdir, "1InverseWarp.nii.gz"))
  native_tensor_map_path <- list(FA = file.path(input_dir, native_tensor_map_subdir, "FA.nii.gz"))
  input_nifti_file = file.path(input_dir, "abnormalities_template.nii.gz")
  output_nifti_file = file.path(output_dir, filename_native)

  # Apply inverse of earlier transformation to template space
  command_inverse <- paste0(
    "antsApplyTransforms -d 3 -i ", input_nifti_file,
    " -r ", native_tensor_map_path$FA,
    " -o ", output_nifti_file,
    " -t [", inverse_transform_paths$genericaffine, ",1] ",
    " -t ", inverse_transform_paths$inversewarp
  )
  system_with_error_handling(command_inverse)

  cat("Finished transforming abnormalities\n")

  # Return path to output file, include filename
  return(output_nifti_file)
}

