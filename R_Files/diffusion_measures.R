# Diffusion measures -----------------------------------------------------------

#' Compute mean diffusivity from diffusion tensor eigenvalues
#'
#' @description
#' Compute the mean diffusivity (MD) from one set of diffusion tensor
#' eigenvalues.
#' To compute MD,
#' 1) all eigenvalues must be greater than or equal to zero.
#' 2) at least one eigenvalue must nonzero.
#'
#' If these criteria is not satisfied, this function returns NaN.
#'
#' @param l1 First eigenvalue (numeric).
#' @param l2 Second eigenvalue (numeric).
#' @param l3 Third eigenvalue (numeric).
#'
#' @return Mean diffusivity (numeric).
#' @export
#'
#' @examples
#' compute_md(100, 200, 300.5)
compute_md <- function(l1, l2, l3) {
  # Check if eigenvalues are valid
  eigenvalues_valid <- check_eigenvalues_valid(l1, l2, l3)

  # If valid, compute MD; otherwise, set MD to NaN
  if (eigenvalues_valid) {
    md <- (l1 + l2 + l3) / 3
  } else {
    md <- NaN
  }

  return(md)
}

#' Compute mean diffusivity for each row of a data frame
#'
#' @param tensor_data_frame A data frame containing (at least) columns
#' corresponding to three diffusion eigenvalues. Each row is a unique voxel.
#' @param l1_col Column name containing first eigenvalues (string).
#' @param l2_col Column name containing second eigenvalues (string).
#' @param l3_col Column name containing third eigenvalues (string).
#'
#' @return A data frame, with columns corresponding to three diffusion
#' eigenvalues replaced with a column corresponding to mean diffusivity.
#' Each row is a unique voxel.
#' @export
#'
#' @examples
#' compute_all_md(
#'   data.frame(
#'     ID = rep("subject_ID", 3),
#'     VoxelID = seq(1:3),
#'     L1 = c(0.1, 0.4, 0.2),
#'     L2 = c(0.2, -0.1, 0.3),
#'     L3 = c(0.5, 0.4, 0)
#'   )
#' )
compute_all_md <- function(tensor_data_frame,
                           l1_col = "L1",
                           l2_col = "L2",
                           l3_col = "L3") {
  # Compute MD for each row of data frame
  md_data_frame <- tensor_data_frame |>
    dplyr::rowwise() |>
    dplyr::mutate(MD = compute_md(
      l1 = get(l1_col),
      l2 = get(l2_col),
      l3 = get(l3_col)
    )) |>
    dplyr::select(-(dplyr::all_of(c(
      l1_col,
      l2_col,
      l3_col
    )))) |>
    dplyr::ungroup()

  return(md_data_frame)
}

# Eigenvalue validity checks ---------------------------------------------------
# Determines whether eigenvalues can be used to compute desired diffusion
# measures.

#' Check whether the diffusion tensor eigenvalues have valid values.
#'
#' @param l1 First eigenvalue (numeric).
#' @param l2 Second eigenvalue (numeric).
#' @param l3 Third eigenvalue (numeric).
#'
#' @return Whether eigenvalues are valid (boolean)
check_eigenvalues_valid <- function(l1, l2, l3) {
  # Either throw error (if indicates breaking pipeline issue) or marked invalid

  # Validity checks
  eigenvalues_not_negative <- check_no_eigenvalues_negative(l1, l2, l3)
  eigenvalues_not_all_zero <- check_eigenvalues_not_all_zero(l1, l2, l3)

  # Eigenvalues are valid if not negative AND not all zero
  return(eigenvalues_not_negative & eigenvalues_not_all_zero)
}


# Individual validity checks for eigenvalues -----------------------------------
# These checks are set up to return TRUE for the desired scenarios

#' Check that none of the diffusion tensor eigenvalues have negative values.
#'
#' @param l1 First eigenvalue (numeric).
#' @param l2 Second eigenvalue (numeric).
#' @param l3 Third eigenvalue (numeric).
#'
#' @return Whether no eigenvalues are negative (i.e., whether all eigenvalues
#' are greater than or equal to zero) (boolean). TRUE is the desired scenario
#' that indicates that the eigenvalues pass this validity check.
check_no_eigenvalues_negative <- function(l1, l2, l3) {
  # Check all values greater than or equal to 0
  return((l1 >= 0) & (l2 >= 0) & (l3 >= 0))
}

#' Check that the diffusion tensor eigenvalues are not all zero.
#'
#' @param l1 First eigenvalue (numeric).
#' @param l2 Second eigenvalue (numeric).
#' @param l3 Third eigenvalue (numeric).
#'
#' @return Whether eigenvalues are not all zero (boolean). TRUE is the desired
#' scenario that indicates that the eigenvalues pass this validity check.
check_eigenvalues_not_all_zero <- function(l1, l2, l3) {
  # Check that at least one value is not 0
  return((l1 != 0) | (l2 != 0) | (l3 != 0))
}

#' Count the number of invalid eigenvalues in a column of a data frame
#'
#' @param tensor_data_frame A data frame containing (at least) columns
#' corresponding to three diffusion eigenvalues. Each row is a unique voxel.
#' @param l1_col Column name containing first eigenvalues (string).
#' @param l2_col Column name containing second eigenvalues (string).
#' @param l3_col Column name containing third eigenvalues (string).
#'
#' @return An integer indicating the number of rows of the data frame with
#' invalid eigenvalues.
#' @export
#'
#' @examples
#' count_invalid_eigenvalues(
#'   data.frame(
#'     ID = rep("subject_ID", 3),
#'     VoxelID = seq(1:3),
#'     L1 = c(0.1, 0.4, 0.2),
#'     L2 = c(0.2, -0.1, 0.3),
#'     L3 = c(0.5, 0.4, 0)
#'   )
#' )
count_invalid_eigenvalues <- function(tensor_data_frame,
                                      l1_col = "L1",
                                      l2_col = "L2",
                                      l3_col = "L3") {
  # Apply check to each row of data frame and count output
  number_invalid_eigenvalues <- sum(apply(tensor_data_frame, 1, function(x) {
    check_eigenvalues_valid(
      l1 = as.numeric(x[l1_col]),
      l2 = as.numeric(x[l2_col]),
      l3 = as.numeric(x[l3_col])
    )
  })
  == FALSE)

  return(number_invalid_eigenvalues)
}


# Column data frame NaN checks -------------------------------------------------
# This returns the number of NaN values in column of a data frame

#' Count the number of NaN values in a column of a data frame
#'
#' @param data_frame A data frame.
#' @param column Column name (string), by default this is "MD".
#'
#' @return The number of NaNs in selected column.
#' @export
#'
#' @examples
#' count_column_nans(
#'   data_frame = data.frame(
#'     ID = rep("subject_ID", 3),
#'     VoxelID = seq(1:3),
#'     MD = c(0.1, NaN, 0.2)
#'   ),
#'   column = "MD"
#' )
count_column_nans <- function(data_frame, column = "MD") {
  # Count number of NaNs
  number_of_nans <- sum(is.nan(data_frame[[column]]))

  return(number_of_nans)
}
