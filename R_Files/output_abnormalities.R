#' Converts abnormality data frame to nifti for visualisation
#'
#' @param abnormality_data_frame A data frame, including a column of abnormality
#' values to be output.
#' @param nifti_template A NIFTI R object with the correct dimensions to
#' be used as a template.
#' @param abnormality_column A string indicating the abnormality column.
#'
#' @return A NIFTI R object of subject abnormalities.
#' @export
#'
output_abnormalities_to_nifti <- function(abnormality_data_frame,
                                          nifti_template,
                                          abnormality_column = "MD_zscore") {
  # Create a copy to be output
  subject_abnormalities_nifti <- nifti_template

  # Check format valid
  check_abnormality_format_valid(
    abnormality_data_frame,
    nifti_template,
    abnormality_column
  )

  # Check not all NA or zero
  check_abnormality_not_all_na(
    abnormality_data_frame,
    nifti_template,
    abnormality_column
  )
  check_abnormality_not_all_zero(
    abnormality_data_frame,
    nifti_template,
    abnormality_column
  )

  #Convert NAs to zeros for visualisation
  abnormality_data_frame[[abnormality_column]] <- ifelse(is.na(abnormality_data_frame[[abnormality_column]]),
                                                  0,
                                                  abnormality_data_frame[[abnormality_column]])

  # Update template data with abnormalities
  methods::slot(subject_abnormalities_nifti, ".Data") <-
    array(abnormality_data_frame[[abnormality_column]],
      dim = dim(nifti_template)
    )

  return(subject_abnormalities_nifti)
}


#' Checks length of abnormality vector is the same as the length of the
#' nifti template array. Function throws error if not.
#'
#' @param abnormality_data_frame A data frame, including a column of abnormality
#' values to be output.
#' @param nifti_template A NIFTI R object with the correct dimensions to
#' be used as a template.
#' @param abnormality_column A string indicating the abnormality column.
#'
#' @return A NIFTI R object of subject abnormalities.
#'
check_abnormality_format_valid <- function(abnormality_data_frame,
                                           nifti_template,
                                           abnormality_column) {
  # Throw error if incompatible
  if (length(abnormality_data_frame[[abnormality_column]]) !=
        length(c(methods::slot(nifti_template, ".Data")))) {
    stop("Error: incompatible format between abnormalities and nifti template")
  }
}

#' Checks all abnormalities are not all NA. Function throws warning if all NAs.
#'
#' @param abnormality_data_frame A data frame, including a column of abnormality
#' values to be output.
#' @param nifti_template A NIFTI R object with the correct dimensions to
#' be used as a template.
#' @param abnormality_column A string indicating the abnormality column.
#'
#' @return A NIFTI R object of subject abnormalities.
#'
check_abnormality_not_all_na <- function(abnormality_data_frame,
                                         nifti_template,
                                         abnormality_column) {
  # Throw error if incompatible
  if (all(is.na(abnormality_data_frame[[abnormality_column]]))) {
    warning("All abnormalities are NA. Please check inputs.")
  }
}

#' Checks all abnormalities are not all zero. Function throws warning if all
#' zero.
#'
#' @param abnormality_data_frame A data frame, including a column of abnormality
#' values to be output.
#' @param nifti_template A NIFTI R object with the correct dimensions to
#' be used as a template.
#' @param abnormality_column A string indicating the abnormality column.
#'
#' @return A NIFTI R object of subject abnormalities.
#'
check_abnormality_not_all_zero <- function(abnormality_data_frame,
                                           nifti_template,
                                           abnormality_column) {
  # Throw error if incompatible
  if ((all(!is.na(abnormality_data_frame[[abnormality_column]]))) &&
        (sum(abnormality_data_frame[[abnormality_column]],
             na.rm = TRUE) == 0)) {
    warning("All abnormalities are zero. Please check inputs.")
  }
}
