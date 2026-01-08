#!/usr/bin/env Rscript

# ---- Parse args ----
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 5) {
  stop("Usage: run_abnormalities.R <L1> <L2> <L3> <outdir> <MNI|UCL>")
}

# ---- Locate script directory ----
args_full <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_full, value = TRUE)
script_path <- normalizePath(sub("^--file=", "", file_arg))
script_dir <- dirname(script_path)

# ---- Source scripts ----
source(file.path(script_dir, "compute_abnormalities.R"))
source(file.path(script_dir, "output_abnormalities.R"))
source(file.path(script_dir, "calculate_and_output_abnormalities.R"))
source(file.path(script_dir, "diffusion_measures.R"))
source(file.path(script_dir, "atlas_functions.R"))
source(file.path(script_dir, "post_abnormality_functions.R"))
# ---- Run pipeline ----
calculate_and_output_abnormalities(
  l1_input_nifti  = args[1],
  l2_input_nifti  = args[2],
  l3_input_nifti  = args[3],
  neuro_output_dir = args[4],
  normative_baseline_space = args[5]
)
