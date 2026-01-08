# CNNP Research Grade Pipeline Version 1
This is the abnormality detection pipeline for the CNNP lab, created by Gerard R Hall (gerard.hall@newcastle.ac.uk). 
Designed to find abnormalities from diffusion weighted MRI scans 

## Instructions for use

Step 1: Open Terminal 

Step 2: Type: bash

Step 3: Type: python3.12 CNNP_Research_Pipeline.py -dwi  /full/path/to/DWI_image.nii.gz -t1 /full/path/to/T1_image.nii.gz -bvec /full/path/to/BVEC_file.bvec -bval /full/path/to/BVAL_file.bval -o /full/path/to/Desired_Output_Location --NODDI_1p6

