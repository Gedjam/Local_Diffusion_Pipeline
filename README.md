# CNNP Research Grade Pipeline Version 1
This is the abnormality detection pipeline for the CNNP lab, created by Gerard R Hall (gerard.hall@newcastle.ac.uk). 
Designed to find abnormalities from diffusion weighted MRI scans 

## Instructions for use

1. **Step 1:** Open Terminal 

2. **Step 2:** Type: *bash*

3. **Step 3:** Type: *cd /path/to/dMRI_Pipeline_Local*

4. **Step 4:** Type: *python3.12 CNNP_Research_Pipeline.py -dwi  /full/path/to/DWI_image.nii.gz -t1 /full/path/to/T1_image.nii.gz -bvec /full/path/to/BVEC_file.bvec -bval /full/path/to/BVAL_file.bval -o /full/path/to/Desired_Output_Location -ID Name_Prefix --NODDI_1p6*

## Additional notes

- The --NODDI_1P6 switch is optional and should only be switched on for the "NODDI 1.6mm" acquistion ran at chalfont. If not, simply don't type the "--NODDI_1p6" switch.

- The "dMRI_Pipeline_Local" is the Local_Diffision_Pipeline directory.

- Don't forget to install *install.packages("devtools")* and  *install_github("spisakt/pTFCE@v0.2.2.1")* inside R

- The output path is a folder that already has to exist! A new folder will be created from the *ID* name inside the output folder.

- The output abnormality files are located in *"Abnormality_Maps/abnormalities_final.nii.gz"* and *"Abnormality_Maps/abnormality_clusters_binarised.nii.gz"*. Open these in an image viewer (e.g. fsleyes, ITKsnap, etc) and view the output!  
  - *"Abnormality_Maps/abnormalities_final.nii.gz"* is the full brain image with the adjusted z-scores measuring abnormalities
  - *"Abnormality_Maps/abnormality_clusters_binarised.nii.gz"* is an image containing the abnormality masks, each one labelled an interger (e.g. 1, 2, 3, ...). Each number sequentially represents the most abnormal cluster being number: 1, second most abnormal cluster being number: 2, to the least abnormal cluster (highest number).

    
