import os
import sys
import argparse
import pandas as pd
import shutil

import Stage_1_DWI
import Stage_2_DWI
import Stage_3_DWI
import Stage_4_DWI
import Stage_5_DWI
import T1_Processing

parser = argparse.ArgumentParser(prog='CNNP_Research_Pipeline',
                                 description='This is the CNNP research Pipeline ran locally on this laptop',
                                 epilog='If you have any problems please contact me at gerard.hall@newcastle.ac.uk',
                                 add_help=True)

parser.add_argument("-dwi","--DWI_Image", help="File Pathway to DWI image (NIFTI: \".nii.gz\")")
parser.add_argument("-t1","--T1_Image", help="File Pathway to T1 image (NIFTI: \".nii.gz\")")
parser.add_argument("-bvec","--bvec_File", help="File Pathway to bvec file (Bvec: \".bvec\")")
parser.add_argument("-bval","--bval_File", help="File Pathway to bval file (Bval: \".bval\")")
parser.add_argument("-o","--Output_Folder_Location", help="Location Pathway for Output Folder (Output)")
parser.add_argument("-ID","--Identifier", help="File Pathway to Identifier file (ID example: \"1043\")")
args = vars(parser.parse_args())

DWI_Image = args['DWI_Image']
T1_Image = args['T1_Image']
bvec_File = args['bvec_File']
bval_File = args['bval_File']
Output_Folder = args['Output_Folder_Location']
Identifier = args['Identifier']

##Critical Paths hardcoded for now
MATLAB_PATH = os.path.join("/Applications/MATLAB_R2024b.app/bin/matlab -batch ")
SIGNAL_DRIFT_DIR = "/Users/ged/Downloads/DWI_PIPELINE/MATLAB"
SYNB0_PATH = "/Users/ged/Documents/dMRI_Pipeline/Current_dMRI_Processing_Scripts/dMRI_Pipeline_Version4.1_Local/SynB0_Pipeline_rocket_ALT.sh"
DWI_DIR = ""
ACQP_PATH = "/Users/ged/Documents/Helping/dMRI_Pipeline_Local/Example_Run/acqp_1P6.txt"
SYNB0_LOC = "/Users/ged/Downloads/DWI_PIPELINE/synb0-DISCO_ROCKET"
FA_Template_PATH = "/Users/ged/Documents/Helping/dMRI_Pipeline_Local/dMRI_Templates/FMRIB58_FA_2mm.nii.gz"
MD_Template_PATH = "/Users/ged/Documents/Helping/dMRI_Pipeline_Local/dMRI_Templates/FSL_HCP1065_MD_2mm_FMRIB58.nii.gz"
TOPUP_DIR = "/Users/ged/Downloads/DWI_PIPELINE/synb0-DISCO_ROCKET/topup"
R_Launch_Script = "/Users/ged/Documents/Helping/dMRI_Pipeline_Local/R_Files/run_abnormalities.R"
#


#Create_Output_Folder
os.makedirs(os.path.join(Output_Folder, Identifier), exist_ok=True)

#Roll with the script now
T1_BRAIN_PATH,WM_MASK_PATH = T1_Processing.T1_Processing(T1_Image, Identifier, Output_Folder)

##ACQP Settings

## ------ Stage 1
Denoise_DWI_OUT = Stage_1_DWI.Denoise(DWI_Image, Identifier, Output_Folder)

MrDegibbs_DWI_OUT = Stage_1_DWI.MrDegibbs(Denoise_DWI_OUT, Identifier, Output_Folder)

BiasCor_DWI_OUT = Stage_1_DWI.BiasCor(MrDegibbs_DWI_OUT, Identifier, Output_Folder, bvec_File, bval_File)

## ------ Stage 2
DRIFT_DWI_OUT = Stage_2_DWI.Drift(BiasCor_DWI_OUT, Identifier, Output_Folder, MATLAB_PATH, bvec_File, SIGNAL_DRIFT_DIR)

## ------ Stage 3
Stage_3_DWI.SYNB0DISCO(SYNB0_PATH, DRIFT_DWI_OUT, T1_BRAIN_PATH, WM_MASK_PATH, Identifier, Output_Folder, ACQP_PATH, SYNB0_LOC)

Eddy_OUT = Stage_3_DWI.EDDY(DRIFT_DWI_OUT, ACQP_PATH, Output_Folder, bvec_File, bval_File, TOPUP_DIR, Identifier, T1_BRAIN_PATH)

print(f"PreProcessing is Completed for {Identifier}. Running PreProcessing cleanup.")

os.makedirs(os.path.join(Output_Folder, Identifier, 'Processed_DWI'), exist_ok=True)

PP_DWI_PATH = os.path.join(Output_Folder, Identifier, 'Processed_DWI', Identifier +'_DWI.nii.gz')
EDDY_DWI = Eddy_OUT + ".nii.gz"
shutil.copyfile(EDDY_DWI, PP_DWI_PATH)

PP_BVEC_PATH = os.path.join(Output_Folder, Identifier, 'Processed_DWI', Identifier +'_DWI.bvec')
EDDY_BVEC = Eddy_OUT + ".eddy_rotated_bvecs"
shutil.copyfile(EDDY_BVEC, PP_BVEC_PATH)

PP_BVAL_PATH = os.path.join(Output_Folder, Identifier, 'Processed_DWI', Identifier +'_DWI.bval')
shutil.copyfile(bval_File, PP_BVAL_PATH)

PP_MASK_PATH = os.path.join(Output_Folder, Identifier, 'Processed_DWI', Identifier +'_DWI_Mask.nii.gz')
EDDY_MASK = os.path.join(SYNB0_LOC, "b0_all_topup_slice_01_brain_mask.nii.gz")
shutil.copyfile(EDDY_MASK, PP_MASK_PATH)

## ------ Stage 4

''' PreProcessing Completed now for Tensor Map and Registration '''

Output_PREFIX = Stage_4_DWI.Tensor_Fitting(PP_DWI_PATH, PP_BVEC_PATH, PP_BVAL_PATH, PP_MASK_PATH, Output_Folder, Identifier)

Tensor_Reg_OUT_Folder = Stage_4_DWI.Tensor_Reg(Output_PREFIX, FA_Template_PATH, MD_Template_PATH, Identifier)

print(f"Tensor fitting and registration is Completed for {Identifier}.")

''' Running Abnormality Calculation '''

## ------ Stage 5
Abnormality_Folder_PATH = os.path.join(Output_Folder, Identifier, 'Abnormality_Maps')
os.makedirs(Abnormality_Folder_PATH, exist_ok=True)
Stage_5_DWI.Calc_Abnormalities(R_Launch_Script, Tensor_Reg_OUT_Folder, Abnormality_Folder_PATH, Identifier)


print(f"Abnormality calculation is Completed for {Identifier}.")
print("Pipeline Completed!")