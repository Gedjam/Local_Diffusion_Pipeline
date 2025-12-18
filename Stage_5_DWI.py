import os
import subprocess

def Calc_Abnormalities(R_Launch_Script, Tensor_Reg_OUT_Folder, Abnormality_Folder_PATH ,ID):

    print("Running Step 8 Calculating Abnormalities (Approx 5 mins)...")

    L1 = os.path.join(Tensor_Reg_OUT_Folder, ID + '_L1_Reg_TL.nii.gz')
    L2 = os.path.join(Tensor_Reg_OUT_Folder, ID + '_L2_Reg_TL.nii.gz')
    L3 = os.path.join(Tensor_Reg_OUT_Folder, ID + '_L3_Reg_TL.nii.gz')

    command_Ab = f"Rscript {R_Launch_Script} {L1} {L2} {L3} {Abnormality_Folder_PATH} MNI"
    print(command_Ab)
    subprocess.run(command_Ab, shell=True, capture_output=True, text=True)

    print("...Finished Abnormality Calculations.")




