import os
import subprocess

def Tensor_Fitting(DWI,BVEC,BVAL,MASK,OUTPUT_FOLDER,ID):
    print("Running Step 7 Tensor Fitting (Approx 2 mins)...")

    Tensor_FOLDER = os.path.join(OUTPUT_FOLDER, ID, "Tensor_Maps")
    os.makedirs(Tensor_FOLDER, exist_ok=True)

    OUT_PREFIX = os.path.join(Tensor_FOLDER, ID)
    command_Ten = f"dtifit -k {DWI} -o {OUT_PREFIX} -m {MASK} -b {BVAL} -r {BVEC}"
    subprocess.run(command_Ten, shell=True, capture_output=True, text=True)

    print("...Finished Tensor Fitting.")
    return OUT_PREFIX


def Tensor_Reg(OUT_PREFIX,FA_Template_PATH,MD_Template_PATH,ID):
    print("Running Step 7 Tensor Registration to MNI (Approx 20 mins)...")

    FA_PATH = OUT_PREFIX + "_FA.nii.gz"
    MD_PATH = OUT_PREFIX + "_MD.nii.gz"

    FA_ero_PATH = OUT_PREFIX + "_FA_ero.nii.gz"
    command_ero = f"fslmaths {FA_PATH} -ero {FA_ero_PATH}"
    subprocess.run(command_ero, shell=True, capture_output=True, text=True)

    Reg_OUT_Folder = os.path.join(OUT_PREFIX,"Tensor_Maps_MNI_FA_Reg_2mm")
    os.makedirs(Reg_OUT_Folder, exist_ok=True)

    command_Ero = f"antsRegistrationSyN.sh -d 3 -f {FA_Template_PATH} -f {MD_Template_PATH} -m {FA_ero_PATH} -m {MD_PATH} -o {Reg_OUT_Folder}/{ID}_ -n 20"
    subprocess.run(command_Ero, shell=True, capture_output=True, text=True)

    Tensor_Reg_OUT_Folder = os.path.join(Reg_OUT_Folder,"Tensor_Maps_Reg_2mm")
    os.makedirs(Tensor_Reg_OUT_Folder, exist_ok=True)

    for type in ["FA","MD", "M0", "L1", "L2", "L3", "S0"]:
        Current_Map = OUT_PREFIX + "_" + type + ".nii.gz"
        Current_Map_OUT = os.path.join(Tensor_Reg_OUT_Folder, ID + "_" + type + "_Reg_TL.nii.gz")
        command_Apply = f"antsApplyTransforms -d 3 -i {Current_Map} -r {FA_Template_PATH} -o {Current_Map_OUT} -n Linear -t {Reg_OUT_Folder}/{ID}_1Warp.nii.gz -t {Reg_OUT_Folder}/{ID}_0GenericAffine.mat"
        subprocess.run(command_Apply, shell=True, capture_output=True, text=True)


    print("...Finished Tensor Registration.")

    return Tensor_Reg_OUT_Folder

