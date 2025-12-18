import os
import subprocess


def Denoise(DWI, Identifier, Output_Folder):
    print("Running Step 1 Denoising... (Approx 10 mins)")
    Denoise_DWI_OUT = os.path.join(Output_Folder, Identifier, Identifier + "_DWI_Denoised.nii.gz")
    command_DN = f"dwidenoise {DWI} {Denoise_DWI_OUT} -nthreads 8"
    subprocess.run(command_DN, shell=True, capture_output=True, text=True)
    print("... Step 1 Complete")
    return Denoise_DWI_OUT


def MrDegibbs(Denoise_DWI_OUT, Identifier, Output_Folder):
    print("Running Step 2 De-Ringing... (Approx 5 mins)")
    MrDegibbs_DWI_OUT = os.path.join(Output_Folder, Identifier, Identifier + "_MrDegibbs.nii.gz")
    command_MDG = f"mrdegibbs {Denoise_DWI_OUT} {MrDegibbs_DWI_OUT}"
    subprocess.run(command_MDG, shell=True, capture_output=True, text=True)
    print("... Step 2 Complete")
    return MrDegibbs_DWI_OUT


def BiasCor(MrDegibbs_DWI_OUT, Identifier, Output_Folder, BVEC, BVAL):
    print("Running Step 3 BiasCor... (Approx 5 mins)")
    BiasCor_DWI_OUT = os.path.join(Output_Folder, Identifier, Identifier + "_BiasCor.nii.gz")
    command_BC = f"dwibiascorrect ants {MrDegibbs_DWI_OUT} {BiasCor_DWI_OUT} -fslgrad {BVEC} {BVAL}"
    subprocess.run(command_BC, shell=True, capture_output=True, text=True)
    print("... Step 3 Complete")
    return BiasCor_DWI_OUT

