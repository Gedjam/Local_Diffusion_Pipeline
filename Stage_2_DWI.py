import os
import subprocess
from pathlib import Path

def Drift(DWI, Identifier, Output_Folder, MATLAB_PATH, BVEC, SIGNAL_DRIFT_DIR):
    print("Running Step 4 Signal Drift Correction, Calling MATLAB... (Approx 10 mins)")

    Drift_DWI_OUT = os.path.join(Output_Folder, Identifier, Identifier + "_DWI_Drift_Corrected.nii")
    Drift_DWI_OUT_Comp = Drift_DWI_OUT.replace(".nii", ".nii.gz")

    if os.path.exists(Drift_DWI_OUT_Comp):
       print("Step 4 file already exists, skipping step.")
    else:
        Sig_Drift_Path = os.path.join(SIGNAL_DRIFT_DIR)

        subprocess.run(["gunzip", "-kf", DWI], check=True)
        Unzip_DWI = DWI.removesuffix(".gz")

        command_DF = f"{MATLAB_PATH}\"addpath(genpath('{Sig_Drift_Path}')); correct_signal_drift('{Unzip_DWI}','{BVEC}','{Drift_DWI_OUT}'); exit;\""

        subprocess.run(command_DF, shell=True, capture_output=True, text=True)

        subprocess.run(["gzip", "-kf", Drift_DWI_OUT], check=True)

        if Path(Unzip_DWI).exists():
            os.remove(Unzip_DWI)

        if Path(Drift_DWI_OUT).exists():
            os.remove(Drift_DWI_OUT)

    print("... Closing MATLAB, Step 4 Complete")
    return Drift_DWI_OUT_Comp

