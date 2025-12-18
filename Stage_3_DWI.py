import os
import subprocess
import ants


def SYNB0DISCO(SYNBO_PATH,DWI_PATH,T1_BRAIN_PATH,WM_SEG_PATH,ID,OUTPUT_PATH,ACQP,SYNB0_LOC):
    print("Running Step 5 SYNB0-DISCO... (Approx 20 mins)")

    DWI = DWI_PATH.replace(".nii.gz","")
    B0 = DWI + "_B0.nii.gz"

    command_B0 = f"fslroi {DWI_PATH} {B0} 0 1"
    subprocess.run(command_B0, shell=True, capture_output=True, text=True)

    command_SYNB0 = f"sh {SYNBO_PATH} {B0} {T1_BRAIN_PATH} {SYNB0_LOC} {ACQP} {ID} {WM_SEG_PATH}"
    subprocess.run(command_SYNB0, shell=True, capture_output=True, text=True)

    print("... Closing ML libraries, Step 5 Complete")



def EDDY(DWI_DRIFT,ACQP,OUTPUT_PATH,BVEC,BVAL,TOPUP_DIR,ID,T1_BRAIN_PATH):
    print("Running Step 6 Eddy... (Approx 4 hours). Please plug in laptop to power supply")


    #Quickly create index and masks required for eddy
    DWI_IMG = ants.image_read(DWI_DRIFT)
    D4 = DWI_IMG.shape[3]
    INDEX_PATH = os.path.join(OUTPUT_PATH, 'index.txt')
    command_idx = f"INDEX={INDEX_PATH}; indx=\"\"; for ((i=1; i<={D4}; i+=1)); do indx=\"$indx 1\"; done; echo $indx > $INDEX"
    subprocess.run(command_idx, shell=True, capture_output=True, text=True)

    #TOPUP Mask
    TOPUP_B0 = TOPUP_DIR.replace("topup","")
    TOPUP_B0_PATH = os.path.join(TOPUP_B0, 'b0_all_topup.nii.gz')
    TOPUP_B0_Slice01 = os.path.join(TOPUP_B0, 'b0_all_topup_slice01.nii.gz')
    T1_BRAIN_Mask = os.path.join(TOPUP_B0, 'T1_Mask.nii.gz')
    TOPUP_B0_OUT_PATH = TOPUP_B0_PATH.replace(".nii.gz","") +  "_slice_01_brain_mask.nii.gz"

    commnad_Reg = (f"fslroi {TOPUP_B0_PATH} {TOPUP_B0_Slice01} 0 1; "
                   f"antsRegistrationSyN.sh -d 3 -f {TOPUP_B0_Slice01} -m {T1_BRAIN_PATH} -o {TOPUP_B0}/T1_2_BO_ -t r -n 4; "
                   f"fslmaths {T1_BRAIN_PATH} -bin {T1_BRAIN_Mask}; "
                   f"antsApplytransforms -d 3 -i {T1_BRAIN_Mask} -r {TOPUP_B0}/T1_2_BO_Warped.nii.gz -o {TOPUP_B0_OUT_PATH} -n MultiLabel -t {TOPUP_B0}T1_2_BO_0GenericAffine.mat")
    subprocess.run(commnad_Reg, shell=True, capture_output=True, text=True)

    #Now setup eddy
    Eddy_OUT = os.path.join(OUTPUT_PATH, ID, 'Eddy_Corrected_DWI_' + ID)

    command_ED = (f"eddy --imain={DWI_DRIFT} --mask={TOPUP_B0_OUT_PATH} --acqp={ACQP} --index={INDEX_PATH} --bvecs={BVEC} "
                  f"--bvals={BVAL} --topup={TOPUP_DIR} --out={Eddy_OUT} --data_is_shelled -v")
    subprocess.run(command_ED, shell=True, capture_output=True, text=True)

    print("... Eddy is completed!, Step 6 Complete")
    return Eddy_OUT

