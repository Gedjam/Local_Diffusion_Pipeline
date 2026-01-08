import os
import ants

def NODDI_Resample(DWI_Image, Identifier, Output_Folder):
    print("Running Quick Initial Processing... ")
    DWI_rs_OUT = os.path.join(Output_Folder, Identifier, Identifier + "_DWI_rs.nii.gz")
    DWI_img = ants.image_read(DWI_Image)

    DWI_img_rs = ants.resample_image(DWI_img, (2, 2, 2, DWI_img.spacing[3]), interp_type=1)

    ants.image_write(DWI_img_rs, DWI_rs_OUT)

    print("Initial Process Complete")
    return DWI_rs_OUT