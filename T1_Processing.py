import os
import ants
import antspynet as apn

def T1_Processing(T1_Pathway, Identifier, OutputPath):

    print("Processing T1 data... (Approx Time 10 mins)")
    Tissue_Map_PATH = os.path.join(OutputPath, Identifier, 'T1_Outputs', 'T1_Segmentation.nii.gz')
    WM_Mask_PATH = os.path.join(OutputPath, Identifier, 'T1_Outputs', 'WM_Mask.nii.gz')
    T1_Brain_PATH = os.path.join(OutputPath, Identifier, 'T1_Outputs', 'T1_Brain.nii.gz')

    if os.path.exists(T1_Brain_PATH):
       print("Step 1 file already exists, skipping step.")
       return T1_Brain_PATH, WM_Mask_PATH
    else:

        os.makedirs(os.path.join(OutputPath, Identifier, 'T1_Outputs'), exist_ok=True)

        T1_img = ants.image_read(T1_Pathway)

        T1_img_BC = ants.n4_bias_field_correction(T1_img)

        T1_Mask = apn.brain_extraction(T1_img_BC, modality='t1combined')
        T1_Mask = ants.get_mask(T1_Mask)

        T1_Brain = T1_img_BC * T1_Mask

        Tissue_Map = ants.atropos(a=T1_img_BC, x=T1_Mask, i='kmeans[3]', m='[0.3,1x1x1]')
        WM_Mask = (Tissue_Map['segmentation'] == 3)

        ants.image_write(Tissue_Map['segmentation'], Tissue_Map_PATH)
        ants.image_write(WM_Mask, WM_Mask_PATH)
        ants.image_write(T1_Brain, T1_Brain_PATH)

        print("... Initial T1 processing completed. Moving onto DWI")
        return T1_Brain_PATH, WM_Mask_PATH