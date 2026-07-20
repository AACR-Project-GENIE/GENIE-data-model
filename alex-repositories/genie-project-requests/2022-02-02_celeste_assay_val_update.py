'''
Description: test adaptations to main GENIE assay.py to make SEQ_ASSAY_ID comparisons case insensitive.
Author: Haley Hunter-Zinck
Date: 2022-02-02
Script: https://github.com/Sage-Bionetworks/Genie/blob/develop/genie_registry/assay.py
'''


import pandas as pd
import synapseclient

syn = synapseclient.Synapse()
syn.login()

center = 'UHN'
assay_info_df = pd.DataFrame(['UHN-48-V1','UHN-50-V2','UHN-54-V1','UHN-OCA-V3','UHN-555-V1','UHN-555-V2','UHN-555-BLADDER-v1','UHN-555-BREAST-v1','UHN-555-GLIOMA-v1','UHN-555-GYNE-v1','UHN-555-HEAD_NECK-v1','UHN-555-LUNG-v1','UHN-555-MELANOMA-v1','UHN-555-PAN-GI-v1','UHN-555-PROSTATE-v1','UHN-555-RENAL-v1','UHN-555-PROSTATE-v1'])
assay_info_df.columns = ['SEQ_ASSAY_ID']

entity = syn.tableQuery("SELECT DISTINCT SEQ_ASSAY_ID as seq FROM syn7517674 WHERE CENTER = 'UHN'")
uniq_seq_df = entity.asDataFrame()

all_seq_assays = assay_info_df.SEQ_ASSAY_ID.replace({'_':'-'}, regex=True).str.upper().unique()

# should be true
all([assay.startswith(center) for assay in all_seq_assays])

missing_seqs = uniq_seq_df["seq"][~uniq_seq_df["seq"].replace({'_':'-'}, regex=True).str.upper().isin(all_seq_assays)]
missing_seqs_str = ", ".join(missing_seqs)
