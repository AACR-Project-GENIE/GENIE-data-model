"""
Can you let me know what you see for
CNA, mutations, and SVs for GENIE-DFCI-010770
and GENIE-DFCI-009325?
"""
import pandas as pd

import synapseclient

syn = synapseclient.login()
git_url = "https://github.com/Sage-Bionetworks/genie-project-requests/blob/main/2022-07-18_patient_genomic_check.py"

# Patient list because shouldn't commit PATIENT_IDs on github
patients_list_synid = "syn33113130"
patient_list_ent = syn.get(patients_list_synid)
patient_list_df = pd.read_csv(patient_list_ent.path)
patients = patient_list_df['PATIENT_ID']

# release 12 files
sample_synid = "syn32299605"
cna_synid = "syn32299602"
fusions_synid = "syn32299607"
mutations_synid = "syn32299610"

sample_ent = syn.get(sample_synid, followLink=True)
cna_ent = syn.get(cna_synid, followLink=True)
fusion_ent = syn.get(fusions_synid, followLink=True)
maf_ent = syn.get(mutations_synid, followLink=True)

sample_df = pd.read_csv(sample_ent.path, sep="\t", comment="#")
cna_df = pd.read_csv(cna_ent.path, sep="\t")
fusion_df = pd.read_csv(fusion_ent.path, sep="\t")
maf_df = pd.read_csv(maf_ent.path, sep="\t")

samples = sample_df['SAMPLE_ID'][sample_df['PATIENT_ID'].isin(patients)]

cna_cols = ['Hugo_Symbol']
cna_cols.extend(samples.to_list())
cna_df[cna_cols].to_csv("subset_cna.tsv", sep="\t")

maf_df[maf_df['Tumor_Sample_Barcode'].isin(samples)].to_csv("subset_maf.tsv", sep="\t")

fusion_df[fusion_df['Tumor_Sample_Barcode'].isin(samples)].to_csv("subset_fusion.tsv", sep="\t")

syn.store(
    synapseclient.File("subset_maf.tsv", parent="syn33113020"),
    used=[patients_list_synid, mutations_synid],
    executed=git_url
)

syn.store(
    synapseclient.File("subset_cna.tsv", parent="syn33113020"),
    used=[patients_list_synid, cna_synid],
    executed=git_url
)

syn.store(
    synapseclient.File("subset_fusion.tsv", parent="syn33113020"),
    used=[patients_list_synid, fusions_synid],
    executed=git_url
)
