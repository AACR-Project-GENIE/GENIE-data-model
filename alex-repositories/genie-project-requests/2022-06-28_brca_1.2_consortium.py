"""This is a request from MSK as they wanted the BrCa 1.2-consortium
release to be exactly the same as the 1.1-consortium release.

1. Modify synapsePythonClient to allow for copying of files with access
restrictions and install from source.

    synapse cp syn26253353 --destinationId syn32298950 --setProvenance None
    synapse cp syn24981909 --destinationId syn32298950 --setProvenance None

2. Update release_version column in the clinical files
3. Update meta_study.txt
"""
import pandas as pd
import synapseclient

syn = synapseclient.login()

git_url = "https://github.com/Sage-Bionetworks/genie-project-requests/blob/main/2022-06-28_brca_1.2_consortium.py"

# Update release_version
release_12_files = syn.getChildren("syn32299038")
for each_file in release_12_files:
    ent = syn.get(each_file['id'])
    ent_df = pd.read_csv(ent.path)
    ent_df['release_version'] = "1.2"
    ent_df.to_csv(ent.name, index=False)
    ent.path = ent.name
    syn.store(ent, executed=git_url)
