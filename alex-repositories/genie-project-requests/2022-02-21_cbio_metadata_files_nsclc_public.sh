# Description: manually copy and modify cBioPortal meta data files for NSCLC 2.0-public
# Author: Haley Hunter-Zinck
# Date: February 21, 2022

# download files
for synid in syn25471835 syn25471834 syn25471833 syn25471831 syn25471830 syn25471828 syn25471829 syn25471826 syn25471825 syn25471823 syn25471824 syn25471821 syn25471819 syn25471820 
{
    synapse get --downloadLocation . $synid 
}

# MANUAL step: modify meta_study.txt

# store in new Synapse folder
for file in $(ls meta*) 
{
    synapse store --parentid syn27199149 $file
}
