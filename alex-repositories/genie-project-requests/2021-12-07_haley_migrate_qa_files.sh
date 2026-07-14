# Description: move all old QA files to archive folder and generate new QA log files for each cohort.
# Author: Haley Hunter-Zinck
# Date: 2021-12-07

#########################
#       parameters      #
#########################

synid_folder_archive=syn26529519
synid_folder_brca=syn25872288
synid_folder_crc=syn25878117
synid_folder_nsclc=syn25872287
synid_folder_panc=syn25882184
synid_folder_prostate=syn25976290

#########################
# migrate old QA files  #
#########################

synapse create -parentid syn25832316 -name archive Folder

# BrCa
synapse list $synid_folder_brca > res.txt
for synid in $(cut -f 1 -d ' ' res.txt) 
{
    synapse mv --id $synid --parentid $synid_folder_archive
}

# CRC
synapse list $synid_folder_crc > res.txt
for synid in $(cut -f 1 -d ' ' res.txt) 
{
    synapse mv --id $synid --parentid $synid_folder_archive
}

# NSCLC
synapse list $synid_folder_nsclc > res.txt
for synid in $(cut -f 1 -d ' ' res.txt) 
{
    synapse mv --id $synid --parentid $synid_folder_archive
}

# PANC
synapse list $synid_folder_panc > res.txt
for synid in $(cut -f 1 -d ' ' res.txt) 
{
    synapse mv --id $synid --parentid $synid_folder_archive
}

# prostate
synapse list $synid_folder_prostate > res.txt
for synid in $(cut -f 1 -d ' ' res.txt) 
{
    synapse mv --id $synid --parentid $synid_folder_archive
}

#########################
# generate new QA files #
#########################

# BrCa
for level in error warning
{
    for site in DFCI MSK VICC
    {
        Rscript main.R -c BrCa -s $site -r upload -l $level -v -u $synid_folder_brca
    }
    Rscript main.R -c BrCa -s all -r table -l $level -v -u $synid_folder_brca
    Rscript main.R -c BrCa -s all -r comparison -l $level -v -u $synid_folder_brca
}

# CRC
for level in error warning
{
    for site in DFCI MSK VICC
    {
        Rscript main.R -c CRC -s $site -r upload -l $level -v -u $synid_folder_crc
    }
    Rscript main.R -c CRC -s all -r table -l $level -v -u $synid_folder_crc
    Rscript main.R -c CRC -s all -r comparison -l $level -v -u $synid_folder_crc
    Rscript main.R -c CRC -s all -r release -l error -v -u $synid_folder_crc
}

# NSCLC
for level in error warning
{
    for site in DFCI MSK UHN VICC
    {
        Rscript main.R -c NSCLC -s $site -r upload -l $level -v -u $synid_folder_nsclc
    }
    Rscript main.R -c NSCLC -s all -r table -l $level -v -u $synid_folder_nsclc
    Rscript main.R -c NSCLC -s all -r comparison -l $level -v -u $synid_folder_nsclc
    Rscript main.R -c NSCLC -s all -r release -l error -v -u $synid_folder_nsclc
}

# PANC
for level in error warning
{
    for site in DFCI MSK UHN VICC
    {
        Rscript main.R -c PANC -s $site -r upload -l $level -v -u $synid_folder_panc
    }
    Rscript main.R -c PANC -s all -r table -l $level -v -u $synid_folder_panc
    Rscript main.R -c PANC -s all -r comparison -l $level -v -u $synid_folder_panc
}

# Prostate
for level in error warning
{
    for site in DFCI MSK UHN VICC
    {
        Rscript main.R -c Prostate -s $site -r upload -l $level -v -u $synid_folder_prostate
    }
    Rscript main.R -c Prostate -s all -r table -l $level -v -u $synid_folder_prostate
    Rscript main.R -c Prostate -s all -r comparison -l $level -v -u $synid_folder_prostate
}

