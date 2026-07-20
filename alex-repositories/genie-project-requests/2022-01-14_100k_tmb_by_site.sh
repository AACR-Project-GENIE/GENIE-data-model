# Description: Calculate tumor mutation burden for each GENIE sample 
#     and plot results by site for 9.1-public release files.  
# Author: Haley Hunter-Zinck
# Date: 2021-01-14

cd ~/

# calculate TMB
#git clone git@github.com:Sage-Bionetworks/TMB-Genie.git
cd TMB_GENIE
Rscript tmb_cli.R -s syn24179661 -g syn24179674 -m syn24179664
synapse store --parentid syn26838302 tmb.tsv \
  --name "tmb" --description "tumor mutation burden for genie 9.1-public" \
  --used syn24179664 \
  --executed https://github.com/Sage-Bionetworks/TMB-Genie/blob/main/tmb_cli.R
rm tmb.tsv
cd ~/

# save and plot
cd genie_requests/
Rscript 2022-01-14_100k_tmb_by_site.R -i syn26838318 -o syn26838302 -v

