# This is not actually a markdown file.  I just did that so it didn't get ignored.

dag {
  bb="-2.929,-4.493,2.771,3.689"
  Age [selected,pos="-0.940,-0.313"]
  Gene_feature [exposure,pos="-0.098,1.042"]
  OS [outcome,pos="0.922,1.052"]
  Sample_type [pos="-1.226,1.024"]
  Unmeas_confounder [pos="-0.903,2.501"]
  Age -> Gene_feature
  Age -> OS
  Gene_feature -> OS
  Sample_type -> Gene_feature
  Unmeas_confounder -> Gene_feature
  Unmeas_confounder -> OS
}

# Version 2:

dag {
bb="-2.929,-4.493,2.771,3.689"
"race/eth" [adjusted,pos="-1.650,-1.744"]
"site practices" [latent,pos="-1.191,0.239"]
Age [adjusted,pos="-0.940,-0.313"]
Gene_feature [exposure,pos="0.534,0.790"]
Institution [adjusted,pos="-2.235,-0.294"]
OS [outcome,pos="2.125,-0.042"]
Sample_type [pos="-1.065,1.024"]
de_novo_met [adjusted,pos="-0.086,-1.463"]
"race/eth" -> OS
"site practices" -> OS
Age -> Gene_feature
Age -> OS
Gene_feature -> OS
Institution -> "site practices"
Institution -> Sample_type
Sample_type -> Gene_feature
de_novo_met -> Gene_feature
de_novo_met -> OS
de_novo_met -> Sample_type
}
