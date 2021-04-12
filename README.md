# STAB-Admin2
Pipeline code for development of subnational estimates of U5MR using SUMMER

# .R Files

  1. **DataProcessing.R**
     *  Combines DHS Births Recode files (\*.dta), DHS GPS files (\*.cpg, \*.dbf, \*.prj, \*.sbn, \*.sbx, \*.shp, \*.shp.xml, \*.shx), and (usually) GADM polygon files (\*.cpg, \*.dbf, \*.prj, \*.shp,  \*.shx) into single data.frame called **mod.dat** saved in file CountryName_cluster_dat.rda for all subsequent analysis
     *  Associated **SUMMER** functions: getBirths() (lines 365--370) 
  2. **DirectEstimates.R**
     *  Uses output of **DataProcessing.R**
     *  Associated **SUMMER** functions: getDirectList() (lines 310--321, 351--356, 376--381), getDirect() (lines 323--334, 358--363, 383--388), getAdjusted() (lines 426--431, 465--471, 486--492)
  
