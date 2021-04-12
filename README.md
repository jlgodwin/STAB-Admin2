# STAB-Admin2
Pipeline code for development of subnational estimates of U5MR using **SUMMER**.
[**COMMAND CENTER**](https://docs.google.com/spreadsheets/d/1GgrysoVHM2bO6DUZx8Cmj7WICKZ5KpTay0GOT72zK24/edit#gid=0)

# Example data
  * Country-specific data
    * Senegal 2005 DHS Births Recode (STATA)
    * Senegal 2005 DHS Geographic Dataset (FLAT ASCII/reads in as SpatialPointsDataFrame)
    * Senegal GADM Polygon files (reads in as SpatialPolygonsDataFrame)
  * Meta-data
    * SurveyNum.csv
    * SurveyList_BR_GE.csv
    * CountryList.csv or []()

# .R Files

  1. **DataProcessing.R**
     *  Combines DHS Births Recode files (\*.dta), DHS GPS files (\*.cpg, \*.dbf, \*.prj, \*.sbn, \*.sbx, \*.shp, \*.shp.xml, \*.shx), and (usually) GADM polygon files (\*.cpg, \*.dbf, \*.prj, \*.shp,  \*.shx) into single data.frame called **mod.dat** saved in file CountryName_cluster_dat.rda for all subsequent analysis
     *  Associated **SUMMER** functions: 
        - `getBirths()`: 365--370
  2. **DirectEstimates.R**
     *  Uses output of **DataProcessing.R** to get design-based discrete hazards estimates and SEs of child mortality adjusts for HIV when appropriate
     *  Associated **SUMMER** functions: 
        - `getDirectList()`: 310-321, 351-356, 376-381
        - `getDirect()`: 323-334, 358-363, 383-388
        - `getAdjusted()`: 426-431, 465-471, 486-492
  3. **SmoothedDirect.R**
      * Uses output of **DataProcessing.R** and **DirectEstimates.R** to get estimates of child mortality smoothed in space and time, benchmarks to UN IGME when `doBenchmark == TRUE`
      * Associated **SUMMER** functions:
        - `aggregateSurvey()`:
        - `smoothDirect()`:
        - `getSmoothed()`:
        - `getAdjusted()`:
  5. **Betabinomial.R**  
     * Uses output of **DataProcessing.R** to get model-based discrete hazards estimates of child mortality smoothed in space and time, adjusts for HIV when appropriate and benchmarks to UN IGME when `doBenchmark == TRUE`
     * Associated **SUMMER** functions:
       - `smoothCluster()`:
       - `getSmoothed()`:
