# STAB Admin 2 U5MR Pipeline

Pipeline code for development of subnational estimates of U5MR using [`SUMMER`](https://github.com/richardli/SUMMER).

Click here for the [**SPACE STATION**](http://faculty.washington.edu/jonno/space-station.html)

Click here for the [**COMMAND CENTER**](https://docs.google.com/spreadsheets/d/1GgrysoVHM2bO6DUZx8Cmj7WICKZ5KpTay0GOT72zK24/edit#gid=0)

# Example data
  * Country-specific data
    * Togo 2013-14, 1998 DHS Births Recode (STATA)
    * Togo 2013-14, 1998 DHS Geographic Dataset (FLAT ASCII/reads in as `SpatialPointsDataFrame`)
    * Togo GADM Polygon files (reads in as `SpatialPolygonsDataFrame`)
  * Meta-data
    * SurveyNum.csv
      - Contains CountryName, unique DHS survey number, DHS survey name key
    * SurveyList_BR_GE.csv
      - Contains SO MUCH INFO including last upload to [DHS](https://www.dhsprogram.com), 
    * CountryList.csv or [**Command Center: Country List**](https://docs.google.com/spreadsheets/d/1GgrysoVHM2bO6DUZx8Cmj7WICKZ5KpTay0GOT72zK24/edit#gid=0)
    * SurveyInfo.csv or [**Command Center: Survey Info**](https://docs.google.com/spreadsheets/d/1GgrysoVHM2bO6DUZx8Cmj7WICKZ5KpTay0GOT72zK24/edit#gid=1656161984)

# .R Files

  1. **DataProcessing.R**
     *  Combines
        - DHS Births Recode files (\*.dta), 
        - DHS GPS files (\*.cpg, \*.dbf, \*.prj, \*.sbn, \*.sbx, \*.shp, \*.shp.xml, \*.shx),
        - GADM (usually) polygon files (\*.cpg, \*.dbf, \*.prj, \*.shp,  \*.shx) 
     *  Associated `SUMMER` functions: 
        - `getBirths()`: 365-370 (Section: Load Data)
     *  Products:
        - `admin1.mat`, `admin2.mat`: `matrix` describing neighborhood structure of subnational polygons for future use in `SUMMER` and nested `INLA` functions
           - Section: Create adjacency matrices
           - Lines: 250-254, 261-265, 273-275, 280-282
           - Output: CountryName_Amat.rda 
        - `admin1.names`, `admin2.names`: `data.frame` keys matching GADM area name strings to internal (unique) area name strings
          - Section: Create adjacency matrices 
          - Lines: 255-256, 266-267, 276-278, 283-285
          - Output: CountryName_Amat_Names.rda
        - `mod.dat`: `data.frame` that summarizes number of deaths and agemonths for each cluster and contains GPS location of clusters and Admin area assignments
          - Sections: Combine survey frames, Check frame, Save frame 
          - Lines: 545, 547, 554, 558-560, 564-565, 569-571
          - Output: CountryName_cluster_dat.rda
     *  Plots:
        - Creates directories: folder.name/Plots, folder.name/Plots/ShapeCheck
        - folder.name/Plots/ShapeCheck/CountryName_adm*_neighb.pdf: plots neighborhood structure on polygons
        - folder.name/Plots/ShapeCheck/Points_SurveyYear_GE.png: plots cluster locations in DHS Geographic Data
        - folder.names/Plots/ShapeCheck/Points_Survery_Year_BR.png: plots cluster locations from DHS Geographic that are present in Births Recode file 
  2. **DirectEstimates.R**
     *  Uses output of **DataProcessing.R** to get design-based discrete hazards estimates and SEs of child mortality adjusts for HIV when appropriate
     *  Associated `SUMMER` functions: 
        - `getDirectList()`: 310-321 (Section: National), 351-356 (Section: National), 376-381 (Section: National)
        - `getDirect()`: 323-334 (Section: National), 358-363 (Section: Admin1), 383-388 (Section: Admin2)
        - `getAdjusted()`: 426-431, 465-471, 486-492 (Section: HIV adjustment)
     * Products:
       - `direct.natl`: `data.frame` output from `getDirect()` or `getDirectList()` with national Horvitz-Thompson estimates of child mortality by 5-year period and survey
         - Sections: Direct Estimates (National)
         - Lines: 310-315, 323-328, 335-336, 342-344
         - Output: CountryName_direct_natl.rda
       - `direct.natl.yearly`: `data.frame` output from `getDirect()` or `getDirectList()` with national Horvitz-Thompson estimates of child mortality by yearand survey
         - Sections: Direct Estimates (National), HIV Adjustments (if appropriate)
         - Lines: 316-321, 329-334, 338-339, 345-347, 426-434, 501-502
         - Output: CountryName_direct_natl_yearly.rda, CountryName_directHIV_natl_yearly.rda
       - `direct.admin1`: `data.frame` output from `getDirect()` or `getDirectList()` with Admin1 Horvitz-Thompson estimates of child mortality by  5-year period and survey
         - Sections: Direct Estimates (Admin1), HIV Adjustments (if appropriate)
         - Lines: 351-356, 358-365, 368-370, 465-475, 503-504
         - Output: CountryName_direct_admin1.rda, CoutnryName_directHIV_admin1.rda
       - `direct.admin2`: `data.frame` output from `getDirect()` or `getDirectList()` with Admin2 Horvitz-Thompson estimates of child mortality by  5-year period and survey
         - Sections: Direct Estimates (Admin2), HIV Adjustments (if appropriate)
         - Lines: 376-381, 383-390, 394-396, 486-496, 506-507
         - Output: CountryName_direct_admin2.rda, CoutnryName_directHIV_admin2.rda
     * Plots:
       - Creates directory: folder.name/Plots/Direct
       - (Ugly-ish, non-`SUMMER`) Polygon plots direct estimates by administrative division
         - CountryName_\*\_direct_poly.png, where \*: natl, admin1, admin2
         - if a country requires HIV adjustment, the plots display the adjusted estimates
       - Spaghetti plots of direct estimates by administrative division
         - CountryName_\*\_direct_spaghetti.png, where \*: natl, admin1, admin2 and (for some reason) CountryName_natl_direct_yearly_spaghetti.png
         - if a country requires HIV adjustment, the plots display the adjusted estimates
  3. **SmoothedDirect.R**
      * Uses output of **DataProcessing.R** and **DirectEstimates.R** to get estimates of child mortality smoothed in space and time, benchmarks to UN IGME when `doBenchmark == TRUE`
      * Associated `SUMMER` functions:
        - `aggregateSurvey()`:
        - `smoothDirect()`:
        - `getSmoothed()`:
        - `getAdjusted()`:
  5. **Betabinomial.R**  
     * Uses output of **DataProcessing.R** to get model-based discrete hazards estimates of child mortality smoothed in space and time, adjusts for HIV when appropriate and benchmarks to UN IGME when `doBenchmark == TRUE`
     * Associated `SUMMER` functions:
       - `smoothCluster()`:
       - `getSmoothed()`:
