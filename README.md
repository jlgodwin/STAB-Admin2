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
        - `getBirths()`: loads DHS Births Recode STATA file from filename and prepares data for survival analysis based on birth history data
          - Section: Load Data
     *  Products:
        - `admin1.mat`, `admin2.mat`: `matrix` describing neighborhood structure of subnational polygons for future use in `SUMMER` and nested `INLA` functions
           - Section: Create adjacency matrices
           - Output: data.dir/folder.name/shapes.sub.dir/CountryName_Amat.rda 
        - `admin1.names`, `admin2.names`: `data.frame` keys matching GADM area name strings to internal (unique) area name strings
          - Section: Create adjacency matrices 
          - Output: data.dir/folder.name/shapes.sub.dir/CountryName_Amat_Names.rda
        - `mod.dat`: `data.frame` that summarizes number of deaths and agemonths for each cluster and contains GPS location of clusters and Admin area assignments
          - Sections: Combine survey frames, Check frame, Save frame 
          - Output: data.dir/folder.name/CountryName_cluster_dat.rda
     *  Plots:
        - Creates directories: data.dir/folder.name/Plots, data.dir/folder.name/Plots/ShapeCheck
        - neighborhood structure on polygons
          - data.dir/folder.name/Plots/ShapeCheck/CountryName_adm\*\_neighb.pdf
        - cluster locations in DHS Geographic Data
          - data.dir/folder.name/Plots/ShapeCheck/Points_SurveyYear_GE.png
        - cluster locations from DHS Geographic that are present in Births Recode file 
          - data.dir/folder.names/Plots/ShapeCheck/Points_Survery_Year_BR.png 
  2. **DirectEstimates.R**
     *  Uses output of **DataProcessing.R** to get design-based discrete hazards estimates and SEs of child mortality adjusts for HIV when appropriate
     *  Associated `SUMMER` functions: 
        - `getDirectList()`: takes output of `getBirths()` and computes H-T estimates of U5MR
          - Section: National
        - `getDirect()`: takes output of `getBirths()` and computes H-T estimates of U5MR when country has single survey
          - Section: National, Admin1, Admin2
        - `getAdjusted()`: takes output of `getDirect()` and divides estimates by a constant/adjsutst SEs; used for HIV adjustment and benchmarking for direct estimates
          - Section: HIV Adjustment
     * Products:
       - `direct.natl`: `data.frame` output from `getDirect()` or `getDirectList()` with national Horvitz-Thompson estimates of child mortality by 5-year period and survey
         - Section: Direct Estimates (National)
         - Output: data.dir/folder.name/CountryName_direct_natl.rda
       - `direct.natl.yearly`: `data.frame` output from `getDirect()` or `getDirectList()` with national Horvitz-Thompson estimates of child mortality by yearand survey
         - Sections: Direct Estimates (National), HIV Adjustments (if appropriate)
         - Output: data.dir/folder.name/CountryName_direct_natl_yearly.rda, data.dir/folder.name/CountryName_directHIV_natl_yearly.rda
       - `direct.admin1`: `data.frame` output from `getDirect()` or `getDirectList()` with Admin1 Horvitz-Thompson estimates of child mortality by  5-year period and survey
         - Sections: Direct Estimates (Admin1), HIV Adjustments (if appropriate)
         - Output: data.dir/folder.name/CountryName_direct_admin1.rda, data.dir/folder.name/CountryName_directHIV_admin1.rda
       - `direct.admin2`: `data.frame` output from `getDirect()` or `getDirectList()` with Admin2 Horvitz-Thompson estimates of child mortality by  5-year period and survey
         - Sections: Direct Estimates (Admin2), HIV Adjustments (if appropriate)
         - Output: data.dir/folder.name/CountryName_direct_admin2.rda, data.dir/folder.name/CountryName_directHIV_admin2.rda
     * Plots:
       - Creates directory: folder.name/Plots/Direct
       - (Ugly-ish, non-`SUMMER`) Polygon plots direct estimates by administrative division
         - data.dir/folder.name/Plots/Direct/CountryName_\*\_direct_poly.png, where \*: natl, admin1, admin2
         - if a country requires HIV adjustment, the plots display the adjusted estimates
       - Spaghetti plots of direct estimates by administrative division
         - data.dir/folder.name/Plots/Direct/CountryName_\*\_direct_spaghetti.png, where \*: natl, admin1, admin2 and (for some reason) data.dir/folder.name/Plots/Direct/CountryName_natl_direct_yearly_spaghetti.png
         - if a country requires HIV adjustment, the plots display the adjusted estimates
  3. **SmoothedDirect.R**
      * Uses output of **DataProcessing.R** and **DirectEstimates.R** to get estimates of child mortality smoothed in space and time, benchmarks to UN IGME when `doBenchmark == TRUE`
      * Associated `SUMMER` functions:
        - `aggregateSurvey()`: takes output of `getDirectList()` to compute meta-analysis estimator for each area and time (i.e. aggregating across surveys)
          - Section:  Aggregate surveys
        - `smoothDirect()`: takes output of `aggregateSurvey()` (or `getDirectList()`) and fits spatiotemporal smoothing model in `INLA`
          - Sections: Fit smoothing model, Benchmarking
        - `getSmoothed()`: takes output of `smoothDirect()` to get spatiotemporal estimates of child mortality in easily usable format
          - Sections: Fit smoothing model, Benchmarking
        - `getAdjusted()`: takes output of `aggregateSurvey()` (or `getDirectList()`) and benchmarks to UN IGME estimates
          - Sections: Benchmarking
      * Products:
        - `res.natl`
           - Section: Fit smoothing model (National), Benchmarking (National)
           - Output: data.dir/folder.name/CountryName_res_natl_SmoothedDirect.rda, data.dir/folder.name/CountryName_res_natlBench_SmoothedDirect.rda
        - `res.natl.yearly`
           - Section: Fit smoothing model (National), Benchmarking (National Yearly)
           - Output: data.dir/folder.name/CountryName_res_natl_yearly_SmoothedDirect.rda, data.dir/folder.name/CountryName_res_natlBench_yearly_SmoothedDirect.rda
        - `res.admin1`
           - Section: Fit smoothing model (National), Benchmarking (National Yearly)
           - Output: data.dir/folder.name/CountryName_res_admin1_SmoothedDirect.rda, data.dir/folder.name/CountryName_res_admin1Bench_SmoothedDirect.rda
        - `res.admin2`
           - Section: Fit smoothing model (National), Benchmarking (National Yearly)
           - Output: data.dir/folder.name/CountryName_res_admin1_SmoothedDirect.rda, data.dir/folder.name/CountryName_res_admin1Bench_SmoothedDirect.rda
      * Plots:
         - Creates directory: data.dir/folder.name/Plots/SmoothedDirect
         - `SUMMER` plots using `plot`, plots benchmarked results if `doBenchmark == TRUE`
            - data.dir/folder.name/Plots/SmoothedDirect/CountryName_\*\_SmoothedDirect.pdf
         - Spaghetti Plots
            - data.dir/folder.name/Plots/SmoothedDirect/CountryName_\*\_SmothedDirect_spaghetti\*\.pdf  
         - Polygon Plots (better to use `SUMMER::mapPlot` maybe)
            - data.dir/folder.name/Plots/SmoothedDirect/CountryName_\*\_SmoothedDirect_poly.pdf 
  5. **Betabinomial.R**  
     * Uses output of **DataProcessing.R** to get model-based discrete hazards estimates of child mortality smoothed in space and time, adjusts for HIV when appropriate and benchmarks to UN IGME when `doBenchmark == TRUE`
     * Associated `SUMMER` functions:
       - `smoothCluster()`:
       - `getSmoothed()`:
