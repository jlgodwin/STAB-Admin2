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
     *  Associated **SUMMER** functions: 
        - `getBirths()`: 365-370 (
     *  Products:
        - `admin1.mat`, `admin2.mat`: adjacency matrices of subnational polygons for future use in `SUMMER` and nested `INLA` functions
           - Section: Create adjacency matrices
           - Lines: 250-254, 261-265, 273-275, 280-282
           - Output: CountryName_Amat.rda 
        - `admin1.names`, `admin2.names`:
          - Section: Create adjacency matrices 
          - Lines: 255-256, 266-267, 276-278, 283-285
          - Output: CountryName_Amat_Names.rda
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
