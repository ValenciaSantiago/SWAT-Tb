# SWAT-Tb

These are the required files to implement the SWAT-Tb variant developed by Valencia et al., (2024) https://www.sciencedirect.com/science/article/pii/S2214581824002374.



## List of files
- subroutine_modified: all the SWAT-T subroutines modified for SWAT-Tb, part of the subroutines are adapted from Alemayehu et al., 2017
- add_SOS_sub_files.R: R script to add SOS months to the existing SWAT project for SWAT-Tb
- plant.dat: sample plant database
- remove_mgtopt.R: R script to remove management settings for trees, perennials, and annual plants
- swat_SWAT-Tb: The SWAT-Tb executable release version. 
- Variables: This file contains all the new variables and arrays added to the SWAT-T subroutines
- window size: This file provides the number of days for aggregation for rainfall and PET, as well as the SMI threshold used by Alemayehu et al., 2017. 

### note for running 
1. Start by adding the SOS months (SOS1, SOS2, SOS3, and SOS4) for each subbasin in the project. If the catchment under consideration is not large, meaning similar climatic conditions across the sub-basins,
   then a single set of SOS months should suffice. Otherwise, the SOS months can be varied across the sub-basins; this, in principle, should be for large basins.
2. Remove the management setting for trees and perennials. If there is no management operation in the natural ecosystem, remove all the default management settings in the *.mgt files using the r-script
3. Replace the default swat.exe file with the new SWAT-Tb executable (swat_SWAT-Tb.exe)
4. Now the SWAT-Tb is set to run

If there are any issues, feel free to contact me at santiagovalencia@arizona.edu or santiagovalenciacardenas1@gmail.com.


