# PEMcollectr 0.0.2

* added and tested functionality for tracklogs

* validation pairs modules now checks for data uploaded, staged, and complete

* added photo module to view and upload photos to postgres

* added AOI and labels to map

* added ability to view staged and completed data on map

* added ability to view uploaded data in map

* added pending for transect triangles for sample areas that have one or 
more in the staging table

* unique constraint has been changed to transect_id and observer in case 
sample plan is duplicated to measure an area twice

* added a warning message for filtered data points that are already in the 
database

* incidentals will have a unique id generated based on point geometry

# PEMcollectr 0.0.1

* added ability to display all correctly uploaded track logs
with on/off option.

* added ability to display all correctly uploaded training points 
with on/off option.

* added ability to display full sample plan (.gpkg) with on/off option.

* added map with basemap

* added transfer data functions for staging to transects

* updated psql schema

* added check for transect_id column before validation

* reactive trigger added to keep data in sync with database

* validations are now filtered by what is in staging already

* review column added for mapunit1

* added module for checking transect pairs

* added check for missing shapefile files

* incidentals don't have observer and point_type checks

* results sorted by pass/fail and added column

* selections only show for validated results

* transect id check added

* soft checks are now in yellow as warnings

* added which members are not in valid set

* Added dbWrite shiny module

* Added geomUpload shiny module

* Added testthat framework

* Added validate functions

* Added a `NEWS.md` file to track changes to the package.
