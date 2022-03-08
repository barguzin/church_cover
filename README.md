# Data and code for the final project in GEOG 294 SPRING 2022 on Multiobjective Optimization 

This project investigates the rationale behind constructing 200 new temples in Moscow, Russia under the 200 temples initiative. The objective function is cast to maximize coverage and minimize overlap. 

## Files 

### Data Pre-Processing 

* prep_church_data.R - a file that prepares the data for further analysis. Saves the following geojson for further processing: 
    * **../church_200.geojson** (file with locations of temples under *200 temples* program)
    * **../church_mos_temples_all.geojson** (file with location of all the temples in and around Moscow)

