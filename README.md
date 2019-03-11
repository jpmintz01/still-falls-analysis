# still-falls-analysis
Analysis Code (R) and Data file (.csv) from Jan 2019 Experiment for Dissertation

#To do (must do):
- add "TFT" column to pw_all_data_with_demo (which adds TFT choices)
- adjust data parse to include NIPR fails (pw-only) 
-- functionalize this in order to quickly swap
-- check fail_data vs good_data
- a manual check and calculation on:
-- one data analysis
-- one demographic alignment (to check data and demographic info are actually matched)
- Demographic data visualization

#To do (nice to have):
- Clean up code
-- functionalize
-- release unused variables & data frames before analysis section
- comment code
- separate out data loading and transformation from analysis
- use only pw_all_data_with_demo and rps_all_data_with_demo in analysis
- rename pw_all_data_with_demo to something shorter ("pw"?)


#Done:
- Adjust data parse to remove test entries (incl attention check = Texas)

Add line from RStudio