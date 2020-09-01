This (draft) tool is intended to analyze effluent water quality monitoring data from industrial stormwater discharge facilities that report to the [California State Water Resources Control Board](https://www.waterboards.ca.gov/) (the tool draws from the data stored in the Waterboard's [SMARTS](https://smarts.waterboards.ca.gov/smarts/faces/SwSmartsLogin.xhtml) database). The tool calculates basic summary statistics for a selected parameter at each facility, and plots the results on an interactive map. Users can also download the tabular dataset of summary statistics, as well as the disaggregated sampling data used to calculate the summary statistics.

## Application Location
The application is available at: https://cawaterdatadive.shinyapps.io/Stormwater-Assessment-Tool/

## Data Sources
#### Stormwater Monitoring and Facility Information
Industrial stormwater effluent monitoring data, as well as information about the industrial discharge facilities where that monitoring occurs, is retrieved from the datasets stored on the data.ca.gov portal, here:
- [Industrial Stormwater Effluent Monitoring Data](https://data.ca.gov/dataset/stormwater-regulatory-including-enforcement-actions-information-and-water-quality-results/resource/7871e8fe-576d-4940-acdf-eca0b399c1aa)
- [Industrial Stormwater Discharger Facility Information](https://data.ca.gov/dataset/stormwater-regulatory-including-enforcement-actions-information-and-water-quality-results/resource/33e69394-83ec-4872-b644-b9f494de1824)

The data contained in those two datasets is from the California Water Resources Control Board's Stormwater Multiple Application and Report Tracking System (SMARTS). These two datasets are updated daily using data contained in files on the [SMARTS Public Access Interface](https://smarts.waterboards.ca.gov/smarts/faces/SwSmartsLogin.xhtml), which can be accessed by following the link to *View SW Data* → *Download NOI Data By Regional Board* → from the dropdown menu select *State Board*. The files used are:
- *Industrial Ad Hoc Reports - Parameter Data* (Monitoring Data)
- *Industrial Application Specific Data* (Facility Data)

The tools used to automatically upload the SMARTS data to the data.ca.gov portal at regular (daily) intervals are available here: https://github.com/daltare/SMARTS_DataPortal_Automation