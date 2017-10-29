# EARL-Boston2017-Presentation
Files associated with analysis and presentation for EARL Boston 2017

The presentation at EARL Boston 2017 will include interesting insights into the extent of homelessness in the US, and how we can use Machine Learning techniques to provide a smart solution.

This includes analysis of homeless datasets from public sites including HUD.  The datasets used for this analysis are available in 'Datasets' folder.

As part of pre-processing, some cleanup of the data was done and the lat/lon information was included for the cities by in the dataset.  As this involves generating the geocodes, this is captured into a separate file "Extract-Lat-Lon-Info-Script.R" located in the R-Scripts folder.  This information can be merged with other datasets that are used in the analysis.

The script "Homeless-Analysis-Script-1.R" located in R-Scripts folder primarily generates the various maps used in the presentation.  This is also generated as an R-Notebook titled "HomelessData-Visualizations".

