#Culverts_PI Paper Repo

Who prioritizes what? A cross jurisdictional comparative analysis of salmon fish passage strategies in Western Washington


##
**Repository Structure**

/code/: Markdown files and script to explore fish barrier culverts. The scripts in /code/ transform the files in /data/ into the files in /output/.

/data/: Raw data inputs. 

/docs/: Files for report hosting here. Please do not edit as these files support the webpages.

/output/: Objects (figures, maps, intermediate datasets, etc.) generated by scripts in /code/.



##
**Getting Started:**

Within this repo, the markdown file code/wdfw_explore_ra.R/ is for exploring the WDFW Fish Passage Inventory and how it relates to the Fish Barrier Culverts of interest. This document will require the user to locally download the WDFW zip file since it is too large to include in the data folder on this repo. This is the first document to familarize yourself with. 
Catalina added to this file in order to calculate some preliminary summary stastics on the WDFW inventory.


##
**Comparing other organizations culvert information with WDFW:**

The next markdown titled code/wsdot_culverts_ra.R/ will demonstrate how the WDFW Fish Passage Inventory data can be used to further insight in context to other projects and juristictions. This example is related to WSDOT specifically.

##
**Data Scraping Code:**

Several R scripts in /code/: are either used to extract PDF data tables into CSV files or to pull CSV data from ArcGis Online tools. Since the ArcGis Online tools are being updated regularly by entities like King County, they should be scraped regularly as well. These documents include: bellingham_pdf_extraction.R, web_mining_script.R, wsdot_pdf_extraction.R

##
**Investigating Local PIs

within /code the document king_pi.R briefly explores the habitat gain achieved by removing different numbers of high scoring barriers. 

--------
**Catalina took over Connors work starting in January 2022. The files decribed below were written by Catalina.

##
**Mapping

/code: The overview_mapping.R file includes code for producing overview maps of the case area and the PI jurisdictions within the case area. This is presented as Figure 1 in the report document.

There is a folder within /code titled FinalProj_SpatialMapping. This folder is a self contained documentation of the maps I produced in Sunny's spatial mapping class. These maps are not included in the final report, but may be useful for further exploration of the spatial data. The project identifies culverts scored by both WDFW and local PIs, and then compares the two scores.

**Comparing Scoring Weight and Variable Comparison

/code: The file linear_score_weight.Rmd is code to plot the weights of variables within the 5 linear PI's (Bellingham, Thurston, Chehalis, CWCC, King).
The scoring weight was compiled from individual reports on the PI's within an excel document in the output folder. This file also includes the variable comparision figures which show number of unique variables by each jurisdiction and each category.

**Summary Statistics

/code: The file summarystatistics.R includes histogram figures showing distribution and skew of each PI. This file also starts to explore the different fish portfolios within the WDFW PI, but we didn't finish exploring this topic.

**WDFW Non-Linear Weight Analysis

/code: The file WDFW_Score_Weight.Rmd explores the WDFW equation to better understand the trade offs under different cost and ESA listing scenarios.
