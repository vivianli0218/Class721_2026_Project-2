# Air Quality at RDU Airport

## Project description
This project processes daily air quality measurements and explores patterns in CO, PM2.5, 
and O3 concentrations using annual summary tables and monthly plots. The analysis provides 
an initial description of potential air pollution exposures for a proposed study of sickle 
cell disease in Durham County, North Carolina.

## Data source
The data were provided for BIOS721 Project 2 and contain measurements from the EPA air 
quality monitor at RDU airport. The three CSV files cover 2018, 2019, and January 
through April 2020. No health outcome data are included in this project.

## File structure
Project 2.Rproj: RStudio project file.

data/: Original air quality CSV files for 2018, 2019, and 2020.

process_aq.R: Data cleaning function and analysis code.

report.Rmd: RMarkdown source containing the code, tables, plots, and written explanations.

report.docx: Word report generated from report.Rmd.

README.md: Project description and reproduction instructions.

## How to reproduce
Download and extract the project folder.

Open Project 2.Rproj in RStudio.

Install the required packages if they are not already installed:

install.packages(c("dplyr", "ggplot2", "knitr", "rmarkdown"))

Open report.Rmd and select Knit to Word. RStudio provides Pandoc, which is 
required to generate the Word document.

The report will be saved as report.docx. Keep the original CSV files in the 
data/ folder so the relative file paths work correctly.


## Author and date
Author: Vivian Li
Date: November 18, 2026