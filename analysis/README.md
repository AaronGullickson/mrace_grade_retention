# analysis directory

This is the main directory for the analysis. All of the scripts necessary to run the analysis are located here. All raw datasets are in the `input` directory and all constructed datasets and saved statistical output is in the `output` directory.

The bash shell script `run_entire_project.sh` will run the entire analysis from scratch. Otherwise, the scripts should be run in the following order:

1. `check_packages.R` - this script will check for required libraries and install them if missing.
2. `useful_functions.R` - this script will add any custom functions used in the analysis.
3. `organize_data.R`  - This script will read in the raw data and construct an analytical dataset for further analysis.
4. `analysis_spec_tests.Rmd`- This R Markdown file tests different specifications of the baseline model used in the analysis.
5. `analysis_main.Rmd` - This R Markdown file runs the main analysis and produces an HTML file of the output.
6. `analysis_resources.Rmd` - This R Markdown file conducts a supplementary analysis of resource differentials by racial group.
