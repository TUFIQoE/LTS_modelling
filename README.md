# LTS_modelling
This repository is dedicated to the data analysis of the Long Term study.

# Getting Started
Navigate to the preliminary_analysis folder to begin. Inside, you'll find the following:

Raw Data Files:

* .json files containing raw results downloaded from the server.
  
Data Transformation:

* phase4_getdata.R: Transforms .json files into a consolidated .csv file named "phase4_update_lts.csv" for further analysis.
Analysis Files:

* analysis_4ph.csv: Facilitates quick analysis with metrics such as the number of watched videos per tester, MOS (Mean Opinion Score), correctly answered content questions, and generates a supporting .csv file ("data_for_plots.csv") for plots.
Plotting:

* plots_for_lts_IEEE_TOM.Rmd: Generates correlation plots, scores distribution plots, and watching time plots.
P1204 Modeling:

* estimated_to_p1204.R: Utilizes "p1204_raw.csv" (containing P1204 scores per video) to produce "phase4_current_results_p1204.csv," a crucial file for modeling.
Model Comparison:

* mos_plots.R: Compares different models using AIC and coefficient comparison, creating relevant plots.

# Bootstrap Analysis
In the bootstrap folder, find the file:

* bootstrap_all.R: Necessary for generating .Rds files, later employed in bootstrap_plots.Rmd.
# GSD Parameters Plot
For the GSD parameters plot, navigate to the pyits folder. It contains the required software.
