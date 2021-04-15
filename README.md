# Mass Media Field Experiment Archive for Replication

This project is to provide dataset and analysis files for [A Mass Media Experiment in Rural Uganda](https://journals.sagepub.com/doi/full/10.1177/0010414020912275) to violence against women by encouraging disclosure. The published paper is [here](https://journals.sagepub.com/doi/full/10.1177/0010414020912275). This multi-wave Randomized Controlled Trials (RCTs) have been conducted by Donald Green, Anna Wilke, and Jasper Cooper. I was responsible for building this archive repository to make this large-scale experiment dataset more accessible to academic commons and the general public.

**Affiliation**: [Institute for Social and Economic Research and Policy](http://iserp.columbia.edu/), Columbia University

**Keywords**: Randomized Controlled Trials, Replication Archive

**Software**: `R`, `LaTex`, `bash`

This repository is also published on [Harvard Dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/2ZRRX2)

## How to run the code 

0. Install the required packages

```bash
   #!/usr/bin/bash
   while IFS=" " read -r package version; 
   do 
     Rscript -e "devtools::install_version('"$package"', version='"$version"')"; 
   done < "requirements.txt"
```

1. Open `PV_replication.Rproj` to ensure that all file paths are set relative to
   the replication archive

2. Open `__main_script.R` and run all scripts from here

3. `True` / `false` logics switch on and off scripts that take a long time to run

## Content Menu

- `01_data/`                            =    Contains raw data
  - boostrap_extrapolation.Rdata        =    Stored output extrapolation
  - cluster_level_data/                 =    Village-level data
    - film_festival.csv                 =    Data on intervention
    - location_data.csv                 =    Data that link cluster IDs to districts
    - sampling_radius.csv               =    Radii for sampling of Rs
    - treatment_assignment.csv          =    Random assignment
  - `codebooks/`                        =    ODK tablet coding
    - endline_choices.csv               =    Endline survey answer choices
    - endline_Qs.csv                    =    Endline survey questions
    - midline_choices.csv               =    Midline survey answer choices
    - midline_Qs.csv                    =    Midline survey questions
    - vht_endline_choices.csv           =    VHT endline survey answer choices
    - vht_endline_Qs.csv                =    Endline VHT survey questions
    - vht_midline_choices.csv           =    VHT midline survey answer choices
    - vht_midline_Qs.csv                =    Midline VHT survey questions
  - `household/  `                      =    Household level data
    - distance_data.csv                 =    Distance of Rs to video hall
    - endline_1.csv                     =    Endline HH survey
    - endline_2.csv                     =    Endline HH survey
    - endline_3.csv                     =    Endline HH survey
    - endline_4.csv                     =    Endline HH survey
    - midline_1.csv                     =    Midline HH survey
    - midline_2.csv                     =    Midline HH survey
    - midline_3_1.csv                   =    Midline HH survey
    - midline_3_2.csv                   =    Midline HH survey
    - midline_3.csv                     =    Midline HH survey
    - midline_4.csv                     =    Midline HH survey
    - midline_5.csv                     =    Midline HH survey
    - random_sampling_1.csv             =    Random sampling of Rs in midline
    - random_sampling_2.csv             =    Random sampling of Rs in midline
  - lasso_selected_covariates.Rdata     =    Covariates selected through lasso
  - UG_VAW_data.Rdata                   =    Stored full data
  - `vht/`                              =    VHT datasets
    - vht_el_1.csv                      =    VHT endline survey
    - vht_ml_1.csv                      =    VHT midline survey
    - vht_ml_2.csv                      =    VHT midline survey
    - vht_ml_3.csv                      =    VHT midline survey
    - vht_ml_4.csv                      =    VHT midline survey
- `02_code/ `                           =    Code scripts
  - __main_script.R                     =    Main script that runs others
  - 00_useful_functions/                =    Functions used throughout
  - 01_codebook/                        =    Scripts that build codebooks
  - 02_load_and_clean_data/             =    Loading and cleaning datasets
  - 03_variable_coding/                 =    Coding outcomes and covariates
  - 04_merging/                         =    Merging datasets
  - 05_covariate_selection/             =    Lasso covariate selection
  - 06_analyses_paper/                  =    Analysis scripts
  - 07_analyses_appendix/               =    Robustness check scripts
- `03_tables/ `                         =    Tables are output here
- `04_figures/`                         =    Figures are output here
- IPV_replication.Rproj                 =    Run everything from here

## Notes

- Most analyses are based on a panel of "compliers" interviewed in both the 
  midline and endline surveys. This subset can be identified using the 
  subset respondent_category == "Complier". 
- We did not re-ask all questions of those in the panel, so their endline 
  responses are merged in from midline.
- The multiple versions of the raw data correspond to the different datasets
  output by ODK / CSO when a change is made to the survey. Each change requires
  a new survey version, thus producing a new dataset.
- The only change made to the raw data files is the removal of PII and variables 
  that are not used in the analysis. 
- All other modifications made to data (changing of values, etc.) in cleaning 
  scripts were implemented by field manager over course of field work. 