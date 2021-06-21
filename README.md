# sensitivity_paper
Code associated with the paper "Mismatch between bird species sensitivity and the protection of intact habitats across the Americas"

This file describes the main goal of each script.

GBrow is the name of the general folder in which all data are stored

HFI.01.Data.export.R: Extract eBird data from raw data in the folder "0.Raw.data", makes a first triage and store them (with different files for data used in analyses and data used to calculate observer calibration index).
HFI.02A and B: Calculate the observer calibration index (A prepare the dataset and B runs models and calculates the index)
HFI.03 Data.format: Subset the data, add the observer calibration information and removes duplicates
HFI.04: Extract GIS values for every checklists (coded by site rather than checklist to avoid running X times a calculation for X checklists made at the same site
HFI.05: Merge checklists in a single table
HFI.06: Apply taxonomy transformation using a table provided by CLO (HBW 3.0-eBird v2018 match). It involves some manual steps: the code stops and says "there is a issue with this species" and I go check manually what I should do, write in the speadsheets that will be used in this code (and is attached in this ZIP) and the code continues
HFI.07A and B: Script reload the raw data, extract breeding code and adds it in the used data (because I removed breeding code at the beginning and it was faster to do that than starting from the scratch). B Calculates breeding season per latitude and apply the subsequent subset to all data
HFI.08: Extracts or calculate the species-level data and stores in a common table with 1 species per line
HFI.10: Run the sensitivity model for each species and stores the value of sensitivity
HFI.10c: Extract the range of HF for each species (ie 1 and 99 quantiles)
HFI.11: Removes species estimates when insufficient contrast in HF. Runs the impute model, plots link between traits and sensitivity and estimates imputed sensitivity
HFI.20: Extract from distribution the species present in each ecoregion.
HFI.21: Extract the number of sensitive species per ecoregion (and proportion, and number of threatened...). Some calculations are eventually not used in the paper
HFI.30: Extract GIS values per ecoregion (coverage by intact protected habitats, trends...)
HFI.31: Contrasts sensitivity and habitat of ecoregions and plot (Fig.2D, 2F)
HFI.32: Calculates the proportion of intact protected habitats per species, contrast with species thresholds (Rodrigues et al. 2004) and maps species of major concern
