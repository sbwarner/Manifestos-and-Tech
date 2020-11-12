# Manifestos-and-Tech
Code and documents relating to project on manifestos, technology, and tech-induced job loss

# Directory

manifesto_data.RData - R data environment. Contains main dataframe with sentence text, unique IDs, party, date, country, text sentiment score, issue ideology scores from V-DEm, and counts of a core set of 36 keywords. Also includes vectors with text feature names to use in subsetting data for analysis.

manifestos_reference_sheet.xlsx - Spreadsheet of all CMP documents with party, data, and country. (Our project only includes the 10% published in English.)

vdem_crosswalk.csv - Spreadsheet of CMP party names and V-DEM party IDs for merging

V-Dem-CPD-Party-V1.csv - V-DEM data on 384 parties

1 Prepare dataframe.R - Script for downloading manifestos from CMP API, converting them to dataframe, assigning IDs, attaching text features, and importing metadata

2 Draw first sample for hand-coding.R - Simple script used to draw first round of sentences for hand-coding

3 Active learning and iterative sampling.R - Creates RFs based on specs that lead to lowest error, tracks OOB error as more sentences are coded, predicts values for sentences and lists preliminary results, draws iterative samples based on predictive uncertainty and manifesto undercoverage.

4 Add keywords as text features.R - Script to add keywords to dataframe after each round of coding.
