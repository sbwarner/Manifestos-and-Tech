# Manifestos-and-Tech
Code and documents relating to project on manifestos, technology, and tech-induced job loss

# Directory

manifesto_data.RData - R data environment. Contains main dataframe with sentence text, unique IDs, party, date, country, text sentiment score, and counts of a core set of 15 keywords. Also includes vectors with text feature names to use in subsetting data for analysis.

manifestos_reference_sheet.xlsx - Spreadsheet of all CMP documents with party, data, and country. (Our project only includes the 10% published in English.)

1 Prepare dataframe.R - Script for downloading manifestos from CMP API, converting them to dataframe, assigning IDs, attaching text features, and importing metadata

2 Draw first sample for hand-coding.R - Simple script used to draw first round of sentences for hand-coding

3 Active learning and iterative sampling - Creates RFs based on specs that lead to lowest error, tracks OOB error as more sentences are coded, predicts values for sentences and lists preliminary results, draws iterative samples based on predictive uncertainty and manifesto undercoverage.
