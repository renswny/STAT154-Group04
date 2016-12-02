# STAT154-Group04

STAT154 Final Project: Unclassified Released Emails of HRC
Authors: Pranay Singal, Kensen Tan, Renee Sweeney, Abigail Chaver
Date: December 2, 2016

This project covers several classification models built on the released emails from Hillary Clinton's server.


## Project Structure:

### Standalone Files
* README
* report.pdf  is our final report
* predict.txt is our best prediction, generated from our RF model
* predict2.txt was a prediction generated from our SVM, which performed worse than RF

### code/ 
This folder contains the scripts used to preprocess the data and build each of the three models. It also includes a script for an alternative method we tried, Latent Dirichlet Allocation, and a trivial script for generating some plots.

### data/  
This folder contains several types of data.
*  The raw TSVs of the email data. 
* The 4 "processed" CSVs cover a few versions of the main data used for analysis. The second one of these was the most useful.
* The "topics" datasets were generated from from filtered methods, with 15 being most useful
* The .Rdata files were used to save workspace images for us to communicate and pass objects more easily

### images/ 
This folder contains plots that were generated through scripts, including some screenshots. All images in the report should also be contained in this folder