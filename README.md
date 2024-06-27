# AI-Incident-Research

This repository exists to open source our manipulations of the AI Incident Database, found at: https://incidentdatabase.ai, as well as our various usage of the database to encourage further research.

The csv folder includes: <br>
final.csv - Our finalized csv after all data cleaning, processing, and modeling.

The src folder includes: <br>
initialClean.R - The first part of the pipeline which removes redundant variables, cleans titles + descriptions, and arranges the dataset by date. <br>
NaiveBayesModel.R - The second part of the pipeline which undergoes a two-step verification process in categorizing new AI incidents based on their title and description (+ other variables) <br>
revisedModel.R - This takes the results from the previous program (NaiveBayesModel.R) and provides the accuracy/confusion matrix of the new categorization. <br> 
finalForecast.R - This is our implementation of the forecast package, utilizing an ARIMA model to predict future months.
