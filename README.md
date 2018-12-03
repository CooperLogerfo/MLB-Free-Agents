# MLB-Free-Agents
Analysis on MLB Free Agents

The "predict" folder houses the data and R code for predicting MLB player salaries. I used the Random Forest algorithm through
the "randomForest" R package for the prediction. I found setting mtry = 4 produces a minimum mse =  0.46802. This is not a fantastic result, but I have new data using my updated <a href = "https://github.com/CooperLogerfo/MLB-FA-Webscrape " > webscrape </a> and more experience with Random Foresets that will hopefully allow me to train a better model with which to predict 2018 Free Agents.

The "MLBFA_shiny" folder houses the data and R code for the shiny web application I wrote. It is an interacitve choropleth map written using the "leaflet" package and plots data on US-born MLB Free Agents. 
