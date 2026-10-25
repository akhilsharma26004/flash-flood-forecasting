Persistence-Guided Evaluation of Machine Learning for Water-Level Forecasting
An interactive R Shiny dashboard for exploring water-level forecasting, evaluating machine-learning models, and examining the effects of simulated rainfall-feature masking.

📌 Project Overview

Flash-flood early warning depends on timely and reliable water-level forecasts. This project evaluates one-hour-ahead water-level forecasting using historical hydrological observations and rainfall-related features.
The project compares a persistence baseline with Random Forest models and examines how simulated masking of rainfall features affects predictive performance.

✨ Features

Interactive dashboard: Explore forecasting results through an R Shiny interface.
Model comparison: Examine persistence and Random Forest forecasting results.
Rainfall-feature masking: Explore model performance under different simulated masking levels.
Results visualization: View available evaluation metrics and research figures.
Data exploration: Inspect available data summaries and preview records.
Export support: Download supported tables or results from the dashboard.

🛠️ Technologies Used
>R
>Shiny
>Random Forest
>Data visualization and statistical evaluation

📊 Experimental Overview

The experiments use historical water-level and rainfall-related observations to evaluate one-hour-ahead forecasting.
The evaluated approaches include:
Persistence baseline: Uses the current water level as the forecast for the next hour.
Random Forest with local features: Uses water-level and local rainfall information.
Random Forest with augmented features: Uses an expanded set of rainfall-related predictors.
The evaluation also examines different levels of simulated rainfall-feature masking.

Key Finding

In the evaluated test set, the persistence baseline achieved lower prediction errors than the Random Forest models. This highlights the importance of comparing machine-learning methods against simple, strong baselines in hydrological forecasting.
Simulated rainfall-feature masking was also evaluated. These experiments do not establish performance during actual sensor failures or demonstrate operational early-warning capability.
