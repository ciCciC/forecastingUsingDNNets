# Time series forecasting using Deep Neural Net models
The objective of this repo is to design, train and evaluate multiple forecast models utilizing deep neural nets in order to forecast American energy consumption.

## Data
"Over 10 years of hourly energy consumption data from PJM in Megawatts. PJM Interconnection LLC (PJM) is a regional transmission organization (RTO) in the United States."
[American Energy Power (Hourly Energy Consumption Data)](https://www.kaggle.com/datasets/robikscube/hourly-energy-consumption?resource=download&select=AEP_hourly.csv)

## Code
- [Complex models (in Python)](https://github.com/ciCciC/forecastingUsingDNNets/blob/main/notebooks/complex_models_energy.ipynb)
  - Baseline
  - Linear Model
  - Deep Dense Neural Network
  - Convolution Neural Network
  - Recurrent Neural Network with Long-Short Term Memory (RNN-LSTM)
- [Prophet (in R)](https://github.com/ciCciC/forecastingUsingDNNets/blob/main/notebooks/r)
  - An additive model developed by Meta (Facebook)


# Evaluation
<img src="./notebooks/comparison.png" height=300>

# Prerequisite
- pip install numpy pandas matplotlib seaborn scikit-learn jupyter
- conda install geopandas prophet

- Tensorflow M1 -> https://developer.apple.com/metal/tensorflow-plugin
