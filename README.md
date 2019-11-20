# Predicting the cab booking cancellations
In Yourcabs, not all drivers are coming up for the scheduled calls and it increases the discontented customers. And, other competitor, Uber is coming in India soon so Yourcabs need to manage their drivers by arranging available resources and reduce the cab cancellations. The dataset sums up a varied set of features about customers collected by Yourcabs from 2011 to through 2013. The goal is to predict whether a cab booking will get cancelled. It is needed to explore the variables which may affect the cancellation of the ride and suggests a revolutionary approach to predict the cancellations. The target variable is the binary indicator of whether a ride was canceled. Because the problem is supervised learning. the important key is to build a concise model of the distribution of class labels in terms of predictor features. There are a lot of missing values, inconsistent data types so it is needed to explore the data and handle these kinds of before building the prediction models. Feature selection methods are used to improve performance and reduce features. We implemented three different machine learning techniques (Logistic Regression, Decision Tree Algorithms, Random Forest) to predict the dependent variable. The feature selection and correlation analysis between the two variables are performed to uncover the most important variables. Features such as distance, from_month, hour_avg_cancellation, vehicle_model_ide, and waiting time are highly significant within the target prediction. Choosing the optimum features are used to maximize the prediction accuracy. Overall, after comparing the AUC results of these different techniques on classifying the cab cancellation, the Random Forest turns out to be the best prediction model.

## 1. Introduction
In this project, our objective is to increase prediction accuracy by finding the main factors that predict whether a ride was canceled in this case. Our first step before building our model, we viewed all the variables given to us and determined what the different variables mean and we check for any missing values or other ways to clean the dataset. Data preparations, namely Exploratory Data Analysis include handling the missing data and imbalanced data, converting categorical features (dummy variables), selecting the important features, and data partitions. After preprocessing the data, we build models based on our dataset and evaluate their predictive performance by using the confusion matrix and lift charts.

## 2. The Objective of Analysis
Our objective is to find a best classifier that would predict whether or not the driver has cancelled the client’s call by using data obtained from the taxi company. By predicting possible cancellations an hour before the pickup time, YourCabs will be better able to manage its vendors and drivers by providing them with up-to-date information about cancellations and reduce the dissatisfaction incurred from drivers’ no-show. The following are the main goals of this project:
1. Visualize the data to understand the categories of each attribute and their influence on the dependent variable.
2. Data preprocessing (Handling the imbalances in the dataset, feature Selection, etc)
3. Build the classification models (Logistic Regression, Decision Tree Algorithms,
Random Forest) and find the best model.
4. Compare and evaluate the AUC for all models
5. Explaining the influence of each independent variable for the target variable

## 3. Data understanding (Data descriptive)
Our data are related with taxi cancellation for 10,000 clients. There are 18 input variables and a target variable (Cab cancellation). The dataset consists of 10,000 bookings and
has a total of 19 variables. It is needed to predict whether the cab is cancelled.
