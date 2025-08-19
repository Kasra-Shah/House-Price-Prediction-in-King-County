King County House Price Prediction
A machine learning project to predict house sale prices in King County, Washington, USA, using various regression models. This project was completed as part of an Applied Statistical Analysis course.

![Linear Regression Results](https://github.com/Kasra-Shah/House-Price-Prediction-in-King-County/raw/main/regression-plot.png)

📖 Project Overview
Determining the accurate price of a property is a complex challenge in any real estate market. This project aims to create a predictive model that can estimate house prices based on features like number of bedrooms, square footage, location, and condition. The goal was to compare the performance of different regression models to find the most accurate one.

📊 Dataset
The dataset consists of house sale records for King County between May 2014 and May 2015.

Source: [Kaggle - House Sales in King County](https://www.kaggle.com/datasets/harlfoxem/housesalesprediction/data)

Number of Instances: 21,612

Number of Features: 21

Key Features: price, bedrooms, bathrooms, sqft_living, sqft_lot, floors, waterfront, view, condition, grade, yr_built, lat, long, and more.

🛠️ Models & Methodology
The data was split into training (80%) and testing (20%) sets. The training set was further divided for estimation and validation to fine-tune model parameters. Three regression models were implemented and evaluated:

Linear Regression: Multiple models were fitted using Forward Selection. The best model included a combination of key features like grade, square footage, and location-based attributes.

K-Nearest Neighbors (KNN): Data was normalized due to the algorithm's sensitivity to scale. The optimal value for K was found to be 13 through validation.

Decision Tree: Models were tuned using the cp (complexity parameter) and minsplit parameters to prevent overfitting and find the best structure.

📈 Results & Conclusion
The models were evaluated based on their Root Mean Square Error (RMSE) on the test set.

Model	RMSE
Linear Regression	154,850
K-Nearest Neighbors (KNN)	184,584
Decision Tree	192,341
Conclusion: The Linear Regression model outperformed the other models for this specific dataset and problem, achieving the lowest error and providing the most reliable predictions.

🚀 How to Run the Code
This project was implemented in R.

Clone the repository:
git clone https://github.com/Kasra-Shah/House-Price-Prediction-in-King-County.git
Open the R Project file in RStudio.

Install required packages: Ensure the following R packages are installed:
tidyverse (for data manipulation and plotting)
caret (for machine learning functions)
rpart (for decision trees)
install.packages(c("tidyverse", "caret", "rpart"))
Run the R script (regression_analysis.R) to reproduce the analysis, model training, and results.

📁 Repository Structure
text
House-Price-Prediction-in-King-County/
├── data/
│   └── kc_house_data.csv     # Original dataset
├── scripts/
├────── regression.R # Main R script for data cleaning, analysis, and modeling
└────── regression_markdown.Rmd # Rmarkdown for better understanding of the analyses process
👨‍💻 Author
Kasra Shahriari
Industrial Engineering Student

📜 License
This project is for academic purposes.
