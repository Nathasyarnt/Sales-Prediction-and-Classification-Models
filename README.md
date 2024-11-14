# Sales-Prediction-and-Classification-Models

## Project Objective
Amazon is an American multinational technology company based in Seattle, United States. Amazon currently focuses on e-Commerce, cloud computing, digital streaming, and artificial intelligence. The company hosts approximately 1,000,000,000 gigabytes of data on over 1,400,000 servers. As one of the companies with a significant e-Commerce business, Amazon is at high risk of retail fraud. The company uses machine learning algorithms to find transactions with a higher probability of fraud. This proactive action has helped the company limit clients with excessive product returns. Create a prediction model for the amount of transactions and a classification model for the type of transactions and determine the best model among the resulting models.

## Dataset and Src
- <a href="https://github.com/Nathasyarnt/Sales-Prediction-and-Classification-Models/tree/main/Dataset">Dataset</a>
- <a href="https://github.com/Nathasyarnt/Sales-Prediction-and-Classification-Models/tree/main/Src">Src</a>


## Process
- Conducting a significance test between variables in the training + validation data
- Deleting insignificant variables in the training + validation data and the actual testing data
- Overcoming imbalanced data in the training + validation data
- Dividing the training + validation data into 80:20 (training data: testing data)
- Creating a prediction model using multiple linear regression methods, support vector regression and random forest regression
- Testing all models using testing data
- Selecting the model with the highest R-Sq value, then testing the model with the actual testing data to produce <a href="https://github.com/Nathasyarnt/Sales-Prediction-and-Classification-Models/tree/main/Result">regression prediction results</a>
- Creating a classification model using the binary logistic regression method, support vector machine, decision tree and random forest classification
- Testing all models using testing data
- Selecting the model with the highest R-Sq value, then testing the model with the actual testing data to produce  <a href="https://github.com/Nathasyarnt/Sales-Prediction-and-Classification-Models/tree/main/Result">classification prediction results</a>


## Project Result
- The best prediction model was obtained from the random forest regression method with an R-sq value of 78.55%
- The best classification model was obtained from the binary logistic regression method with an accuracy value of 99.20%
