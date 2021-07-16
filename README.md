# **Masters Thesis Project**
## **Masters Thesis Project**

This is the R code for my thesis project, which scraped and analysed vaccine related tweets from Twitter, predicted vaccination status in children using machine learning models built in R Studio with patient survey data, visualises the results of these models/model performance comparisons, and predicted future rates of vaccinations in children using time series forecasting.

## **Google Trends Analysis**
The Google Trends analysis analysed the interest in vaccine related topics over time and across different US states:

<img width="1594" alt="image" src="https://user-images.githubusercontent.com/45274520/125887952-12d44138-c239-42e2-88df-8bb7ccfd9976.png">

<img width="1771" alt="image" src="https://user-images.githubusercontent.com/45274520/125887884-1ec6e906-6de3-41fa-975f-e8401375cb65.png">

## **Twitter Analysis Using Visualisations and NLP**
Twitter data from Texas related to vaccinations was scraped using the TwitterR package, and analysed using visualisations. These included visuals of word freqencies and sentiment analysis. 

<img width="1722" alt="image" src="https://user-images.githubusercontent.com/45274520/125888085-f911f285-6fa6-4a11-a2a1-e3df6c97c9d0.png">

<img width="1604" alt="image" src="https://user-images.githubusercontent.com/45274520/125888126-0e0422fb-d843-4a55-9e21-1d1a6858d80e.png">

<img width="631" alt="image" src="https://user-images.githubusercontent.com/45274520/125888171-566223ac-ebf3-41e0-80f8-f51988e60627.png">


## **Predictive Models for Vaccination Status**

Ten machine learning models were built, and compared in terms of accuracy, sensitivity and specificity. These models were complex because of the large number of independent variables in the training dataset, as shown by the artificial neural network below:

<img width="1740" alt="image" src="https://user-images.githubusercontent.com/45274520/125877371-7fb8e45a-1c04-4571-bf06-6bc8d98b47fd.png">

The accuracy of the models was assessed:

<img width="690" alt="image" src="https://user-images.githubusercontent.com/45274520/125888469-5b2e5379-32a9-44c8-87ec-e0527e5a24b4.png">

## **Timeseries Forecasting**
Timeseries analysis and forecasting was performed to predict the future rates of vaccinations in Texas, Oklahoma and Arkansas based on historical vaccination rates for these states. 

<img width="637" alt="image" src="https://user-images.githubusercontent.com/45274520/125888319-1156a78a-1f5b-4023-915b-d6bf9fa9ce92.png">
