# Titanic-Kaggle
Trying to predict who lived and died on the Titanic

The first file "Titanic.R" is my first attempt without seeking any tutorials or help. That got me a score of 0.78469, after multiple attempts ofcourse

The second file "R tutorial TS.R" is me following along with a blog containing an R tutorial, to learn things I might have missed out on. It was interesting how the approach to data wrangling was very different form mine. Also, the author performed a lot of  feature engineering, which was very interesting, since I didn't do any at all, when on my own. However, after performing his random forest, he got less of a score than mine: 0.77512. Then, after that, in the end of the article, he introduced a package called "party" which apparantly uses "a forest of conditional inference trees". I followed along and surprisingly got a score of 0.81340! This was even better than using xgboost or ANNs.
