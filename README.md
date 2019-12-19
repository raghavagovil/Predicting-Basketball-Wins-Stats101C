# Motivation

We strove to use the presented data to predict whether a home team will win or lose a given basketball game.

# Data Cleaning and Transforming

Thankfully, there were no missing values to be dealt with in the data. We performed a cursory spot check and found no outliers in the columns of the data via the 1.5*IQR rule.

To construct our models, we decided to mostly use variables that we created on our own. We did this because as avid basketball analytics enthusiasts, we went into this project with the mindset that the strongest predictors of performance in basketball are advanced statistics, i.e., statistics not gathered simply from the box-score. While there were some advanced metrics in the data such as the plus-minus and the pmx(U/W) variables, we decided it would be prudent to engineer some commonly used advanced basketball statistics that have helped push the evaluation of the game forward in recent decades.

We used the data to calculate fairly common advanced basketball statistics on a teal-level basis using formulas and definitions from the impeccable [Basketball Reference](https://www.basketball-reference.com/about/glossary.html). Full formulas and definitions can be found on their website. The statistics we calculated included:

- Possessions: An estimate of the number of possessions a team had during a game. 

- Offensive Rating: An estimate of the number of points a team ***scored*** per 100 possessions. 

- Defensive Rating: An estimate of the number of points a team ***allowed*** per 100 possessions. 

- Effective Field Goal Percentage: A measure of shooting efficiency that adjusts for the value of a three-pointer being higher than that of a 2-point field goal. 

- True Shooting Percentage: A measure of shooting efficiency that takes into account 2-point field goals, 3-point field goals and free throws.  

- Turnover Percentage: An estimate of the number of turnovers a team committed per 100 possessions.

In addition to these statistics that we calculated, we also calculated win percentages across the entire training data for each team to get a very rough ranking of the teams. 

After calculating all of these variables, we took differences of corresponding team scored variables for the home and visiting teams. For example, to create our Offensive Rating difference variable, we subtracted the home team's Offensive Rating from the visiting team's Offensive Rating. By doing this, we were able to include less variables in our model and thus decrease complexity. It also made interpretability of the model better, as we could clearly see how positive and negative differences in certain statistics impacted home team winning probability. 

# Variable Selection and Modeling

We used LASSO regression to select the variables our model, where lambda was chosen via cross-validation. We used LASSO so that we could throw away variables that were not "carrying their weight" (i.e., not relatively significant for prediction) and keep only our important variables. 

After variable selection via LASSO, we implemented a logistic regression model with those variables. We experimented with tree-based models such as random forest and XGBoost, but found that both of those methods were prone to making disproportionate false positive errors. The misclassification rate on the whole training set with our model was 32.03%. To avoid overfitting, we performed *k*-fold cross-validation (*k* = 10) with the model on the training data to obtain a misclassification rate of 32.34%.

# Conclusion

After ordering the variables in the model by the absolute values of their t-statistics, the three most important variables to predict whether the home team would win or not were **Difference in Win Percentage**, **Difference in Defensive Rating** and **Difference in Plus-Minus, Starter 3**. It is intuitive that difference in win percentage would be a strong predictor of whether the home team wins or not, as at its most basic, it is a measure of which team is historically stronger. We found the importance of defensive rating, though, to be particularly noteworthy. 

We feel that our model worked well (relative to the Kaggle leaderboard) because of our use of advanced statistics. Defensive rating is a great example of the power an advanced statistic could harness; no one variable present in the raw training data could give us the explanatory power of a holistic view of defense that the combination of variables that culminated in defensive rating did. By using such advanced metrics, we were able to stand on the shoulders of the giants of basketball analytics and leverage their work to predict the most important aspect of any sport: victory. 
