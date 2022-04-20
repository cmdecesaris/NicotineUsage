# Personality and Demographic Characteristics Influencing Nicotine Usage Status

## Project Introduction 

Nicotine is a highly addictive substance known to cause cancer and other health problems (Saha et al,
2007). To better understand the personality and demographic characteristics which influence nicotine
usage, a survey dataset containing responses from 1885 participants regarding their drug usage history
and personality scores was processed and fit to a baseline-odds logistic regression model with three
outcomes: Never Used, Recent User, and Past User. Model inference revealed that men, people with high
Openness Scores, lower Conscientiousness Scores, and without university degrees were more likely to be
Recent or Past Users. No lack of fit was detected in the model.


## Data Description

The dataset contained the demographics, personality assessment, and drug usage history of 1885 participants. Personality
characteristics were quantified using the Big Five personality traits model and alternative models relying
on impulsivity and sensation-seeking behaviors from phycological research (Fehrman et al, 2017).
The drug consumption dataset was collected through an anonymous online survey and contains 5
categorical variables, 7 numerical score variables, and a series of categorical responses indicating the
participant’s drug usage history for 18 legal and illegal substances. The data was originally presented in a numerical code format which was deciphered in the Preprocessing.R file.

Original Owners of the Dataset: Elaine Fehrman, Vincent Egan, Evgeny M. Mirkes
Dataset Doner: Evgeny M. Mirkes

For more about the data see: https://archive.ics.uci.edu/ml/datasets/Drug+consumption+%28quantified%29

## Running the Project

1. Preprocessing.R is sourced to run before the Graphs_Tables.R and Logistic_Regression.R files
2. The EDA_Tables.R, EDA_Graphs.R and BaselineOdds_Model.R files can be run independently  

## Files in this Repository 
1. data/drug_consumption.csv: data donated by D. Wangner used in this project
2. Preprocessing.R: File which cleans and wrangles the data into a suitable form for visualizaiton and modeling approaches
3. EDA_Graphs.R: File which generates graphs and found in the Report.pdf and Summary_Poster.pdf
4. EDA_Tables.R: File which generates tables and found in the Report.pdf and Summary_Poster.pdf
5. BaselineOdds_Model.R: File contains the modeling approach, diagnostics, and evaluation of models
6. Report.pdf: A detailed report of the project and methods used in code files. Reference this file for model and data interpretations
7. Summary_Poster.pdf: Summary of the report and key parts of the project


## References

Allen, A. M., Scheuermann, T. S., Nollen, N., Hatsukami, D., & Ahluwalia, J. S. (2016). Gender
Differences in Smoking Behavior and Dependence Motives Among Daily and Nondaily Smokers. Nicotine
& tobacco research : official journal of the Society for Research on Nicotine and Tobacco, 18(6), 1408–
1413. https://doi.org/10.1093/ntr/ntv138

Benowitz N. L. (2010). Nicotine addiction. The New England journal of medicine, 362(24), 2295–2303.
https://doi.org/10.1056/NEJMra0809890

Faraway, J. (2016). Extending the linear model with R. Second Edition, Chapman and Hall. ISBN
9781498720960

Fehrman, Elaine & Muhammad, Awaz & Mirkes, Evgeny & Egan, Vincent & Gorban, Alexander. (2017).
The Five Factor Model of Personality and Evaluation of Drug Consumption Risk. 10.1007/978-3-319-
55723-6_18

Saha, S. P., Bhalla, D. K., Whayne, T. F., Jr, & Gairola, C. (2007). Cigarette smoke and adverse health
effects: An overview of research trends and future needs. The International journal of angiology : official
publication of the International College of Angiology, Inc, 16(3), 77–83. https://doi.org/10.1055/s-0031-
1278254


