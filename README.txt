Reed College 2016 MindStorm Challenge - Tableau

Washington state schools are struggling with meeting testing standards. Can you utilize the dataset to help craft policy recommendations
for K-12 educational institutions in Washington?

I created a K-nearest neighbors (KNN) app that acts as a tool to do peer-wise comparison analysis for specific schools. 
You wouldn't compare Reed College with MIT or Stanford, but you would compare Reed with Swarthmore, Carlton, etc. The KNN model
looks for schools most similar to a selected school (based on 3 predictors: % of low income students, % of English Language Learners (ELL),
and enrollment count). The app selects the k schools with the lowest euclidean distance in predictors from the selected school. The
variable used to compare schools is the percentage of low income students who pass the math state exam (this variable can be exchanged
for a different variable easily if needed). 

Some interesting cases:

A G West Black Hills - Has a 31% pass rate and underperforms its 5 nearest neighbor peers by 11%. When restricted to high schools only,
			only underperforms by 3%
Cascade Elementary - Has an excellent 51% pass rate but still underperforms its 5 nearest neighbor peers by 3%. No pat on the back yet.
Wilson High School - Has an abysmal 25% pass rate and underperforms its 5 nearest neighbor peers by a huge amount (22%). Even when
			restricting to high schools only, Wilson still underperforms by nearly 9%. This school requires some more 
			investigation.
Almira Elementary - Has an average pass rate of 40% but outperforms its 5 nearest neighbors by an astounding 15%.

Overall, this tool can be used to give more context. If a school is doing extremely poorly, it may not be doing that poorly compared to
schools with similar demographics. Similarly, a school can do very well but not that much better than its peers. For policy recommendations,
if a school is greatly underperforming/outperforming its peers, it is not because of the 3 predictors used since the KNN model selects 
schools that are similar in those regards. It would be more efficient to investigate other demographics not used in the model to examine
the reason why there is significant under/over performance.