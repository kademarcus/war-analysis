# Arab Israeli War Analysis 

## Overview
- This was a semester long class paper (Global Conflicts) where explanations of war in political science were tested empircialy
- Explanations of war that were explored were power levels (Balance of Power Theory) and leader characteristics.
- The final paper includes a history of the Arab Israeli War of 1948, arguments for why certain explanations would apply in this case, and empirical testing.

## Tools used
- This report used exclusivley R for data processing, cleaning, and analysis.

## Data
- The data this report used comes from Military Mark Souva's Material Military Power (https://journals.sagepub.com/doi/abs/10.1177/00223433221112970)
Correlates of War's Composite Index of National Capability, and Michael Horowitz and Allan Stam's Leader Experience, Attribute, and Decision dataset.
- Citations and where to find the data can be found in the "Report" folder in the Works Cited Page

## Analaysis
- For the power analysis, I argued that the Balance of Power theory applies, and compared the power levels of the countries involved in the Arab Israeli War of 1948
- Israel is not coded as a state until 1948 (exactly when the war starts), so I ran a linear regression to see if Israels power was able to be predicted based on future data (which it was) 
- For the leader analysis, I replicated Horowitz and Stam's logistic regression, and outputted predicted probabilites of initation and compared it to the average probability of leaders to initiate.
- Figures can be found in both the report and the "figures" folder within the "Report folder
  
## Conclusions
- This war did fit the profile of the Balance of Power theory of War, as Israel had a large increase in power that outpaced the other nations at war with it
- This war also supproted Horowitz and Stam's logistic regression analysis, as the probabilites of leader initation were either at the average probability or higher. 
