# Analyse numerical features
range(imbalanceddata$Duration)
range(imbalanceddata$Income_Group)
mean(imbalanceddata$Duration)
mean(imbalanceddata$Income_Group)
var(imbalanceddata$Duration)
var(imbalanceddata$Income_Group)

levels(imbalanceddata$Vaccination_Status) <- c("Unvaccinated", "Vaccinated")

My_Theme = theme(
  axis.title.x = element_text(size = 20),
  axis.text.x = element_text(size = 25),
  axis.title.y = element_text(size = 25),
  plot.title = element_text(size = 22))

bf_histogram <- qplot(imbalanceddata$Duration, geom = "histogram", 
                      main = "Histogram for Breastfeeding Duration", xlab = "Breastfeeding Duration",
                      fill = 'red') + theme(legend.position = "none") + My_Theme
income_histogram <- qplot(imbalanceddata$Income_Group, geom = "histogram", 
                          main = "Histogram for Income", xlab = "Income Group",
                          fill = 'red') + theme(legend.position = "none") + My_Theme
boxplot_bf <- ggplot(imbalanceddata, aes(x = Duration)) + geom_boxplot(fill = 'red') + 
  coord_flip() + ggtitle("Boxplot of Breastfeeding Duration") + My_Theme
boxplot_income <- ggplot(imbalanceddata, aes(x = Income_Group)) + geom_boxplot(fill = 'red') + 
  coord_flip() + ggtitle("Boxplot of Income Group") + My_Theme
plot_grid(boxplot_bf, boxplot_income, bf_histogram, income_histogram, nrow = 2, 
          ncol= 2, labels = "AUTO", label_size= 20, align = "v")

# Statistical Analysis
# code based on https://www.datacamp.com/community/tutorials/feature-selection-R-boruta

# https://www.analyticsvidhya.com/blog/2015/10/inferential-descriptive-statistics-beginners-r/
# https://towardsdatascience.com/data-analysis-and-visualisations-using-r-955a7e90f7dd 

# Test whether or not 
# The p-values are both below 0.05, which suggests that there is a statistically significant difference
# between vaccination rates between those with different incomes and those that were breastfed for 
# different durations 
t.test(imbalanceddata$Duration ~ imbalanceddata$Vaccination_Status,var.equal=TRUE)
t.test(imbalanceddata$Income_Group ~ imbalanceddata$Vaccination_Status,var.equal=TRUE)

# Chi-sqaured testing
chisq.test(imbalanceddata$Vaccination_Status, imbalanceddata$Household_Size)
chisq.test(imbalanceddata$Vaccination_Status, imbalanceddata$Was_Child_Breastfed)
chisq.test(imbalanceddata$Vaccination_Status, imbalanceddata$Child_Number)
chisq.test(imbalanceddata$Vaccination_Status, imbalanceddata$WIC)
chisq.test(imbalanceddata$Vaccination_Status, imbalanceddata$Education_Status)
chisq.test(imbalanceddata$Vaccination_Status, imbalanceddata$Firstborn)
chisq.test(imbalanceddata$Vaccination_Status, imbalanceddata$Mother_Age_Group)
chisq.test(imbalanceddata$Vaccination_Status, imbalanceddata$Marital_Status)
chisq.test(imbalanceddata$Vaccination_Status, imbalanceddata$Race)
chisq.test(imbalanceddata$Vaccination_Status, imbalanceddata$House_Ownership_Status)
chisq.test(imbalanceddata$Vaccination_Status, imbalanceddata$Provider_Facility)
chisq.test(imbalanceddata$Vaccination_Status, imbalanceddata$Insurance_Type)
chisq.test(imbalanceddata$Vaccination_Status, imbalanceddata$Number_Providers)
chisq.test(imbalanceddata$Vaccination_Status, imbalanceddata$Region)

# ANOVA Testing
grouped_education <- group_by(imbalanceddata, Education_Status)
summarise(grouped_education, group_mean = mean(Income_Group, na.rm = TRUE))
education_ANOVA <- lm(Income_Group ~ Education_Status, data=imbalanceddata)
anova(education_ANOVA)

grouped_household <- group_by(imbalanceddata, Household_Size)

summarise(grouped_household, group_mean = mean(Income_Group, na.rm = TRUE))
household_ANOVA <- lm(Income_Group ~ Household_Size, data=imbalanceddata)
anova(household_ANOVA)

grouped_race <- group_by(imbalanceddata, Race)
summarise(grouped_race, group_mean = mean(Income_Group, na.rm = TRUE))
race_ANOVA <- lm(Income_Group ~ Race, data=imbalanceddata)
anova(race_ANOVA)

grouped_insurance <- group_by(imbalanceddata, Insurance_Type)
summarise(grouped_insurance, group_mean = mean(Income_Group, na.rm = TRUE))
insurance_ANOVA <- lm(Income_Group ~ Race, data=imbalanceddata)
anova(insurance_ANOVA)
table(imbalanceddata$Education_Status)
