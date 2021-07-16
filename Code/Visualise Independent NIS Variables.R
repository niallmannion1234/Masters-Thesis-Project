# First set of plots
LT = dim(imbalanceddata)[1]
imbalanceddata$Race <- revalue(imbalanceddata$Race, 
                               c("HISPANIC"="HISPANIC", "NON-HISPANIC BLACK ONLY"="BLACK", 
                                 "NON-HISPANIC OTHER + MULTIPLE RACE" = "MULTIPLE RACE", 
                                 "NON-HISPANIC WHITE ONLY"="WHITE"))
imbalanceddata$Education_Status <- revalue(imbalanceddata$Education_Status, 
                                           c("< 12 YEARS"="<12y", "> 12 YEARS, NON-COLLEGE GRAD"=">12y Non-Grad",
                                             "12 YEARS" = "12y", "COLLEGE GRAD"="Graduate"))
imbalanceddata$Education_Status <- factor(imbalanceddata$Education_Status, 
                                          levels=c("Graduate", ">12y Non-Grad", "12y", "<12y"))
imbalanceddata$Insurance_Type <- revalue(imbalanceddata$Insurance_Type, 
                                         c("ANY MEDICAID" = "Medicaid", "OTHER INSURANCE"="Other", 
                                           "PRIVATE INSURANCE ONLY" = "Private", "UNINSURED" = "Uninsured", 
                                           "OTHER INSURANCE (CHIP, IHS, MILITARY, OR OTHER, ALONE OR IN COMB. WITH PRIVATE INSURANCE)"="Public"))
imbalanceddata$Insurance_Type <- factor(imbalanceddata$Insurance_Type, 
                                        levels=c("Private", "Public", "Medicaid", "Other", "Uninsured"))
imbalanceddata$Child_Number <- factor(imbalanceddata$Child_Number, 
                                      levels=c("ONE", "TWO OR THREE", "FOUR OR MORE"))
imbalanceddata$Income_Level <- cut(imbalanceddata$Income_Group, 3,  
                                   include.lowest=TRUE, labels=c("Low", "Medium", "High"))

plot1 <-ggplot(data = imbalanceddata[1:LT,], aes(x = Household_Size,fill =
                                                   Vaccination_Status)) + geom_bar(position="fill") + theme(legend.position = "none")
plot3 <-ggplot(data = imbalanceddata[1:LT,], aes(x = Child_Number,fill =
                                                   Vaccination_Status)) + geom_bar(position="fill") + theme(legend.position = "none")
plot5 <-ggplot(data = imbalanceddata[1:LT,], aes(x = Education_Status,fill =
                                                   Vaccination_Status)) + geom_bar(position="fill") + theme(legend.position = "none")
plot9 <-ggplot(data = imbalanceddata[1:LT,], aes(x = Race,fill =
                                                   Vaccination_Status)) + geom_bar(position="fill") + theme(legend.position = "none")
plot10 <-ggplot(data = imbalanceddata[1:LT,], aes(x = Insurance_Type,fill = 
                                                    Vaccination_Status)) + geom_bar(position="fill") + theme(legend.position = "none")
plot16 <-ggplot(data = imbalanceddata, aes(x = Income_Level,fill =
                                             Vaccination_Status)) + geom_bar(position="fill") + theme(legend.position = "none")
plot_grid(plot1, plot3, plot5, plot16, plot10, plot9, nrow = 3, ncol = 2,
          labels = "AUTO", label_size = 26, align = "v")

# Second set of plots
imbalanceddata$Marital_Status <- revalue(imbalanceddata$Marital_Status, 
                                         c("MARRIED"="Married", 
                                           "NEVER MARRIED/WIDOWED/DIVORCED/SEPARATED/DECEASED/LIVING WITH PARTNER" = "Not Currently Married"))
plot2 <- ggplot(data = imbalanceddata[1:LT,], aes(x = Was_Child_Breastfed,fill = Vaccination_Status)) +
  geom_bar(position="fill") + theme(legend.position = "none")
plot4 <- ggplot(data = imbalanceddata[1:LT,], aes(x = WIC,fill = Vaccination_Status)) +
  geom_bar(position="fill") + theme(legend.position = "none")
plot6 <- ggplot(data = imbalanceddata[1:LT,], aes(x = Firstborn,fill = Vaccination_Status)) +
  geom_bar(position="fill") + theme(legend.position = "none")
plot8 <- ggplot(data = imbalanceddata[1:LT,], aes(x = Marital_Status,fill = Vaccination_Status)) +
  geom_bar(position="fill") + theme(legend.position = "none")
plot_grid(plot2, plot4, plot6, plot8, nrow = 2, ncol = 2, labels = "AUTO",
          label_size = 26, align = "v")

# Third set of plots
imbalanceddata$Provider_Facility <- revalue(imbalanceddata$Provider_Facility, 
                                            c("ALL HOSPITAL FACILITIES" = "All", "ALL MILITARY/OTHER FACILITIES" =
                                                "Military", "ALL PRIVATE FACILITIES" = "Private", 
                                              "ALL PUBLIC FACILITIES" = "Public", "MIXED"="Mixed"))
imbalanceddata$Provider_Facility <- factor(imbalanceddata$Provider_Facility, 
                                           levels=c("Private", "Public", "Medicaid", "Other", "Uninsured"))
plot11 <-ggplot(data = imbalanceddata[1:LT,], aes(x = Number_Providers,
                                                  fill = Vaccination_Status)) + geom_bar(position="fill") + ylab("Frequency")
plot12 <-ggplot(data = imbalanceddata[1:LT,], aes(x = House_Ownership_Status,
                                                  fill = Vaccination_Status)) + geom_bar(position="fill") + ylab("Frequency") +
  theme(legend.position = "none")
plot13 <-ggplot(data = imbalanceddata[1:LT,], aes(x = Provider_Facility,
                                                  fill = Vaccination_Status)) + geom_bar(position="fill") + ylab("Frequency") +
  theme(legend.position = "none")
plot14 <-ggplot(data = imbalanceddata[1:LT,], aes(x = Mother_Age_Group,
                                                  fill = Vaccination_Status)) + geom_bar(position="fill") + ylab("") 
plot_grid(plot11, plot12, plot13, plot14, nrow = 2, ncol = 2, labels = "AUTO",
          label_size = 26, align = "v")

#Remove income level factor (already have income group)
imbalanceddata <- imbalanceddata[ ,c(1:17)]

# Normalise numerical columns
imbalanceddata$Duration <- normalize(imbalanceddata$Duration, 
                                     method = "standardize", range = c(0, 1), 
                                     margin = 1L, on.constant = "quiet")
imbalanceddata$Income_Group <- normalize(imbalanceddata$Income_Group, 
                                         method = "standardize", range = c(0, 1),
                                         margin = 1L, on.constant = "quiet")
