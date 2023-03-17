## Read in data
happiness.dat = read.csv('C:/Users/eliza/stat318-git/stat318-project/data_files/Happiness2018.csv', header = TRUE)
gdi.dat = read.csv('C:/Users/eliza/stat318-git/stat318-project/data_files/GDI2019.csv',  header = TRUE)
gdi.dat = gdi.dat[-1,] # Remove extra header row
maternal.dat = read.csv('C:/Users/eliza/stat318-git/stat318-project/data_files/Maternal2017.csv', header = TRUE, skip=1)
lifexF.dat = read.csv('C:/Users/eliza/stat318-git/stat318-project/data_files/LifeExF2018.csv', header = TRUE, skip=1)
lifexM.dat = read.csv('C:/Users/eliza/stat318-git/stat318-project/data_files/LifeExM2018.csv', header = TRUE, skip=1)
politics.dat = read.csv('C:/Users/eliza/stat318-git/stat318-project/data_files/Politics20172019.csv', header = TRUE)
schooling.dat = read.csv('C:/Users/eliza/stat318-git/stat318-project/data_files/Schooling2019.csv', header = TRUE)
employment.dat = read.csv('C:/Users/eliza/stat318-git/stat318-project/data_files/EmploymentDiscrimination2018.csv', header=TRUE)

## Create data frame
happiness_scores = data.frame(happiness.dat$Score, happiness.dat$Country.or.region)
colnames(happiness_scores) = c('Happiness Score','Country')

gdi_scores = data.frame(gdi.dat$GDI_Value, gdi.dat$Country)
colnames(gdi_scores) = c('GDI Value','Country')

lifeexF_scores = matrix(c(lifexF.dat$X2018, lifexF.dat$Country.Name), ncol = 2)
lifeexM_scores = matrix(c(lifexM.dat$X2018, lifexM.dat$Country.Name), ncol = 2)
lifeex_diff = as.numeric(lifeexF_scores[,1]) - as.numeric(lifeexM_scores[,1]) # Female - Male
lifeex_scores = data.frame(lifeex_diff, lifexF.dat$Country.Name)
colnames(lifeex_scores) = c('Life Expectancy Difference (Female - Male)','Country')

politics_scores = data.frame(politics.dat$Value, politics.dat$Country)
colnames(politics_scores) = c('Female Parlementarians (%)','Country')

maternal_scores = data.frame(maternal.dat$X2017, maternal.dat$Country.Name)
colnames(maternal_scores) = c('Maternal Mortality (Per 100,000 Births)','Country')

schooling_scores = data.frame(schooling.dat$Excep_Yrs_Schooling_Diff, schooling.dat$Country)
colnames(schooling_scores) = c('Length of Schooling (Years)(Female-Male)','Country')

employment_scores = data.frame(employment.dat$Value, employment.dat$Country.Name)
colnames(employment_scores) = c('Employment Discrimination Illegal', 'Country')

## Join data sets
library(dplyr)

fullJoinDf1 = full_join(happiness_scores,gdi_scores,by="Country")
fullJoinDf2 = full_join(fullJoinDf1, lifeex_scores, by="Country") 
fullJoinDf3 = full_join(fullJoinDf2, politics_scores, by="Country")
fullJoinDf4 = full_join(fullJoinDf3, maternal_scores, by="Country")
fullJoinDf5 = full_join(fullJoinDf4, schooling_scores, by="Country")
fullJoinDf6 = full.join(fullJoinDf5, employment_scores, by="Country")

## Export
attach(fullJoinDf6)
final.dat = data.frame(Country, `Happiness Score`, `GDI Value`,
                        `Life Expectancy Difference (Female - Male)`, `Female Parlementarians (%)`, 
                        `Maternal Mortality (Per 100,000 Births)`, `Length of Schooling (Years)(Female-Male)`, 
                       `Employment Discrimination Illegal`)
detach(fullJoinDf6)

colnames(final.dat) = c('Country','HappinessScore','GDI','LifeExDiff','FemaleParliament',
                        'MaternalMort','SchoolingDiff', 'EmployDiscrimination')

write.csv(final.dat, 'C:/Users/eliza/stat318-git/stat318-project/FinalData.csv')
