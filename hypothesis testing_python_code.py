import pandas as pd   #invoke library
import scipy 
from scipy import stats
import statsmodels.api as sm




# Q1   
from statsmodels.formula.api import ols
                         # hypothesis testing for continuous variable 
                                                         #Y = continuous 
                                                         # X = factor discrete                       



######## 2 populations compare (2 sample  t test for two population)

cutlets_ = pd.read_csv("D:\\360digitM\\assi\\Module_5\\Datasets\\Cutlets.csv") # load File
# here X = "unit a","unit b" 
#  Y = diameter mean

#  removing nall and nan value form data
cutlets = cutlets_.dropna(how='any',axis =0)


# change columns name

cutlets.columns = "unit_a","unit_b"


##1st step normality(normally distribution) test with shapiro.test
# Ho : both follow normal distrbution
# Ha : anyone or both not follow normal distribution


#Normality test- We will see if data is normally distributed or not
#Create hypothesis for Unit A
                                       #Ho= Data is Normally distributed
                                      #Ha=Data is not Normally distributed
print(stats.shapiro(cutlets.unit_a))  
# p_value = 0.32 (p high null fly mean fail to reject Ho: follow normal distribution)

#Normality test- We will see if data is normally distributed or not
#Create hypothesis for Unit A
                                    #Ho= Data is Normally distributed
                                      #Ha=Data is not Normally distributed
print(stats.shapiro(cutlets.unit_b)) 
# p_value = 0.522 (p high null fly mean fail to reject Ho: follow normal distribution)

# we fail to reject null hypothesis mean both are follow normal distribution

               

                   ##2nd test for equal variance with different external conditions 

# Ho : Variance of diameters of Unit A is equal to the variance of diameters of Unit B
# Ha : Variance of diameters of Unit A is not equal to the variance of diameters of Unit B

scipy.stats.levene(cutlets.unit_a,cutlets.unit_b)
# p_value = 0.417 (p high null fly ,mean fail to reject Ho:)

#P-value>0.05.
#P High Ho fly. We fail to reject Null hypothesis. So we will accept it and hence Variances of A is equal to Variances of B.
                

                           ###final two sample T test for equal varianve test (mean test )


#  two sided test( equal or unequal)
# Ho : Mean(a) = mean(b)
# Ha : Mean(a) != mean (b)

scipy.stats.ttest_ind(cutlets.unit_a,cutlets.unit_b)
#alternative = "two.sided " mean we are checking for equal and unequal
#means
#p=0.4723 (p high null fly ,mean fail to reject Ho:) then go with Ho : equal mean
# its mean ,average of unit_a  i equal to average unit_b



#Q2
######## more than 2 population compare  

                                 # hypothesis testing for continuous variable 
                                                         #Y = continuous 
                                                          # X = factor discrete


labtat_ = pd.read_csv("D:\\360digitM\\assi\\Module_5\\Datasets\\LabTaT.csv")


                              # in df we have na value, so removing nall and nan value form data
labtat = labtat_.dropna(how='any',axis =0)

#changing columns name
labtat.columns = "laboratory_1","laboratory_2","laboratory_3","laboratory_4"


# 1st step normality test  
#Ho: all data are normally distributed
#Ha : min one of the data is not normally distributed

print(stats.shapiro(labtat.laboratory_1)) 
print(stats.shapiro(labtat.laboratory_2)) 
print(stats.shapiro(labtat.laboratory_3)) 
print(stats.shapiro(labtat.laboratory_4)) 


#P-value is >0.05. P High Ho Fly.So all data are normally distributed
# Ho: take no action
# we fail to reject null hypothesis mean all are follow normal distribution



### 2nd step variance test 
                           # Ho: all variance are equal
                            # Ha : atlest one variance is different


#Create Hypothesis for variances of Lab 1 and Lab 2
                          #Ho= Variance of TAT of Lab 1 is equal to variance of TAT of Lab 2
                          #Ha= Variance of TAT of Lab 1 is not equal to variance of TAT of Lab 2
scipy.stats.levene(labtat.laboratory_1,labtat.laboratory_2)
#P-value>0.05. P High Ho fly. We fail to reject Null hypothesis.
#So we will accept it and hence Variances of 1 is equal to variances of 2


#Create Hypothesis for variances of Lab 2 and Lab 3
#Ho= Variance of TAT of Lab 2 is equal to variance of TAT of Lab 3
#Ha= Variance of TAT of Lab 2 is not equal to variance of TAT of Lab 3
scipy.stats.levene(labtat.laboratory_2,labtat.laboratory_3)
#P-value>0.05. P High Ho fly. We fail to reject Null hypothesis.
#So we will accept it and hence Variances of 2 is equal to variances of 3

#Create Hypothesis for variances of Lab 4 and Lab 3
#Ho= Variance of TAT of Lab 4 is equal to variance of TAT of Lab 3
#Ha= Variance of TAT of Lab 4 is not equal to variance of TAT of Lab 3
scipy.stats.levene(labtat.laboratory_3,labtat.laboratory_4)
#P-value>0.05. P High Ho fly. We fail to reject Null hypothesis.
#So we will accept it and hence Variances of 4 is equal to variances of 3

#Create Hypothesis for variances of Lab 1 and Lab 4
#Ho= Variance of TAT of Lab 1 is equal to variance of TAT of Lab 4
#Ha= Variance of TAT of Lab 1 is not equal to variance of TAT of Lab 4
scipy.stats.levene(labtat.laboratory_4,labtat.laboratory_1)
#P-value>0.05. P High Ho fly. We fail to reject Null hypothesis.
#So we will accept it and hence Variances of 1 is equal to variances of 4


# we fail to reject the Ho: So we will go with Ho :

# 3rd # anova test with equal variance 

                                              #Ho= Average TAT for all the samples is same
                                              #Ha= Averages TAT for all the samples is not same

#As there are more than 2 discrete variables and output variable TAT is a continuous variable. Hence we will go with Anova one way test.
# stacking the data first for anova test #one way anova




fvalue, pvalue = stats.f_oneway(labtat.laboratory_1,labtat.laboratory_2,labtat.laboratory_3,labtat.laboratory_4)
pvalue
#you can see that P -value is < 0.05. P Low so Ho go.
#Hence there is  difference in the average TAT for all the labs.
 



##Q3

#########(BuyerRatio.csv)##########

buyer_ratio = pd.read_csv("D:\\360digitM\\assi\\Module_5\\Datasets\\BuyerRatio.csv")

attach(buyer_ratio)

 

buy_rat_male <- data.frame(region= rep(c("east",'west',"north","south"), times =c (50,142,131,70)),
                      gender= rep(c(1),times = c(393)))

buy_rat_female <- data.frame(region= rep(c("east",'west',"north","south"), times =c (435,1535,1356,750)),
                      gender= rep(c(0),times = c(4076)))

buyer_ratio_ <- rbind(buy_rat_male, buy_rat_female)





###### chi squared  proportions  test for more than 2 popultions
                                    # Ho : all proportions are equal
                                    # Ha: not all proportions are equal 


table(buyer_ratio_$region,buyer_ratio_$gender)  #contigency table

chisq.test(table(buyer_ratio_$region,buyer_ratio_$gender))

#you can see that P -value is > 0.05. P high so Ho fly.
#Hence there is no  difference in the average 



###Q4

cus_ord_form_ = pd.read_csv("D:\\360digitM\\assi\\Module_5\\Datasets\\CustomerOrderform.csv")
###### chi squared  proportions  test for more than 2 popultions
                                              # Ho : all proportions are equal
                                              # Ha: not all proportions are equal 
#  removing nall and nan value form data
cus_ord_form = cus_ord_form_.dropna(how='any',axis =0)

stacked_form <-stack(cus_ord_form)    # stacking the data 
attach(stacked_form)
View(stacked_form)

table(stacked_form$ind,stacked_form$values)   # contigency table

# chi squred test 
chisq.test(table(stacked_form$ind,stacked_form$values))

#you can see that P -value is > 0.05. P high so Ho fly.
#Hence there is no  difference in the average defective % varies  by center.



#####Q5

fantaloons_ = pd.read_csv("D:\\360digitM\\assi\\Module_5\\Datasets\\Fantaloons.csv")

#  removing nall and nan value form data
fantaloons = fantaloons_.dropna(how='any',axis =0)
           # 2 proportion t test
                            # Create hypothesis
                            #Ho= Proportions of Male and Female are same
                            #Ha= Proportions of Male and Female are not same
from statsmodels.stats.proportion import proportions_ztest

# contigency table
table_w_d =fantaloons.Weekdays.value_counts()
table_w_d

table_w_e =fantaloons.Weekend.value_counts()
table_w_e


total_female=287+233
total_female
total_male=113+167
total_male

import numpy as np
count = np.array([120,47])
nobs = np.array([520,280])


######  proportions  test
                              
# two sided
stats,pval = proportions_ztest(count, nobs,alternative='two-sided') 
print(pval) 
#p low null go 
# Unequal proportions 

#one sided
stats,pval = proportions_ztest(count, nobs,alternative='larger')
print(pval)   
# Ha -> Proportions of Female > Proportions of Male    
# Ho -> Proportions of Male > Proportions of Female
# p-value = 0.018 < 0.05 Fail to accept null hypothesis 

# so proportion of Male > proportion of Female
