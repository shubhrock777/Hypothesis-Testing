# Q1   
                         # hypothesis testing for continuous variable 
                                                         #Y = continuous 
                                                         # X = factor discrete                       
library(readr)   #invoke library


######## 2 populations compare (2 sample  t test for two population)

cutlets_ <- read_csv(file.choose()) # load File
# here X = "unit a","unit b" 
#  Y = diameter mean

#  removing nall and nan value form data
cutlets <- cutlets_[complete.cases(cutlets_),]


# change columns name

colnames(cutlets) <- c("unit_a","unit_b")

attach(cutlets)



##1st step normality(normally distribution) test with shapiro.test
# Ho : both follow normal distrbution
# Ha : anyone or both not follow normal distribution


#Normality test- We will see if data is normally distributed or not
#Create hypothesis for Unit A
                                       #Ho= Data is Normally distributed
                                      #Ha=Data is not Normally distributed
shapiro.test(unit_a)
# p_value = 0.32 (p high null fly mean fail to reject Ho: follow normal distribution)

#Normality test- We will see if data is normally distributed or not
#Create hypothesis for Unit A
                                    #Ho= Data is Normally distributed
                                      #Ha=Data is not Normally distributed
shapiro.test(unit_b)
# p_value = 0.522 (p high null fly mean fail to reject Ho: follow normal distribution)

# we fail to reject null hypothesis mean both are follow normal distribution

               

                   ##2nd test for equal variance with different external conditions 

# Ho : Variance of diameters of Unit A is equal to the variance of diameters of Unit B
# Ha : Variance of diameters of Unit A is not equal to the variance of diameters of Unit B

var.test(unit_a,unit_b)
# p_value = 0.313 (p high null fly ,mean fail to reject Ho:)

#P-value>0.05.
#P High Ho fly. We fail to reject Null hypothesis. So we will accept it and hence Variances of A is equal to Variances of B.
                

                           ###final two sample T test for equal varianve test (mean test )


#  two sided test( equal or unequal)
# Ho : Mean(a) = mean(b)
# Ha : Mean(a) != mean (b)

t.test(unit_a,unit_b,alternative = "two.sided", conf.level = 0.95)
#alternative = "two.sided " mean we are checking for equal and unequal
#means
#p=0.4723 (p high null fly ,mean fail to reject Ho:) then go with Ho : equal mean
# its mean ,average of unit_a  i equal to average unit_b



#Q2
######## more than 2 population compare  

                                 # hypothesis testing for continuous variable 
                                                         #Y = continuous 
                                                          # X = factor discrete


labtat_ <- read_csv(file.choose())


                              # in df we have na value, so removing nall and nan value form data
labtat <- labtat_[complete.cases(labtat_),]

#changing columns name
colnames(labtat)<- c("laboratory_1","laboratory_2","laboratory_3","laboratory_4")
attach(labtat)

# 1st step normality test  
#Ho: all data are normally distributed
#Ha : min one of the data is not normally distributed

shapiro.test(laboratory_1)
shapiro.test(laboratory_2)
shapiro.test(laboratory_3)
shapiro.test(laboratory_4)
boxplot(laboratory_1,laboratory_2,laboratory_3,laboratory_4)

#P-value is >0.05. P High Ho Fly.So all data are normally distributed
# Ho: take no action
# we fail to reject null hypothesis mean all are follow normal distribution



### 2nd step variance test 
                           # Ho: all variance are equal
                            # Ha : atlest one variance is different


#Create Hypothesis for variances of Lab 1 and Lab 2
                          #Ho= Variance of TAT of Lab 1 is equal to variance of TAT of Lab 2
                          #Ha= Variance of TAT of Lab 1 is not equal to variance of TAT of Lab 2
var.test(laboratory_1,laboratory_2)
#P-value>0.05. P High Ho fly. We fail to reject Null hypothesis.
#So we will accept it and hence Variances of 1 is equal to variances of 2


#Create Hypothesis for variances of Lab 2 and Lab 3
#Ho= Variance of TAT of Lab 2 is equal to variance of TAT of Lab 3
#Ha= Variance of TAT of Lab 2 is not equal to variance of TAT of Lab 3
var.test(laboratory_2,laboratory_3)
#P-value>0.05. P High Ho fly. We fail to reject Null hypothesis.
#So we will accept it and hence Variances of 2 is equal to variances of 3

#Create Hypothesis for variances of Lab 4 and Lab 3
#Ho= Variance of TAT of Lab 4 is equal to variance of TAT of Lab 3
#Ha= Variance of TAT of Lab 4 is not equal to variance of TAT of Lab 3
var.test(laboratory_3,laboratory_4)
#P-value>0.05. P High Ho fly. We fail to reject Null hypothesis.
#So we will accept it and hence Variances of 4 is equal to variances of 3

#Create Hypothesis for variances of Lab 1 and Lab 4
#Ho= Variance of TAT of Lab 1 is equal to variance of TAT of Lab 4
#Ha= Variance of TAT of Lab 1 is not equal to variance of TAT of Lab 4
var.test(laboratory_4,laboratory_1)
#P-value>0.05. P High Ho fly. We fail to reject Null hypothesis.
#So we will accept it and hence Variances of 1 is equal to variances of 4




# we fail to reject the Ho: So we will go with Ho :

# 3rd # anova test with equal variance 

                                              #Ho= Average TAT for all the samples is same
                                              #Ha= Averages TAT for all the samples is not same

#As there are more than 2 discrete variables and output variable TAT is a continuous variable. Hence we will go with Anova one way test.
# stacking the data first for anova test #one way anova

stacked_labtat <- stack(labtat)
attach(stacked_labtat)

anova_results <- aov(values~ind ,data = stacked_labtat)
summary(anova_results)
 
#you can see that P -value is < 0.05. P Low so Ho go.
#Hence there is  difference in the average TAT for all the labs.
 



##Q3

#########(BuyerRatio.csv)##########

buyer_ratio <- read_csv(file.choose())

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

cus_ord_form  <- read_csv(file.choose())
###### chi squared  proportions  test for more than 2 popultions
                                              # Ho : all proportions are equal
                                              # Ha: not all proportions are equal 

stacked_form <-stack(cus_ord_form)    # stacking the data 
attach(stacked_form)
View(stacked_form)

table(stacked_form$ind,stacked_form$values)   # contigency table

# chi squred test 
chisq.test(table(stacked_form$ind,stacked_form$values))

#you can see that P -value is > 0.05. P high so Ho fly.
#Hence there is no  difference in the average defective % varies  by center.



#####Q5

fantaloons_ <- read.csv(file.choose())

#  removing nall and nan value form data
fantaloons <- fantaloons_[complete.cases(fantaloons_),]

           # 2 proportion t test
                            # Create hypothesis
                            #Ho= Proportions of Male and Female are same
                            #Ha= Proportions of Male and Female are not same



attach(fantaloons)

# contigency table
table_w_d <- table(Weekdays)
table_w_d

table_w_e <- table(Weekend)
table_w_e

total_female=287+233
total_female
total_male=113+167
total_male
table_t <- table(Weekdays,Weekend)
table_t

######  proportions  test
                              
# two sided
prop.test(x=c(120,47),n=c(520,280),conf.level = 0.95,alternative = "two.sided")
#p low null go 
# Unequal proportions 

#one sided
prop.test(x=c(120,47),n=c(520,280),conf.level = 0.95,alternative = "greater")
# Ha -> Proportions of Female > Proportions of Male    
# Ho -> Proportions of Male > Proportions of Female
# p-value = 0.022 < 0.05 Fail to accept null hypothesis 
# so proportion of Male > proportion of Female


