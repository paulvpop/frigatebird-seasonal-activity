##Chi-squared goodness-of-fit test for Christmas Island Frigatebird (CIFR)####
# Step 1: Create a matrix from the data
cifr <- matrix(c(4, 15, 3), nrow = 1)
#where 4, 15, and 3 corresponds to pre-monsoon, monsoon, and post-monsoon
# Step 2: Apply the chi-square test function
result <- chisq.test(cifr)
# View the result
print(result)
#
# Chi-squared test for given probabilities
# 
# data:  cifr
# X-squared = 12.091, df = 2, p-value = 0.002369
#p-value is less than a threshold of 0.005. So, statistically suggestive.
#That means that there is no equal distribution of occurrences across seasons.
#View the expected value:
result[["expected"]]
# [1] 7.333333 7.333333 7.333333
#This satisfy the assumption of expected value being greater than 5

#An alternate way to do the chi-squared test:
cifr <- c(4, 15, 3)
res <- chisq.test(cifr, p = c(1/3, 1/3, 1/3))
res

#Check the residuals to know which season deviates from the expected distribution
result$residuals
# [1] -1.230915  2.831104 -1.600189
#Residuals >[2] indicate suggestive deviation from expected values.
#So, monsoon has suggestively higher than expected occurrences of CIFR

##Chi-squared goodness-of-fit test for Lesser Frigatebird####
#create a matrix
lefr <- matrix(c(5, 22, 7), nrow = 1)
# Apply the chi-square test function
result <- chisq.test(lefr)
# View the result
print(result)
# 
# Chi-squared test for given probabilities
# 
# data:  lefr
# X-squared = 15.235, df = 2, p-value = 0.0004917
#View the expected value:
result[["expected"]]
# [1] 11.33333 11.33333 11.33333
#Assumption of expected value being >5 satisfied
#Check the residuals to know which season deviates from the expected distribution
result$residuals
# [1] -1.881280  3.168472 -1.287192
#Residuals >|2| indicate suggestive deviation from expected values, and > |3| indicate
#even stronger deviation (as is the case here).
#So, monsoon is has suggestively high than expected occurrences of LEFR

##Chi-squared goodness-of-fit test for Great Frigatebird####
#create a matrix
grfr <- matrix(c(1, 11, 2), nrow = 1)
# Apply the chi-square test function
result <- chisq.test(grfr)
# Warning message:
#   In chisq.test(grfr) : Chi-squared approximation may be incorrect
#The above warning message because of lower than threshold expected value.
# View the result
print(result)
# 
# Chi-squared test for given probabilities
# 
data:  grfr
# X-squared = 13, df = 2, p-value = 0.001503
#View the expected value:
result[["expected"]]
#Assumption of expected value being >5 is not satisfied

#So, an exact (binomial) test can be carried out which doesn't have the assumption
#of >5 expected value

##Binomial test for Great Frigatebird####
p1 <- binom.test(1, 14, p = 1/3)$p.value  # Pre-monsoon vs. others
p2 <- binom.test(11, 14, p = 1/3)$p.value # Monsoon vs. others
p3 <- binom.test(2, 14, p = 1/3)$p.value  # Post-monsoon vs. others
p.adjust(c(p1, p2, p3), method = "bonferroni")  # Adjusts 3 p-values
# [1] 0.13451289 0.00207298 0.48885013
#Instead of using Bonferroni corrections, the threshold can be considered as 0.005
#itself (as per the recommendation in the paper "Three Recommendations for Improving
#the Use of p-Values https://www.tandfonline.com/doi/pdf/10.1080/00031305.2018.1543135)
p1
# [1] 0.04483763
p2
# [1] 0.0006909934
p3
# [1] 0.16295
#It's clear that only monsoon as suggestive values of deviation from the expected.

##Chi-squared goodness-of-fit test for all frigatebird species (including Frigata sp.) combined####
#create a matrix
all <- matrix(c(14, 57, 14), nrow = 1)
# Apply the chi-square test function
result <- chisq.test(all)
# View the result
print(result)
# 
# Chi-squared test for given probabilities
# 
# data:  all
# X-squared = 43.506, df = 2, p-value = 3.571e-10
#View the expected value:
result[["expected"]]
# [1] 28.33333 28.33333 28.33333
#Assumption of expected value being >5 satisfied
#Check the residuals to know which season deviates from the expected distribution
result$residuals
# [1] -2.692764  5.385529 -2.692764
#Residuals >|2| indicate suggestive deviation from expected values, and > |3| indicate
#even stronger deviation (as is the case here).
#So, monsoon is has suggestively high than expected occurrences of Frigatebirds.
#But the other two seasons also show >|2|. But comparatively, the deviation is 
#nearly twice as more for monsoon.









