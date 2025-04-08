library(car)

setwd("~/BRCA Msc. Thesis")

brca = read.csv('Winsorized Sqrt Wisconsin.csv')
attach(brca)


# Conduct Levene’s Test for Equality of Variances for Each Feature in Regards to Diagnosis
# Using α = 0.05
# H0: The variance among the groups is equal
# H1: The variance among the groups is not equal

# radius_mean
leveneTest(radius_mean ~ diagnosis, data = brca)
# p-value = 1.597e-11 < 0.05, we reject H0. Thus, the variance among the groups is not equal 

# texture_mean
leveneTest(texture_mean ~ diagnosis, data = brca)
# p-value = 0.01183 < 0.05, we reject H0. Thus, the variance among the groups is not equal 

# perimeter_mean 
leveneTest(perimeter_mean ~ diagnosis, data = brca)
# p-value = 9.489e-12 < 0.05, we reject H0. Thus, the variance among the groups is not equal 

# area_mean
leveneTest(area_mean ~ diagnosis, data = brca)
# p-value < 2.2e-16 < 0.05, we reject H0. Thus, the variance among the groups is not equal 

# smoothness_mean
leveneTest(smoothness_mean ~ diagnosis, data = brca)
# p-value = 0.04407 < 0.05, we reject H0. Thus, the variance among the groups is not equal  

# compactness_mean 
leveneTest(compactness_mean ~ diagnosis, data = brca)
# p-value = 0.0007146 < 0.05, we reject H0. Thus, the variance among the groups is not equal 

# concavity_mean*
leveneTest(concavity_mean ~ diagnosis, data = brca)
# p-value = 0.1048 > 0.05, we fail to reject H0. 
# Thus, there is insufficient evidence to conclude 
# that the variance among the groups is not equal 

# concave.points_mean*
leveneTest(concave.points_mean ~ diagnosis, data = brca)
# p-value = 0.2986 > 0.05, we fail to reject H0. 
# Thus, there is insufficient evidence to conclude 
# that the variance among the groups is not equal  

# symmetry_mean*
leveneTest(symmetry_mean ~ diagnosis, data = brca)
# p-value = 0.3032 > 0.05, we fail to reject H0. 
# Thus, there is insufficient evidence to conclude 
# that the variance among the groups is not equal 

# fractal_dimension_mean
leveneTest(fractal_dimension_mean ~ diagnosis, data = brca)
# p-value = 2.515e-05 < 0.05, we reject H0. Thus, the variance among the groups is not equal 

# Next, perform 2 independent sample t-test on each feature to find the features
# that are significantly different between Malignant cases and Benign cases

# First, separate into Malignant and Benign
df_m = brca[diagnosis == "M", -c(1)]
df_b = brca[diagnosis == "B", -c(1)]

# Using α = 0.05

# H0: μ_m = μ_b
# H1: μ_m ≠ μ_b


# radius_mean
t.test(df_m[,1], df_b[,1], alternative = 'two.sided', CL = 0.95, , var.equal= FALSE)
# p-value < 2.2e-16 < 0.05, we reject H0. 
# Thus, there is a statistically significant difference between the means of the 2 groups. 

# texture_mean
t.test(df_m[,2], df_b[,2], alternative = 'two.sided', CL = 0.95, , var.equal= FALSE)
# p-value < 2.2e-16 < 0.05, we reject H0. 
# Thus, there is a statistically significant difference between the means of the 2 groups.

# perimeter_mean
t.test(df_m[,3], df_b[,3], alternative = 'two.sided', CL = 0.95, , var.equal= FALSE)
# p-value < 2.2e-16 < 0.05, we reject H0. 
# Thus, there is a statistically significant difference between the means of the 2 groups.

# area_mean
t.test(df_m[,4], df_b[,4], alternative = 'two.sided', CL = 0.95, , var.equal= FALSE)
# p-value < 2.2e-16 < 0.05, we reject H0. 
# Thus, there is a statistically significant difference between the means of the 2 groups.

# smoothness_mean
t.test(df_m[,5], df_b[,5], alternative = 'two.sided', CL = 0.95, , var.equal= FALSE)
# p-value < 2.2e-16 < 0.05, we reject H0. 
# Thus, there is a statistically significant difference between the means of the 2 groups.

# compactness_mean 
t.test(df_m[,6], df_b[,6], alternative = 'two.sided', CL = 0.95, , var.equal= FALSE)
# p-value < 2.2e-16 < 0.05, we reject H0. 
# Thus, there is a statistically significant difference between the means of the 2 groups.

# concavity_mean
t.test(df_m[,7], df_b[,7], alternative = 'two.sided', CL = 0.95, , var.equal= TRUE)
# p-value < 2.2e-16 < 0.05, we reject H0.
# Thus, there is a statistically significant difference between the means of the 2 groups.

# concave.points_mean
t.test(df_m[,8], df_b[,8], alternative = 'two.sided', CL = 0.95, , var.equal= TRUE)
# p-value < 2.2e-16 < 0.05, we reject H0.
# Thus, there is a statistically significant difference between the means of the 2 groups.

# symmetry_mean
t.test(df_m[,9], df_b[,9], alternative = 'two.sided', CL = 0.95, , var.equal= TRUE)
# p-value = 5.733e-16 < 0.05, we reject H0.
# Thus, there is a statistically significant difference between the means of the 2 groups.

# fractal_dimension_mean
t.test(df_m[,10], df_b[,10], alternative = 'two.sided', CL = 0.95, , var.equal= FALSE)
# p-value = 0.9678 > 0.05, we fail to reject H0.
# Thus, there is not a statistically significant difference between the means of the 2 groups.

# Since there is no significant difference between the fractal_dimension_mean of Benign and 
# Malignant cases, fractal_dimension_mean will be excluded from the model-making process.
