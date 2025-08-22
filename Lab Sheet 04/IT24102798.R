getwd()
setwd("C:\\Users\\IT24102798\\Documents\\IT24102798")
getwd()


# Question 01

branch_data <- read.table("Exercise.txt", header = TRUE, sep = ",")
attach(branch_data)


# Question 02

typeof(branch_data)
typeof(Branch)
typeof(Sales_X1)
typeof(Advertising_X2)
typeof(Years_X3)


# Question 03

boxplot(Sales_X1, main = "Sales Distribution", ylab = "Sales")


# Question 04

summary(Advertising_X2)
IQR(Advertising_X2)


# Question 05

find.outliers = function(numeric_vector) {
  
  q1 <- quantile(numeric_vector)[2]
  q3 <- quantile(numeric_vector)[4]
  iqr <- q3 - q1
  
  ub <- q3 + (1.5 * iqr)
  lb <- q1 + (1.5 * iqr)
  
  print(paste("Upper Bound =", ub))
  print(paste("Lower Bound =", lb))
  print(paste("Outliers:", paste( sort( numeric_vector[ (numeric_vector < lb) | (numeric_vector > ub) ]), collapse = ",")))
  
}

find.outliers(Years_X3)




