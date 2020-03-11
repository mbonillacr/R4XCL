x1 <- rnorm( 100 )
x2 <- 2 * x1
y <- rnorm( 100 )
vif( lm( y ~ x1 + x2 ) )
alias( lm( y ~ x1 + x2 ) )


VariablesTrabajo=data.frame()
class(VariablesTrabajo)
setwd("C:/Users/mboni/desktop")
getwd()
datos = read.csv("FIFA_Statistics.csv", header = TRUE, dec = ".", sep = ";")
summary(datos)
VariablesTrabajo = data.frame()
VariablesTrabajo=Filter(is.numeric,datos)
VariablesTrabajoCentrado=scale(VariablesTrabajo,center = TRUE, scale = TRUE)
cov(VariablesTrabajoCentrado)
cor(VariablesTrabajoCentrado)
prcomp(na.omit(VariablesTrabajoCentrado))

require(stats)
x <- matrix(1:10, ncol = 2)
(centered.x <- scale(x, scale = FALSE))

x1=scale(x,center = TRUE, scale = TRUE)
cov(x1)


gender_vector <- c("Male", "Female", "Female", "Male", "Male")
class(gender_vector)
# Convert gender_vector to a factor
factor_gender_vector <-factor(gender_vector)
class(factor_gender_vector)


install.packages("dummies")
library(dummies)
gender_vector <- c("M", "F", "T", "M", "M", "T", "F", "M")
dummy(gender_vector)

