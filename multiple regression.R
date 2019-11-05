#R - Multiple Regression
#In simple linear relation we have one predictor and one response variable, but in multiple regression 
#we have more than one predictor variable and one response variable.
#lm(y ~ x1+x2+x3...,data)

input <- mtcars[,c("mpg","disp","hp","wt")]
print(head(input))
#Create Relationship Model & get the Coefficients
input <- mtcars[,c("mpg","disp","hp","wt")]

# Create the relationship model.
model <- lm(mpg~disp+hp+wt, data = input)

# Show the model.
print(model)

# Get the Intercept and coefficients as vector elements.
cat("# # # # The Coefficient Values # # # ","\n")

a <- coef(model)[1]
print(a)

Xdisp <- coef(model)[2]
Xhp <- coef(model)[3]
Xwt <- coef(model)[4]

print(Xdisp)
print(Xhp)
print(Xwt)
# Create Equation for Regression Model
#Y = a+Xdisp+Xhp+Xwt
#Y
#Apply Equation for predicting New Values
#For a car with disp = 221, hp = 102 and wt = 2.91 the predicted mileage is
#Y = 37.15+(-0.000937)*221+(-0.0311)*102+(-3.8008)*2.91 
#Y
# Find mpg of the car where disp hp and wt is given
a <- data.frame(disp = 221,hp = 102, wt = 2.91)
result <-  predict(model,a)
print(result)

# manual calculations
y=37.105505+221*( -0.000937)+102*(-0.031157)+2.91*(-3.800891)
