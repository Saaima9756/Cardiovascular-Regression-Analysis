#Cardiovascular Disease Analytics
#Pures college

#upload the Cardiovascular disease data set

cardiac_data = read.csv("cardiovascular/Heart Failure Clinical Records.csv")
View(cardiac_data)

#Scale numeric variables
cardiac_data[,c("age",
                "high_blood_pressure",
                "serum_creatinine")] = scale(cardiac_data[,
                                                          c("age", "high_blood_pressure",
                                                            "serum_creatinine")])
View(cardiac_data)
#A linear regression model
model_A = lm (platelets ~ age,
              data = cardiac_data)

model_B = lm(platelets ~ diabetes + serum_creatinine,
             data = cardiac_data)
#Display summary statistics
summary(model_A)
summary(model_B)

#Diagnostic plots
par(mfrow = c(2,2))
plot(model_A)

#Display coefficients 
coefficients(model_A)

#Making predictions
new_data <- data.frame(age = c(60, 55),
                       high_blood_pressure = c(130, 140),
                       platelets = c(65000, 150200))
prediction <- predict(model_A, newdata = new_data)
summary(new_data)
View(new_data)
model_c = lm(platelets ~ age,
             data = new_data)
summary(model_c)

#Fitted model
View(model_A)
fit = fitted(model_A)
head(fit)

#Use of anova()
result_anova = anova(model_A, model_B)
summary(result_anova)
print(result_anova)
