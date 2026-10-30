#Step 0: Remove Everything
rm(list = ls())

#Step 1: Load Data + Packages
library(car) #For VIF
library(ggplot2)
bf <- read.csv("BodyFat.csv", header = TRUE)

#Step 2 Data Cleaning
bf <- subset(bf, bf$BODYFAT > 0 & bf$HEIGHT > 50)

#Step 3 Final Model
model <- lm(BODYFAT ~ ABDOMEN + WEIGHT, data = bf)
summary(model)

#Step 4 Check VIF
vif(model)

#Step 5 Diagnostics + Graphs
par(mfrow = c(2,2))
plot(model)
par(mfrow = c(2,1))

bf$predicted <- predict(model)
#The following graphs will check the model's accuracy and robustness, respectively
ggplot(bf, aes(x=predicted, y=BODYFAT)) +
  geom_point(color="darkgreen", alpha=0.5) +
  geom_abline(slope=1, intercept=0, linetype="dashed") +
  theme_light() +
  labs(title="Model Accuracy Check",
       x="Predicted Body Fat (Using Rule-of-Thumb)",
       y="Actual Body Fat (Underwater Weighing)")
ggplot(model, aes(x=.fitted, y=.resid)) +
  geom_point(alpha=0.5) +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  theme_minimal() +
  labs(title="Residual Plot (Robustness Test)",
       subtitle="Random scatter proves the model is reliable across different body sizes",
       x="Fitted Values", y="Residuals (Model Error)")
