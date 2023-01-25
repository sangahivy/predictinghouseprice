library(readxl)
library(ggplot2)
library(dplyr)
options(scipen=99999)

data.BCRE <- read_excel(file.choose(), sheet = 2)
data.BCRE



############Our final model is model 7 which is included at the end of code below.#############

# Descriptive data analysis
str(data.BCRE)
summary(data.BCRE)
qplot(data.BCRE$Price, data.BCRE$Parking  , xlab = "Price", ylab = "Parking")

# Regression model without CityZone dummy variables
model1 <- lm( Price  ~ m2 + Rooms + Elevator + Atico + Terrasse + Parking + Kitchen + Type + Yard, data = data.BCRE)
summary(model1)
plot(data.BCRE$Price, data.BCRE$Rooms)

# Create dummy variables for the CityZone
City1 <- ifelse(data.BCRE$CityZone == 'Sant Andreu', 1,0)
City2 <- ifelse(data.BCRE$CityZone == 'Sarria - Sant Gervasi', 1,0)
City3 <- ifelse(data.BCRE$CityZone == 'Eixample', 1,0)
City4 <- ifelse(data.BCRE$CityZone == 'Horta - Guinardó', 1,0)
City5 <- ifelse(data.BCRE$CityZone == 'Gràcia', 1,0)
City6 <- ifelse(data.BCRE$CityZone == 'Sants - Montjuïc', 1,0)
City7 <- ifelse(data.BCRE$CityZone == 'Nou Barris', 1,0)
City8 <- ifelse(data.BCRE$CityZone == 'Ciutat Vella', 1,0)
City9 <- ifelse(data.BCRE$CityZone == 'Sant Marti', 1,0)
City10 <- ifelse(data.BCRE$CityZone == 'Les Corts', 1,0)

data.BCRE<- data.frame(data.BCRE, 'City1' = City1, 'City2' = City2, 'City3' = City3, 'City4' = City4, 'City5' = City5, 'City6' = City6, 'City7' = City7, 'City8' = City8, 'City9' = City9, 'City10' = City10)

#Regression model2 with CityZone Dummy Variables and m2, rooms
model2 <- lm(Price ~ m2 + Rooms + City1 + City2 + City3 + City4 + City5 + City6 + City7 + City8 + City9 + City10, data= data.BCRE)
summary(model2)

# Check the assumptions (including the auto-correlation of residuals)
hist(residuals(model2), breaks = 500)
plot(fitted.values(model2), residuals(model2))
acf(residuals(model2))

#Regression model3: log price, log m2, log Rooms (log bathroom X )
data.BCRE <- data.frame(data.BCRE, "lnPrice" = log(data.BCRE$Price), "lnm2" = log(data.BCRE$m2), "lnrooms" = log(data.BCRE$Rooms))
model3 <- lm(lnPrice ~ lnm2 + lnrooms + City1 + City2 + City3 + City4 + City5 + City6 + City7 + City8 + City9 + City10, data= data.BCRE)
summary(model3)

#Regression model4: log price, log m2, log Rooms, other varibales 
model4 <- lm(lnPrice ~ lnm2 + lnrooms + Bathrooms + Elevator + Atico + Terrasse + Parking + Kitchen + Type + Yard + City1 + City2 + City3 + City4 + City5 + City6 + City7 + City8 + City9 + City10, data= data.BCRE)
summary(model4)

#Regression model5: log price, log m2, log Rooms, other varibales  except CIty 10('Les Corts') , Type X
model5 <- lm(lnPrice ~ lnm2 + lnrooms + Bathrooms + Elevator + Atico + Terrasse + Parking + Kitchen + Yard + City1 + City2 + City3 + City4 + City5 + City6 + City7 + City8 + City9 , data= data.BCRE)
summary(model5)
    
# Create interaction effects for the lag1lnQR variable
data.BCRE <- data.frame(data.BCRE, "lnroomsbathroom" = log(data.BCRE$Rooms*data.BCRE$Bathrooms), "AticoTerrasse" = data.BCRE$Atico*data.BCRE$Terrasse )

# Regression model with the interaction effects for the lag1lnQR variable as additional independent variables
model6 <- lm(lnPrice ~ lnm2 + lnrooms + Bathrooms + Elevator + Terrasse + Parking + Kitchen + Yard + City1 + City2 + City3 + City4 + City5 + City6 + City7 + City8 + City9 + lnroomsbathroom + AticoTerrasse , data= data.BCRE)
summary(model6)

######## Final Model: model 7 ########
# Regression model with the interaction effects for the AticoTerrasse variable as additional independent variables
model7 <- lm(lnPrice ~ lnm2 + lnrooms + Bathrooms + Elevator + Terrasse + Parking + Kitchen + Yard + City1 + City2 + City3 + City4 + City5 + City6 + City7 + City8 + City9 + AticoTerrasse , data= data.BCRE)
summary(model7)

# Check the assumptions (including the auto-correlation of residuals)
hist(residuals(model7), breaks = 500)
plot(fitted.values(model7), residuals(model7))
plot(data.BCRE$lnPrice, residuals(model7))
acf(residuals(model7))

# Make a forecast for the Barcelona House 
data.forecast <- read_excel(file.choose(), sheet = 1)
data.forecast

#dummy variable for CityZone
City1 <- ifelse(data.forecast$CityZone == 'Sant Andreu', 1,0)
City2 <- ifelse(data.forecast$CityZone == 'Sarria - Sant Gervasi', 1,0)
City3 <- ifelse(data.forecast$CityZone == 'Eixample', 1,0)
City4 <- ifelse(data.forecast$CityZone == 'Horta - Guinardó', 1,0)
City5 <- ifelse(data.forecast$CityZone == 'Gràcia', 1,0)
City6 <- ifelse(data.forecast$CityZone == 'Sants - Montjuïc', 1,0)
City7 <- ifelse(data.forecast$CityZone == 'Nou Barris', 1,0)
City8 <- ifelse(data.forecast$CityZone == 'Ciutat Vella', 1,0)
City9 <- ifelse(data.forecast$CityZone == 'Sant Marti', 1,0)

data.forecast <- data.frame(data.forecast, 'City1' = City1, 'City2' = City2, 'City3' = City3, 'City4' = City4, 'City5' = City5, 'City6' = City6, 'City7' = City7, 'City8' = City8, 'City9' = City9)
data.forecast <- data.frame(data.forecast, "lnm2" = log(data.forecast$m2), "lnrooms" = log(data.forecast$Rooms), "AticoTerrasse" = data.forecast$Atico*data.forecast$Terrasse)
forecast <- predict(model7, newdata = data.forecast, interval = "prediction")
data.forecast <- data.frame(data.forecast, exp(forecast))
data.forecast

require(writexl)
write.xlsx(data.forecast, "C:/Users/Ivory/Desktop/Applied Statistics and Probability/Class 12 TeamAssignment/forecast.xlsx", sheetName="Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)