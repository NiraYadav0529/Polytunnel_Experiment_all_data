setwd("//ad.uws.edu.au/dfshare/HomesBLK$/90958427/My Documents/Niraj Pot Experiment/Modified Data File")
> #reading CSV file 
overall <- read.csv("Pot_ExP_Data_For_ph_ec_soil.csv")
head(Pot_ExP_Data_For_ph_ec_soil)
boxplot(pH.Harvest.day~Dose,data=overall) 
head(Pot_ExP_Data_For_ph_ec_soil)
read.csv(overall1)
head(overoverall1head(overall1)
boxplot(Al_.ppm.~Dose*fEC_Second,data=overall1) 
setwd("//ad.uws.edu.au/dfshare/HomesBLK$/90958427/My Documents/Niraj Pot Experiment/Modified Data File")
list.files()
xrffile <- read.csv("XRF_NUTRIENTS_DATA.csv")
head(xrffile)
boxplot(Al_.ppm.~Dose, data=xrffile)
boxplot(Ca_.ppm.~Dose, data=xrffile)
boxplot(Mg_..ppm.~Dose, data=xrffile)
boxplot(K_.ppm.~Dose, data=xrffile)
boxplot(P_.ppm.~Dose, data=xrffile)
setwd("//ad.uws.edu.au/dfshare/HomesBLK$/90958427/My Documents/Niraj Pot Experiment/Modified Data File")
list.files()
xrffile <- read.csv("XRF_NUTRIENTS_DATA.csv")
head(xrffile)
boxplot(Al_.ppm.~Dose, data=xrffile)
boxplot(Ca_.ppm.~Dose, data=xrffile)
boxplot(Mg_..ppm.~Dose, data=xrffile)
boxplot(K_.ppm.~Dose, data=xrffile)
boxplot(P_.ppm.~Dose, data=xrffile)

setwd("//ad.uws.edu.au/dfshare/HomesBLK$/90958427/My Documents/Niraj Pot Experiment/Modified Data File")
list.files()
xrffile <- read.csv("XRF_NUTRIENTS_DATA.csv")
head(xrffile)
boxplot(Al_.ppm.~Dose, data=xrffile)
boxplot(Ca_.ppm.~Dose, data=xrffile)
boxplot(Mg_..ppm.~Dose, data=xrffile)
boxplot(K_.ppm.~Dose, data=xrffile)
boxplot(P_.ppm.~Dose, data=xrffile)
summary(xrffile)
anova(xrffile)
boxplot(Al_.ppm.~sample.identity+Dose, data=xrffile)
boxplot( P_.ppm.~sample.identity+Dose, data=xrffile)
boxplot(Ca_.ppm.~sample.identity+Dose, data=xrffile)
boxplot(Mg_..ppm.~sample.identity+Dose, data=xrffile)
boxplot(K_.ppm.~sample.identity+Dose, data=xrffile)
boxplot(P_.ppm.~Dose, data=xrffile)
setwd("//ad.uws.edu.au/dfshare/HomesBLK$/90958427/My Documents/Niraj Pot Experiment/Modified Data File")

xrf<- read.csv("Pot_ExP_Data_For_ph_ec_soil.csv")
head(xrf)
## zeros in the dataframe, all from high N treatments -- should these be NAs?
x <- which(rowSums(select(xrf, Si......:Zn_.ppm.)) == 0)
xrf[x, ]
xrf <- xrf[-x, ]  # drop those rows

# Check if 'species' variable exists in your dataset
print(names(xrf))
ggplot(xrf, aes(x=sample, y=P_.ppm., colour=Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type)


# Check the structure of your dataset
str(xrffile)
## load libraries
library(tidyverse)
library(car)
library(emmeans)
install.packages(emmeans)
setwd("//ad.uws.edu.au/dfshare/HomesBLK$/90958427/My Documents/Niraj Pot Experiment/Modified Data File")

xrf<- read.csv("Pot_ExP_Data_For_ph_ec_soil.csv")
head(xrf)
## read and inspect data (use read_csv because of symbols in column names)


## zeros in the dataframe, all from high N treatments -- should these be NAs?
x <- which(rowSums(select(xrf, Si......:Zn_.ppm.)) == 0)
xrf[x, ]
xrf <- xrf[-x, ]  # drop those rows

# Check if 'species' variable exists in your dataset
print(names(xrf))

# Ensure that the 'species' variable is correctly spelled and formatted
# If not, correct it
# For example, if 'Species' is the correct variable name:
# xrf$species <- xrf$Species

# Clean your data if necessary
# For example, removing rows with missing values
# xrf <- na.omit(xrf)
## plot P data by Plant.type, tissue and treatment
ggplot(xrf, aes(x=sample.identity, y=P_.ppm., colour=Dose)) + 
  geom_boxplot() + 
  facet_wrap(~Plant.type)

## fit model and compare treatments
# fit model
m1 <- lm(P_.ppm. ~ Dose * species * sample.identity, data=xrf)
# check residuals to see if transformation needed
residualPlot(m1)
qqPlot(m1)
# test significance
Anova(m1)
# comparing means by dose within species (ignores sample.identity)
m1.emm <- emmeans(m1, ~ Dose | species)
multcomp::cld(m1.emm)



#Co-relation with alumunum

# Load required packages
library(ggplot2)
install.packages("reshape2")
library(reshape2)

# Read the data
xrffile <- read.csv("//ad.uws.edu.au/dfshare/HomesBLK$/90958427/My Documents/Niraj Pot Experiment/Modified Data File/XRF_NUTRIENTS_DATA.csv")
head(overall)
setwd("//ad.uws.edu.au/dfshare/HomesBLK$/90958427/My Documents/Niraj Pot Experiment/Modified Data File")
> #reading CSV file 
  overall <- read.csv("Pot_ExP_Data_For_ph_ec_soil.csv")
head(overall)
# Calculate correlation matrix
correlation_matrix <- cor(overall[c("Al_.ppm.", "Ca_.ppm.", "Mg_..ppm.", "K_.ppm.,P_.ppm.,Fe_.ppm.,Cl_.ppm.,Mn_.ppm.,Zn_.ppm.,Na._.ppm.")])

# Convert correlation matrix to dataframe
correlation_df <- melt(correlation_matrix)

# Plot correlation using heatmap
ggplot(correlation_df, aes(Var1, Var2, fill=value)) +
  geom_tile() +
  scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0, limit=c(-1,1)) +
  theme_minimal() +
  labs(title="Correlation between Nutrients", x="Nutrients", y="Nutrients", fill="Correlation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Read the data
xrffile <- read.csv("//ad.uws.edu.au/dfshare/HomesBLK$/90958427/My Documents/Niraj Pot Experiment/Modified Data File/XRF_NUTRIENTS_DATA.csv")

# Select relevant columns for correlation
nutrients <- c("Al_.ppm.", "Ca_.ppm.", "Mg_..ppm.", "K_.ppm.", "P_.ppm.", "Fe_.ppm.", "Cl_.ppm.", "Mn_.ppm.", "Zn_.ppm.", "Na._.ppm.")

# Subset the data
nutrient_data <- xrffile[nutrients]

# Calculate correlation matrix
correlation_matrix <- cor(nutrient_data)

# Convert correlation matrix to dataframe
correlation_df <- as.data.frame(correlation_matrix)

# Plot correlation using heatmap
library(ggplot2)
library(reshape2)

# Melt the correlation matrix
correlation_df_melt <- melt(correlation_df)

# Plot the heatmap
ggplot(correlation_df_melt, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0, limit=c(-1,1)) +
  theme_minimal() +
  labs(title="Correlation between Nutrients", x="Nutrients", y="Nutrients", fill="Correlation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Set working directory
setwd("//ad.uws.edu.au/dfshare/HomesBLK$/90958427/My Documents/Niraj Pot Experiment/Modified Data File")

# Read CSV files
overall <- read.csv("Pot_ExP_Data_For_ph_ec_soil.csv")
xrffile <- read.csv("XRF_NUTRIENTS_DATA.csv")

# Check the structure of your data
str(overall)
str(xrffile)

# Assuming 'Dose' variable represents treatment/control
# Count samples in each treatment group and control group
treated_count_MF100 <- sum(overall$Dose == "MF-100")
treated_count_MF200 <- sum(overall$Dose == "MF-200")
treated_count_UF100 <- sum(overall$Dose == "UF-100")
treated_count_UF200 <- sum(overall$Dose == "UF-200")
control_count <- sum(overall$Dose == "Control")

# Calculate the total number of treated samples
total_treated <- treated_count_MF100 + treated_count_MF200 + treated_count_UF100 + treated_count_UF200

# Calculate the ratio of treated to control
ratio <- total_treated / control_count
ratio
boxplot(ratio)


# Set working directory
setwd("//ad.uws.edu.au/dfshare/HomesBLK$/90958427/My Documents/Niraj Pot Experiment/Modified Data File")

# Read CSV files
overall <- read.csv("Pot_ExP_Data_For_ph_ec_soil.csv")
xrffile <- read.csv("XRF_NUTRIENTS_DATA.csv")

# Assuming 'Dose' variable represents treatment/control
# Filter data for MF-200, UF-200, and Control groups
mf200_data <- xrffile[xrffile$Dose == "UF_100", ]
uf200_data <- xrffile[xrffile$Dose == "UF_200", ]
control_data <- xrffile[xrffile$Dose == "C", ]

# Plotting
par(mar=c(5,5,2,5)) # Set margins
barplot(height = c(mean(c_data$Al_.ppm.), mean(UF_100_data$Al_.ppm.), mean(UF_200_data$Al_.ppm.)),
        names.arg = c("c", "UF_100", "UF_200"),
        ylab = "Mean Al_.ppm.",
        col = c("blue", "green", "red"),
        ylim = c(0, max(c(mean(C_data$Al_.ppm.), mean(UF_100_data$Al_.ppm.), mean(UF_200_data$Al_.ppm.))) * 1.2),
        main = "Mean Al_.ppm. for UF_100, UF_200, and C")
# Set working directory
setwd("//ad.uws.edu.au/dfshare/HomesBLK$/90958427/My Documents/Niraj Pot Experiment/Modified Data File")

# Read CSV files
overall <- read.csv("Pot_ExP_Data_For_ph_ec_soil.csv")
xrffile <- read.csv("XRF_NUTRIENTS_DATA.csv")

# Assuming 'Dose' variable represents treatment/control
# Filter data for MF-200, UF-100, UF-200, and Control groups
mf200_data <- xrffile[xrffile$Dose == "MF-200", ]
uf100_data <- xrffile[xrffile$Dose == "UF-100", ]
uf200_data <- xrffile[xrffile$Dose == "UF-200", ]
control_data <- xrffile[xrffile$Dose == "C", ]

# Plotting
par(mar=c(5,5,2,5)) # Set margins
barplot(height = c(mean(control_data$Al_.ppm.), mean(uf100_data$Al_.ppm.), mean(uf200_data$Al_.ppm.), mean(mf200_data$Al_.ppm.)),
        names.arg = c("Control", "UF-100", "UF-200", "MF-200"),
        ylab = "Mean Al_.ppm.",
        col = c("blue", "green", "red", "orange"),
        ylim = c(0, max(c(mean(control_data$Al_.ppm.), mean(uf100_data$Al_.ppm.), mean(uf200_data$Al_.ppm.), mean(mf200_data$Al_.ppm.))) * 1.2),
        main = "Mean Al_.ppm. for UF-100, UF-200, MF-200, and Control")

# Check the maximum value of Al_.ppm. in all groups
max_value <- max(
  mean(control_data$Al_.ppm, na.rm = TRUE),
  mean(uf100_data$Al_.ppm, na.rm = TRUE),
  mean(uf200_data$Al_.ppm, na.rm = TRUE),
  mean(mf200_data$Al_.ppm, na.rm = TRUE)
)

# Plotting
par(mar=c(5,5,2,5)) # Set margins
barplot(height = c(mean(control_data$Al_.ppm), mean(uf100_data$Al_.ppm), mean(uf200_data$Al_.ppm), mean(mf200_data$Al_.ppm)),
        names.arg = c("Control", "UF-100", "UF-200", "MF-200"),
        ylab = "Mean Al_.ppm.",
        col = c("blue", "green", "red", "orange"),
        ylim = c(0, max_value * 1.2),  # Set ylim based on max_value
        main = "Mean Al_.ppm. for UF-100, UF-200, MF-200, and Control")

# Make sure the column name is correct
# Use the correct column name with the boxplot function
boxplot(Al_.ppm.~ Dose, data = xrffile)

# If there are non-numeric entries in the column, convert them to NA
xrffile$Al_.ppm. <- as.numeric(xrffile$Al_.ppm.)

# Check for NA values in the column
sum(is.na(xrffile$Al_.ppm.))

# Remove rows with NA values in the Al_.ppm. column
xrffile <- xrffile[!is.na(xrffile$Al_.ppm.), ]

# Now, try the boxplot again
boxplot(Cl_.ppm. ~ Dose, data = xrffile)
head(xrffile)
boxplot(Cl_.ppm.~ Dose, data = xrffile)
# Convert numeric columns to numeric
num_cols <- c("Si......", "Na._.ppm.", "Mg_..ppm.", "Al_.ppm.", "P_.ppm.", "K_.ppm.", 
              "Ca_.ppm.", "Fe_.ppm.", "Cl_.ppm.", "SiO2_...", "Ag_.cps.", "Cu_.ppm.", 
              "Mn_.ppm.", "S_.ppm.", "Zn_.ppm.")

xrffile[num_cols] <- lapply(xrffile[num_cols], as.numeric)

# Check for NA values in the numeric columns
sum(is.na(xrffile[, num_cols]))

# Remove rows with NA values in any of the numeric columns
xrffile <- xrffile[complete.cases(xrffile[, num_cols]), ]

# Now, try the boxplot again
boxplot(Al_.ppm. ~ Dose, data = xrffile)
boxplot(Cl_.ppm. ~ Dose, data = xrffile)
boxplot(Mn_.ppm. ~ Dose, data = xrffile)
boxplot(Zn_.ppm. ~ Dose, data = xrffile)
head(xrffile)


# Convert numeric columns to numeric
num_cols <- c("Si......", "Na._.ppm.", "Mg_..ppm.", "Al_.ppm.", "P_.ppm.", "K_.ppm.", 
              "Ca_.ppm.", "Fe_.ppm.", "Cl_.ppm.", "SiO2_...", "Ag_.cps.", "Cu_.ppm.", 
              "Mn_.ppm.", "S_.ppm.", "Zn_.ppm.")

xrffile[num_cols] <- lapply(xrffile[num_cols], as.numeric)

# Check for NA values in the numeric columns
sum(is.na(xrffile[, num_cols]))

# Remove rows with NA values in any of the numeric columns
xrffile <- xrffile[complete.cases(xrffile[, num_cols]), ]

# Now, try the boxplot again
boxplot(Al_.ppm., sample.identity ~ Dose, data = xrffile)
boxplot(Cl_.ppm. ~ Dose, data = xrffile)

num_cols <- c("SiO2_...", "AgOO", "CuOO", "MnOO", "S", "ZnOO")
num_cols <- make.names(num_cols)
names(xrffile) <- num_cols
boxplot(AlOO ~ Dose, data = xrffile)
boxplot(ClOO ~ Dose, data = xrffile)
boxplot(MnOO ~ Dose, data = xrffile)
boxplot(AlOO ~ Dose, data = xrffile)
boxplot(ClOO ~ Dose, data = xrffile)
boxplot(MnOO ~ Dose, data = xrffile)
