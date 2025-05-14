################################
######FEMALES ADOLESCENT#######
################################

# Install and load the metafor package if not already installed
if(!require(metafor)) install.packages("metafor", dependencies = TRUE)
library(metafor)

# Create the dataset
data <- data.frame(
  sex = c("female", "female", "female", "female", "female", "female"),
  stressor = c("RS", "RS", "RS", "SI", "SI", "SI"),
  readout = c("PPT-MF", "PPT-GS", "PWT", "PPT-MF", "PPT-GS", "PWT"),
  p_value_U = c(0.1996, 0.061369, 0.026976, 0.017064, 0.32515, 0.0716),
  U = c(70, 57, 50, 52, 84, 65),
  n_stress = c(11, 11, 11, 12, 12, 12),
  n_controls = c(18, 18, 18, 18, 18, 18),
  d = c(-0.499, -0.749, -0.896, -0.96, -0.378, -0.705),
  SE = c(0.388275375, 0.395141845, 0.400382694, 0.392745323, 0.3758594, 0.383630863),
  lower_d = c(-1.260019736, -1.523478016, -1.68075008, -1.729780833, -1.114684425, -1.456916491),
  upper_d = c(0.262019736, 0.025478016, -0.11124992, -0.190219167, 0.358684425, 0.046916491)
)

# View the dataset
print(data)

# Calculate variance from CI
data$variance <- ((data$upper_d - data$lower_d) / (2 * 1.96))^2

# Perform meta-analysis using random-effects model
meta_analysis <- rma(yi = d, vi = data$variance, data = data, method = "REML")

# Create the forest plot
forest(meta_analysis,
       slab = paste(data$sex, data$stressor, data$readout, sep = " - "),
       xlab = "Effect Size (Cohen's d)",
       xlim = c(-5, 4),
       at = seq(-3, 2, 1),
       refline = 0,
       col = "blue",
       psize = 1.5,
       cex = 2.0,          # Adjusts text size in plot
       cex.lab = 1.4,      # Adjusts axis labels
       cex.axis = 1.2)     # Adjusts axis numbering size

# Add titles
title(main = "Forest Plot of Cohen's d Effect Sizes by females adolescent",
      sub = "Random Effects Meta-Analysis Model")


################################
######MALES ADOLESCENT##########
################################
# Install and load the metafor package if not already installed
if(!require(metafor)) install.packages("metafor", dependencies = TRUE)
library(metafor)

data <- data.frame(
  sex = c("male", "male", "male", "male", "male", "male"),
  stressor = c("RS", "RS", "RS", "SI", "SI", "SI"),
  readout = c("PPT-MF", "PPT-GS", "PWT", "PPT-MF", "PPT-GS", "PWT"),
  p_value_U = c(0.000044, NA, 0.000737, 0.047865, 0.011598, 0.000542),
  U = c(6, NA, 14, 61, 49, 30),
  n_stress = c(11, NA, 11, 12, 12, 12),
  n_controls = c(12, NA, 12, 18, 18, 18),
  d = c(-2.414, NA, -1.792, -0.78, -1.025, -1.511),
  SE = c(0.548566264, NA, 0.494016711, 0.3860426, 0.395473521, 0.420643442),
  lower_d = c(-3.489189877, NA, -2.760272754, -1.536643496, -1.800128101, -2.335461147),
  upper_d = c(-1.338810123, NA, -0.823727246, -0.023356504, -0.249871899, -0.686538853)
)

# Print the dataset
print(data)

# Calculate variance and add as a new column
data$variance <- with(data, ((upper_d - lower_d) / (2 * 1.96))^2)

# Print the updated dataset
print(data)

# Meta-analysis using random-effects model
meta_analysis <- rma(yi = d, vi = data$variance, data = data, method = "REML")

# Create the forest plot with adjusted text sizes
forest(meta_analysis,
       slab = paste(data$sex,data$stressor, data$readout, sep = " - "),
       xlab = "Effect Size (Cohen's d)",
       xlim = c(-5, 4),
       at = seq(-3, 2, 1),
       refline = 0,
       col = "blue",
       psize = 1.5,
       cex = 2.0,          # Adjusts text size in plot
       cex.lab = 1.4,      # Adjusts axis labels
       cex.axis = 1.2)     # Adjusts axis numbering size

# Add titles and additional labels with adjusted sizes
title(main = "Forest Plot of Cohen's d Effect Sizes (Male Subgroup)",
      sub = "Random Effects Meta-Analysis Model",
      cex.main = 1.5,     # Adjusts title size
      cex.sub = 1.3)      # Adjusts subtitle size

# Print meta-analysis summary
print(meta_analysis)

################################
######FEMALES ADULTHOOD##########
################################
# Install and load the metafor package if not already installed
if(!require(metafor)) install.packages("metafor", dependencies = TRUE)
library(metafor)

data <- data.frame(
  sex = c("female", "female", "female", "female", "female", "female"),
  stressor = c("RS", "RS", "RS", "SI", "SI", "SI"),
  readout = c("PPT-MF", "PPT-GS", "PWT", "PPT-MF", "PPT-GS", "PWT"),
  p_value_U = c(0.2379, 0.083554, 0.527879, 0.602069, 0.053097, 0.638939),
  U = c(72, 60, 84.5, 95, 62, 96.5),
  n_stress = c(11, 11, 11, 12, 12, 12),
  n_controls = c(18, 18, 18, 18, 18, 18),
  d = c(0.463, -0.689, -0.244, -0.202, -0.761, -0.178),
  SE = c(0.387505695, 0.393254995, 0.384045738, 0.373589287, 0.385410049, 0.3733858),
  lower_d = c(-0.296511162, -1.459779791, -0.996729647, -0.934235003, -1.516403695, -0.909836168),
  upper_d = c(1.222511162, 0.081779791, 0.508729647, 0.530235003, -0.005596305, 0.553836168)
)

# Print the dataset
print(data)
# Calculate variance and add as a new column
data$variance <- with(data, ((upper_d - lower_d) / (2 * 1.96))^2)

# Print the updated dataset
print(data)

# Meta-analysis using random-effects model
meta_analysis <- rma(yi = d, vi = data$variance, data = data, method = "REML")

# Create the forest plot with adjusted text sizes
forest(meta_analysis,
       slab = paste(data$sex,data$stressor, data$readout, sep = " - "),
       xlab = "Effect Size (Cohen's d)",
       xlim = c(-5, 4),
       at = seq(-3, 2, 1),
       refline = 0,
       col = "blue",
       psize = 1.5,
       cex = 2.0,          # Adjusts text size in plot
       cex.lab = 1.4,      # Adjusts axis labels
       cex.axis = 1.2)     # Adjusts axis numbering size

# Add titles and additional labels with adjusted sizes
title(main = "Forest Plot of Cohen's d Effect Sizes (Male Subgroup)",
      sub = "Random Effects Meta-Analysis Model",
      cex.main = 1.5,     # Adjusts title size
      cex.sub = 1.3)      # Adjusts subtitle size

# Print meta-analysis summary
print(meta_analysis)


################################
######MALES ADULTHOOD###########
################################
# Install and load the metafor package if not already installed
if(!require(metafor)) install.packages("metafor", dependencies = TRUE)
library(metafor)


# Create the dataset
data <- data.frame(
  sex = c("male", "male", "male", "male", "male", "male"),
  stressor = c("RS", "RS", "RS", "SI", "SI", "SI"),
  readout = c("PPT-MF", "PPT-GS", "PWT", "PPT-MF", "PPT-GS", "PWT"),
  p_value_U = c(0.016372, NA, 0.051219, 0.661522, 0.391305, 0.368427),
  U = c(27.5, NA, 34, 97, 87, 86),
  n_stress = c(11, NA, 11, 12, 12, 12),
  n_controls = c(12, NA, 12, 18, 18, 18),
  d = c(-1.137, NA, -0.901, -0.171, -0.329, -0.345),
  SE = c(0.449828965, NA, 0.438052819, 0.373331272, 0.37509053, 0.37533004),
  lower_d = c(-2.018664772, NA, -1.759583525, -0.902729294, -1.064177438, -1.080646878),
  upper_d = c(-0.255335228, NA, -0.042416475, 0.560729294, 0.406177438, 0.390646878)
)

# Print the dataset
print(data)

# Calculate variance and add as a new column
data$variance <- with(data, ((upper_d - lower_d) / (2 * 1.96))^2)

# Print the updated dataset
print(data)

# Meta-analysis using random-effects model
meta_analysis <- rma(yi = d, vi = data$variance, data = data, method = "REML")

# Create the forest plot with adjusted text sizes
forest(meta_analysis,
       slab = paste(data$sex,data$stressor, data$readout, sep = " - "),
       xlab = "Effect Size (Cohen's d)",
       xlim = c(-5, 4),
       at = seq(-3, 2, 1),
       refline = 0,
       col = "blue",
       psize = 1.5,
       cex = 2.0,          # Adjusts text size in plot
       cex.lab = 1.4,      # Adjusts axis labels
       cex.axis = 1.2)     # Adjusts axis numbering size

# Add titles and additional labels with adjusted sizes
title(main = "Forest Plot of Cohen's d Effect Sizes (Male Subgroup)",
      sub = "Random Effects Meta-Analysis Model",
      cex.main = 1.5,     # Adjusts title size
      cex.sub = 1.3)      # Adjusts subtitle size

# Print meta-analysis summary
print(meta_analysis)



################################
######SI ADULTHOOD###########
################################
# Install and load the metafor package if not already installed
if(!require(metafor)) install.packages("metafor", dependencies = TRUE)
library(metafor)

# Create the dataset for the provided data
data <- data.frame(
  sex = c("female", "female", "female", "male", "male", "male"),
  stressor = c("SI", "SI", "SI", "SI", "SI", "SI"),
  readout = c("PPT-MF", "PPT-GS", "PWT", "PPT-MF", "PPT-GS", "PWT"),
  p_value_U = c(0.602069, 0.053097, 0.638939, 0.661522, 0.391305, 0.368427),
  U = c(95, 62, 96.5, 97, 87, 86),
  n_stress = c(12, 12, 12, 12, 12, 12),
  n_controls = c(18, 18, 18, 18, 18, 18),
  d = c(-0.202, -0.761, -0.178, -0.171, -0.329, -0.345),
  SE = c(0.373589287, 0.385410049, 0.3733858, 0.373331272, 0.37509053, 0.37533004),
  lower_d = c(-0.934235003, -1.516403695, -0.909836168, -0.902729294, -1.064177438, -1.080646878),
  upper_d = c(0.530235003, -0.005596305, 0.553836168, 0.560729294, 0.406177438, 0.390646878)
)

# Print the dataset
print(data)

# Calculate variance and add as a new column
data$variance <- with(data, ((upper_d - lower_d) / (2 * 1.96))^2)

# Print the updated dataset
print(data)

# Meta-analysis using random-effects model
meta_analysis <- rma(yi = d, vi = data$variance, data = data, method = "REML")

# Create the forest plot with adjusted text sizes
forest(meta_analysis,
       slab = paste(data$sex,data$stressor, data$readout, sep = " - "),
       xlab = "Effect Size (Cohen's d)",
       xlim = c(-5, 4),
       at = seq(-3, 2, 1),
       refline = 0,
       col = "blue",
       psize = 1.5,
       cex = 2.0,          # Adjusts text size in plot
       cex.lab = 1.4,      # Adjusts axis labels
       cex.axis = 1.2)     # Adjusts axis numbering size

# Add titles and additional labels with adjusted sizes
title(main = "Forest Plot of Cohen's d Effect Sizes (Male Subgroup)",
      sub = "Random Effects Meta-Analysis Model",
      cex.main = 1.5,     # Adjusts title size
      cex.sub = 1.3)      # Adjusts subtitle size

# Print meta-analysis summary
print(meta_analysis)



################################
###### RS ADOLESCENCE ###########
################################

# Install and load the metafor package if not already installed
if(!require(metafor)) install.packages("metafor", dependencies = TRUE)
library(metafor)

# Create the dataset with the new values
data <- data.frame(
  sex = c("female", "female", "female", "male", "male", "male"),
  stressor = c("RS", "RS", "RS", "RS", "RS", "RS"),
  readout = c("PPT-MF", "PPT-GS", "PWT", "PPT-MF", "PPT-GS", "PWT"),
  p_value_U = c(0.1996, 0.061369, 0.026976, 0.000044, NA, 0.000737),
  U = c(70, 57, 50, 6, NA, 14),
  n_stress = c(11, 11, 11, 11, 11, 11),
  n_controls = c(18, 18, 18, 12, 12, 12),
  d = c(-0.499, -0.749, -0.896, -2.414, NA, -1.792),
  SE = c(0.388275375, 0.395141845, 0.400382694, 0.548566264, NA, 0.494016711),
  lower_d = c(-1.260019736, -1.523478016, -1.68075008, -3.489189877, NA, -2.760272754),
  upper_d = c(0.262019736, 0.025478016, -0.11124992, -1.338810123, NA, -0.823727246)
)

# Print the dataset
print(data)

# Meta-analysis using random-effects model
# Since the variance is not provided, we will calculate it from SE (variance = SE^2)
data$variance <- data$SE^2

meta_analysis <- rma(yi = d, vi = data$variance, data = data, method = "REML")

# Create the forest plot with adjusted text sizes
forest(meta_analysis,
       slab = paste(data$sex, data$stressor, data$readout, sep = " - "),
       xlab = "Effect Size (Cohen's d)",
       xlim = c(-5, 4),
       at = seq(-3, 2, 1),
       refline = 0,
       col = "blue",
       psize = 1.5,
       cex = 2.0,          # Adjusts text size in plot
       cex.lab = 1.4,      # Adjusts axis labels
       cex.axis = 1.2)     # Adjusts axis numbering size

# Add titles and additional labels with adjusted sizes
title(main = "Forest Plot of Cohen's d Effect Sizes (RS Stressor)",
      sub = "Random Effects Meta-Analysis Model",
      cex.main = 1.5,     # Adjusts title size
      cex.sub = 1.3)      # Adjusts subtitle size

# Print meta-analysis summary
print(meta_analysis)



################################
###### SI ADOLESCENCE ###########
################################

# Install and load the metafor package if not already installed
if(!require(metafor)) install.packages("metafor", dependencies = TRUE)
library(metafor)

# Create the dataset with the new values
data <- data.frame(
  sex = c("female", "female", "female", "male", "male", "male"),
  stressor = c("SI", "SI", "SI", "SI", "SI", "SI"),
  readout = c("PPT-MF", "PPT-GS", "PWT", "PPT-MF", "PPT-GS", "PWT"),
  p_value_U = c(0.017064, 0.32515, 0.0716, 0.047865, 0.011598, 0.000542),
  U = c(52, 84, 65, 61, 49, 30),
  n_stress = c(12, 12, 12, 12, 12, 12),
  n_controls = c(18, 18, 18, 18, 18, 18),
  d = c(-0.96, -0.378, -0.705, -0.78, -1.025, -1.511),
  SE = c(0.392745323, 0.3758594, 0.383630863, 0.3860426, 0.395473521, 0.420643442),
  lower_d = c(-1.729780833, -1.114684425, -1.456916491, -1.536643496, -1.800128101, -2.335461147),
  upper_d = c(-0.190219167, 0.358684425, 0.046916491, -0.023356504, -0.249871899, -0.686538853)
)

# Print the dataset
print(data)

# Meta-analysis using random-effects model
# Since the variance is not provided, we will calculate it from SE (variance = SE^2)
data$variance <- data$SE^2

meta_analysis <- rma(yi = d, vi = data$variance, data = data, method = "REML")

# Create the forest plot with adjusted text sizes
forest(meta_analysis,
       slab = paste(data$sex, data$stressor, data$readout, sep = " - "),
       xlab = "Effect Size (Cohen's d)",
       xlim = c(-5, 4),
       at = seq(-3, 2, 1),
       refline = 0,
       col = "blue",
       psize = 1.5,
       cex = 2.0,          # Adjusts text size in plot
       cex.lab = 1.4,      # Adjusts axis labels
       cex.axis = 1.2)     # Adjusts axis numbering size

# Add titles and additional labels with adjusted sizes
title(main = "Forest Plot of Cohen's d Effect Sizes (SI Stressor)",
      sub = "Random Effects Meta-Analysis Model",
      cex.main = 1.5,     # Adjusts title size
      cex.sub = 1.3)      # Adjusts subtitle size

# Print meta-analysis summary
print(meta_analysis)


################################
###### PPT-MF ADOLESCENCE ######
################################

# Install and load the metafor package if not already installed
if(!require(metafor)) install.packages("metafor", dependencies = TRUE)
library(metafor)

# Create the dataset with the new values
data <- data.frame(
  sex = c("female", "female", "male", "male"),
  stressor = c("RS", "SI", "RS", "SI"),
  readout = c("PPT-MF", "PPT-MF", "PPT-MF", "PPT-MF"),
  p_value_U = c(0.1996, 0.017064, 0.000044, 0.047865),
  U = c(70, 52, 6, 61),
  n_stress = c(11, 12, 11, 12),
  n_controls = c(18, 18, 12, 18),
  d = c(-0.499, -0.96, -2.414, -0.78),
  SE = c(0.388275375, 0.392745323, 0.548566264, 0.3860426),
  lower_d = c(-1.260019736, -1.729780833, -3.489189877, -1.536643496),
  upper_d = c(0.262019736, -0.190219167, -1.338810123, -0.023356504)
)

# Print the dataset
print(data)

# Meta-analysis using random-effects model
# Since the variance is not provided, we will calculate it from SE (variance = SE^2)
data$variance <- data$SE^2

meta_analysis <- rma(yi = d, vi = data$variance, data = data, method = "REML")

# Create the forest plot with adjusted text sizes
forest(meta_analysis,
       slab = paste(data$sex, data$stressor, data$readout, sep = " - "),
       xlab = "Effect Size (Cohen's d)",
       xlim = c(-5, 4),
       at = seq(-3, 2, 1),
       refline = 0,
       col = "blue",
       psize = 1.5,
       cex = 2.0,          # Adjusts text size in plot
       cex.lab = 1.4,      # Adjusts axis labels
       cex.axis = 1.2)     # Adjusts axis numbering size

# Add titles and additional labels with adjusted sizes
title(main = "Forest Plot of Cohen's d Effect Sizes (RS and SI Stressors)",
      sub = "Random Effects Meta-Analysis Model",
      cex.main = 1.5,     # Adjusts title size
      cex.sub = 1.3)      # Adjusts subtitle size

# Print meta-analysis summary
print(meta_analysis)


################################
###### PPT-GS ADOLESCENCE ######
################################

# Install and load the metafor package if not already installed
if(!require(metafor)) install.packages("metafor", dependencies = TRUE)
library(metafor)

# Create the dataset with the new values
data <- data.frame(
  sex = c("female", "female", "male", "male"),
  stressor = c("RS", "SI", "RS", "SI"),
  readout = c("PPT-GS", "PPT-GS", "PPT-GS", "PPT-GS"),
  p_value_U = c(0.061369, 0.32515, NA, 0.011598),
  U = c(57, 84, NA, 49),
  n_stress = c(11, 12, 12, 12),
  n_controls = c(18, 18, 18, 18),
  d = c(-0.749, -0.378, NA, -1.025),
  SE = c(0.395141845, 0.3758594, NA, 0.395473521),
  lower_d = c(-1.523478016, -1.114684425, NA, -1.800128101),
  upper_d = c(0.025478016, 0.358684425, NA, -0.249871899)
)

# Print the dataset
print(data)

# Meta-analysis using random-effects model
# Since the variance is not provided, we will calculate it from SE (variance = SE^2)
data$variance <- data$SE^2

# Perform the meta-analysis (excluding rows with NA values for d and SE)
meta_analysis <- rma(yi = d, vi = data$variance, data = data, method = "REML", subset = !is.na(d) & !is.na(variance))

# Create the forest plot with adjusted text sizes
forest(meta_analysis,
       slab = paste(data$sex, data$stressor, data$readout, sep = " - "),
       xlab = "Effect Size (Cohen's d)",
       xlim = c(-5, 4),
       at = seq(-3, 2, 1),
       refline = 0,
       col = "blue",
       psize = 1.5,
       cex = 2.0,          # Adjusts text size in plot
       cex.lab = 1.4,      # Adjusts axis labels
       cex.axis = 1.2)     # Adjusts axis numbering size

# Add titles and additional labels with adjusted sizes
title(main = "Forest Plot of Cohen's d Effect Sizes (RS and SI Stressors)",
      sub = "Random Effects Meta-Analysis Model",
      cex.main = 1.5,     # Adjusts title size
      cex.sub = 1.3)      # Adjusts subtitle size

# Print meta-analysis summary
print(meta_analysis)

################################
###### PWT ADOLESCENCE ######
################################

# Install and load the metafor package if not already installed
if(!require(metafor)) install.packages("metafor", dependencies = TRUE)
library(metafor)

# Create the dataset with the new values
data <- data.frame(
  sex = c("female", "female", "male", "male"),
  stressor = c("RS", "SI", "RS", "SI"),
  readout = c("PWT", "PWT", "PWT", "PWT"),
  p_value_U = c(0.026976, 0.0716, 0.000737, 0.000542),
  U = c(50, 65, 14, 30),
  n_stress = c(11, 12, 11, 12),
  n_controls = c(18, 18, 12, 18),
  d = c(-0.896, -0.705, -1.792, -1.511),
  SE = c(0.400382694, 0.383630863, 0.494016711, 0.420643442),
  lower_d = c(-1.68075008, -1.456916491, -2.760272754, -2.335461147),
  upper_d = c(-0.11124992, 0.046916491, -0.823727246, -0.686538853)
)

# Print the dataset
print(data)

# Meta-analysis using random-effects model
# Since the variance is not provided, we will calculate it from SE (variance = SE^2)
data$variance <- data$SE^2

# Perform the meta-analysis (excluding rows with NA values for d and SE)
meta_analysis <- rma(yi = d, vi = data$variance, data = data, method = "REML", subset = !is.na(d) & !is.na(variance))

# Create the forest plot with adjusted text sizes
forest(meta_analysis,
       slab = paste(data$sex, data$stressor, data$readout, sep = " - "),
       xlab = "Effect Size (Cohen's d)",
       xlim = c(-5, 4),
       at = seq(-3, 2, 1),
       refline = 0,
       col = "blue",
       psize = 1.5,
       cex = 2.0,          # Adjusts text size in plot
       cex.lab = 1.4,      # Adjusts axis labels
       cex.axis = 1.2)     # Adjusts axis numbering size

# Add titles and additional labels with adjusted sizes
title(main = "Forest Plot of Cohen's d Effect Sizes (RS and SI Stressors)",
      sub = "Random Effects Meta-Analysis Model",
      cex.main = 1.5,     # Adjusts title size
      cex.sub = 1.3)      # Adjusts subtitle size

# Print meta-analysis summary
print(meta_analysis)


################################
###### PPT-MF. ADULTHOOD ######
################################

# Install and load the metafor package if not already installed
if(!require(metafor)) install.packages("metafor", dependencies = TRUE)
library(metafor)

# Create the dataset with the new values
data <- data.frame(
  sex = c("female", "female", "male", "male"),
  stressor = c("RS", "SI", "RS", "SI"),
  readout = c("PPT-MF", "PPT-MF", "PPT-MF", "PPT-MF"),
  p_value_U = c(0.2379, 0.602069, 0.016372, 0.661522),
  U = c(72, 95, 27.5, 97),
  n_stress = c(11, 12, 11, 12),
  n_controls = c(18, 18, 12, 18),
  d = c(0.463, -0.202, -1.137, -0.171),
  SE = c(0.387505695, 0.373589287, 0.449828965, 0.373331272),
  lower_d = c(-0.296511162, -0.934235003, -2.018664772, -0.902729294),
  upper_d = c(1.222511162, 0.530235003, -0.255335228, 0.560729294)
)

# Print the dataset
print(data)

# Meta-analysis using random-effects model
# Since the variance is not provided, we will calculate it from SE (variance = SE^2)
data$variance <- data$SE^2

# Perform the meta-analysis (excluding rows with NA values for d and SE)
meta_analysis <- rma(yi = d, vi = data$variance, data = data, method = "REML", subset = !is.na(d) & !is.na(variance))

# Create the forest plot with adjusted text sizes
forest(meta_analysis,
       slab = paste(data$sex, data$stressor, data$readout, sep = " - "),
       xlab = "Effect Size (Cohen's d)",
       xlim = c(-5, 4),
       at = seq(-3, 2, 1),
       refline = 0,
       col = "blue",
       psize = 1.5,
       cex = 2.0,          # Adjusts text size in plot
       cex.lab = 1.4,      # Adjusts axis labels
       cex.axis = 1.2)     # Adjusts axis numbering size

# Add titles and additional labels with adjusted sizes
title(main = "Forest Plot of Cohen's d Effect Sizes (RS and SI Stressors)",
      sub = "Random Effects Meta-Analysis Model",
      cex.main = 1.5,     # Adjusts title size
      cex.sub = 1.3)      # Adjusts subtitle size

# Print meta-analysis summary
print(meta_analysis)


################################
###### GS aduthood ######
################################

# Install and load the metafor package if not already installed
if(!require(metafor)) install.packages("metafor", dependencies = TRUE)
library(metafor)

# Create the dataset with the new values
data <- data.frame(
  sex = c("female", "female", "male", "male"),
  stressor = c("RS", "SI", "RS", "SI"),
  readout = c("PPT-GS", "PPT-GS", "PPT-GS", "PPT-GS"),
  p_value_U = c(0.083554, 0.053097, NA, 0.391305),
  U = c(60, 62, NA, 87),
  n_stress = c(11, 12, NA, 12),
  n_controls = c(18, 18, NA, 18),
  d = c(-0.689, -0.761, NA, -0.329),
  SE = c(0.393254995, 0.385410049, NA, 0.37509053),
  lower_d = c(-1.459779791, -1.516403695, NA, -1.064177438),
  upper_d = c(0.081779791, -0.005596305, NA, 0.406177438)
)

# Print the dataset
print(data)

# Meta-analysis using random-effects model
# Since the variance is not provided, we will calculate it from SE (variance = SE^2)
data$variance <- data$SE^2

# Perform the meta-analysis (excluding rows with NA values for d and SE)
meta_analysis <- rma(yi = d, vi = data$variance, data = data, method = "REML", subset = !is.na(d) & !is.na(variance))

# Create the forest plot with adjusted text sizes
forest(meta_analysis,
       slab = paste(data$sex, data$stressor, data$readout, sep = " - "),
       xlab = "Effect Size (Cohen's d)",
       xlim = c(-5, 4),
       at = seq(-3, 2, 1),
       refline = 0,
       col = "blue",
       psize = 1.5,
       cex = 2.0,          # Adjusts text size in plot
       cex.lab = 1.4,      # Adjusts axis labels
       cex.axis = 1.2)     # Adjusts axis numbering size

# Add titles and additional labels with adjusted sizes
title(main = "Forest Plot of Cohen's d Effect Sizes (RS and SI Stressors)",
      sub = "Random Effects Meta-Analysis Model",
      cex.main = 1.5,     # Adjusts title size
      cex.sub = 1.3)      # Adjusts subtitle size

# Print meta-analysis summary
print(meta_analysis)


################################
###### PWT adulthood ######
################################

# Install and load the metafor package if not already installed
if(!require(metafor)) install.packages("metafor", dependencies = TRUE)
library(metafor)

# Create the dataset with the new values
data <- data.frame(
  sex = c("female", "female", "male", "male"),
  stressor = c("RS", "SI", "RS", "SI"),
  readout = c("PWT", "PWT", "PWT", "PWT"),
  p_value_U = c(0.527879, 0.638939, 0.051219, 0.368427),
  U = c(84.5, 96.5, 34, 86),
  n_stress = c(11, 12, 11, 12),
  n_controls = c(18, 18, 12, 18),
  d = c(-0.244, -0.178, -0.901, -0.345),
  SE = c(0.384045738, 0.3733858, 0.438052819, 0.37533004),
  lower_d = c(-0.996729647, -0.909836168, -1.759583525, -1.080646878),
  upper_d = c(0.508729647, 0.553836168, -0.042416475, 0.390646878)
)

# Print the dataset
print(data)

# Meta-analysis using random-effects model
# Since the variance is not provided, we will calculate it from SE (variance = SE^2)
data$variance <- data$SE^2

# Perform the meta-analysis (excluding rows with NA values for d and SE)
meta_analysis <- rma(yi = d, vi = data$variance, data = data, method = "REML", subset = !is.na(d) & !is.na(variance))

# Create the forest plot with adjusted text sizes
forest(meta_analysis,
       slab = paste(data$sex, data$stressor, data$readout, sep = " - "),
       xlab = "Effect Size (Cohen's d)",
       xlim = c(-5, 4),
       at = seq(-3, 2, 1),
       refline = 0,
       col = "blue",
       psize = 1.5,
       cex = 2.0,          # Adjusts text size in plot
       cex.lab = 1.4,      # Adjusts axis labels
       cex.axis = 1.2)     # Adjusts axis numbering size

# Add titles and additional labels with adjusted sizes
title(main = "Forest Plot of Cohen's d Effect Sizes (RS and SI Stressors)",
      sub = "Random Effects Meta-Analysis Model",
      cex.main = 1.5,     # Adjusts title size
      cex.sub = 1.3)      # Adjusts subtitle size

# Print meta-analysis summary
print(meta_analysis)



#################################
#######saline injection pooled_SI#
#################################

# Install and load the metafor package if not already installed
if(!require(metafor)) install.packages("metafor", dependencies = TRUE)
library(metafor)

# Create the dataset with the updated values
data <- data.frame(
  sex = c("Female", "Female", "Female", "Male", "Male", "Male"),
  stressor = c("SI", "SI", "SI", "SI", "SI", "SI"),
  readout = c("PPT-MF", "PPT-GS", "PWT", "PPT-MF", "PPT-GS", "PWT"),
  p_value_U = c(0.5589876, 0.842836, 0.712535, 0.513723, 0.755285, 0.218921),
  U = c(62, 68, 65, 60, 66, 50),
  d = c(0.237, 0.094, 0.166, 0.286, 0.142, 0.537),
  n_PD85 = c(12, 12, 12, 12, 12, 12),
  n_PD86 = c(12, 12, 12, 12, 12, 12),
  SE = c(0.409678965, 0.408473683, 0.408950792, 0.41033005, 0.408762462, 0.415541038),
  lower_d = c(-0.541390033, -0.682099998, -0.611006504, -0.493627095, -0.634648677, -0.252527972),
  upper_d = c(1.039970771, 0.894608419, 0.967543552, 1.090246898, 0.943174425, 1.351460434)
)

# Print the dataset
print(data)

# Calculate variance from SE
data$variance <- data$SE^2

# Perform the meta-analysis (excluding rows with NA values for d and SE)
meta_analysis <- rma(yi = d, vi = data$variance, data = data, method = "REML", subset = !is.na(d) & !is.na(variance))

# Create the forest plot with adjusted text sizes
forest(meta_analysis,
       slab = paste(data$sex, data$stressor, data$readout, sep = " - "),
       xlab = "Effect Size (Cohen's d)",
       xlim = c(-1.5, 1.5),  # Adjusted range based on your data
       at = seq(-1, 1, 0.5), # Ticks for x-axis
       refline = 0,
       col = "blue",
       psize = 1.5,          # Point size
       cex = 1.2,            # Text size for labels in the plot
       cex.lab = 1.2,        # Axis label text size
       cex.axis = 1.1)       # Axis numbering size

# Add titles and additional labels with adjusted sizes
title(main = "Forest Plot of Cohen's d Effect Sizes (SI Stressor)",
      sub = "Random Effects Meta-Analysis Model",
      cex.main = 1.3,       # Title size
      cex.sub = 1.2)        # Subtitle size

# Print meta-analysis summary
print(meta_analysis)


#####SI-controls_firstSalinj###



# Install and load the metafor package if not already installed
if(!require(metafor)) install.packages("metafor", dependencies = TRUE)
library(metafor)

# Create the dataset with the updated values
data <- data.frame(
  sex = c("Female", "Female", "Female", "Male", "Male", "Male"),
  stressor = c("SI_Control", "SI_Control", "SI_Control", "SI_Control", "SI_Control", "SI_Control"),
  readout = c("PPT-MF", "PPT-GS", "PWT", "PPT-MF", "PPT-GS", "PWT"),
  p_value_U = c(0.842836, 0.347358, 0.786983, 0.347358, 0.966219, 0.318585),
  U = c(68, 55, 67, 55, 71, 54),
  d = c(0.094, 0.409, 0.118, 0.409, 0.024, 0.434),
  n_PD85 = c(12, 12, 12, 12, 12, 12),
  n_PD86 = c(12, 12, 12, 12, 12, 12),
  SE = c(0.408473683, 0.41249447, 0.408603414, 0.41249447, 0.408262987, 0.413026331),
  lower_d = c(-0.706608419, -0.399489161, -0.682862692, -0.399489161, -0.776195455, -0.375531609),
  upper_d = c(0.894608419, 1.217489161, 0.918862692, 1.217489161, 0.824195455, 1.243531609)
)

# Print the dataset
print(data)

# Calculate variance from SE
data$variance <- data$SE^2

# Perform the meta-analysis (excluding rows with NA values for d and SE)
meta_analysis <- rma(yi = d, vi = data$variance, data = data, method = "REML", subset = !is.na(d) & !is.na(variance))

# Create the forest plot with adjusted text sizes
forest(meta_analysis,
       slab = paste(data$sex, data$stressor, data$readout, sep = " - "),
       xlab = "Effect Size (Cohen's d)",
       xlim = c(-1.5, 1.5),  # Adjusted range based on your data
       at = seq(-1, 1, 0.5), # Ticks for x-axis
       refline = 0,
       col = "blue",
       psize = 1.5,          # Point size
       cex = 1.2,            # Text size for labels in the plot
       cex.lab = 1.2,        # Axis label text size
       cex.axis = 1.1)       # Axis numbering size

# Add titles and additional labels with adjusted sizes
title(main = "Forest Plot of Cohen's d Effect Sizes (SI_Control Stressor)",
      sub = "Random Effects Meta-Analysis Model",
      cex.main = 1.3,       # Title size
      cex.sub = 1.2)        # Subtitle size

# Print meta-analysis summary
print(meta_analysis)


########RS_first saline injection#####
# Install and load the metafor package if not already installed
if(!require(metafor)) install.packages("metafor", dependencies = TRUE)
library(metafor)

# Create the dataset with the new values
data <- data.frame(
  sex = c("Female", "Female", "Female", "Male", "Male"),
  stressor = c("RS", "RS", "RS", "RS", "RS"),
  readout = c("PPT-MF", "PPT-GS", "PWT", "PPT-MF", "PWT"),
  p_value_U = c(0.023308, 0.606318, 0.151275, 0.911552, 0.652185),
  U = c(26, 52, 38, 58.5, 53),
  d = c(1.103, 0.24, 0.664, 0.056, 0.211),
  n_PD85 = c(11, 11, 11, 11, 11),
  n_PD86 = c(11, 11, 11, 11, 11),
  SE = c(0.457677164, 0.427933725, 0.437993773, 0.426484999, 0.427586275),
  lower_d = c(0.233413389, -0.573074077, -0.168188169, -0.754321498, -0.601413923),
  upper_d = c(2.000047241, 1.0787501, 1.522467796, 0.891910598, 1.049069099)
)

# Print the dataset
print(data)

# Remove rows with missing data (e.g., d or SE)
data <- subset(data, !is.na(d) & !is.na(SE))

# Calculate variance from SE
data$variance <- data$SE^2

# Perform the meta-analysis using a random-effects model
meta_analysis <- rma(yi = d, vi = data$variance, data = data, method = "REML")

# Create the forest plot
forest(meta_analysis,
       slab = paste(data$sex, data$stressor, data$readout, sep = " - "),
       xlab = "Effect Size (Cohen's d)",
       xlim = c(-1.5, 2.5),  # Adjust range to fit the data
       at = seq(-1, 2, 0.5), # Ticks for x-axis
       refline = 0,          # Reference line at d = 0
       col = "blue",
       psize = 1.5,          # Point size for effect size estimates
       cex = 1.2,            # Font size for labels
       cex.lab = 1.2,        # Font size for axis labels
       cex.axis = 1.1)       # Font size for axis numbering

# Add titles to the plot
title(main = "Forest Plot of Cohen's d Effect Sizes (RS Stressor)",
      sub = "Random Effects Meta-Analysis Model",
      cex.main = 1.3,       # Title font size
      cex.sub = 1.2)        # Subtitle font size

# Print the meta-analysis summary
print(meta_analysis)



######RS_control saline injection#####

# Install and load the metafor package if not already installed
if(!require(metafor)) install.packages("metafor", dependencies = TRUE)
library(metafor)

# Create the dataset
data <- data.frame(
  sex = c("Female", "Female", "Female", "Male", "Male"),
  stressor = c("RS_Control", "RS_Control", "RS_Control", "RS_Control", "RS_Control"),
  readout = c("PPT-MF", "PPT-GS", "PWT", "PPT-MF", "PWT"),
  p_value_U = c(0.088734, 0.798745, 0.442833, 0.588745, 0.132035),
  U = c(42, 67, 58, 14, 8),
  d = c(0.756, 0.118, 0.335, 0.376, 1.043),
  n_PD85 = c(12, 12, 12, 6, 6),
  n_PD86 = c(12, 12, 12, 6, 6),
  SE = c(0.422579776, 0.408603414, 0.411101797, 0.582429395, 0.615353862),
  lower_d = c(-0.07225636, -0.682862692, -0.470759522, -0.765561614, -0.163093569),
  upper_d = c(1.58425636, 0.918862692, 1.140759522, 1.517561614, 2.249093569)
)

# Print the dataset
print(data)

# Remove rows with missing data (e.g., d or SE)
data <- subset(data, !is.na(d) & !is.na(SE))

# Calculate variance from SE
data$variance <- data$SE^2

# Perform the meta-analysis using a random-effects model
meta_analysis <- rma(yi = d, vi = data$variance, data = data, method = "REML")

# Create the forest plot
forest(meta_analysis,
       slab = paste(data$sex, data$stressor, data$readout, sep = " - "),
       xlab = "Effect Size (Cohen's d)",
       xlim = c(-1.5, 3),    # Adjust range to fit the data
       at = seq(-1, 2, 0.5), # Ticks for x-axis
       refline = 0,          # Reference line at d = 0
       col = "blue",
       psize = 1.5,          # Point size for effect size estimates
       cex = 1.2,            # Font size for labels
       cex.lab = 1.2,        # Font size for axis labels
       cex.axis = 1.1)       # Font size for axis numbering

# Add titles to the plot
title(main = "Forest Plot of Cohen's d Effect Sizes (RS_Control Stressor)",
      sub = "Random Effects Meta-Analysis Model",
      cex.main = 1.3,       # Title font size
      cex.sub = 1.2)        # Subtitle font size

# Print the meta-analysis summary
print(meta_analysis)


######secondinjection_NGF_SI#######

# Install and load the metafor package if not already installed
if(!require(metafor)) install.packages("metafor", dependencies = TRUE)
library(metafor)

# Create the dataset
data <- data.frame(
  sex = c("Female", "Female", "Female", "Male", "Male", "Male"),
  stressor = c("SI", "SI", "SI", "SI", "SI", "SI"),
  readout = c("PPT-MF", "PPT-GS", "PWT", "PPT-MF", "PPT-GS", "PWT"),
  p_value_U = c(0.588745, 0.064935, 0.484848, 0.393939, 0.309524, 0.24026),
  U = c(14, 6, 13, 12, 11, 10),
  d = c(0.376, 1.333, 0.475, 0.577, 0.684, 0.796),
  n_PD90 = c(6, 6, 6, 6, 6, 6),
  n_PD91 = c(6, 6, 6, 6, 6, 6),
  SE = c(0.582429395, 0.638255729, 0.585435201, 0.589241355, 0.593992705, 0.599778292),
  lower_d = c(-0.73061585, 0.120314116, -0.637326883, -0.542558575, -0.444586139, -0.343578756),
  upper_d = c(1.517561614, 2.583981228, 1.622452995, 1.731913057, 1.848225701, 1.971565453)
)

# Print the dataset
print(data)

# Remove rows with missing data (e.g., d or SE)
data <- subset(data, !is.na(d) & !is.na(SE))

# Calculate variance from SE
data$variance <- data$SE^2

# Perform the meta-analysis using a random-effects model
meta_analysis <- rma(yi = d, vi = data$variance, data = data, method = "REML")

# Create the forest plot
forest(meta_analysis,
       slab = paste(data$sex, data$stressor, data$readout, sep = " - "),
       xlab = "Effect Size (Cohen's d)",
       xlim = c(-1.5, 3),    # Adjust range to fit the data
       at = seq(-1, 3, 0.5), # Ticks for x-axis
       refline = 0,          # Reference line at d = 0
       col = "blue",
       psize = 1.5,          # Point size for effect size estimates
       cex = 1.2,            # Font size for labels
       cex.lab = 1.2,        # Font size for axis labels
       cex.axis = 1.1)       # Font size for axis numbering

# Add titles to the plot
title(main = "Forest Plot of Cohen's d Effect Sizes (SI Stressor)",
      sub = "Random Effects Meta-Analysis Model",
      cex.main = 1.3,       # Title font size
      cex.sub = 1.2)        # Subtitle font size

# Print the meta-analysis summary
print(meta_analysis)


######second_NGF_SI_controls####
# Install and load the metafor package if not already installed
if(!require(metafor)) install.packages("metafor", dependencies = TRUE)
library(metafor)

# Create the dataset
data <- data.frame(
  sex = c("Female", "Female", "Female", "Male", "Male", "Male"),
  stressor = c("SI_Control", "SI_Control", "SI_Control", "SI_Control", "SI_Control", "SI_Control"),
  readout = c("PPT-MF", "PPT-GS", "PWT", "PPT-MF", "PPT-GS", "PWT"),
  p_value_U = c(0.588745, 0.818182, 0.818182, 0.064935, 0.999999, 0.393939),
  U = c(14, 16, 16, 6, 18, 12),
  d = c(0.376, 0.186, 0.186, 1.333, 0, 0.577),
  n_PD90 = c(6, 6, 6, 6, 6, 6),
  n_PD91 = c(6, 6, 6, 6, 6, 6),
  SE = c(0.582429395, 0.578597298, 0.578597298, 0.638255729, 0.577350269, 0.589241355),
  lower_d = c(-0.765561614, -0.948050704, -0.948050704, 0.082018772, -1.131606528, -0.577913057),
  upper_d = c(1.517561614, 1.320050704, 1.320050704, 2.583981228, 1.131606528, 1.731913057)
)

# Print the dataset
print(data)

# Remove rows with missing data (e.g., d or SE)
data <- subset(data, !is.na(d) & !is.na(SE))

# Calculate variance from SE
data$variance <- data$SE^2

# Perform the meta-analysis using a random-effects model
meta_analysis <- rma(yi = d, vi = data$variance, data = data, method = "REML")

# Create the forest plot
forest(meta_analysis,
       slab = paste(data$sex, data$stressor, data$readout, sep = " - "),
       xlab = "Effect Size (Cohen's d)",
       xlim = c(-2, 3),    # Adjust range to fit the data
       at = seq(-1, 3, 1), # Ticks for x-axis
       refline = 0,        # Reference line at d = 0
       col = "blue",
       psize = 1.5,        # Point size for effect size estimates
       cex = 1.2,          # Font size for labels
       cex.lab = 1.2,      # Font size for axis labels
       cex.axis = 1.1)     # Font size for axis numbering

# Add titles to the plot
title(main = "Forest Plot of Cohen's d Effect Sizes (SI_Control Stressor)",
      sub = "Random Effects Meta-Analysis Model",
      cex.main = 1.3,       # Title font size
      cex.sub = 1.2)        # Subtitle font size

# Print the meta-analysis summary
print(meta_analysis)


######Second_NGF_RS########
# Install and load the metafor package if not already installed
if(!require(metafor)) install.packages("metafor", dependencies = TRUE)
library(metafor)

# Create the dataset
data <- data.frame(
  sex = c("Female", "Female", "Female", "Male", "Male", "Male"),
  stressor = c("RS", "RS", "RS", "RS", "RS", "RS"),
  readout = c("PPT-MF", "PPT-GS", "PWT", "PPT-MF", "PPT-GS", "PWT"),
  p_value_U = c(0.547619, 0.84127, 0.81427, 0.309524, NA, 0.420635),
  U = c(9, 11, 11, 7, NA, 8),
  d = c(0.475, 0.199, 0.199, 0.78, NA, NA),
  n_PD90 = c(5, 5, 5, 5, 5, 5),
  n_PD91 = c(5, 5, 5, 5, 5, 5),
  SE = c(0.641312131, 0.634018967, NA, 0.78, NA, 0.623),
  lower_d = c(-0.74349305, -1.005636037, NA, -0.702, NA, -1.1837),
  upper_d = c(1.731971778, 1.441677175, NA, 2.3088, NA, 1.22108)
)

# Print the dataset
print(data)

# Remove rows with missing data (e.g., d or SE)
data <- subset(data, !is.na(d) & !is.na(SE))

# Calculate variance from SE
data$variance <- data$SE^2

# Perform the meta-analysis using a random-effects model
meta_analysis <- rma(yi = d, vi = data$variance, data = data, method = "REML")

# Create the forest plot
forest(meta_analysis,
       slab = paste(data$sex, data$stressor, data$readout, sep = " - "),
       xlab = "Effect Size (Cohen's d)",
       xlim = c(-2, 3),    # Adjust range to fit the data
       at = seq(-1, 3, 1), # Ticks for x-axis
       refline = 0,        # Reference line at d = 0
       col = "blue",
       psize = 1.5,        # Point size for effect size estimates
       cex = 1.2,          # Font size for labels
       cex.lab = 1.2,      # Font size for axis labels
       cex.axis = 1.1)     # Font size for axis numbering

# Add titles to the plot
title(main = "Forest Plot of Cohen's d Effect Sizes (RS Stressor)",
      sub = "Random Effects Meta-Analysis Model",
      cex.main = 1.3,       # Title font size
      cex.sub = 1.2)        # Subtitle font size

# Print the meta-analysis summary
print(meta_analysis)


######Second_NGF_RS_controls#####

# Install and load the metafor package if not already installed
if(!require(metafor)) install.packages("metafor", dependencies = TRUE)
library(metafor)

# Create the dataset
data <- data.frame(
  sex = c("Female", "Female", "Female", "Male", "Male", "Male"),
  stressor = c("RS_Control", "RS_Control", "RS_Control", "RS_Control", "RS_Control", "RS_Control"),
  readout = c("PPT-MF", "PPT-GS", "PWT", "PPT-MF", "PPT-GS", "PWT"),
  p_value_U = c(0.937229, 0.064935, 0.588745, 0.393939, NA, 0.818182),
  U = c(17, 6, 14, 12, NA, 16),
  d = c(0.093, 1.333, 0.376, 0.577, NA, 0.186),
  n_PD90 = c(6, 6, 6, 6, 6, 6),
  n_PD91 = c(6, 6, 6, 6, 6, 6),
  SE = c(0.577662279, 0.638255729, 0.582429395, 0.589241355, NA, 0.578597298),
  lower_d = c(-1.039218066, 0.082018772, -0.765561614, -0.577913057, NA, -0.948050704),
  upper_d = c(1.225218066, 2.583981228, 1.517561614, 1.731913057, NA, 1.320050704)
)

# Print the dataset
print(data)

# Remove rows with missing data (e.g., d or SE)
data <- subset(data, !is.na(d) & !is.na(SE))

# Calculate variance from SE
data$variance <- data$SE^2

# Perform the meta-analysis using a random-effects model
meta_analysis <- rma(yi = d, vi = data$variance, data = data, method = "REML")

# Create the forest plot
forest(meta_analysis,
       slab = paste(data$sex, data$stressor, data$readout, sep = " - "),
       xlab = "Effect Size (Cohen's d)",
       xlim = c(-2, 3),    # Adjust range to fit the data
       at = seq(-1, 3, 1), # Ticks for x-axis
       refline = 0,        # Reference line at d = 0
       col = "blue",
       psize = 1.5,        # Point size for effect size estimates
       cex = 1.2,          # Font size for labels
       cex.lab = 1.2,      # Font size for axis labels
       cex.axis = 1.1)     # Font size for axis numbering

# Add titles to the plot
title(main = "Forest Plot of Cohen's d Effect Sizes (RS_Control Stressor)",
      sub = "Random Effects Meta-Analysis Model",
      cex.main = 1.3,       # Title font size
      cex.sub = 1.2)        # Subtitle font size

# Print the meta-analysis summary
print(meta_analysis)



#####SI_femles_NGF##3
# Install and load the metafor package if not already installed
if(!require(metafor)) install.packages("metafor", dependencies = TRUE)
library(metafor)

# Create the dataset
data <- data.frame(
  sex = c("Female", "Female", "Female"),
  stressor = c("SI", "SI", "SI"),
  readout = c("PPT-MF", "PPT-GS", "PWT"),
  p_value_U = c(0.588745, 0.064935, 0.484848),
  U = c(14, 6, 13),
  d = c(-0.376, -1.333, -0.475),
  n_PD90 = c(6, 6, 6),
  n_PD91 = c(6, 6, 6),
  SE = c(0.582429395, 0.638255729, 0.585435201),
  lower_d = c(-0.73061585, 0.120314116, -0.637326883),
  upper_d = c(1.517561614, 2.583981228, 1.622452995)
)

# Print the dataset
print(data)

# Calculate variance from SE
data$variance <- data$SE^2

# Perform the meta-analysis using a random-effects model
meta_analysis <- rma(yi = d, vi = data$variance, data = data, method = "REML")

# Create the forest plot
forest(meta_analysis,
       slab = paste(data$sex, data$stressor, data$readout, sep = " - "),
       xlab = "Effect Size (Cohen's d)",
       xlim = c(-5, 4),    # Adjust range to fit the data
       at = seq(-1, 3, 1), # Ticks for x-axis
       refline = 0,        # Reference line at d = 0
       col = "blue",
       psize = 1.5,        # Point size for effect size estimates
       cex = 1.2,          # Font size for labels
       cex.lab = 1.2,      # Font size for axis labels
       cex.axis = 1.1)     # Font size for axis numbering

# Add titles to the plot
title(main = "Forest Plot of Cohen's d Effect Sizes (SI Stressor)",
      sub = "Random Effects Meta-Analysis Model",
      cex.main = 1.3,       # Title font size
      cex.sub = 1.2)        # Subtitle font size

# Print the meta-analysis summary
print(meta_analysis)


####Males_NGF_SI####

# Install and load the metafor package if not already installed
if(!require(metafor)) install.packages("metafor", dependencies = TRUE)
library(metafor)

# Create the dataset
data <- data.frame(
  sex = c("Male", "Male", "Male"),
  stressor = c("SI", "SI", "SI"),
  readout = c("PPT-MF", "PPT-GS", "PWT"),
  p_value_U = c(0.393939, 0.309524, 0.24026),
  U = c(12, 11, 10),
  d = c(0.577, 0.684, -0.796),
  n_PD90 = c(6, 6, 6),
  n_PD91 = c(6, 6, 6),
  SE = c(0.589241355, 0.593992705, 0.599778292),
  lower_d = c(-0.542558575, -0.444586139, -0.343578756),
  upper_d = c(1.731913057, 1.848225701, 1.971565453)
)

# Print the dataset
print(data)

# Calculate variance from SE (variance = SE^2)
data$variance <- data$SE^2

# Perform the meta-analysis using random-effects model
meta_analysis <- rma(yi = d, vi = data$variance, data = data, method = "REML")

# Create the forest plot
forest(meta_analysis,
       slab = paste(data$sex, data$stressor, data$readout, sep = " - "),
       xlab = "Effect Size (Cohen's d)",
       xlim = c(-5, 4),    # Adjust range to fit the data
       at = seq(-1, 3, 1), # Ticks for x-axis
       refline = 0,        # Reference line at d = 0
       col = "blue",
       psize = 1.5,        # Point size for effect size estimates
       cex = 1.2,          # Font size for labels
       cex.lab = 1.2,      # Font size for axis labels
       cex.axis = 1.1)     # Font size for axis numbering

# Add titles to the plot
title(main = "Forest Plot of Cohen's d Effect Sizes (SI Stressor)",
      sub = "Random Effects Meta-Analysis Model",
      cex.main = 1.3,       # Title font size
      cex.sub = 1.2)        # Subtitle font size

# Print the meta-analysis summary
print(meta_analysis)


####SI_control_females_NGF###
# Install and load the metafor package if not already installed
if(!require(metafor)) install.packages("metafor", dependencies = TRUE)
library(metafor)

# Create the dataset
data <- data.frame(
  sex = c("Female", "Female", "Female"),
  stressor = c("SI_Control", "SI_Control", "SI_Control"),
  readout = c("PPT-MF", "PPT-GS", "PWT"),
  p_value_U = c(0.588745, 0.818182, 0.818182),
  U = c(14, 16, 16),
  d = c(0.376, 0.186, 0.186),
  n_PD90 = c(6, 6, 6),
  n_PD91 = c(6, 6, 6),
  SE = c(0.582429395, 0.578597298, 0.578597298),
  lower_d = c(-0.765561614, -0.948050704, -0.948050704),
  upper_d = c(1.517561614, 1.320050704, 1.320050704)
)

# Print the dataset to verify
print(data)

# Calculate variance from SE (variance = SE^2)
data$variance <- data$SE^2

# Perform the meta-analysis using random-effects model
meta_analysis <- rma(yi = d, vi = data$variance, data = data, method = "REML")

# Create the forest plot
forest(meta_analysis,
       slab = paste(data$sex, data$stressor, data$readout, sep = " - "),
       xlab = "Effect Size (Cohen's d)",
       xlim = c(-5, 4),    # Adjust range to fit the data
       at = seq(-1, 3, 1), # Ticks for x-axis
       refline = 0,        # Reference line at d = 0
       col = "blue",
       psize = 1.5,        # Point size for effect size estimates
       cex = 1.2,          # Font size for labels
       cex.lab = 1.2,      # Font size for axis labels
       cex.axis = 1.1)     # Font size for axis numbering

# Add titles to the plot
title(main = "Forest Plot of Cohen's d Effect Sizes (SI_Control Stressor)",
      sub = "Random Effects Meta-Analysis Model",
      cex.main = 1.3,       # Title font size
      cex.sub = 1.2)        # Subtitle font size

# Print the meta-analysis summary
print(meta_analysis)


####Males_Controls_SI_NGF###3
# Install and load the metafor package if not already installed
if(!require(metafor)) install.packages("metafor", dependencies = TRUE)
library(metafor)

# Create the dataset
data <- data.frame(
  sex = c("Male", "Male", "Male"),
  stressor = c("SI_Control", "SI_Control", "SI_Control"),
  readout = c("PPT-MF", "PPT-GS", "PWT"),
  p_value_U = c(0.064935, 0.999999, 0.393939),
  U = c(6, 18, 12),
  d = c(-1.333, 0, -0.577),
  n_PD90 = c(6, 6, 6),
  n_PD91 = c(6, 6, 6),
  SE = c(0.638255729, 0.577350269, 0.589241355),
  lower_d = c(0.082018772, -1.131606528, -0.577913057),
  upper_d = c(2.583981228, 1.131606528, 1.731913057)
)

# Print the dataset to verify
print(data)

# Calculate variance from SE (variance = SE^2)
data$variance <- data$SE^2

# Perform the meta-analysis using random-effects model
meta_analysis <- rma(yi = d, vi = data$variance, data = data, method = "REML")

# Create the forest plot
forest(meta_analysis,
       slab = paste(data$sex, data$stressor, data$readout, sep = " - "),
       xlab = "Effect Size (Cohen's d)",
       xlim = c(-5, 4),    # Adjust range to fit the data
       at = seq(-1, 3, 1), # Ticks for x-axis
       refline = 0,        # Reference line at d = 0
       col = "blue",
       psize = 1.5,        # Point size for effect size estimates
       cex = 1.2,          # Font size for labels
       cex.lab = 1.2,      # Font size for axis labels
       cex.axis = 1.1)     # Font size for axis numbering

# Add titles to the plot
title(main = "Forest Plot of Cohen's d Effect Sizes (SI_Control Stressor)",
      sub = "Random Effects Meta-Analysis Model",
      cex.main = 1.3,       # Title font size
      cex.sub = 1.2)        # Subtitle font size

# Print the meta-analysis summary
print(meta_analysis)


#####RS_female_controls_NGF####
# Install and load the metafor package if not already installed
if (!require(metafor)) install.packages("metafor", dependencies = TRUE)
library(metafor)

# Create the dataset
data <- data.frame(
  sex = c("Female", "Female", "Female"),
  stressor = c("RS_Control", "RS_Control", "RS_Control"),
  readout = c("PPT-MF", "PPT-GS", "PWT"),
  p_value_U = c(0.937229, 0.064935, 0.588745),
  U = c(17, 6, 14),
  d = c(0.093, 1.333, 0.376),
  n_PD90 = c(6, 6, 6),
  n_PD91 = c(6, 6, 6),
  SE = c(0.577662279, 0.638255729, 0.582429395),
  lower_d = c(-1.039218066, 0.082018772, -0.765561614),
  upper_d = c(1.225218066, 2.583981228, 1.517561614)
)

# Print the dataset to verify
print(data)

# Calculate variance from SE (variance = SE^2)
data$variance <- data$SE^2

# Perform the meta-analysis using random-effects model
meta_analysis <- rma(yi = d, vi = data$variance, data = data, method = "REML")

# Create the forest plot
forest(meta_analysis,
       slab = paste(data$sex, data$stressor, data$readout, sep = " - "),
       xlab = "Effect Size (Cohen's d)",
       xlim = c(-5, 4),    # Adjust range to fit the data
       at = seq(-1, 3, 1), # Ticks for x-axis
       refline = 0,        # Reference line at d = 0
       col = "blue",
       psize = 1.5,        # Point size for effect size estimates
       cex = 1.2,          # Font size for labels
       cex.lab = 1.2,      # Font size for axis labels
       cex.axis = 1.1)     # Font size for axis numbering

# Add titles to the plot
title(main = "Forest Plot of Cohen's d Effect Sizes (RS_Control Stressor)",
      sub = "Random Effects Meta-Analysis Model",
      cex.main = 1.3,       # Title font size
      cex.sub = 1.2)        # Subtitle font size

# Print the meta-analysis summary
print(meta_analysis)

#######RS_femlaes_NGF###
# Install and load the metafor package if not already installed
if (!require(metafor)) install.packages("metafor", dependencies = TRUE)
library(metafor)

# Create the dataset
data <- data.frame(
  sex = c("Female", "Female", "Female"),
  stressor = c("RS", "RS", "RS"),
  readout = c("PPT-MF", "PPT-GS", "PWT"),
  p_value_U = c(0.547619, 0.84127, 0.81427),
  U = c(9, 11, 11),
  d = c(-0.475, -0.199, 0.199),
  n_PD90 = c(5, 5, 5),
  n_PD91 = c(5, 5, 5),
  SE = c(0.641312131, 0.634018967, 0.199),
  lower_d = c(-0.74349305, -1.005636037, 0.199),
  upper_d = c(1.731971778, 1.441677175, 0.199)
)

# Print the dataset to verify
print(data)

# Calculate variance from SE (variance = SE^2)
data$variance <- data$SE^2

# Perform the meta-analysis using random-effects model
meta_analysis <- rma(yi = d, vi = data$variance, data = data, method = "REML")

# Create the forest plot
forest(meta_analysis,
       slab = paste(data$sex, data$stressor, data$readout, sep = " - "),
       xlab = "Effect Size (Cohen's d)",
       xlim = c(-5, 4),    # Adjust range to fit the data
       at = seq(-1, 3, 1), # Ticks for x-axis
       refline = 0,        # Reference line at d = 0
       col = "blue",
       psize = 1.5,        # Point size for effect size estimates
       cex = 1.2,          # Font size for labels
       cex.lab = 1.2,      # Font size for axis labels
       cex.axis = 1.1)     # Font size for axis numbering

# Add titles to the plot
title(main = "Forest Plot of Cohen's d Effect Sizes (RS Stressor)",
      sub = "Random Effects Meta-Analysis Model",
      cex.main = 1.3,       # Title font size
      cex.sub = 1.2)        # Subtitle font size

# Print the meta-analysis summary
print(meta_analysis)


####Males_controls_RS_NGF###
# Install and load the metafor package if not already installed
if (!require(metafor)) install.packages("metafor", dependencies = TRUE)
library(metafor)

# Create the dataset
data <- data.frame(
  sex = c("Male", "Male", "Male"),
  stressor = c("RS_Control", "RS_Control", "RS_Control"),
  readout = c("PPT-MF", "PPT-GS", "PWT"),
  p_value_U = c(0.393939, NA, 0.818182),  # NA used for missing values
  U = c(12, NA, 16),
  d = c(0.577, NA, 0.186),  # NA used for missing values
  n_PD90 = c(6, 6, 6),
  n_PD91 = c(6, 6, 6),
  SE = c(0.589241355, NA, 0.578597298),
  lower_d = c(-0.577913057, NA, -0.948050704),
  upper_d = c(1.731913057, NA, 1.320050704)
)

# Print the dataset to verify
print(data)

# Handle missing values: remove rows with NA in critical columns
data <- na.omit(data)

# Calculate variance from SE (variance = SE^2)
data$variance <- data$SE^2

# Perform the meta-analysis using random-effects model
meta_analysis <- rma(yi = d, vi = data$variance, data = data, method = "REML")

# Create the forest plot
forest(meta_analysis,
       slab = paste(data$sex, data$stressor, data$readout, sep = " - "),
       xlab = "Effect Size (Cohen's d)",
       xlim = c(-5, 4),    # Adjust range to fit the data
       at = seq(-1, 3, 1), # Ticks for x-axis
       refline = 0,        # Reference line at d = 0
       col = "blue",
       psize = 1.5,        # Point size for effect size estimates
       cex = 1.2,          # Font size for labels
       cex.lab = 1.2,      # Font size for axis labels
       cex.axis = 1.1)     # Font size for axis numbering

# Add titles to the plot
title(main = "Forest Plot of Cohen's d Effect Sizes (RS_Control Stressor - Male)",
      sub = "Random Effects Meta-Analysis Model",
      cex.main = 1.3,       # Title font size
      cex.sub = 1.2)        # Subtitle font size

# Print the meta-analysis summary
print(meta_analysis)


####Males_NGF_RS###
# Install and load the metafor package if not already installed
if (!require(metafor)) install.packages("metafor", dependencies = TRUE)
library(metafor)

# Create the dataset
data <- data.frame(
  sex = c("Male", "Male", "Male"),
  stressor = c("RS", "RS", "RS"),
  readout = c("PPT-MF", "PPT-GS", "PWT"),
  p_value_U = c(0.309524, NA, 0.420635),
  U = c(7, NA, 8),
  d = c(0.78, NA, 0.623),
  n_PD90 = c(5, 5, 5),
  n_PD91 = c(5, 5, 5),
  SE = c(0.78, NA, 0.623),
  lower_d = c(-0.702, NA, -0.5607),
  upper_d = c(2.3088, NA, 1.84408)
)

# Print the dataset to verify
print(data)

# Calculate variance from SE (variance = SE^2)
data$variance <- data$SE^2

# Remove rows with missing effect sizes (d) or variances to ensure compatibility
cleaned_data <- na.omit(data[, c("d", "variance", "sex", "stressor", "readout")])

# Perform the meta-analysis using random-effects model
meta_analysis <- rma(yi = cleaned_data$d, vi = cleaned_data$variance, method = "REML")

# Create the forest plot
forest(meta_analysis,
       slab = paste(cleaned_data$sex, cleaned_data$stressor, cleaned_data$readout, sep = " - "),
       xlab = "Effect Size (Cohen's d)",
       xlim = c(-5, 4),    # Adjust range to fit the data
       at = seq(-1, 3, 1), # Ticks for x-axis
       refline = 0,        # Reference line at d = 0
       col = "blue",
       psize = 1.5,        # Point size for effect size estimates
       cex = 1.2,          # Font size for labels
       cex.lab = 1.2,      # Font size for axis labels
       cex.axis = 1.1)     # Font size for axis numbering

# Add titles to the plot
title(main = "Forest Plot of Cohen's d Effect Sizes (RS Stressor)",
      sub = "Random Effects Meta-Analysis Model",
      cex.main = 1.3,       # Title font size
      cex.sub = 1.2)        # Subtitle font size

# Print the meta-analysis summary
print(meta_analysis)

#####SI_females_salineinjection###
# Install and load the metafor package if not already installed
if (!require(metafor)) install.packages("metafor", dependencies = TRUE)
library(metafor)

# Create the dataset
data <- data.frame(
  sex = c("Female", "Female", "Female"),
  stressor = c("SI", "SI", "SI"),
  readout = c("PPT-MF", "PPT-GS", "PWT"),
  p_value_U = c(0.5589876, 0.842836, 0.712535),
  U = c(62, 68, 65),
  d = c(-0.237, -0.094, -0.166),
  n_PD85 = c(12, 12, 12),
  n_PD86 = c(12, 12, 12),
  SE = c(0.409678965, 0.408473683, 0.408950792),
  lower_d = c(-0.541390033, -0.682099998, -0.611006504),
  upper_d = c(1.039970771, 0.894608419, 0.967543552)
)

# Print the dataset to verify
print(data)

# Calculate variance from SE (variance = SE^2)
data$variance <- data$SE^2

# Remove rows with missing effect sizes (d) or variances to ensure compatibility
cleaned_data <- na.omit(data[, c("d", "variance", "sex", "stressor", "readout")])

# Perform the meta-analysis using random-effects model
meta_analysis <- rma(yi = cleaned_data$d, vi = cleaned_data$variance, method = "REML")

# Create the forest plot
forest(meta_analysis,
       slab = paste(cleaned_data$sex, cleaned_data$stressor, cleaned_data$readout, sep = " - "),
       xlab = "Effect Size (Cohen's d)",
       xlim = c(-5, 4),    # Adjust range to fit the data
       at = seq(-1, 3, 1), # Ticks for x-axis
       refline = 0,        # Reference line at d = 0
       col = "blue",
       psize = 1.5,        # Point size for effect size estimates
       cex = 1.2,          # Font size for labels
       cex.lab = 1.2,      # Font size for axis labels
       cex.axis = 1.1)     # Font size for axis numbering

# Add titles to the plot
title(main = "Forest Plot of Cohen's d Effect Sizes (SI Stressor)",
      sub = "Random Effects Meta-Analysis Model",
      cex.main = 1.3,       # Title font size
      cex.sub = 1.2)        # Subtitle font size

# Print the meta-analysis summary
print(meta_analysis)

####SI_controls_salineinjection##3
# Install and load the metafor package if not already installed
if (!require(metafor)) install.packages("metafor", dependencies = TRUE)
library(metafor)

# Create the dataset
data <- data.frame(
  sex = c("Female", "Female", "Female"),
  stressor = c("SI_Control", "SI_Control", "SI_Control"),
  readout = c("PPT-MF", "PPT-GS", "PWT"),
  p_value_U = c(0.842836, 0.347358, 0.786983),
  U = c(68, 55, 67),
  d = c(-0.094, -0.409, 0.118),
  n_PD85 = c(12, 12, 12),
  n_PD86 = c(12, 12, 12),
  SE = c(0.408473683, 0.41249447, 0.408603414),
  lower_d = c(-0.706608419, -0.399489161, -0.682862692),
  upper_d = c(0.894608419, 1.217489161, 0.918862692)
)

# Print the dataset to verify
print(data)

# Calculate variance from SE (variance = SE^2)
data$variance <- data$SE^2

# Remove rows with missing effect sizes (d) or variances to ensure compatibility
cleaned_data <- na.omit(data[, c("d", "variance", "sex", "stressor", "readout")])

# Perform the meta-analysis using random-effects model
meta_analysis <- rma(yi = cleaned_data$d, vi = cleaned_data$variance, method = "REML")

# Create the forest plot
forest(meta_analysis,
       slab = paste(cleaned_data$sex, cleaned_data$stressor, cleaned_data$readout, sep = " - "),
       xlab = "Effect Size (Cohen's d)",
       xlim = c(-5, 4),    # Adjust range to fit the data
       at = seq(-1, 3, 1), # Ticks for x-axis
       refline = 0,        # Reference line at d = 0
       col = "blue",
       psize = 1.5,        # Point size for effect size estimates
       cex = 1.2,          # Font size for labels
       cex.lab = 1.2,      # Font size for axis labels
       cex.axis = 1.1)     # Font size for axis numbering

# Add titles to the plot
title(main = "Forest Plot of Cohen's d Effect Sizes (SI_Control Stressor)",
      sub = "Random Effects Meta-Analysis Model",
      cex.main = 1.3,       # Title font size
      cex.sub = 1.2)        # Subtitle font size

# Print the meta-analysis summary
print(meta_analysis)

####Males_SI_salineinj##
# Install and load the metafor package if not already installed
if (!require(metafor)) install.packages("metafor", dependencies = TRUE)
library(metafor)

# Create the dataset
data <- data.frame(
  sex = c("Male", "Male", "Male"),
  stressor = c("SI", "SI", "SI"),
  readout = c("PPT-MF", "PPT-GS", "PWT"),
  p_value_U = c(0.513723, 0.755285, 0.218921),
  U = c(60, 66, 50),
  d = c(-0.286, 0.142, -0.537),
  n_PD85 = c(12, 12, 12),
  n_PD86 = c(12, 12, 12),
  SE = c(0.41033005, 0.408762462, 0.415541038),
  lower_d = c(-0.493627095, -0.634648677, -0.252527972),
  upper_d = c(1.090246898, 0.943174425, 1.351460434)
)

# Print the dataset to verify
print(data)

# Calculate variance from SE (variance = SE^2)
data$variance <- data$SE^2

# Remove rows with missing effect sizes (d) or variances to ensure compatibility
cleaned_data <- na.omit(data[, c("d", "variance", "sex", "stressor", "readout")])

# Perform the meta-analysis using random-effects model
meta_analysis <- rma(yi = cleaned_data$d, vi = cleaned_data$variance, method = "REML")

# Create the forest plot
forest(meta_analysis,
       slab = paste(cleaned_data$sex, cleaned_data$stressor, cleaned_data$readout, sep = " - "),
       xlab = "Effect Size (Cohen's d)",
       xlim = c(-5, 4),    # Adjust range to fit the data
       at = seq(-1, 3, 1), # Ticks for x-axis
       refline = 0,        # Reference line at d = 0
       col = "blue",
       psize = 1.5,        # Point size for effect size estimates
       cex = 1.2,          # Font size for labels
       cex.lab = 1.2,      # Font size for axis labels
       cex.axis = 1.1)     # Font size for axis numbering

# Add titles to the plot
title(main = "Forest Plot of Cohen's d Effect Sizes (SI Stressor)",
      sub = "Random Effects Meta-Analysis Model",
      cex.main = 1.3,       # Title font size
      cex.sub = 1.2)        # Subtitle font size

# Print the meta-analysis summary
print(meta_analysis)

###Males_controls_Salineinj##
# Install and load the metafor package if not already installed
if (!require(metafor)) install.packages("metafor", dependencies = TRUE)
library(metafor)

# Create the dataset
data <- data.frame(
  sex = c("Male", "Male", "Male"),
  stressor = c("SI_Control", "SI_Control", "SI_Control"),
  readout = c("PPT-MF", "PPT-GS", "PWT"),
  p_value_U = c(0.347358, 0.966219, 0.318585),
  U = c(55, 71, 54),
  d = c(0.409, 0.024, 0.434),
  n_PD85 = c(12, 12, 12),
  n_PD86 = c(12, 12, 12),
  SE = c(0.41249447, 0.40826299, 0.41302633),
  lower_d = c(-0.399489161, -0.776195455, -0.375531609),
  upper_d = c(1.217489161, 0.824195455, 1.243531609)
)

# Print the dataset to verify
print(data)

# Calculate variance from SE (variance = SE^2)
data$variance <- data$SE^2

# Remove rows with missing effect sizes (d) or variances to ensure compatibility
cleaned_data <- na.omit(data[, c("d", "variance", "sex", "stressor", "readout")])

# Perform the meta-analysis using random-effects model
meta_analysis <- rma(yi = cleaned_data$d, vi = cleaned_data$variance, method = "REML")

# Create the forest plot
forest(meta_analysis,
       slab = paste(cleaned_data$sex, cleaned_data$stressor, cleaned_data$readout, sep = " - "),
       xlab = "Effect Size (Cohen's d)",
       xlim = c(-5, 4),    # Adjust range to fit the data
       at = seq(-1, 3, 1), # Ticks for x-axis
       refline = 0,        # Reference line at d = 0
       col = "blue",
       psize = 1.5,        # Point size for effect size estimates
       cex = 1.2,          # Font size for labels
       cex.lab = 1.2,      # Font size for axis labels
       cex.axis = 1.1)     # Font size for axis numbering

# Add titles to the plot
title(main = "Forest Plot of Cohen's d Effect Sizes (SI_Control Stressor)",
      sub = "Random Effects Meta-Analysis Model",
      cex.main = 1.3,       # Title font size
      cex.sub = 1.2)        # Subtitle font size

# Print the meta-analysis summary
print(meta_analysis)

###Female_RS_salineinj##
# Install and load the metafor package if not already installed
if (!require(metafor)) install.packages("metafor", dependencies = TRUE)
library(metafor)

# Create the dataset
data <- data.frame(
  sex = c("Female", "Female", "Female"),
  stressor = c("RS", "RS", "RS"),
  readout = c("PPT-MF", "PPT-GS", "PWT"),
  p_value_U = c(0.023308, 0.606318, 0.151275),
  U = c(26, 52, 38),
  d = c(-1.103, 0.24, -0.664),
  n_PD85 = c(11, 11, 11),
  n_PD86 = c(11, 11, 11),
  SE = c(0.457677164, 0.427933725, 0.437993773),
  lower_d = c(0.233413389, -0.573074077, -0.168188169),
  upper_d = c(2.000047241, 1.0787501, 1.522467796)
)

# Print the dataset to verify
print(data)

# Calculate variance from SE (variance = SE^2)
data$variance <- data$SE^2

# Remove rows with missing effect sizes (d) or variances to ensure compatibility
cleaned_data <- na.omit(data[, c("d", "variance", "sex", "stressor", "readout")])

# Perform the meta-analysis using random-effects model
meta_analysis <- rma(yi = cleaned_data$d, vi = cleaned_data$variance, method = "REML")

# Create the forest plot
forest(meta_analysis,
       slab = paste(cleaned_data$sex, cleaned_data$stressor, cleaned_data$readout, sep = " - "),
       xlab = "Effect Size (Cohen's d)",
       xlim = c(-5, 4),    # Adjust range to fit the data
       at = seq(-1, 3, 1), # Ticks for x-axis
       refline = 0,        # Reference line at d = 0
       col = "blue",
       psize = 1.5,        # Point size for effect size estimates
       cex = 1.2,          # Font size for labels
       cex.lab = 1.2,      # Font size for axis labels
       cex.axis = 1.1)     # Font size for axis numbering

# Add titles to the plot
title(main = "Forest Plot of Cohen's d Effect Sizes (RS Stressor)",
      sub = "Random Effects Meta-Analysis Model",
      cex.main = 1.3,       # Title font size
      cex.sub = 1.2)        # Subtitle font size

# Print the meta-analysis summary
print(meta_analysis)

###female_controls_RS_salineinj##
# Install and load the metafor package if not already installed
if (!require(metafor)) install.packages("metafor", dependencies = TRUE)
library(metafor)

# Create the dataset
data <- data.frame(
  sex = c("Female", "Female", "Female"),
  stressor = c("RS_Control", "RS_Control", "RS_Control"),
  readout = c("PPT-MF", "PPT-GS", "PWT"),
  p_value_U = c(0.088734, 0.798745, 0.442833),
  U = c(42, 67, 58),
  d = c(0.756, 0.118, -0.335),
  n_PD85 = c(12, 12, 12),
  n_PD86 = c(12, 12, 12),
  SE = c(0.422579776, 0.408603414, 0.411101797),
  lower_d = c(-0.07225636, -0.682862692, -0.470759522),
  upper_d = c(1.58425636, 0.918862692, 1.140759522)
)

# Print the dataset to verify
print(data)

# Calculate variance from SE (variance = SE^2)
data$variance <- data$SE^2

# Remove rows with missing effect sizes (d) or variances to ensure compatibility
cleaned_data <- na.omit(data[, c("d", "variance", "sex", "stressor", "readout")])

# Perform the meta-analysis using random-effects model
meta_analysis <- rma(yi = cleaned_data$d, vi = cleaned_data$variance, method = "REML")

# Create the forest plot
forest(meta_analysis,
       slab = paste(cleaned_data$sex, cleaned_data$stressor, cleaned_data$readout, sep = " - "),
       xlab = "Effect Size (Cohen's d)",
       xlim = c(-5, 4),    # Adjust range to fit the data
       at = seq(-1, 3, 1), # Ticks for x-axis
       refline = 0,        # Reference line at d = 0
       col = "blue",
       psize = 1.5,        # Point size for effect size estimates
       cex = 1.2,          # Font size for labels
       cex.lab = 1.2,      # Font size for axis labels
       cex.axis = 1.1)     # Font size for axis numbering

# Add titles to the plot
title(main = "Forest Plot of Cohen's d Effect Sizes (RS_Control Stressor)",
      sub = "Random Effects Meta-Analysis Model",
      cex.main = 1.3,       # Title font size
      cex.sub = 1.2)        # Subtitle font size

# Print the meta-analysis summary
print(meta_analysis)

####Males_RS_salineinj##
# Install and load the metafor package if not already installed
if (!require(metafor)) install.packages("metafor", dependencies = TRUE)
library(metafor)

# Create the dataset
data <- data.frame(
  sex = c("Male", "Male", "Male"),
  stressor = c("RS", "RS", "RS"),
  readout = c("PPT-MF", "PPT-GS", "PWT"),
  p_value_U = c(0.911552, NA, 0.652185),
  U = c(58.5, NA, 53),
  d = c(-0.056, NA, -0.211),
  n_PD85 = c(11, 11, 11),
  n_PD86 = c(11, 11, 11),
  SE = c(0.426484999, NA, 0.427586275),
  lower_d = c(-0.754321498, NA, -0.601413923),
  upper_d = c(0.891910598, NA, 1.049069099)
)

# Print the dataset to verify
print(data)

# Calculate variance from SE (variance = SE^2) for rows with valid effect sizes
data$variance <- ifelse(!is.na(data$SE), data$SE^2, NA)

# Remove rows with missing effect sizes (d) or variances to ensure compatibility
cleaned_data <- na.omit(data[, c("d", "variance", "sex", "stressor", "readout")])

# Perform the meta-analysis using random-effects model
meta_analysis <- rma(yi = cleaned_data$d, vi = cleaned_data$variance, method = "REML")

# Create the forest plot
forest(meta_analysis,
       slab = paste(cleaned_data$sex, cleaned_data$stressor, cleaned_data$readout, sep = " - "),
       xlab = "Effect Size (Cohen's d)",
       xlim = c(-5, 4),    # Adjust range to fit the data
       at = seq(-1, 3, 1), # Ticks for x-axis
       refline = 0,        # Reference line at d = 0
       col = "blue",
       psize = 1.5,        # Point size for effect size estimates
       cex = 1.2,          # Font size for labels
       cex.lab = 1.2,      # Font size for axis labels
       cex.axis = 1.1)     # Font size for axis numbering

# Add titles to the plot
title(main = "Forest Plot of Cohen's d Effect Sizes (RS Stressor)",
      sub = "Random Effects Meta-Analysis Model",
      cex.main = 1.3,       # Title font size
      cex.sub = 1.2)        # Subtitle font size

# Print the meta-analysis summary
print(meta_analysis)


###Male_Controls_RS##3
# Install and load the metafor package if not already installed
if (!require(metafor)) install.packages("metafor", dependencies = TRUE)
library(metafor)

# Create the dataset
data <- data.frame(
  sex = c("Male", "Male", "Male"),
  stressor = c("RS_Control", "RS_Control", "RS_Control"),
  readout = c("PPT-MF", "PPT-GS", "PWT"),
  p_value_U = c(0.588745, NA, 0.132035),
  U = c(14, NA, 8),
  d = c(0.376, NA, -1.043),
  n_PD85 = c(6, 6, 6),
  n_PD86 = c(6, 6, 6),
  SE = c(0.582429395, NA, 0.615353862),
  lower_d = c(-0.765561614, NA, -0.163093569),
  upper_d = c(1.517561614, NA, 2.249093569)
)

# Print the dataset to verify
print(data)

# Calculate variance from SE (variance = SE^2) for rows with valid effect sizes
data$variance <- ifelse(!is.na(data$SE), data$SE^2, NA)

# Remove rows with missing effect sizes (d) or variances to ensure compatibility
cleaned_data <- na.omit(data[, c("d", "variance", "sex", "stressor", "readout")])

# Perform the meta-analysis using random-effects model
meta_analysis <- rma(yi = cleaned_data$d, vi = cleaned_data$variance, method = "REML")

# Create the forest plot
forest(meta_analysis,
       slab = paste(cleaned_data$sex, cleaned_data$stressor, cleaned_data$readout, sep = " - "),
       xlab = "Effect Size (Cohen's d)",
       xlim = c(-5, 4),    # Adjust range to fit the data
       at = seq(-1, 3, 1), # Ticks for x-axis
       refline = 0,        # Reference line at d = 0
       col = "blue",
       psize = 1.5,        # Point size for effect size estimates
       cex = 1.2,          # Font size for labels
       cex.lab = 1.2,      # Font size for axis labels
       cex.axis = 1.1)     # Font size for axis numbering

# Add titles to the plot
title(main = "Forest Plot of Cohen's d Effect Sizes (RS_Control Stressor)",
      sub = "Random Effects Meta-Analysis Model",
      cex.main = 1.3,       # Title font size
      cex.sub = 1.2)        # Subtitle font size

# Print the meta-analysis summary
print(meta_analysis)

