library(ggplot2)

# Standard rows analysis

standardRows <- read.table(file.choose(), header=TRUE, sep=";")

# Deleting volumes not on curve

standardRows_deletion <- subset(standardRows, volume!=0)
standardRows_deletion <- subset(standardRows_deletion, volume!=50)

# Plotting

ggplot(standardRows_deletion,
       aes(y = absorbance, x = volume)) +
  geom_line(aes(color = experiment)) + 
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE)    # Don't add shaded confidence region


# Calculate coefficient of determination (R-squared) and linear regression model

standardCurve = lm(volume ~ absorbance, data=standardRows_deletion)
summary(standardCurve)$r.squared 
standardCurve_coeffs <- summary(standardCurve)$coefficients


######################################
# Pipetting precision testing analysis

robotData <- read.table(file.choose(), header=TRUE, sep=";")

# Apply the linear regression function from the analysis of standard curve

robotData$volume <- standardCurve_coeffs[1]+standardCurve_coeffs[2]*robotData$absorbance


# Calculate statistical values

aggdata_mean <- aggregate(robotData, by=list(robotData$experiment), 
                    FUN=mean)

aggdata_stdev <- aggregate(robotData, by=list(robotData$experiment), 
                    FUN=sd)

experiment_stats <- data.frame(experiment=unique(robotData$experiment),
                               mean=aggdata_mean$volume,
                               stdev=aggdata_stdev$volume)

experiment_stats$cv <- (experiment_stats$stdev/experiment_stats$mean) * 100


# Select the experiment to plot

experimentSelection <- "150916_1"
robotDataSelection <- subset(robotData, experiment==experimentSelection)


# Plotting

raw_map(robotDataSelection$volume, robotDataSelection$well, plate = 384)

ggplot(robotDataSelection, aes(x=volume)) + 
  geom_histogram(data=subset(robotDataSelection, step == 1), fill = "red", alpha = 0.2) + 
  geom_histogram(data=subset(robotDataSelection, step == 2), fill = "blue", alpha = 0.2) +
  geom_histogram(data=subset(robotDataSelection, step == 3), fill = "green", alpha = 0.2) +
  geom_histogram(data=subset(robotDataSelection, step == 4), fill = "black", alpha = 0.2)


robotDataSelection$step <- as.factor(robotDataSelection$step)

ggplot(robotDataSelection, aes(x=step, y=volume)) + 
  geom_boxplot()

