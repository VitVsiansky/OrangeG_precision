library(ggplot2)

Robot_data <- read.table(file.choose(), header=TRUE, 	sep=";")

Plate1 <- sort(as.vector(Robot_data[["Plate1"]]))
Plate2 <- sort(as.vector(Robot_data[["Plate2"]]))
Plate3 <- sort(as.vector(Robot_data[["Plate3"]]))

par(mfrow=c(1,1))
plot.default(Plate1, Plate2)
abline(lm(Plate2~Plate1),col="red",lwd=2)
plot.default(Plate1, Plate3)
abline(lm(Plate3~Plate1),col="red",lwd=2)
plot.default(Plate1, Plate3)
abline(lm(Plate3~Plate1),col="red",lwd=2)




plot <- ggplot(Robot_data, aes(x=Robot_data["Plate1"], y=Robot_data["Plate2"])) +
        geom_point(shape=1) +    # Use hollow circles
        geom_smooth(method=lm,   # Add linear regression line
        se=FALSE)    # Don't add shaded confidence region

plot + coord_cartesian(xlim = c(2.5, 3), ylim = c(2.5, 3)) 