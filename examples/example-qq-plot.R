set.seed(0)
x <- sample(0:9, 100, rep=TRUE)

### SPSS like

# Standard QQ-plot
qq <- qqnorm_spss(x, 1)
plot(qq, 1)  
ggplot(qq, 1)

qq <- qqnorm_spss(x, 1, standardize=TRUE)
plot(qq, 1)  
ggplot(qq, 1)

# Detrended QQ-plot (plottype=2)
plot(qq, 1, plottype=2)  
ggplot(qq, 1, plottype=2)

### R
qqnorm(x, datax=TRUE)
qqline(x, datax=TRUE)


