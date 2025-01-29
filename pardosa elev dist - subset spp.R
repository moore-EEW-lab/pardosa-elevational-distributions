
sp.dat <- read.csv('cleaned.pards.co.elev.csv')


library(car)
library(MASS)
library(ggplot2)

r.dat <- sp.dat[which(sp.dat$CO.sample.size >=10),]

mean(r.dat$co.max.elev) # mean maximum elevation = 3330.31
median(r.dat$co.max.elev) # medium maximum elevation = 3353
sd(r.dat$co.max.elev) # standard deviation of maximum elevation = 509.9992
range(r.dat$co.max.elev) # range of maximum elevations = 2125 4338

mean(r.dat$co.mid.elev) # mean mid elevation = 2478.293
median(r.dat$co.mid.elev) # median mid elevation = 2491.5
sd(r.dat$co.mid.elev) # standard deviation of mid elevation = 311.666
range(r.dat$co.mid.elev) # range of mid elevations = 1760.5 3036.0

mean(r.dat$co.elev.breadth) # mean maximum elevation = 1704.034
median(r.dat$co.elev.breadth) # medium maximum elevation = 1499
sd(r.dat$co.elev.breadth) # standard deviation of maximum elevation = 663.5368
range(r.dat$co.elev.breadth) # range of maximum elevations = 729 3218

##for each 100 m band, does each species occur in it
band.1000.1100 <- ifelse(r.dat$co.min.elev < 1100 & r.dat$co.max.elev > 1000, 1, 0)
band.1100.1200 <- ifelse(r.dat$co.min.elev < 1200 & r.dat$co.max.elev > 1100, 1, 0)
band.1200.1300 <- ifelse(r.dat$co.min.elev < 1300 & r.dat$co.max.elev > 1200, 1, 0)
band.1300.1400 <- ifelse(r.dat$co.min.elev < 1400 & r.dat$co.max.elev > 1300, 1, 0)
band.1400.1500 <- ifelse(r.dat$co.min.elev < 1500 & r.dat$co.max.elev > 1400, 1, 0)
band.1500.1600 <- ifelse(r.dat$co.min.elev < 1600 & r.dat$co.max.elev > 1500, 1, 0)
band.1600.1700 <- ifelse(r.dat$co.min.elev < 1700 & r.dat$co.max.elev > 1600, 1, 0)
band.1700.1800 <- ifelse(r.dat$co.min.elev < 1800 & r.dat$co.max.elev > 1700, 1, 0)
band.1800.1900 <- ifelse(r.dat$co.min.elev < 1900 & r.dat$co.max.elev > 1800, 1, 0)
band.1900.2000 <- ifelse(r.dat$co.min.elev < 2000 & r.dat$co.max.elev > 1900, 1, 0)
band.2000.2100 <- ifelse(r.dat$co.min.elev < 2100 & r.dat$co.max.elev > 2000, 1, 0)
band.2100.2200 <- ifelse(r.dat$co.min.elev < 2200 & r.dat$co.max.elev > 2100, 1, 0)
band.2200.2300 <- ifelse(r.dat$co.min.elev < 2300 & r.dat$co.max.elev > 2200, 1, 0)
band.2300.2400 <- ifelse(r.dat$co.min.elev < 2400 & r.dat$co.max.elev > 2300, 1, 0)
band.2400.2500 <- ifelse(r.dat$co.min.elev < 2500 & r.dat$co.max.elev > 2400, 1, 0)
band.2500.2600 <- ifelse(r.dat$co.min.elev < 2600 & r.dat$co.max.elev > 2500, 1, 0)
band.2600.2700 <- ifelse(r.dat$co.min.elev < 2700 & r.dat$co.max.elev > 2600, 1, 0)
band.2700.2800 <- ifelse(r.dat$co.min.elev < 2800 & r.dat$co.max.elev > 2700, 1, 0)
band.2800.2900 <- ifelse(r.dat$co.min.elev < 2900 & r.dat$co.max.elev > 2800, 1, 0)
band.2900.3000 <- ifelse(r.dat$co.min.elev < 3000 & r.dat$co.max.elev > 2900, 1, 0)
band.3000.3100 <- ifelse(r.dat$co.min.elev < 3100 & r.dat$co.max.elev > 3000, 1, 0)
band.3100.3200 <- ifelse(r.dat$co.min.elev < 3200 & r.dat$co.max.elev > 3100, 1, 0)
band.3200.3300 <- ifelse(r.dat$co.min.elev < 3300 & r.dat$co.max.elev > 3200, 1, 0)
band.3300.3400 <- ifelse(r.dat$co.min.elev < 3400 & r.dat$co.max.elev > 3300, 1, 0)
band.3400.3500 <- ifelse(r.dat$co.min.elev < 3500 & r.dat$co.max.elev > 3400, 1, 0)
band.3500.3600 <- ifelse(r.dat$co.min.elev < 3600 & r.dat$co.max.elev > 3500, 1, 0)
band.3600.3700 <- ifelse(r.dat$co.min.elev < 3700 & r.dat$co.max.elev > 3600, 1, 0)
band.3700.3800 <- ifelse(r.dat$co.min.elev < 3800 & r.dat$co.max.elev > 3700, 1, 0)
band.3800.3900 <- ifelse(r.dat$co.min.elev < 3900 & r.dat$co.max.elev > 3800, 1, 0)
band.3900.4000 <- ifelse(r.dat$co.min.elev < 4000 & r.dat$co.max.elev > 3900, 1, 0)
band.4000.4100 <- ifelse(r.dat$co.min.elev < 4100 & r.dat$co.max.elev > 4000, 1, 0)
band.4100.4200 <- ifelse(r.dat$co.min.elev < 4200 & r.dat$co.max.elev > 4100, 1, 0)
band.4200.4300 <- ifelse(r.dat$co.min.elev < 4300 & r.dat$co.max.elev > 4200, 1, 0)
band.4300.4400 <- ifelse(r.dat$co.min.elev < 4400 & r.dat$co.max.elev > 4300, 1, 0)

## make dataframe that combines the presence/absence for each species within the elevational windows
rich.dat <- data.frame(rbind(band.1000.1100,band.1100.1200,band.1200.1300,band.1300.1400,band.1400.1500,band.1500.1600,band.1600.1700,band.1700.1800,band.1800.1900,band.1900.2000,band.2000.2100,band.2100.2200,band.2200.2300,band.2300.2400,band.2400.2500,band.2500.2600,band.2600.2700,band.2700.2800,band.2800.2900,band.2900.3000,band.3000.3100,band.3100.3200,band.3200.3300,band.3300.3400,band.3400.3500,band.3500.3600,band.3600.3700,band.3700.3800,band.3800.3900,band.3900.4000,band.4000.4100,band.4100.4200,band.4200.4300,band.4300.4400))

## sum the rows to calculate species richness within each elevational window
rich.dat$richness <- rowSums(rich.dat)
rich.dat$elev <-seq(1050,4350, by = 100)
head(rich.dat)

## which elevation is species richness highest
rich.dat[which(rich.dat$richness==max(rich.dat$richness)),] #2500-2600

cor.test(log(r.dat$co.elev.breadth), log(r.dat$CO.sample.size))
# t = 4.1013, df = 27, p-value = 0.0003384
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
 # 0.3273920 0.8035902
# sample estimates:
      # cor 
# 0.6195565 

# maxima - weighted regression
mod01 <- lm(co.elev.breadth ~ co.max.elev, data = r.dat, weights = log(CO.sample.size))
summary(mod01)

# Call:
# lm(formula = co.elev.breadth ~ co.max.elev, data = r.dat, weights = log(CO.sample.size))

# Weighted Residuals:
     # Min       1Q   Median       3Q      Max 
# -2016.30  -166.79    18.85   419.12  1268.48 

# Coefficients:
              # Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1899.0001   508.5447  -3.734  0.00089 ***
# co.max.elev     1.0923     0.1481   7.373 6.24e-08 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 769.3 on 27 degrees of freedom
# Multiple R-squared:  0.6681,	Adjusted R-squared:  0.6559 
# F-statistic: 54.36 on 1 and 27 DF,  p-value: 6.244e-08

# maxima unweighted regresssion
mod01b <- lm(co.elev.breadth ~ co.max.elev, data = r.dat)
summary(mod01b)

# Call:
# lm(formula = co.elev.breadth ~ co.max.elev, data = r.dat)

# Residuals:
    # Min      1Q  Median      3Q     Max 
# -944.66  -92.31   69.69  233.67  560.40 

# Coefficients:
              # Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1791.8904   498.1833  -3.597  0.00127 ** 
# co.max.elev     1.0497     0.1479   7.096 1.25e-07 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 399.2 on 27 degrees of freedom
# Multiple R-squared:  0.651,	Adjusted R-squared:  0.638 
# F-statistic: 50.36 on 1 and 27 DF,  p-value: 1.248e-07

# mid points - weighted
mod02 <- lm(co.elev.breadth ~ co.mid.elev, data = r.dat, weights = log(CO.sample.size))
summary(mod02)

# Call:
# lm(formula = co.elev.breadth ~ co.mid.elev, data = r.dat, weights = log(CO.sample.size))

# Weighted Residuals:
    # Min      1Q  Median      3Q     Max 
# -2391.3  -932.8  -312.8   849.3  2750.7 

# Coefficients:
             # Estimate Std. Error t value Pr(>|t|)
# (Intercept)  408.4843  1049.4026   0.389    0.700
# co.mid.elev    0.5632     0.4183   1.346    0.189

# Residual standard error: 1293 on 27 degrees of freedom
# Multiple R-squared:  0.06291,	Adjusted R-squared:  0.02821 
# F-statistic: 1.813 on 1 and 27 DF,  p-value: 0.1894

# mid-points - weighted
mod02b <- lm(co.elev.breadth ~ co.mid.elev, data = r.dat)
summary(mod02b)

# Call:
# lm(formula = co.elev.breadth ~ co.mid.elev, data = r.dat)

# Residuals:
     # Min       1Q   Median       3Q      Max 
# -1135.71  -415.12   -58.91   533.99  1377.45 

# Coefficients:
            # Estimate Std. Error t value Pr(>|t|)
# (Intercept) 354.5312   989.1133   0.358    0.723
# co.mid.elev   0.5445     0.3961   1.375    0.181

# Residual standard error: 653.2 on 27 degrees of freedom
# Multiple R-squared:  0.06542,	Adjusted R-squared:  0.0308 
# F-statistic:  1.89 on 1 and 27 DF,  p-value: 0.1805


## max elevation as a function of min temp of coldest month
max06 <- lm(co.max.elev ~ pam.bio6, data = r.dat, weights = log(CO.sample.size))
summary(max06)
# Call:
# lm(formula = co.max.elev ~ pam.bio6, data = r.dat, weights = log(CO.sample.size))

# Weighted Residuals:
     # Min       1Q   Median       3Q      Max 
# -1296.91  -708.18   -40.93   624.79  1786.66 

# Coefficients:
            # Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 2176.911    362.633   6.003  2.1e-06 ***
# pam.bio6     -10.141      2.941  -3.448  0.00187 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 832.8 on 27 degrees of freedom
# Multiple R-squared:  0.3057,	Adjusted R-squared:   0.28 
# F-statistic: 11.89 on 1 and 27 DF,  p-value: 0.001867

max.occ <- lm(co.max.elev ~ occ.bio6, data = r.dat, weights = log(CO.sample.size))
summary(max.occ)

# Call:
# lm(formula = co.max.elev ~ occ.bio6, data = r.dat, weights = log(CO.sample.size))

# Weighted Residuals:
    # Min      1Q  Median      3Q     Max 
# -1522.7  -668.2  -324.9   707.1  1739.6 

# Coefficients:
            # Estimate Std. Error t value Pr(>|t|)   
# (Intercept) 1901.992    600.066   3.170  0.00378 **
# occ.bio6     -10.545      4.189  -2.517  0.01807 * 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 899.4 on 27 degrees of freedom
# Multiple R-squared:  0.1901,	Adjusted R-squared:  0.1601 
# F-statistic: 6.337 on 1 and 27 DF,  p-value: 0.01807

mid06 <- lm(co.mid.elev ~ pam.bio6, data = r.dat, weights = log(CO.sample.size))
summary(mid06)

# Call:
# lm(formula = co.mid.elev ~ pam.bio6, data = r.dat, weights = log(CO.sample.size))

# Weighted Residuals:
    # Min      1Q  Median      3Q     Max 
# -871.77 -246.57   38.43  337.77  673.24 

# Coefficients:
            # Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 1622.440    194.209   8.354 5.78e-09 ***
# pam.bio6      -7.221      1.575  -4.584 9.31e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 446 on 27 degrees of freedom
# Multiple R-squared:  0.4377,	Adjusted R-squared:  0.4169 
# F-statistic: 21.02 on 1 and 27 DF,  p-value: 9.307e-05

mid.occ <- lm(co.mid.elev ~ occ.bio6, data = r.dat, weights = log(CO.sample.size))
summary(mid.occ)
# Call:
# lm(formula = co.mid.elev ~ occ.bio6, data = r.dat, weights = log(CO.sample.size))

# Weighted Residuals:
    # Min      1Q  Median      3Q     Max 
# -647.28 -298.29  -45.03  215.57  814.62 

# Coefficients:
            # Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1013.25     273.60   3.703 0.000965 ***
# occ.bio6      -10.42       1.91  -5.458 8.92e-06 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 410.1 on 27 degrees of freedom
# Multiple R-squared:  0.5246,	Adjusted R-squared:  0.5069 
# F-statistic: 29.79 on 1 and 27 DF,  p-value: 8.922e-06

