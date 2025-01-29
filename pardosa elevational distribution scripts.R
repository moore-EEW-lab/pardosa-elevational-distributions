
sp.dat <- read.csv('cleaned.pards.co.elev.csv')


library(car)
library(MASS)
library(ggplot2)


#### Elevational distributions


mean(sp.dat$co.max.elev) # mean maximum elevation = 3343.765
median(sp.dat$co.max.elev) # medium maximum elevation = 3353
sd(sp.dat$co.max.elev) # standard deviation of maximum elevation = 537.3442
range(sp.dat$co.max.elev) # range of maximum elevations = 2125 4338

mean(sp.dat$co.mid.elev) # mean mid elevation = 2497.662
median(sp.dat$co.mid.elev) # median mid elevation = 2508.75
sd(sp.dat$co.mid.elev) # standard deviation of mid elevation = 331.6238
range(sp.dat$co.mid.elev) # range of mid elevations = 1760.5 3189.0

mean(sp.dat$co.elev.breadth) # mean maximum elevation = 1692.206
median(sp.dat$co.elev.breadth) # medium maximum elevation = 1508.5
sd(sp.dat$co.elev.breadth) # standard deviation of maximum elevation = 656.4158
range(sp.dat$co.elev.breadth) # range of maximum elevations = 669 3218


#### make a graph of # of species collected across elevations in CO. Use 100 m windows 

### first, calculate species richness per 100 m elevational window

##for each 100 m band, does each species occur in it
band.1000.1100 <- ifelse(sp.dat$co.min.elev < 1100 & sp.dat$co.max.elev > 1000, 1, 0)
band.1100.1200 <- ifelse(sp.dat$co.min.elev < 1200 & sp.dat$co.max.elev > 1100, 1, 0)
band.1200.1300 <- ifelse(sp.dat$co.min.elev < 1300 & sp.dat$co.max.elev > 1200, 1, 0)
band.1300.1400 <- ifelse(sp.dat$co.min.elev < 1400 & sp.dat$co.max.elev > 1300, 1, 0)
band.1400.1500 <- ifelse(sp.dat$co.min.elev < 1500 & sp.dat$co.max.elev > 1400, 1, 0)
band.1500.1600 <- ifelse(sp.dat$co.min.elev < 1600 & sp.dat$co.max.elev > 1500, 1, 0)
band.1600.1700 <- ifelse(sp.dat$co.min.elev < 1700 & sp.dat$co.max.elev > 1600, 1, 0)
band.1700.1800 <- ifelse(sp.dat$co.min.elev < 1800 & sp.dat$co.max.elev > 1700, 1, 0)
band.1800.1900 <- ifelse(sp.dat$co.min.elev < 1900 & sp.dat$co.max.elev > 1800, 1, 0)
band.1900.2000 <- ifelse(sp.dat$co.min.elev < 2000 & sp.dat$co.max.elev > 1900, 1, 0)
band.2000.2100 <- ifelse(sp.dat$co.min.elev < 2100 & sp.dat$co.max.elev > 2000, 1, 0)
band.2100.2200 <- ifelse(sp.dat$co.min.elev < 2200 & sp.dat$co.max.elev > 2100, 1, 0)
band.2200.2300 <- ifelse(sp.dat$co.min.elev < 2300 & sp.dat$co.max.elev > 2200, 1, 0)
band.2300.2400 <- ifelse(sp.dat$co.min.elev < 2400 & sp.dat$co.max.elev > 2300, 1, 0)
band.2400.2500 <- ifelse(sp.dat$co.min.elev < 2500 & sp.dat$co.max.elev > 2400, 1, 0)
band.2500.2600 <- ifelse(sp.dat$co.min.elev < 2600 & sp.dat$co.max.elev > 2500, 1, 0)
band.2600.2700 <- ifelse(sp.dat$co.min.elev < 2700 & sp.dat$co.max.elev > 2600, 1, 0)
band.2700.2800 <- ifelse(sp.dat$co.min.elev < 2800 & sp.dat$co.max.elev > 2700, 1, 0)
band.2800.2900 <- ifelse(sp.dat$co.min.elev < 2900 & sp.dat$co.max.elev > 2800, 1, 0)
band.2900.3000 <- ifelse(sp.dat$co.min.elev < 3000 & sp.dat$co.max.elev > 2900, 1, 0)
band.3000.3100 <- ifelse(sp.dat$co.min.elev < 3100 & sp.dat$co.max.elev > 3000, 1, 0)
band.3100.3200 <- ifelse(sp.dat$co.min.elev < 3200 & sp.dat$co.max.elev > 3100, 1, 0)
band.3200.3300 <- ifelse(sp.dat$co.min.elev < 3300 & sp.dat$co.max.elev > 3200, 1, 0)
band.3300.3400 <- ifelse(sp.dat$co.min.elev < 3400 & sp.dat$co.max.elev > 3300, 1, 0)
band.3400.3500 <- ifelse(sp.dat$co.min.elev < 3500 & sp.dat$co.max.elev > 3400, 1, 0)
band.3500.3600 <- ifelse(sp.dat$co.min.elev < 3600 & sp.dat$co.max.elev > 3500, 1, 0)
band.3600.3700 <- ifelse(sp.dat$co.min.elev < 3700 & sp.dat$co.max.elev > 3600, 1, 0)
band.3700.3800 <- ifelse(sp.dat$co.min.elev < 3800 & sp.dat$co.max.elev > 3700, 1, 0)
band.3800.3900 <- ifelse(sp.dat$co.min.elev < 3900 & sp.dat$co.max.elev > 3800, 1, 0)
band.3900.4000 <- ifelse(sp.dat$co.min.elev < 4000 & sp.dat$co.max.elev > 3900, 1, 0)
band.4000.4100 <- ifelse(sp.dat$co.min.elev < 4100 & sp.dat$co.max.elev > 4000, 1, 0)
band.4100.4200 <- ifelse(sp.dat$co.min.elev < 4200 & sp.dat$co.max.elev > 4100, 1, 0)
band.4200.4300 <- ifelse(sp.dat$co.min.elev < 4300 & sp.dat$co.max.elev > 4200, 1, 0)
band.4300.4400 <- ifelse(sp.dat$co.min.elev < 4400 & sp.dat$co.max.elev > 4300, 1, 0)

## make dataframe that combines the presence/absence for each species within the elevational windows
rich.dat <- data.frame(rbind(band.1000.1100,band.1100.1200,band.1200.1300,band.1300.1400,band.1400.1500,band.1500.1600,band.1600.1700,band.1700.1800,band.1800.1900,band.1900.2000,band.2000.2100,band.2100.2200,band.2200.2300,band.2300.2400,band.2400.2500,band.2500.2600,band.2600.2700,band.2700.2800,band.2800.2900,band.2900.3000,band.3000.3100,band.3100.3200,band.3200.3300,band.3300.3400,band.3400.3500,band.3500.3600,band.3600.3700,band.3700.3800,band.3800.3900,band.3900.4000,band.4000.4100,band.4100.4200,band.4200.4300,band.4300.4400))

## sum the rows to calculate species richness within each elevational window
rich.dat$richness <- rowSums(rich.dat)
rich.dat$elev <-seq(1050,4350, by = 100)
head(rich.dat)

## which elevation is species richness highest
rich.dat[which(rich.dat$richness==max(rich.dat$richness)),] # elevational band from 2500 to 2600 has highest spp richness

## make graph. bars for each 100 m elevational window and a smoothed line to show the general trend
pards.rich.plot <- 
ggplot(data = rich.dat, aes(x = elev, y = richness)) + 
geom_bar(stat = 'identity', fill = '#CCCCCC') +
geom_smooth(method = 'loess', fill = 'cornflowerblue', se = FALSE, size = 2.5) +
coord_cartesian(ylim = c(0,33), expand = FALSE) +
scale_x_continuous(breaks = c(1600, 2400, 3200, 4000)) +
labs(x = 'Elevation (m asl)', y = 'Number of Species in Colorado')

bgrd =
theme(axis.text = element_text(color="Black"),
axis.title.x = element_text(face = "plain", size = 18, margin = margin(t = 10)), 
axis.text.x = element_text(size = 15),
axis.title.y = element_text(face = "plain", size = 18, margin = margin(r = 10)), 
axis.text.y = element_text(size = 15),
panel.background = element_rect(fill = "White"),
panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
axis.line.x = element_line(linetype = 'solid', color = 'black', size = 0.8),
axis.line.y = element_line(linetype = 'solid', color = 'black', size = 0.8),
legend.key = element_rect(fill = 'white')) 

pards.rich.fig <- pards.rich.plot + bgrd
pards.rich.fig

png('pards.richness.elev.png', height = 7, width = 6, units = 'in', res = 600)
print(pards.rich.fig)
dev.off()




##### make a graph of species' elevational ranges

binom.vec <- strsplit(sp.dat$binom, split = " ", fixed = FALSE)
binom.mat <- matrix(unlist(binom.vec), ncol = 2, byrow=T)

sp.dat$species <- binom.mat[,2]

## graph 
pards.el.br.plot <- 
ggplot(data = sp.dat, aes(x = reorder(species, co.mid.elev), y = co.mid.elev)) +
geom_errorbar(aes(x = reorder(species, co.mid.elev), ymax = co.max.elev, ymin = co.min.elev), width = 0) +
geom_point(size = 4, shape = '+', aes(x = reorder(species, co.mid.elev))) +
labs(y = 'Elevational Range of Colorado Collections (m asl)') 

bgrd01 =
theme(axis.text = element_text(color="Black"),
axis.title.x = element_blank(), 
axis.text.x = element_text(size = 16, angle = 45, hjust = 1, vjust = 1),
axis.title.y = element_text(face = "plain", size = 18, margin = margin(r = 10)), 
axis.text.y = element_text(size = 15),
panel.background = element_rect(fill = "White"),
panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
axis.line.x = element_line(linetype = 'solid', color = 'black', size = 0.8),
axis.line.y = element_line(linetype = 'solid', color = 'black', size = 0.8),
legend.key = element_rect(fill = 'white')) 

pards.el.br.fig <- pards.el.br.plot + bgrd01
pards.el.br.fig

png('pards.elev.breadth.png', height = 7, width = 8, units = 'in', res = 600)
print(pards.el.br.fig)
dev.off()


### correlation between elevational range and sample size

cor.test(log(sp.dat$co.elev.breadth), log(sp.dat$CO.sample.size))
# data:  log(sp.dat$co.elev.breadth) and log(sp.dat$CO.sample.size)
# t = 2.8625, df = 32, p-value = 0.007355
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
 # 0.1337592 0.6850697
# sample estimates:
      # cor 
# 0.4515022 



##### are species collected in narrow elevational bands?
### if species occupy narrow elevational bands, then we should find no relationship between elevational breadth and max elev or mid elev

## weighted regression for max elevation to decrease influence of points with small sample sizes
mod01 <- lm(co.elev.breadth ~ co.max.elev, data = sp.dat, weights = log(CO.sample.size))
summary(mod01)
# Call:
# lm(formula = co.elev.breadth ~ co.max.elev, data = sp.dat, weights = log(CO.sample.size))

# Weighted Residuals:
     # Min       1Q   Median       3Q      Max 
# -1977.84  -347.47    73.73   388.20  1317.70 

# Coefficients:
              # Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1808.0201   450.8547  -4.010  0.00034 ***
# co.max.elev     1.0610     0.1314   8.072 3.23e-09 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 731.8 on 32 degrees of freedom
# Multiple R-squared:  0.6707,	Adjusted R-squared:  0.6604 
# F-statistic: 65.16 on 1 and 32 DF,  p-value: 3.229e-09

##unweighted regression for max elevation
mod01b <- lm(co.elev.breadth ~ co.max.elev, data = sp.dat)
summary(mod01b)

# Call:
# lm(formula = co.elev.breadth ~ co.max.elev, data = sp.dat)

# Residuals:
   # Min     1Q Median     3Q    Max 
# -909.1 -142.4  106.9  228.0  598.2 

# Coefficients:
              # Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1625.4571   426.4812  -3.811 0.000593 ***
# co.max.elev     0.9922     0.1260   7.876 5.49e-09 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 388.9 on 32 degrees of freedom
# Multiple R-squared:  0.6597,	Adjusted R-squared:  0.6491 
# F-statistic: 62.03 on 1 and 32 DF,  p-value: 5.494e-09

## weighted regression for mid elevation to decrease influence of points with small sample sizes
mod02 <- lm(co.elev.breadth ~ co.mid.elev, data = sp.dat, weights = log(CO.sample.size))
summary(mod02)

# Call:
# lm(formula = co.elev.breadth ~ co.mid.elev, data = sp.dat, weights = log(CO.sample.size))

# Weighted Residuals:
    # Min      1Q  Median      3Q     Max 
# -2404.3  -908.7  -245.0   783.9  2773.5 

# Coefficients:
            # Estimate Std. Error t value Pr(>|t|)
# (Intercept) 247.6889   921.7955   0.269    0.790
# co.mid.elev   0.6183     0.3665   1.687    0.101

# Residual standard error: 1222 on 32 degrees of freedom
# Multiple R-squared:  0.08168,	Adjusted R-squared:  0.05298 
# F-statistic: 2.846 on 1 and 32 DF,  p-value: 0.1013

## unweighted regression for mid elevation
mod02b <- lm(co.elev.breadth ~ co.mid.elev, data = sp.dat)
summary(mod02b)

# Call:
# lm(formula = co.elev.breadth ~ co.mid.elev, data = sp.dat)

# Residuals:
    # Min      1Q  Median      3Q     Max 
# -1149.7  -473.6   -29.9   527.5  1376.3 

# Coefficients:
            # Estimate Std. Error t value Pr(>|t|)  
# (Intercept)  78.7028   833.1437   0.094   0.9253  
# co.mid.elev   0.6460     0.3308   1.953   0.0596 .
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 630.1 on 32 degrees of freedom
# Multiple R-squared:  0.1065,	Adjusted R-squared:  0.07859 
# F-statistic: 3.815 on 1 and 32 DF,  p-value: 0.05959


### figure to look at whether species with higher max elevations tend to have wider elevational ranges

pards.max.band.plot <- 
ggplot(data = sp.dat, aes(x = co.max.elev, y = co.elev.breadth)) +
geom_point(shape = 16, aes(size = log(CO.sample.size)), show.legend = FALSE) +
scale_x_continuous(breaks = c(2150, 2850, 3550, 4250)) +
scale_y_continuous(breaks = c(700, 1500, 2300, 3100)) +
annotate(geom = "text", x = 2290, y = 3200, parse = TRUE, label = paste("R^2==0.671"), color = '#666666', size = 6) +
annotate(geom = "text", x = 2290, y = 3050, parse = TRUE, label = paste("P<0.001"), color = '#666666', size = 6) +
labs(x = "Species' Elevational Maximum in CO (m asl)", y = "Species' Elevational Range in CO (m asl)")

bgrd02 =
theme(axis.text = element_text(color="Black"),
axis.title.x = element_text(face = "plain", size = 18, margin = margin(t = 10)), 
axis.text.x = element_text(size = 15),
axis.title.y = element_text(face = "plain", size = 18, margin = margin(r = 10)), 
axis.text.y = element_text(size = 15),
panel.background = element_rect(fill = "White"),
panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
axis.line.x = element_line(linetype = 'solid', color = 'black', size = 0.8),
axis.line.y = element_line(linetype = 'solid', color = 'black', size = 0.8),
legend.key = element_rect(fill = 'white')) 

pards.max.band.fig <- pards.max.band.plot + bgrd02
pards.max.band.fig

png('pards.max.elev.breadth.png', height = 7, width = 7, units = 'in', res = 600)
print(pards.max.band.fig)
dev.off()


### figure to look at whether species with higher elevational mid-points tend to have wider elevational ranges
pards.mid.band.plot <- 
ggplot(data = sp.dat, aes(x = co.mid.elev, y = co.elev.breadth)) +
geom_point(shape = 16, aes(size = log(CO.sample.size)), show.legend = FALSE) +
scale_x_continuous(breaks = c(1850, 2250, 2650, 3050)) +
scale_y_continuous(breaks = c(700, 1500, 2300, 3100)) +
annotate(geom = "text", x = 1866, y = 3200, parse = TRUE, label = paste("R^2==0.082"), color = '#666666', size = 6) + 
annotate(geom = "text", x = 1866, y = 3050, parse = TRUE, label = paste("P==0.101"), color = '#666666', size = 6) + 
labs(x = "Species' Elevational Mid-Point in CO (m asl)", y = "Species' Elevational Range in CO (m asl)")

pards.mid.band.fig <- pards.mid.band.plot + bgrd02
pards.mid.band.fig

png('pards.mid.elev.breadth.png', height = 7, width = 7, units = 'in', res = 600)
print(pards.mid.band.fig)
dev.off()




##### are cold-adapted species most living at higher elevations? are warm-adapted speciees mostly living at lower elevations?

### Are species with higher elevational maxima those that tend to live in colder parts of NA?

## max elevation as a function of min temp of coldest month
max06 <- lm(sp.dat$co.max.elev ~ sp.dat$pam.bio6, data = sp.dat, weights = log(CO.sample.size))
summary(max06)

# Call:
# lm(formula = sp.dat$co.max.elev ~ sp.dat$pam.bio6, data = sp.dat, 
    # weights = log(CO.sample.size))

# Weighted Residuals:
    # Min      1Q  Median      3Q     Max 
# -1270.5  -650.6  -189.9   598.2  1812.2 

# Coefficients:
                # Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     2120.358    347.307   6.105 8.01e-07 ***
# sp.dat$pam.bio6  -10.481      2.794  -3.751 0.000701 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 820.3 on 32 degrees of freedom
# Multiple R-squared:  0.3054,	Adjusted R-squared:  0.2837 
# F-statistic: 14.07 on 1 and 32 DF,  p-value: 0.0007013


## using mean bio6 across collection locations

max.occ <- lm(sp.dat$co.max.elev ~ sp.dat$occ.bio6, data = sp.dat, weights = log(CO.sample.size))
summary(max.occ)

# Call:
# lm(formula = sp.dat$co.max.elev ~ sp.dat$occ.bio6, data = sp.dat, 
    # weights = log(CO.sample.size))

# Weighted Residuals:
    # Min      1Q  Median      3Q     Max 
# -1486.3  -650.7  -298.6   661.6  1756.0 

# Coefficients:
                # Estimate Std. Error t value Pr(>|t|)   
# (Intercept)     1849.080    558.612   3.310  0.00232 **
# sp.dat$occ.bio6  -10.837      3.881  -2.792  0.00876 **
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 882.6 on 32 degrees of freedom
# Multiple R-squared:  0.1959,	Adjusted R-squared:  0.1708 
# F-statistic: 7.796 on 1 and 32 DF,  p-value: 0.008762

### anyway you cut it, the answer is absolutely: yes!


# relationship between MAT across species range and the maximum elevation at which it was collected in CO
bio6.max.elev.plot <-
ggplot(data = sp.dat, aes(x = pam.bio6/10, y = co.max.elev)) +
geom_point(shape = 16, aes(size = log(CO.sample.size)), show.legend = FALSE) +
scale_y_continuous(breaks = c(2150, 2850, 3550, 4250)) +
scale_x_continuous(breaks = c(-15.5, -12.0, -8.5, -5.0)) +
annotate(geom = 'text', x = -6, y = 4300, parse = TRUE, label = paste("R^2==0.305"), color = '#666666', size = 6) +
annotate(geom = 'text', x = -6, y = 4160, parse = TRUE, label = paste("P<0.001"), color = '#666666', size = 6) +
xlab(label = expression(atop('Mean Minimum Temperature', paste('of Coldest Month '(degree*C))))) + 
ylab(label = "Species' Maximum Elevation in CO (m asl)")

bio6.max.elev.fig <- bio6.max.elev.plot + bgrd02
bio6.max.elev.fig

png('BIO6.max.elev.png', width = 7, height = 7, units = 'in', res = 600)
print(bio6.max.elev.fig)
dev.off()



### Are species with higher elevational midpoints those that tend to live in colder parts of NA?


mid06 <- lm(sp.dat$co.mid.elev ~ sp.dat$pam.bio6, data = sp.dat, weights = log(CO.sample.size))
summary(mid06)

# Call:
# lm(formula = sp.dat$co.mid.elev ~ sp.dat$pam.bio6, data = sp.dat, 
    # weights = log(CO.sample.size))

# Weighted Residuals:
    # Min      1Q  Median      3Q     Max 
# -872.28 -247.35   33.59  359.15  662.83 

# Coefficients:
                # Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     1585.841    187.312   8.466 1.13e-09 ***
# sp.dat$pam.bio6   -7.505      1.507  -4.980 2.11e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 442.4 on 32 degrees of freedom
# Multiple R-squared:  0.4366,	Adjusted R-squared:  0.419 
# F-statistic:  24.8 on 1 and 32 DF,  p-value: 2.105e-05


mid.occ <- lm(sp.dat$co.mid.elev ~ sp.dat$occ.bio6, data = sp.dat, weights = log(CO.sample.size))
summary(mid.occ)

# Call:
# lm(formula = sp.dat$co.mid.elev ~ sp.dat$occ.bio6, data = sp.dat, 
    # weights = log(CO.sample.size))

# Weighted Residuals:
    # Min      1Q  Median      3Q     Max 
# -643.90 -302.98  -44.57  219.76  822.00 

# Coefficients:
                # Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      995.371    259.299   3.839  0.00055 ***
# sp.dat$occ.bio6  -10.542      1.802  -5.851 1.67e-06 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 409.7 on 32 degrees of freedom
# Multiple R-squared:  0.5169,	Adjusted R-squared:  0.5018 
# F-statistic: 34.24 on 1 and 32 DF,  p-value: 1.67e-06



# also yes!


bio6.mid.elev.plot <-
ggplot(data = sp.dat, aes(x = pam.bio6/10, y = co.mid.elev)) +
geom_point(shape = 16, aes(size = log(CO.sample.size)), show.legend = FALSE) +
scale_y_continuous(breaks = c(1900, 2300, 2700, 3100)) +
scale_x_continuous(breaks = c(-15.5, -12.0, -8.5, -5.0)) +
annotate(geom = 'text', x = -6.5, y = 3163, parse = TRUE, label = paste("R^2==0.437"), color = '#666666', size = 6) +
annotate(geom = 'text', x = -6.5, y = 3077, parse = TRUE, label = paste("P<0.001"), color = '#666666', size = 6) +
xlab(label = expression(atop('Mean Minimum Temperature', paste('of Coldest Month '(degree*C))))) + 
ylab(label = "Species' Elevational Mid-Point in CO (m asl)")

bio6.mid.elev.fig <- bio6.mid.elev.plot + bgrd02
bio6.mid.elev.fig

png('BIO6.mid.elev.png', width = 7, height = 7, units = 'in', res = 600)
print(bio6.mid.elev.fig)
dev.off()



#### very high elevation spp - live higher than 4000
sp.dat[which(sp.dat$co.max.elev > 4000), c('binom','co.elev.breadth')]

# how many of these species have elevational breadths that are >1 sd larger than the average elevational breadth?
sp.dat$sd1.breadth <- ifelse(sp.dat$co.elev.breadth >= 2348.63, 'y', 'n')

sp.dat[which(sp.dat$co.max.elev > 4000),]


### very high elevation spp - live above treeline (~3350)
length(sp.dat[which(sp.dat$co.max.elev > 3350),]) # 16 species above treeline
sp.dat[which(sp.dat$co.max.elev > 3350), c('binom', 'co.elev.breadth', 'sd1.breadth')] # 7 species





### Are cold-adapted species restricted to higher elevations? look at max range-wide temp and elevational minima 
cor.test(sp.dat$co.min.elev, sp.dat$pam.bio1)  # r = -0.4393293, 95%CIs = -0.6769266 -0.1188156, t = -2.7665, df = 32, p-value = 0.009334
cor.test(sp.dat$co.min.elev, sp.dat$pam.bio5)  # r = -0.4857625, 95%CIs = -0.7076790 -0.1766079, t = -3.1437, df = 32, p-value = 0.003587
cor.test(sp.dat$co.min.elev, sp.dat$pam.bio10) # r = -0.4965881, 95%CIs = -0.7147299 -0.1903953, t = t = -3.2364, df = 32, p-value = 0.002815


# using temp as BIO1 (MAT), and BIO11 (mean temp of coldest quarter)

max1 <- lm(co.max.elev ~ pam.bio1, data = sp.dat, weights = log(CO.sample.size))
summary(max1)

# Call:
# lm(formula = co.max.elev ~ pam.bio1, data = sp.dat, weights = log(CO.sample.size))

# Weighted Residuals:
    # Min      1Q  Median      3Q     Max 
# -1297.7  -672.3  -196.2   491.3  1763.6 

# Coefficients:
            # Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 4072.332    207.721  19.605  < 2e-16 ***
# pam.bio1     -10.793      3.069  -3.517  0.00133 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 835.9 on 32 degrees of freedom
# Multiple R-squared:  0.2788,	Adjusted R-squared:  0.2563 
# F-statistic: 12.37 on 1 and 32 DF,  p-value: 0.001329

mid1 <- lm(co.mid.elev ~ pam.bio1, data = sp.dat, weights = log(CO.sample.size))
summary(mid1) 

# Call:
# lm(formula = co.mid.elev ~ pam.bio1, data = sp.dat, weights = log(CO.sample.size))

# Weighted Residuals:
    # Min      1Q  Median      3Q     Max 
# -840.34 -298.94   62.56  331.74  642.09 

# Coefficients:
            # Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 3027.466    106.267  28.489  < 2e-16 ***
# pam.bio1      -8.425      1.570  -5.367 6.83e-06 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 427.6 on 32 degrees of freedom
# Multiple R-squared:  0.4737,	Adjusted R-squared:  0.4573 
# F-statistic:  28.8 on 1 and 32 DF,  p-value: 6.832e-06


max11 <- lm(co.max.elev ~ pam.bio11, data = sp.dat, weights = log(CO.sample.size))
summary(max11)

# Call:
# lm(formula = co.max.elev ~ pam.bio11, data = sp.dat, weights = log(CO.sample.size))

# Weighted Residuals:
    # Min      1Q  Median      3Q     Max 
# -1359.6  -683.7  -135.2   466.6  1724.9 

# Coefficients:
            # Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 2963.318    127.738  23.198  < 2e-16 ***
# pam.bio11    -10.643      2.605  -4.086 0.000275 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 797.9 on 32 degrees of freedom
# Multiple R-squared:  0.3429,	Adjusted R-squared:  0.3223 
# F-statistic:  16.7 on 1 and 32 DF,  p-value: 0.0002748

mid11 <- lm(co.mid.elev ~ pam.bio11, data = sp.dat, weights = log(CO.sample.size))
summary(mid11) # 

# Call:
# lm(formula = co.mid.elev ~ pam.bio11, data = sp.dat, weights = log(CO.sample.size))

# Weighted Residuals:
    # Min      1Q  Median      3Q     Max 
# -929.11 -296.01  -22.26  356.80  694.30 

# Coefficients:
            # Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 2200.407     69.605  31.613  < 2e-16 ***
# pam.bio11     -7.350      1.419  -5.179 1.18e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 434.8 on 32 degrees of freedom
# Multiple R-squared:  0.456,	Adjusted R-squared:  0.4389 
# F-statistic: 26.82 on 1 and 32 DF,  p-value: 1.182e-05

