library(haven)
library(Epi)
library(dplyr)
library(xlsx)
library(writexl)
library(tidyr)

#### 1. spearman's correlation coefficient ####
# as incidences are poisson distributed (-> poisson model), normality cannot be assumed (?) and a rank coefficient instead of Pearson is used

spear1 <- cor.test(x = inc.prev.4x10$P1, y = (inc.prev.4x10$N25_34*100000/inc.prev.4x10$P25_34),
                   alternative = "two.sided", 
                   method = "spearman", 
                   exact = FALSE)

spear2 <- cor.test(x = inc.prev.4x10$P2, y = (inc.prev.4x10$N35_44*100000/inc.prev.4x10$P35_44),
                   alternative = "two.sided", 
                   method = "spearman", 
                   exact = FALSE)


spear3 <- cor.test(x = inc.prev.4x10$P3, y = (inc.prev.4x10$N45_54*100000/inc.prev.4x10$P45_54),
                   alternative = "two.sided", 
                   method = "spearman", 
                   exact = FALSE)


spear4 <- cor.test(x = inc.prev.4x10$P4, y = (inc.prev.4x10$N55_64*100000/inc.prev.4x10$P55_64),
                   alternative = "two.sided", 
                   method = "spearman", 
                   exact = FALSE)


#### 2. glm of the four age groups ####

### additive model rather than multiplicative model?
### is it possible to eliminate intercept with multiplicative model?

# scatterplot of x = prev, y= inc
# including abline of poisson model


####2.1: 25 - 34 years ####
inc.rates.prev.4x10 <- merge(inc.rates.4x10, prev.pooled, by = c("sgcentre", "cid", "loc"))
m.inc.4x10.1 <- glm(N25_34*100000/P25_34 ~ P1 - 1, data = inc.prev.4x10, family = poisson(link = identity), weights = P25_34)

summary(m.inc.4x10.1)
ci.lin(m.inc.4x10.1)
# plot(m.inc.4x10.1)

par(mfrow =  c(2, 2))
plot.4x10.1 <- plot(inc.prev.4x10$P1, inc.prev.4x10$N25_34*100000/inc.prev.4x10$P25_34, pch = 20, xlim = c(0, 65), ylim = c(0, 100), 
                    main = "International correlation of HPV and CC, 25-34y",
                    ylab = "Cervical cancer incidence rate(/100 000 py)", 
                    xlab  = "HPV prevalence (%)",
                    col = '#d95f02')
points(inc2.prev2.4x10$P1, inc2.prev2.4x10$N25_34*100000/inc2.prev2.4x10$P25_34, pch = 20, xlim = c(0, 65), ylim = c(0, 100),
     col = '#1b9e77') # qual2 data in green
points(inc3.prev3.4x10$P1, inc3.prev3.4x10$N25_34*100000/inc3.prev3.4x10$P25_34, pch = 20, xlim = c(0, 65), ylim = c(0, 100),
       col = "#7570b3") # qual3 data in purple
mtext("rho =", adj = 0.1, cex = 0.7)
mtext(round(as.numeric(spear1$estimate), 3), adj = 0.25, cex = 0.7)
mtext("p=", adj = 0.6, cex = 0.7)
mtext(round(as.numeric(spear1$p.value), 3), adj = 0.75, cex = 0.7)
abline(0, ci.lin(m.inc.4x10.1)[1])
text(inc.prev.4x10$P1, inc.prev.4x10$N25_34*100000/inc.prev.4x10$P25_34, inc.prev.4x10$cid, pos = 3, cex = 0.7) # labeling the points


#### 2.2: 35-44 years ####
m.inc.4x10.2 <- glm(N35_44*100000/P35_44 ~ P2 -1, data = inc.prev.4x10, family = poisson(link = identity), weights = P35_44)
summary(m.inc.4x10.2)
ci.lin(m.inc.4x10.2)
#plot(m.inc.4x10.2)
str(m.inc.4x10.2)

plot.4x10.2 <- plot(inc.prev.4x10$P2, inc.prev.4x10$N35_44*100000/inc.prev.4x10$P35_44, pch = 20, xlim = c(0, 65), ylim = c(0, 100),
                    main = "International correlation of HPV and CC, 35-44y",
                    ylab = "Cervical cancer incidence rate(/100 000 py)", 
                    xlab  = "HPV prevalence (%)",
                    col = '#d95f02')
points(inc2.prev2.4x10$P2, inc2.prev2.4x10$N35_44*100000/inc2.prev2.4x10$P35_44, pch = 20, xlim = c(0, 65), ylim = c(0, 100),
       col = '#1b9e77')
points(inc3.prev3.4x10$P2, inc3.prev3.4x10$N35_44*100000/inc3.prev3.4x10$P35_44, pch = 20, xlim = c(0, 65), ylim = c(0, 100),
       col = "#7570b3")
mtext("rho =", adj = 0.1, cex = 0.7)
mtext(round(as.numeric(spear2$estimate), 3), adj = 0.25, cex = 0.7)
mtext("p=", adj = 0.6, cex = 0.7)
mtext(round(as.numeric(spear2$p.value), 3), adj = 0.75, cex = 0.7)
abline(0, ci.lin(m.inc.4x10.2)[1])
text(inc.prev.4x10$P2, inc.prev.4x10$N35_44*100000/inc.prev.4x10$P35_44, inc.prev.4x10$cid, pos = 3, cex = 0.7)


#data.pred <- data.frame(P2 = 0:30)
#mod.pred <- predict(m.inc.4x10.2, newdata = data.pred, type = "response" ) 
#lines(data.pred$P2, mod.pred)
#points(na.omit(m.inc.4x10.2$data$P2), m.inc.4x10.2$fitted.values) #  all predictions the same
#plot(na.omit(m.inc.4x10.2$data$P2), m.inc.4x10.2$y, xlim = c(0, 30), ylim = c(0, 100))
# same result as abline


####2.3: 45-55 years ####
m.inc.4x10.3 <- glm(N45_54*100000/P45_54 ~ P3 -1, data = inc.prev.4x10, family = poisson(link = identity), weights = P45_54)
summary(m.inc.4x10.3)
ci.lin(m.inc.4x10.3)
# plot(m.inc.4x10.3)

plot.4x10.3 <- plot(inc.prev.4x10$P3, inc.prev.4x10$N45_54*100000/inc.prev.4x10$P45_54, pch = 20, xlim = c(0, 50), ylim = c(0, 200),
                    main = "International correlation of HPV and CC, 45-54y",
                    ylab = "Cervical cancer incidence rate(/100 000 py)",
                    xlab  = "HPV prevalence (%)",
                    col = '#d95f02') 
points(inc2.prev2.4x10$P3, inc2.prev2.4x10$N45_54*100000/inc2.prev2.4x10$P45_54, pch = 20, xlim = c(0, 50), ylim = c(0, 200), 
                          col = '#1b9e77')
points(inc3.prev3.4x10$P3, inc3.prev3.4x10$N45_54*100000/inc3.prev3.4x10$P45_54, pch = 20, xlim = c(0, 50), ylim = c(0, 200), 
       col = "#7570b3")                   
mtext("rho =", adj = 0.1, cex = 0.7)
mtext(round(as.numeric(spear3$estimate), 3), adj = 0.25, cex = 0.7)
mtext("p=", adj = 0.6, cex = 0.7)
mtext(round(as.numeric(spear3$p.value), 3), adj = 0.75, cex = 0.7)
abline(0, ci.lin(m.inc.4x10.3)[1])
text(inc.prev.4x10$P3, inc.prev.4x10$N45_54*100000/inc.prev.4x10$P45_54, inc.prev.4x10$cid, pos = 3, cex = 0.7)

#### 2.4: 55- 64 years ####
m.inc.4x10.4 <- glm(N55_64*100000/P55_64 ~ P4 -1, data = inc.prev.4x10, family = poisson(link = identity), weights = P55_64)
summary(m.inc.4x10.4)
ci.lin(m.inc.4x10.4)
# plot(m.inc.4x10.4)

plot.4x10.4 <- plot(inc.prev.4x10$P4, inc.prev.4x10$N55_64*100000/inc.prev.4x10$P55_64, pch = 20, xlim = c(0, 50), ylim = c(0, 200),
                    main = "International correlation of HPV and CC, 55-64y",
                    ylab = "Cervical cancer incidence rate(/100 000 py)", 
                    xlab  = "HPV prevalence (%)",
                    col = '#d95f02')
points(inc2.prev2.4x10$P4, inc2.prev2.4x10$N55_64*100000/inc2.prev2.4x10$P55_64, pch = 20, xlim = c(0, 50), ylim = c(0, 200),
       col = '#1b9e77')
points(inc3.prev3.4x10$P4, inc3.prev3.4x10$N55_64*100000/inc3.prev3.4x10$P55_64, pch = 20, xlim = c(0, 50), ylim = c(0, 200),
       col = "#7570b3")
mtext("rho =", adj = 0.1, cex = 0.7) # to include the spearman test output in graph
mtext(round(as.numeric(spear4$estimate), 3), adj = 0.25, cex = 0.7)
mtext("p=", adj = 0.6, cex = 0.7)
mtext(round(as.numeric(spear4$p.value), 3), adj = 0.75, cex = 0.7)
abline(0, ci.lin(m.inc.4x10.4)[1])
text(inc.prev.4x10$P4, inc.prev.4x10$N55_64*100000/inc.prev.4x10$P55_64, inc.prev.4x10$cid, pos = 3, cex = 0.7)

