require(ggplot2)
y <- read.table("ANOVAProjectData.txt", header = TRUE)
performance <- y[,1]
model <- y[,2]
method <- y[,3]
performance.df <- data.frame("Performance" = performance, "Model" = factor(model), "Method" = factor(method))

#preliminary visualization of the data
ggplot(performance.df, aes(Performance, Method, colour = Model)) + geom_point()
ggplot(data = performance.df, aes(Model, Performance)) + geom_boxplot()
qplot(model, performance, col = method) + scale_x_discrete(breaks = c(1, 2), labels = c("Model 1", "Model 2"))

ggplot(data = performance.df, aes(Method, Performance)) + geom_boxplot() + scale_x_discrete(labels = c("5NN", "7NN", "Logistic Reg"))
qplot(method, performance, col = model) + scale_x_discrete(labels = c("5NN", "7NN", "Logistic Reg."))



#preliminary anova and mean tables
anova <- aov(Performance ~ Model + Method + Model*Method, data = performance.df)
anovaInteraction <- aov(Performance ~ Model*Method, data = performance.df)
anovaModel <- aov(Performance ~ Model, data = performance.df)
anovaMethod <- aov(Performance ~ Method, data = performance.df)
model.tables(anova, type = "means")

#residual analysis to show normality
performance.res = performance.df
performance.res$M1.Fit = fitted(anovaInteraction)
performance.res$M1.Resid = resid(anovaInteraction)
qplot(performance.res$M1.Resid, geom = "histogram") #hist(performance.res$M1.Resid)
ggplot(performance.res, aes(M1.Fit, M1.Resid)) + geom_point() + xlab("Fitted Values") + ylab("Residuals")
ggplot(performance.res, aes(sample = M1.Resid)) + stat_qq()

#Two-way Bartlett-test for homogenity of variances
cell11 <- performance[model == 1 & method == 1]
cell12 <- performance[model == 1 & method == 2]
cell13 <- performance[model == 1 & method == 3]
cell21 <- performance[model == 2 & method == 1]
cell22 <- performance[model == 2 & method == 2]
cell23 <- performance[model == 2 & method == 3]
cells <- as.data.frame(cbind(cell11, cell12, cell13, cell21, cell22, cell23))
bartlett.test(cells)

#anova tables
summary(anova)
summary(anovaInteraction)
summary(anovaModel)
summary(anovaMethod)

#Tukey to show mean differences
TukeyHSD(anovaInteraction, which = "Method")
performance.hsd = data.frame(TukeyHSD(anovaComp, which = "Method")$Method)
performance.hsd$Comparison = row.names(performance.hsd)
ggplot(performance.hsd, aes(Comparison, y = diff, ymin = lwr, ymax = upr)) + geom_pointrange() + ylab("Difference in Mean Performance by Method") + coord_flip()

TukeyHSD(anovaInteraction, which = "Model")
performance.hsd = data.frame(TukeyHSD(anovaInteraction, which = "Model")$Model)
performance.hsd$Comparison = row.names(performance.hsd)
ggplot(performance.hsd, aes(Comparison, y = diff, ymin = lwr, ymax = upr)) + geom_pointrange() + ylab("Difference in Mean Performances by Model")













#arcsine(sqrt) Transformation
y <- read.table("ANOVAProjectData.txt", header = TRUE)
performance <- y[,1]
model <- y[,2]
method <- y[,3]
performance.sqrt <- sqrt(performance)
performance.transform <- 2*asin(performance.sqrt)
performance.df <- data.frame("Performance" = performance.transform, "Model" = factor(model), "Method" = factor(method))

cell11 <- performance.transform[model == 1 & method == 1]
cell12 <- performance.transform[model == 1 & method == 2]
cell13 <- performance.transform[model == 1 & method == 3]
cell21 <- performance.transform[model == 2 & method == 1]
cell22 <- performance.transform[model == 2 & method == 2]
cell23 <- performance.transform[model == 2 & method == 3]
cells <- as.matrix(cbind(cell11, cell12, cell13, cell21, cell22, cell23))
par(mfrow = c(1,1))
boxplot(cells)

par(mfrow = c(2, 3))
hist(cell11, main = "")
hist(cell12, main = "")
hist(cell13, main = "")
hist(cell21, main = "")
hist(cell22, main = "")
hist(cell23, main = "")

qqnorm(cell11); qqline(cell11)
qqnorm(cell12); qqline(cell12)
qqnorm(cell13); qqline(cell13)
qqnorm(cell21); qqline(cell21)
qqnorm(cell22); qqline(cell22)
qqnorm(cell23); qqline(cell23)

#preliminary visualization of the data
ggplot(performance.df, aes(Method, Performance, colour = Model)) + geom_point() + scale_x_discrete(labels = c("5NN", "7NN", "Logistic Reg"))
#ggplot(data = performance.df, aes(Method, Performance)) + geom_boxplot()
ggplot(performance.df, aes(Model, Performance, colour = Method)) + geom_point()
#ggplot(data = performance.df, aes(Model, Performance)) + geom_boxplot() 


#preliminary anova and mean tables
anova <- aov(Performance ~ Model*Method, data = performance.df)
#anovaInteraction <- aov(Performance ~ interaction(Model, Method), data = performance.df)
#anovaModel <- aov(Performance ~ Model, data = performance.df)
#anovaMethod <- aov(Performance ~ Method, data = performance.df)
model.tables(anova, type = "means")

#residual analysis to show normality
#performance.res = performance.df
#performance.res$M1.Fit = fitted(anovaInteraction)
#performance.res$M1.Resid = resid(anovaInteraction)
#hist(performance.res$M1.Resid)
#ggplot(performance.res, aes(M1.Fit, M1.Resid)) + geom_point() + xlab("Fitted Values") + ylab("Residuals")
#ggplot(performance.res, aes(sample = M1.Resid)) + stat_qq()
#ggplot(performance.res, aes(M1.Fit, M1.Resid, colour = Method)) +
#  geom_point() + xlab("Fitted Values") + ylab("Residuals") +
#  facet_wrap( ~ Model)
#ggplot(performance.res, aes(M1.Fit, M1.Resid, colour = Model)) +
#  geom_point() + xlab("Fitted Values") + ylab("Residuals") +
#  facet_wrap( ~ Method)


#Two-way Bartlett-test for homogenity of variances

bartlett.test(Performance ~ interaction(Model, Method), data = performance.df) #bartlett.test(cells)

#anova tables
summary(anova)
summary(anovaInteraction)
summary(anovaModel)
summary(anovaMethod)

#Tukey to show mean differences
TukeyHSD(anova, which = "Method")
performance.hsd = data.frame(TukeyHSD(anova, which = "Method")$Method)
performance.hsd$Comparison = row.names(performance.hsd)
ggplot(performance.hsd, aes(Comparison, y = diff, ymin = lwr, ymax = upr)) + geom_pointrange() + ylab("Difference in Mean Performance by Method")

TukeyHSD(anova, which = "Model")

TukeyHSD(anova)
performance.hsd = data.frame(TukeyHSD(anova)$`Model:Method`)
performance.hsd$Comparison = row.names(performance.hsd)
ggplot(performance.hsd, aes(Comparison, y = diff, ymin = lwr, ymax = upr)) + geom_pointrange() + ylab("Difference in Mean Performance by Cell") + coord_flip()


cat("Normality p-values by Factor Method: ")
for(i in unique(factor(y$Method))){
  cat(shapiro.test(y[y$Method==i, ]$Performance)$p.value," ")                     
}

cat("Normality p-values by Factor Model: ")
for(i in unique(factor(y$Model))){
  cat(shapiro.test(y[y$Model==i, ]$Performance)$p.value, " ")
}

