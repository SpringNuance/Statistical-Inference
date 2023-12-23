#1
library(quantreg)
?barro
data(barro)

#a
plot(barro)

#b
fitted_barro <- lm(y.net ~., data=barro)
summary(fitted_barro)
library(car)
vif(fitted_barro)
plot(fitted(fitted_barro), resid(fitted_barro), xlab = "Fitted value", ylab = "Residual")
abline(h = 0)

#c
?step

#backward selection 
backward <- step(lm(y.net ~ ., data = barro), direction = "backward")
# result:
# y.net ~ lgdp2 + mse2 + lexp2 + lintr2 + Iy2 + gcony2 + lblakp2 + pol2 + ttrad2
bwfb <- lm(y.net ~ lgdp2 + mse2 + lexp2 + lintr2 + Iy2 + gcony2 + lblakp2 + pol2 + ttrad2, data=barro)

#forward selection 
forward <- step(lm(y.net ~ 1, data = barro), scope = list(lower = lm(y.net ~ 1, data = barro), upper = lm(y.net ~ ., data = barro)),
direction = "forward")
# result: 
# y.net ~ lblakp2 + Iy2 + lgdp2 + gcony2 + lexp2 + ttrad2 + mse2 + pol2 + lintr2

#d.
names(coef(backward)) %in% names(coef(forward))
names(coef(forward)) %in% names(coef(backward))

#e.
# Black Market Premium - lblakp2
# Investment/GDP - Iy2
# Initial Per Capita GDP - lgdp2
# Public Consumption/GDP -gcony2
# Life Expectancy - lexp2
# Growth Rate Terms Trade - ttrad2
# Male Secondary Education - mse2
# Political Instability - pol2
# Human Capital - lintr2

#2
library(glmnet)
library(plotmo)

#a
?glmnet
lasso_barro <- glmnet(as.matrix(barro[, 2:14]), as.matrix(barro[, 1]), alpha = 1)

#b
plot_glmnet(lasso_barro, xvar = "lambda", label = TRUE)

#c
?cv.glmnet
set.seed(08032019)
barro_cv <- cv.glmnet(as.matrix(barro[, 2:14]), as.matrix(barro[, 1]), alpha = 1, nfolds = 10)
set.seed(Sys.time())

plot_glmnet(lasso_barro, xvar = "lambda", label = TRUE)
abline(v = log(barro_cv$lambda.min), lty = 2, col = "blue")
abline(v = log(barro_cv$lambda.1se), lty = 2, col = "red")

barro_lasso_1se <- glmnet(as.matrix(barro[, 2:14]), as.matrix(barro[, 1]), alpha = 1, lambda = barro_cv$lambda.1se)
coef(barro_lasso_1se)