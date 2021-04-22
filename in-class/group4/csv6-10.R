#cv labs 6-10

#lab6
library(cvTools)
library(robustbase)
data("coleman")

fit <- lmrob(Y ~ ., data=coleman)
cvFit(fit, data = coleman, y = coleman$Y, cost = rtmspe,
      K = 5, R = 10, costArgs = list(trim = 0.1), seed = 1234)

cvFit(lmrob, formula = Y ~ ., data = coleman, cost = rtmspe,
      K = 5, R = 10, costArgs = list(trim = 0.1), seed = 1234)

call <- call("lmrob", formula = Y ~ .)
cvFit(call, data = coleman, y = coleman$Y, cost = rtmspe,
      K = 5, R = 10, costArgs = list(trim = 0.1), seed = 1234)

#cv7
set.seed(2143)
cvFolds(20, K = 5, type = "random")
cvFolds(20, K = 5, type = "consecutive")
cvFolds(20, K = 5, type = "interleaved")
cvFolds(20, K = 5, R = 10)

#cv8
fitLts <- ltsReg(Y ~ ., data = coleman)
cvFitLts <- cvLts(fitLts, cost = rtmspe, K = 5, R = 10,
                  fit = "both", trim = 0.1, seed = 1234)
cvFitLts
cvReshape(cvFitLts)

#cv9
folds <- cvFolds(nrow(coleman), K = 5, R = 10)
fitLm <- lm(Y ~ ., data = coleman)
cvFitLm <- cvLm(fitLm, cost = rtmspe,
                folds = folds, trim = 0.1)

fitLmrob <- lmrob(Y ~ ., data = coleman)
cvFitLmrob <- cvLmrob(fitLmrob, cost = rtmspe,
                      folds = folds, trim = 0.1)

fitLts <- ltsReg(Y ~ ., data = coleman)
cvFitLts <- cvLts(fitLts, cost = rtmspe,
                  folds = folds, trim = 0.1)

cvSelect(LS = cvFitLm, MM = cvFitLmrob, LTS = cvFitLts)

fitLts50 <- ltsReg(Y ~ ., data = coleman, alpha = 0.5)
cvFitLts50 <- cvLts(fitLts50, cost = rtmspe, folds = folds,
                    fit = "both", trim = 0.1)
fitLts75 <- ltsReg(Y ~ ., data = coleman, alpha = 0.75)
cvFitLts75 <- cvLts(fitLts75, cost = rtmspe, folds = folds,
                    fit = "both", trim = 0.1)
cvSelect("0.5" = cvFitLts50, "0.75" = cvFitLts75)

#cv10

tuning <- list(tuning.psi = c(3.443689, 4.685061))
cvTuning(lmrob, formula = Y ~ ., data = coleman, tuning = tuning,
         cost = rtmspe, K = 5, R = 10, costArgs = list(trim = 0.1),
         seed = 1234)

call <- call("lmrob", formula = Y ~ .)

cvTuning(call, data = coleman, y = coleman$Y, tuning = tuning,
         cost = rtmspe, K = 5, R = 10, costArgs = list(trim = 0.1),
         seed = 1234)
