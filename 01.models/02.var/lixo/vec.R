library(vars)

SR <- matrix(NA, nrow = 4, ncol = 4)
SR[4, 2] <- 0
LR <- matrix(NA, nrow = 4, ncol = 4)
LR[1,2:4] <- 0
LR[2:4,4] <- 0
svecm <- SVEC(vec.can, r = 2, LR = LR, SR = SR, max.iter = 201, lrtest = TRUE, boot = FALSE)
svecm.irf <- irf(svecm, impulse = "e", response = "rw", boot = FALSE, cumulative = FALSE, runs = 100)
svecm.fevd <- fevd(svecm)
