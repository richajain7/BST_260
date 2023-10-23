filename <- "rdas/murders.csv"
dat <- read.csv(filename)
out <- "rdas"
dat <- save(dat, file = out)
