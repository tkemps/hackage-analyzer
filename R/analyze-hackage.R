library(data.table)
library(ggplot2)
library(DBI)
library(RPostgreSQL)
library(MASS)

drv <- dbDriver("PostgreSQL")
tryCatch({
  con <- dbConnect(drv, host = "127.0.0.1", dbname = "postgres")
  p <- data.table(dbGetQuery(con, "SELECT * FROM hackage.package_snapshot;"))
  d <- data.table(dbGetQuery(con, "SELECT * FROM hackage.dependency;"))
  b <- data.table(dbGetQuery(con, "SELECT * FROM hackage.build_status;"))
}, finally = {
  dbDisconnect(con)})

p[, `:=`(
  daysSinceUpload = as.integer(Sys.Date() - as.Date(p$uploaded))
  )]

setkey(p, package_name, version, run_date)
setkey(d, package_name, version, run_date, package_name_dependency)
setkey(b, package_name, version, run_date, ghc_version)

x <- substr(p$versions, start = 2, stop = 1000)
x <- substr(x, start = 1, stop = nchar(x) - 1)
p[, numVersions := sapply(strsplit(x, ","), length)]

p[, rating1 := ifelse(is.na(rating),0,rating)]
p[, sizeF := factor(ifelse(size < 1000, 1, ifelse(size > 1e5, 3, 2)))]

b1 <- b[, .(anyOk = any(build_status == "OK")), by = .(package_name, version, run_date)]
setkey(b1, package_name, version, run_date)
p1 <- b1[p[run_date >= as.Date("2018-01-30")]]
p1[, anyOk := ifelse(is.na(anyOk), FALSE, anyOk)]

summary(p1)

ggplot(p1, aes(daysSinceUpload)) +
  geom_histogram(bins = 50)
ggplot(p1[downloads_30days > 1000], aes(daysSinceUpload)) +
  geom_histogram(bins = 50) +
  scale_y_log10()
ggplot(p1, aes(x = size, y = downloads_30days)) +
  geom_point(size = 0.2) +
  geom_smooth() +
  scale_y_log10() +
  scale_x_log10()
ggplot(p1, aes(downloads_30days)) +
  geom_histogram(bins = 50) +
  scale_x_log10()
ggplot(p1, aes(daysSinceUpload, downloads_30days)) +
  geom_point() +
  geom_smooth() +
  scale_y_log10()
ggplot(p1, aes(numVersions, downloads_30days)) +
  geom_point(size=0.2) +
  geom_smooth() +
  scale_y_log10()
ggplot(p1, aes(downloads_30days, rating)) +
  geom_point(size=0.2) +
  geom_smooth()
ggplot(p1, aes(numVersions, rating)) +
  geom_point(size=0.2) +
  geom_smooth()
ggplot(p1, aes(anyOk, downloads_30days)) +
  geom_boxplot() +
  scale_y_log10()

summary(lm(downloads_30days ~ daysSinceUpload + I(daysSinceUpload^2) + I(daysSinceUpload^3), data=p1))
summary(lm(daysSinceUpload ~ downloads_30days + I(downloads_30days^2) + I(downloads_30days^3) + I(downloads_30days^4), data=p1))
summary(stepAIC(lm(log(downloads_30days, 10) ~ daysSinceUpload + I(daysSinceUpload^2) +
             numVersions + I(numVersions^2) +
             rating1 + size*sizeF + anyOk,
           data = p1[downloads_30days>0]),
           k = log(nrow(p1), 2)))

