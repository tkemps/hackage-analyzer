library(data.table)
library(ggplot2)

ps <- fromJSON("~/devel/hackage-analyzer/full-packages-meta-data.json")
packs <- data.table(
  packageName = factor(ps$`_packageName`),
  description = ps$`_description`,
  homepage = ps$`_homepage`,
  sourceRepository = ps$`_sourceRepository`,
  bugTracker = ps$`_bugTracker`,
  link = ps$`_link`,
  downloadsTotal = ps$`_totalDownloads`,
  downloads30days = ps$`_thirtyDaysDownloads`,
  authors = factor(ps$`_authors`),
  maintainers = factor(ps$`_maintainers`),
  noVersions = sapply(ps$`_versions`, length),
  noExtensions = sapply(ps$`_extensionsDeclared`[[1]], nrow),
  uploaded = as.Date(ps$`_uploaded`),
  rating = ps$`_rating`,
  stringsAsFactors = F)
packs$daysSinceUpload <- as.integer(as.Date("2018-01-05") - packs$uploaded)
summary(packs)

ggplot(packs, aes(daysSinceUpload)) +
  geom_histogram(bins = 50)
ggplot(packs[downloads30days>1000], aes(daysSinceUpload)) +
  geom_histogram(bins = 50) +
  scale_y_log10()
ggplot(packs, aes(downloads30days)) +
  geom_histogram(bins = 50) +
  scale_x_log10()
ggplot(packs, aes(daysSinceUpload, downloads30days)) +
  geom_point() +
  geom_smooth() +
  scale_y_log10()
ggplot(packs, aes(noVersions, downloads30days)) +
  geom_point() +
  geom_smooth() +
  scale_y_log10()
ggplot(packs, aes(downloads30days, rating)) +
  geom_point() +
  geom_smooth()
ggplot(packs, aes(noVersions, rating)) +
  geom_point() +
  geom_smooth()

packs[downloads30days>1000 & daysSinceUpload>365, packageName]

summary(lm(downloads30days ~ daysSinceUpload + I(daysSinceUpload^2) + I(daysSinceUpload^3), data=packs))
summary(lm(daysSinceUpload ~ downloads30days + I(downloads30days^2) + I(downloads30days^3) + I(downloads30days^4), data=packs))
summary(lm(downloads30days ~ daysSinceUpload + noVersions, data=packs))
summary(lm(downloads30days ~ noVersions, data=packs))

summary(lm(rating ~ downloads30days + noVersions, data=packs))
