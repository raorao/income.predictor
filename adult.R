setwd("./")

adult.data <- read.csv('adult.data', header = F)
adult.test <- read.csv('adult.test', header = F, comment.char = '|')
writeLines("data loaded!\n")

adult.names <- c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours.per.week","native.country", "fifty.k.status")

set.nas <- function(frame, colname, na.char = ' ?') {
  index <- grep(paste('^', colname, '$', sep = ""), colnames(frame))
  frame[frame[,index] == na.char, index] <- NA
  frame[,index] <- droplevels(frame[,index])
  frame
}

prep.data.frame <- function(frame) {
  #set colnames
  colnames(frame) <- adult.names


  #clean NAs
  frame <- set.nas(frame, 'workclass')
  frame <- set.nas(frame, 'occupation')
  frame <- set.nas(frame, 'native.country')


  # remove data points without occupation or workclass
  frame <- frame[!(is.na(frame$occupation) & is.na(frame$workclass)), ]

  #remove data points without country
  frame <- frame[!(is.na(frame$native.country)), ]

  # standardize fifty.k.status
  levels(frame$fifty.k.status) <- c(" <=50K", " >50K")

  # create martal.status.cleaned feature
  frame$marital.status.cleaned = frame$marital.status
  levels(frame$marital.status.cleaned) <- list(
    never.married = c(' Never-married'),
    married = c(' Married-civ-spouse', ' Married-AF-spouse'),
    complicated = c(' Married-spouse-absent', ' Separated', ' Divorced', ' Widowed')
  )

  # create workclass.cleaned feature
  frame$workclass.cleaned = frame$workclass
  levels(frame$workclass.cleaned) <- list(
    government = c(' Federal-gov', ' Local-gov', ' State-gov'),
    self.employed = c(' Self-emp-inc', ' Self-emp-not-inc'),
    never.worked = c(' Never-worked'),
    without.pay = c(' Without-pay'),
    private = c(' Private')
  )

  # prune to only necessary features
  frame <- frame[, -c(2,3,5,6,8)]

  frame
}

adult.data <- prep.data.frame(adult.data)
adult.test <- prep.data.frame(adult.test)
writeLines("data successfully cleaned!\n")

writeLines('summary!\n')
summary(adult.data)
