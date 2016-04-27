setwd("./")

require('randomForest')
require('caret')

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

calculate.hour.category <- function(hourValue) {
  if (hourValue > 60) {
    'over-employed'
  } else if (hourValue > 40) {
    'with-overtime'
  } else if (hourValue == 40) {
    'standard'
  } else if (hourValue > 25) {
    'part-time'
  } else {
    'under-employed'
  }
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

  #remove data points without occupation
  frame <- frame[!(is.na(frame$occupation)), ]

  #convert education to ordinal value
  frame$education = ordered(
    frame$education, levels= c(
      " Preschool",
      " 1st-4th",
      " 5th-6th",
      " 7th-8th",
      " 9th",
      " 10th",
      " 11th",
      " 12th",
      " HS-grad",
      " Prof-school",
      " Assoc-acdm",
      " Assoc-voc",
      " Some-college",
      " Bachelors",
      " Masters",
      " Doctorate"
    )
  )

  # standardize fifty.k.status
  levels(frame$fifty.k.status) <- c(" <=50K", " >50K")

  # create martal.status.cleaned feature
  frame$marital.status.cleaned <- frame$marital.status
  levels(frame$marital.status.cleaned) <- list(
    never.married = c(' Never-married'),
    married = c(' Married-civ-spouse', ' Married-AF-spouse'),
    complicated = c(' Married-spouse-absent', ' Separated', ' Divorced', ' Widowed')
  )

  # create workclass.cleaned feature
  frame$workclass.cleaned <- frame$workclass
  levels(frame$workclass.cleaned) <- list(
    government = c(' Federal-gov', ' Local-gov', ' State-gov'),
    self.employed = c(' Self-emp-inc', ' Self-emp-not-inc'),
    never.worked = c(' Never-worked'),
    without.pay = c(' Without-pay'),
    private = c(' Private')
  )

  frame$american.native <- (frame$native.country == ' United-States')


  frame$hours.per.week.category <- sapply(frame$hours.per.week, calculate.hour.category)

  frame$hours.per.week.category <- ordered(
    frame$hours.per.week.category,
    levels = c(
      "over-employed",
      "with-overtime",
      "standard",
      "part-time",
      "under-employed"
    )
  )

  # prune to only necessary features
  drops <- c(
    "workclass",
    "fnlwgt",
    "education.num",
    "marital.status",
    "relationship",
    'native.country',
    "hours.per.week"
  )

  frame <- frame[,!(colnames(frame) %in% drops)]
  frame
}

adult.data <- prep.data.frame(adult.data)
adult.test <- prep.data.frame(adult.test)

partition <- sample(1:nrow(adult.data), 0.5*nrow(adult.data))
adult.data1 <- adult.data[sample(1:nrow(adult.data), 0.5*nrow(adult.data)), ]
adult.data2 <- adult.data[-sample(1:nrow(adult.data), 0.5*nrow(adult.data)), ]


writeLines("data successfully cleaned!\n")

writeLines('summary!\n')
summary(adult.data)

writeLines('generating a model!\n')
adult.rf.model = randomForest(fifty.k.status ~ ., data = adult.data1)

pdf("adults.pdf")
varImpPlot(adult.rf.model)
dev.off()

adult.rf.predictions <- predict(adult.rf.model, adult.test, type="response")

print(confusionMatrix(adult.rf.predictions, adult.test$fifty.k.status))
