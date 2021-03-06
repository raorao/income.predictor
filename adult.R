setwd("./")

require('randomForest')
require('caret')

set.seed(27)

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

calculate.aggregate.gains <- function(median.gain) {
  function(gain) {
    if (gain == 99999) {
      "99999+"
    } else if (gain > median.gain) {
      "more-than-median"
    } else if (gain > 0) {
      "less-than-median"
    } else if (gain == 0) {
      "none"
    } else {
      "lost"
    }
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

  # create blue.white.collar feature
  frame$blue.white.collar <- frame$occupation
  levels(frame$blue.white.collar) <- list(
    blue.collar = c(
      " Transport-moving",
      " Protective-serv",
      " Priv-house-serv",
      " Other-service",
      " Machine-op-inspct",
      " Handlers-cleaners",
      " Farming-fishing",
      " Craft-repair",
      " Armed-Forces"
    ),

    white.collar = c(
      " Tech-support",
      " Sales",
      " Prof-specialty",
      " Exec-managerial",
      " Adm-clerical"
    )
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

  frame$capital.aggregate.gains <- (frame$capital.gain - frame$capital.loss)
  frame$capital.aggregate.gains.category <- ordered(
    sapply(
      frame$capital.aggregate.gains,
      calculate.aggregate.gains(median(frame$capital.aggregate.gains))
    ),
    c("lost", "none", "less-than-median", "more-than-median", "99999+")
  )


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
    "hours.per.week",
    "capital.loss",
    "capital.gain",
    "capital.aggregate.gains",
    "occupation"
  )

  frame <- frame[,!(colnames(frame) %in% drops)]
  frame
}

original.data <- adult.data
adult.data <- prep.data.frame(original.data)
adult.test <- prep.data.frame(adult.test)

partition <- sample(1:nrow(adult.data), 0.7*nrow(adult.data))
adult.data1 <- adult.data[partition, ]
adult.data2 <- adult.data[-partition, ]


writeLines("data successfully cleaned!\n")

# writeLines('summary!\n')
# summary(adult.data)

writeLines('generating a model!\n')
adult.rf.model = randomForest(
  fifty.k.status ~ .,
  data = adult.data1,
  importance=TRUE,
  mtry = 3
)

pdf("adults.pdf")
varImpPlot(adult.rf.model)
dev.off()

adult.rf.predictions <- predict(adult.rf.model, adult.data2, type="response")

print(confusionMatrix(adult.rf.predictions, adult.data2$fifty.k.status, positive = " >50K"))

precision <- posPredValue(adult.rf.predictions, adult.data2$fifty.k.status)
recall <- sensitivity(adult.rf.predictions, adult.data2$fifty.k.status)

F1 <- (2 * precision * recall) / (precision + recall)

writeLines("your F1 is...!\n")
print(F1)
