#' process the data
#' This function does some data processing.
#' @param x an object inherited from list.
#' @return data for linear models.
#' @importFrom dplyr inner_join left_join group_by
#' @importFrom nlme lme
#' @importFrom lmtest bptest
#' @importFrom car vif
#' @export
##as.numeric(factor(demo1$SEX,levels = unique(demo1$SEX),exclude = NULL)) 将category改成了numeric

DataPlot <- function(x){
  demo1 = left_join(x$adsl,x$adae,by='SUBJID')
  demo1$SEX = as.numeric(factor(demo1$SEX,levels = unique(demo1$SEX),exclude = NULL))
  demo1$RACE = as.numeric(factor(demo1$RACE,levels = unique(demo1$RACE),exclude = NULL))
  demo1$ATRT = as.numeric(factor(demo1$ATRT,levels = unique(demo1$ATRT),exclude = NULL))
  demo2 = left_join(demo1,x$biomark,by='SUBJID')
  demo2$BMMTR1 = as.numeric(factor(demo2$BMMTR1,levels = unique(demo2$BMMTR1),exclude = NULL))
  dataset = demo2|>
    group_by(SUBJID)|>
    summarize(y = DTH,
              x1_sex = SEX,
              x2_race = RACE,
              x3_atrt = ATRT,
              x4_biomark = BMMTR1,
              x5_age = AGE
              )
  #加了qualitative变量之后可以plot看关系，或者用scatterplotMatrix
  plot(dataset$x4_biomark,dataset$y, main = "the relationship between sex and death", xlab = "sex", ylab = "death")
}

DataProcess <- function(x){
  demo1 = left_join(x$adsl,x$adae,by='SUBJID')
  demo1$SEX = as.numeric(factor(demo1$SEX,levels = unique(demo1$SEX),exclude = NULL))
  demo1$RACE = as.numeric(factor(demo1$RACE,levels = unique(demo1$RACE),exclude = NULL))
  demo1$ATRT = as.numeric(factor(demo1$ATRT,levels = unique(demo1$ATRT),exclude = NULL))
  demo2 = left_join(demo1,x$biomark,by='SUBJID')
  demo2$BMMTR1 = as.numeric(factor(demo2$BMMTR1,levels = unique(demo2$BMMTR1),exclude = NULL))
  dataset = demo2|>
    group_by(SUBJID)|>
    summarize(y = DTH,
              x1_sex = SEX,
              x2_race = RACE,
              x3_atrt = ATRT,
              x4_biomark = BMMTR1,
              x5_age = AGE)

  m1 <- lm(y ~ (x1_sex+x2_race+x3_atrt+x5_age), random=~1|x4_biomark, data=unique(dataset))
  print(summary(m1))
  m2  = lm(y ~ (x1_sex+x2_race+x3_atrt+x4_biomark + x5_age), data = unique(dataset))
  print(summary(m2))
}


test <- function(x){
  demo1 = left_join(x$adsl,x$adae,by='SUBJID')
  demo1$SEX = as.numeric(factor(demo1$SEX,levels = unique(demo1$SEX),exclude = NULL))
  demo1$RACE = as.numeric(factor(demo1$RACE,levels = unique(demo1$RACE),exclude = NULL))
  demo1$ATRT = as.numeric(factor(demo1$ATRT,levels = unique(demo1$ATRT),exclude = NULL))
  demo2 = left_join(demo1,x$biomark,by='SUBJID')
  demo2$BMMTR1 = as.numeric(factor(demo2$BMMTR1,levels = unique(demo2$BMMTR1),exclude = NULL))
  dataset = demo2|>
    group_by(SUBJID)|>
    summarize(y = DTH,
              x1_sex = SEX,
              x2_race = RACE,
              x3_atrt = ATRT,
              x4_biomark = BMMTR1,
              x5_age = AGE)
  m1 <- lme(y ~ (x1_sex+x2_race+x3_atrt+x5_age), random=~1|x4_biomark, data=unique(dataset))
  print(summary(m1))
  m2  = lm(y ~ (x1_sex+x2_race+x3_atrt+x4_biomark + x5_age), data = unique(dataset))
  print(summary(m2))
  print(bptest(m2))
  print(vif(m2))
}

