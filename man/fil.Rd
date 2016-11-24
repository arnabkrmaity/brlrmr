\name{fil}
\alias{fil}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
fil
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This provides the estimates using IL method and FIL method as described in the reference.
}
\usage{
fil(data, parameter = NULL, family = binomial, tau = qnorm(0.975),
interaction = FALSE, k = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
The first column of data is binary missing response. The missing observations are denoted by NA. The rest of the columns are covariates or explanatory variables. Note that data is a matrix not a data frame.
}
  \item{parameter}{
%%     ~~Describe \code{parameter} here~~
The starting values of the parameters as (beta, alpha) where beta is the parameters of original model and alpha is the for the missing data model.
}
  \item{family}{
  as in \code{\link{glm}}. \code{brlrmr} currently supports only the  \code{"binomial"} family with links
  }
  \item{tau}{
%%     ~~Describe \code{tau} here~~
Upper 100(1 - tau)\% point of standard Normal distribution. The default is 1.96.
}
  \item{interaction}{
%%     ~~Describe \code{interaction} here~~
TRUE or FALSE, whether to consider interaction in the missing data model. Currenly only one intercation between response and covariates is supported. FALSE by default.
}
  \item{k}{
%%     ~~Describe \code{k} here~~
Which covariate has interaction with response. Takes integer values. User must assign a value if interaction = TRUE.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{n }{number of observations.}
  \item{nmissing }{the number of missing observations.}
  \item{missing.proportion }{proportion of missing observations.}
  \item{beta.hat.il }{parameter estimate of logsitic regression of y on x using IL method.}
  \item{beta.se.hat.il }{standard error using IL method.}
  \item{z.value.il }{Wald Z value using IL method.}
  \item{p.value.il }{p value using IL method.}
  \item{significance.beta.il }{is indicator output whether regressors are significant using IL method, 1 if significant and 0 if not significant.}
  \item{LCL.il }{Lower Confidence Limits of 95\% Confidence Intervals.}
  \item{UCL.il }{Upper Confidence Limits of 95\% Confidence Intervals.}
  \item{alpha.hat.il }{parameter estimate due to missing model using IL.}
  \item{alpha.se.hat.il }{standard error of the them.}
  \item{z.value.il.alpha }{Wald Z value for them.}
  \item{p.value.il.alpha }{p values for them.}
  \item{beta.hat.firth }{parameter estimate of logistic regression of y on x using FIL method.}
  \item{beta.se.hat.firth }{standard error using FIL method.}
  \item{z.value.firth }{Wald Z value using FIL method.}
  \item{p.value.firth }{p value using FIL method.}
  \item{significance.beta.firth }{indicator output whether regressors are significant using FIL method, 1 if significant and 0 if not significant.}
  \item{LCL.firth }{Lower Confidence Limits of 95\% Confidence Intervals.}
  \item{UCL.firth }{Upper Confidence Limits of 95\% Confidence Intervals.}
  \item{alpha.hat.fil }{parameter estimate due to missing model using FIL.}
  \item{alpha.se.hat.fil }{standard error of the them.}
  \item{z.value.fil.alpha }{Wald Z value for them.}
  \item{p.value.fil.alpha }{p values for them.}
}
\references{
%% ~put references to the literature/web site here ~
Bias Reduction in Logistic Regression with Missing Responses when the Missing Data Mechanism is Nonignorable. Maity A.K., Pradhan V. and Das, U.
}
\author{
%%  ~~who you are~~
Arnab Kumar Maity
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}

%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.

\examples{
\dontrun{
#############################################
########### Simulated Example ###############
#############################################
require(boot)  # load boot

n <- 100  # number of observations

# parameter definition
beta0 <- 1
beta1 <- 1
beta2 <- 1
beta3 <- 1
beta4 <- 1

# parameter definition for missing indicator
alpha0 <- -1.1
alpha1 <- -1
alpha2 <- 1
alpha3 <- 1
alpha4 <- 1
alpha5 <- -1

parameter <- c(beta0, beta1, beta2, beta3, beta4,
               alpha0, alpha1, alpha2, alpha3, alpha4, alpha5)

x1 <- rnorm(n)  # random sample from N(0, sd = 1)
x2 <- rnorm(n)  # random sample from N(0, sd = 1)
x3 <- rnorm(n)  # random sample from N(0, sd = 1)
x4 <- rnorm(n)  # random sample from N(0, sd = 1)

probability.y <- inv.logit(beta0 + beta1*x1 + beta2*x2 + beta3*x3 + beta4*x4)
# probabilities to generate y

bernoulli <- function(probability)
  # This will return random sample from a Bernoulli Distribution
  # with given probability
{
  return(rbinom(1, size = 1, probability))
}

y <- sapply(probability.y, bernoulli)   # response

probability.r <- inv.logit(alpha0 + alpha1*x1 + alpha2*x2 + alpha3*x3
                           + alpha4*x4 + alpha5*y) # probabilities to generate r

r <- sapply(probability.r, bernoulli)   # missing indicator

original.data  <- cbind(y, x1, x2, x3, x4, r)
observed.data  <- subset(original.data, r == 0)
missing.data   <- subset(original.data, r == 1)
missing.data[, 1] <- NA

data <- rbind(observed.data, missing.data)[, 1:5]

fil(data, parameter, family = binomial(link = "logit"))
}

\dontrun{
#############################################
##### Real data example with separation #####
#############################################
require(mice)  # load mice

data <- cbind(nhanes[, 3], nhanes[, 1])
n    <- nrow(data) # sample size

# age is entered in the model as two dummy variables
age2 <- rep(0, n)
age3 <- rep(0, n)

for(i in 1:n)
{
  if(data[i, 2] == 2)
  {
    age2[i] = 1
  }
  if(data[i, 2] == 3)
  {
    age3[i] = 1
  }
}

data <- cbind(nhanes[, 3], age2, age3)

# parameter definition
beta0 <- 1
beta1 <- 1
beta2 <- 1

# parameter definition for missing indicator
alpha0 <- 2
alpha1 <- -10
alpha2 <- -7
alpha3 <- -10

parameter <- c(beta0, beta1, beta2,
               alpha0, alpha1, alpha2, alpha3)

data[which(data[, 1] == 1), 1] <- 0
data[which(data[, 1] == 2), 1] <- 1

fil(data, parameter, family = binomial(link = "logit"))
fil(data, family = binomial(link = "logit"))  # Result should be same
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
