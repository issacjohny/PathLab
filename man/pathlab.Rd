\name{pathlab_fit}
\alias{pathlab_fit}
\alias{validate_spec}
\alias{prepare_sample}
\alias{compile_equations}
\alias{fit_equations}
\alias{evaluate_estimands}
\alias{bootstrap_estimands}
\alias{native_spec}
\alias{paths}
\title{Observed-variable conditional-process graphs}
\description{Validate explicit acyclic observed-variable models, compile regression
equations with continuous mediators, fit linear or binary logistic outcomes, and
evaluate conditional path associations using a paired whole-case percentile
bootstrap. Categorical predictors, moderators and covariates use explicit
indicator contrasts against their declared reference levels. Version 0.4.6 is a
release candidate; its build and Desktop evidence are recorded
separately from numerical tests.}
\usage{
pathlab_fit(data, spec,
  settings = list(outcome_family = "linear", categorical = list(), probes = list()),
  draws = 1000L, seed = 419L, robust = TRUE, alpha = .05, indices = NULL,
  progress = NULL)
}
\arguments{
\item{data}{Data frame of prepared observed variables. Mediators are continuous
numerical scores. Categorical exogenous variables require explicit typed level
and reference declarations. A logistic outcome passed to the core API must be
numerical 0/1 with both values present on the common analysis sample.}
\item{spec}{Graph schema 1 list: nodes, edges, moderations and schema_version.}
\item{settings}{Versioned source settings with \code{outcome_family} equal to
\code{"linear"} or \code{"logistic"}; \code{categorical} is a list of declarations
with \code{node_id}, ordered \code{levels} and \code{reference}; \code{probes} is a
list of moderator node IDs and values. Categorical levels are two to ten distinct
numerical codes or text labels of one type. Settings schema 3 permits three
exogenous moderators and specifies automatic probe and interval quantile
policies. Historical settings retain their own limits. Unsupported requests are
errors.}
\item{draws}{200 to 5000 paired case resamples.}
\item{seed}{Nonnegative R seed. RNG kind and prior state are restored.}
\item{robust}{TRUE for OLS HC3 and generalized logistic HC3 covariance, with
normal coefficient references. FALSE uses classical OLS covariance and residual
t references for linear equations, and model-based information covariance and
normal z references for logistic equations. The logistic overall model test is
the model-based likelihood-ratio chi-square under either choice.}
\item{alpha}{Two-sided interval error probability.}
\item{indices}{Optional draws by complete-case N matrix of one-based row indices.
Supply the same saved indices for exact cross-language bootstrap comparisons.}
\item{progress}{Optional function taking completed and requested draw counts.
Called before resampling, at intervals of ten draws and after resampling. Errors
or interrupts propagate immediately and restore the prior RNG state. The native
interface uses this hook for progress and cooperative jamovi restart checks.}
}
\value{A versioned list containing the original graph, fixed sample provenance,
compiled equations, coding counts, coefficients, covariance, conditional
estimands and paired contrasts, accepted draw identities, bootstrap counts and
distribution, diagnostics and software provenance. Categorical X estimands
identify their focal contrast. Binary-outcome results identify their log-odds
statistical-path-product scale; logistic equations retain likelihood and
convergence diagnostics. The native \code{paths()} interface returns jamovi
result classes, with general-mode effects in consecutive visible
\code{effectPages$items} tables and the complete public reproduction JSON.
Optional native Image outputs display effect forests, fitted interaction means
and analytic Johnson--Neyman path-slope regions. Their saved states contain only
aggregate coordinates.}
\details{All equations use the same complete-case sample. No implicit centering,
standardization, score construction or imputation occurs. Indirect quantities are
statistical path products, not identified causal effects. Categorical moderator
probes default to all declared levels; numerical probes default to fixed
original-sample percentiles. The original indicator design is retained in each
resample, so missing resampled levels can cause rank failure. At least 95 percent
of the requested draws must succeed and failed draws are not replaced.

Every selected arrow and required interaction hierarchy term remains explicit.
Categorical moderators have level comparisons, not numerical derivatives or
Johnson--Neyman regions. Numerical derivatives and univariate Johnson--Neyman
slices may be conditional on categorical levels. Other numerical equation
predictors are held at complete-case means and categorical predictors at their
declared references for the diagnostic contrasts.

Only the outcome equation may use logistic fitting; mediator equations remain
OLS. Column scaling preserves original-unit parameters and covariance. Original
logistic fits require certified separation checks, convergence and full-rank
weighted information. Linear-program work and iteration limits are enforced;
uncertifiable or over-budget numerical cases fail conservatively.
Binary-outcome path products are not natural causal effects, probability
effects, odds ratios of indirect effects, marginal total effects or coefficients
from a reduced logistic equation that omits the mediators.

Native \code{paths()} controls include \code{categoricalCoding} role/reference
rows, \code{outcomeFamily}, and \code{logisticEvent}. Factor level order is
retained; raw text and numerical category levels are sorted before common
sampling. Numeric codes require an explicit categorical row. The native event
maps an exact numeric code, text label or factor label to 1, the other level to
0, and preserves missing values. Blank event input is defined only for levels
0 and 1 and selects 1. The core API instead takes explicit coding declarations
and an already numerical 0/1 outcome.

See the package README,
METHODS and validation report for conventions and the current feature boundary.
They document known near-boundary separation and pathological failure-code
differences from the private Python reference, remaining compatibility work,
and version-specific execution evidence.
The smaller exported functions provide an independently testable computation
pipeline; their list arguments are the corresponding preceding pipeline outputs.}
\examples{
example_dir <- system.file("examples", package = "pathlab")
data <- read.csv(file.path(example_dir, "serial-first-stage.csv"))
model <- jsonlite::fromJSON(file.path(example_dir,
    "serial-first-stage-model.json"), simplifyVector = FALSE)
fit <- pathlab_fit(data, model$graph, model$settings,
    draws = 200L, seed = 419L, robust = FALSE)
fit$sample$n
fit$bootstrap$successful
}
\references{Hayes (2015), \doi{10.1080/00273171.2014.962683}.}
