# Statistical methods

This describes the bounded observed-variable subset in the 0.4.6 release
candidate. Implementation and numerical validation do not establish causal
identification or suitability for a particular study. Numerical tests and
source-specific Desktop lifecycle evidence are recorded separately in
[VALIDATION.md](VALIDATION.md).

PathLab fits observed-variable acyclic graphs with continuous-mediator OLS
regressions and an optional binary logistic outcome equation. The declared edges, interaction factor lists, displayed equations
and matrix columns share one compiler. Regression uses `stats::lm.fit` and QR
rank checks. An independent reference fits handwritten formulas with `stats::lm`.
There is no global SEM fit test, latent measurement model, clustering adjustment,
causal identification or full/partial mediation classification.

All graph columns define one complete-case sample. Missing source row numbers and
missing node IDs are retained; R row numbers are one-based. Nonfinite numerical
values, factors passed as continuous, constants, cycles, unused mediators,
duplicate IDs and rank-deficient designs are rejected. Required residual degrees
of freedom follow the source engine: N must exceed the column count plus two.
jamovi delivers data after its own filter and user-missing semantics. Reported
source N and R source-row positions refer to the data delivered to the analysis,
not an assertion about the unfiltered spreadsheet row count or original row IDs.
Desktop verification of these semantics is tracked separately from the core
complete-case tests. Active jamovi weights are explicitly rejected; neither
weighted fitting nor weighted bootstrap inference is implemented.

The legacy native M1 and M2 assignments are optional. All selected arrows and moderation
attachments still require their assigned roles; an empty role never silently
removes a selected path. Covariates are added only to the explicitly selected
M1, M2 and/or Y equations. A shared covariate is represented by one graph node
with an arrow to each assigned equation, and enters the common sample even when
it appears in only one equation. Covariates cannot reuse a focal, mediator or
moderator column. These are exogenous adjustment terms, not additional focal
routes or implicit mediators.

The general native editor maps ordered mediator, moderator and covariate lists
to M1–M10, W/Z/V and C1–C20 slots. It compiles only the explicit source/target
path rows and interaction rows into the same graph used by the R API. Reordering
a list changes its slot mapping; the resolved graph and equations identify the
actual columns. No path or interaction is inferred from mediator list order.
The default legacy editor preserves the interpretation of saved 0.1.1 options;
switching modes selects the active editor rather than merging both specifications.

An interaction attachment has one or two distinct exogenous moderators, neither
equal to its source or target. A joint X×W×Z term in an equation requires the
explicit main-effect paths X, W and Z to that equation and all three pairwise
terms X×W, X×Z and W×Z. The W×Z term can be declared as Z moderating the W
path. Missing hierarchy terms, duplicate equivalent factor sets and unsupported
four-factor terms are rejected. Covariate equation assignments in general mode
are ordinary explicit covariate-to-target path rows.

No centering, standardization, reverse coding, imputation or score aggregation
is implicit. Ordinal numeric scores are quantitative unless explicitly declared
categorical and require an equal-spacing/linear-score justification when modeled
as scores. Mediators remain continuous numerical scores.

## Categorical coding and conditional associations

X, moderators and covariates may be categorical. The R API requires an explicit
declaration containing a node ID, ordered levels and reference level. Levels are
two to ten distinct finite numerical codes or nonempty text labels of one type;
numeric codes and text labels are never mixed. One indicator column is created
for each non-reference level, retaining its original declared level index.
Interaction columns are Cartesian products of the component indicator or
quantitative bases, using the same explicit hierarchy as continuous graphs.
No main effect, lower-order interaction or selected path is added or removed.

The native `categoricalCoding` rows identify a role and its exact reference.
Native factors retain their declared level order; raw text and numerical columns
resolve levels from their sorted observed values before listwise sampling.
Numeric columns become categorical only through an explicit row. The observed
level set on the common complete-case sample must equal the declared set. A level
removed entirely by listwise deletion is an error, rather than a new coding
scheme. Coding output records the levels, reference and common-sample counts.

Each categorical-X non-reference level defines a separate focal contrast against
the reference. Conditional edge contrasts enter the route products, totals and
paired bootstrap differences separately. Native tables retain the focal contrast
in their labels. No pairwise contrast between two non-reference X levels is
implicitly substituted. Other categorical predictors are held at their declared
references when evaluating simple-slope diagnostics, unless a stated moderator
condition sets their level.

Categorical moderators default to all declared levels, while custom probes must
select valid levels. They have level comparisons, not numerical derivatives or
Johnson–Neyman regions. A numerical moderator's derivative can still depend on
the categorical level; the reported condition and focal contrast identify that
estimand. Polynomial-degree checks include dependence on all moderators, so a
derivative conditional on category is not called a global constant index.

The original complete-case indicator design is retained in every bootstrap
replication. A resample that omits a needed level is rank deficient and fails;
the module does not drop its indicator, choose a new reference, or refactor the
resample. Sparse categories therefore require particular attention to the
recorded failure count.

## Linear and logistic inference

Classical covariance is residual MSE times the QR-based inverse cross-product,
with residual t references for coefficients and an F model test. HC3 covariance
is `(X'X)^-1 X' diag((residual/(1-h))^2) X (X'X)^-1`; coefficients use a normal
reference. The overall model test uses a residual-df HC3 Wald F, matching the
source engine's statsmodels OLS convention. The interaction-block test uses an
HC3 Wald chi-square. Simple slopes use the same coefficient
covariance and reference. These choices match the selected Python contract.

For a binary outcome, the R core requires both numerical 0 and 1. The native
`outcomeFamily` control selects logistic fitting, and `logisticEvent` explicitly
maps a numeric code, text label or factor label to 1; the other level becomes 0
and missing values stay missing. Blank event input is defined only for levels
0 and 1 and selects 1. The mapping is recorded. Mediator equations continue to
use OLS and their chosen classical or HC3 inference.

Logistic maximum likelihood uses Newton iterations on an uncentered matrix with
each column divided by its maximum absolute value. Coefficients and covariance
are converted back to the original units. Original-sample fitting first checks
the column-scaled signed-design separation cone using a bounded linear program.
The implementation refines its active basis and checks primal and dual
certificates. Complete or quasi-complete separation is rejected; an uncertifiable
numerical result is reported as nonconvergence, not accepted as overlap. The fit
limits linear-program tableau work as well as iterations and fails with
nonconvergence if that budget is exhausted; an incomplete certificate never
permits the model to proceed. The fit
also requires finite parameters, convergence within 100 iterations and full-rank
weighted information. Bootstrap fits retain the original design and check their
own convergence, finite parameters and weighted information; they do not run the
original-sample linear program for every draw.

Let `p = logistic(X beta)`, `W = diag(p*(1-p))`,
`B = inverse(X' W X)` and `h_i = p_i*(1-p_i) * x_i' B x_i`.
Model-based logistic covariance is `B`. The generalized logistic HC3 covariance
is `B X' diag(((y-p)/(1-h))^2) X B`, evaluated through the scaled design and
returned in the original units. Logistic coefficient and simple-slope inference
uses normal z references under either covariance choice. The overall logistic
model test is the model-based likelihood-ratio chi-square; an interaction-block
test is a covariance-based Wald chi-square. Log likelihood, null log likelihood,
McFadden pseudo-R2, AIC, BIC and convergence are recorded. OLS R2/RMSE and Pearson
cross-equation OLS residual correlations are inapplicable to the mixed
linear/logistic system and are not substituted with another definition.

Binary-outcome indirect and summed associations are statistical path products on
the log-odds outcome-equation scale. They are not natural causal effects, risk or
probability differences, or odds ratios of indirect effects. Logistic
noncollapsibility prevents interpreting the summed total path association as a
marginal total effect or the coefficient from a reduced logistic equation that
omits the mediators.

The focused categorical/logistic fixtures passed against the unchanged private
Python engine, independent NumPy QR/Newton/HC3 algebra, and independent R
`glm` plus manual information/HC3 calculations. Their shared resamples validate
accepted draws and all paired intervals. This does not imply identical failure
codes for every pathological input. Artificial completely/quasi-separated
bootstrap examples are rejected by both implementations, while R may record
`SEPARATION` or `SINGULAR_DESIGN` where Python records `NUMERICAL_FAILURE`.

A 114-case separation stress audit matched Python/HiGHS on all 96 well-scaled
cases and 15 of 18 near-boundary cases. In the three discrepancies, duplicated
design points carry opposing outcomes and therefore establish mathematical
overlap. At gap `1e-8`, R certifies overlap while HiGHS' accepted tolerance yields
a separation label. At gaps `1e-10` and `1e-12`, R refuses an uncertifiable
candidate and reports nonconvergence while Python reports separation. The
conservative R behavior and these three exceptions are documented limitations,
not universal separation-decision parity.

A route association multiplies the conditional edge slopes at fixed exogenous
moderator values. All explicitly declared component paths remain in the fit.
Different routes are summed jointly. Derivatives use the product rule, including
all assignments of partial derivatives to edges. Polynomial degree determines
whether an index is constant, conditional on other moderators, or varies with the
same moderator. A varying derivative is never called a constant linear index.

Whole observational rows are sampled with replacement. Every equation uses the
same row indices in each replication. Fixed original-sample numerical moderator
probes default to the 16th, 50th and 84th percentiles; categorical probes default
to declared levels. Source settings schema 2/3 permit
linear/type-7 or Weibull/type-6 probe quantiles; interval quantiles remain type 7.
Exact custom probes are retained and extrapolation is flagged. Multiple marginal
probe conditions do not establish joint data support.
The native general editor exposes separate W, Z and V custom probe fields.
Their Cartesian product must contain at most 25 conditions. Three sets of three
default probes exceed that budget; the user must explicitly choose fewer custom
values for at least one moderator. A budget error does not silently reduce
conditions or change the quantile convention.

Intervals are pointwise percentile bootstrap intervals, not BC/BCa. Bootstrap SE
uses sample SD (ddof=1). Requested, successful and failed draws and failure codes
are recorded. At least 95% must succeed, without replacement of failed draws.
This threshold does not eliminate possible selection bias from bootstrap failure.
Nominal 95% does not guarantee 95% finite-sample coverage. The seeded release
simulation found important limitations for sparse categorical interaction cells:
with N=100, unequal three-level X and binary W, 164/1,000 fits failed and one
population-null indirect contrast excluded zero in 89/836 successful fits
(10.65%; pointwise). The same design at N=300 had no fit failures, but this is
not a universal minimum sample-size rule. Inspect actual joint cell support,
not just total N or marginal category counts. Do not remove failed datasets,
collapse declared levels or change paths merely to obtain a fitted result.
Composite-null path products can instead be very conservative: when two serial
paths were population-zero, observed false-positive rates were 0.1%-0.6%.
More bootstrap draws reduce resampling noise; they do not resolve sparse-cell
identification, selection among successful fits or nonregular product inference.
These measured limitations need to accompany public-beta claims; numerical
R/Python agreement alone does not establish nominal inferential calibration.
Paired effect and index contrasts are differences within the same replication.
Constant route indices are displayed once but remain in comparisons with
conditional route derivatives at every relevant probe. Result identities
distinguish a moderator ID containing an underscore from a mixed derivative of
two other moderators; native row keys therefore cannot merge those estimands.
Cross-language tests match derivative and route semantics rather than requiring
identical internal ID encodings.
Native derivative/index route labels also state the actual moderator names and
order, for example `[d/d w]` or `[d2/(d w d z)]`. Paired contrasts identify both
source estimands. These labels distinguish the reported derivative without
changing its numerical value, bootstrap distribution or interval.
No bootstrap p-values are invented. Different significance decisions across two
conditions do not establish a difference between the effects.

The ordinary R bootstrap fixes and records Mersenne-Twister/Inversion/Rejection,
then restores the previous RNG configuration and seed. Supplied resample matrices
are **draws × common-sample N**, with one-based row indices. Seed equality across
languages does not imply sample equality. Exact parity fixtures use saved matrices
read by both implementations.

Simple-slope diagnostics include every equation term involving the focal source.
Declared moderator conditions are held at their reported values; other numerical
equation predictors are held at their complete-case means. This matters for a
moderator-source path in a joint interaction. For example, the W slope in an
equation containing X×W and X×W×Z includes those terms evaluated at mean(X),
rather than dropping them. The slope and its SE use the same complete coefficient
contrast vector. Conditions that differ only in the focal source's own value
are reported once because the derivative does not depend on that value.
For a categorical focal source, the analogous row is its indicator contrast
against the reference and explicitly names that contrast. Other categorical
equation predictors are held at their declared references.

Univariate Johnson–Neyman output solves the quadratic simple-slope significance
inequality using the fitted covariance. With additional moderators, each result
is a one-dimensional slice conditional on their stated values and other numerical
predictors' complete-case means. It is not a joint significance surface. Native
tables show boundaries within the observed range and the fixed conditions;
detailed JSON retains all roots, the polynomial and observed range.
No in-range boundary does not by itself state whether a slope is significant
throughout the range or nowhere; inspect the slope estimates and diagnostics.
This is pointwise coefficient/simple-slope inference; no grid of indirect-effect
intervals is described as an exact Johnson–Neyman region.

The native diagram, human-readable paths and fitted equations are generated from
the resolved specification. Its geometry is a display layout, not an estimated
SEM or evidence of temporal/causal ordering. Native slope and JN-boundary tables
are separate from optional coefficient-covariance, interaction-diagnostic and
reproduction JSON output. Dedicated interaction/effect plots and JN-region
visualization are described below. Dynamic native outputs are cleared before a
rerun and after incomplete/error states to avoid retaining prior-model results;
save/reopen, invalidation, cancellation and exports need the separately
recorded Desktop checks for each materially changed interface. Historical 0.1.1
checks do not establish the behavior of the expanded general editor.

Budgets bound terms (80/equation), routes (100), moderators (3 under settings v3),
conditions (25), categorical levels (10 per role), result rows (8000), matrix
elements, regression work and derivative work. Expanded indicator terms and X
contrasts count toward those budgets; logistic regression has the source
engine's larger work multiplier. The general editor permits up to 150 path rows and 150 interaction rows.
Up to ten mediators and twenty covariates are structural compatibility limits,
not a promise that every graph within those counts is estimable.

## Reporting example

We estimated a continuous observed-variable serial mediation graph by OLS,
retaining X→M1, X→M2, M1→M2, X→Y, M1→Y, M2→Y and W→M1, with W moderating
X→M1. All equations used one common complete-case sample. We report the selected
classical or HC3 covariance, the fixed W probe values, pointwise percentile
bootstrap intervals, confidence level, requested/successful draws, seed, software
versions and the exact graph specification. Indirect results are statistical path
associations under independent-observation assumptions; causal claims require
additional substantive identification assumptions.

## Methods references

* Hayes (2015). An index and test of linear moderated mediation.
  https://doi.org/10.1080/00273171.2014.962683
* MacKinnon, Lockwood & Williams (2004). Confidence limits for the indirect effect:
  distribution of the product and resampling methods.
  https://doi.org/10.1207/s15327906mbr3901_4
* R regression and quantile documentation are installed with R:
  `help(lm)`, `help(lm.fit)`, `help(glm)`, `help(binomial)`, `help(quantile)`.

These references acknowledge methods; they do not assert proprietary PROCESS
equivalence or endorsement. No proprietary PROCESS source is bundled.

## Native fit reuse (0.4.1)

Each native analysis retains one successful fitted summary in jamovi result
state. A SHA-256 fingerprint includes the full data delivered by jamovi (values,
row order, missingness and attributes), all model controls, estimation settings,
engine/R platform and embedded built-source manifest. Only the fingerprint is
retained, not the input values. Validation and data preparation occur before a
cache lookup. A valid hit preserves the coefficients, covariance, conditional
effects, percentile intervals and accepted bootstrap draw identities exactly.
Plot coordinates are then rebuilt for the requested display selection.

Presentation controls cover optional details/covariance output, the three plot
visibility controls, effect kind/page, arrow source/target, moderator, fixed
probe combination, focal contrast, logistic response/link scale and gallery
mode/page. Native image width/height scaling, including numbered gallery images,
is also a presentation change.
They reuse the existing fit. Changing the model's probe values changes fitted
estimands and therefore refits. Data notifications, statistical option changes,
active weights, incomplete specifications and failures clear cached state;
old-schema, wrong-key or integrity-mismatched payloads cause a new fit. A dataset
fingerprint also guards against value/order/filter changes without notifications.

The compressed payload contains public fitted summaries and provenance, not raw
case values, design matrices, case residuals/fitted values, leverage/weights or
the bootstrap distribution. It preserves the existing sample row-index metadata
and accepted draw IDs. An invisible Preformatted result retains native state
and an additional data-only JSON copy. jamovi 28 discards serialized R state on
file import, initializes an analysis, saves the result tree, then may fit in a
second R instance. PathLab preserves the JSON text through this handoff and
validates it against the full delivered data fingerprint before reuse. It never
unserializes discarded R objects or evaluates saved text. The codec accepts only
primitive data/list types and names/dim/dimnames attributes; exact numeric bytes
and character encodings preserve the payload hash. Invalid or unsupported text,
a payload over 8 MiB, or an SDK result file over 64 MiB causes a normal refit.
An older saved file without this text, or a different embedded source/runtime,
also refits once. No input values or bootstrap distributions are added to the
cache. Native statistical changes, data notifications and errors still invalidate
both copies. The host and its security behavior are left unchanged.
The payload digest detects accidental corruption; it is not an authentication or
untrusted-file security mechanism. Creation IDs use time/process information and
hashing without consuming statistical RNG draws. Reproduction JSON fit_cache
metadata reports computed/reused, fit_id, input_sha256, computed_utc and compressed
summary size. Rendering still replaces native output tables and images.

## Cooperative cancellation (introduced in 0.4.2)

The optional R API `progress(completed, total)` callback runs before resampling,
between ten-draw batches, and at completion. It runs outside the per-resample
failure handler: cancellation propagates without becoming a failed draw or
invoking replacement sampling. The existing exit handler restores RNG kind and
state on success and interruption. With the same saved indices, callbacks leave
the entire linear and logistic fit and bootstrap distribution exactly unchanged.

The native callback polls jamovi's restart checkpoint at these boundaries;
status updates flush at the start, end, or after at least half a second. Native
output construction also polls between equation blocks, 50-row effect pages,
and slope/JN row batches. Restart clears partial visible results. Only successful
fits enter the cache; cancellation during presentation preserves an already
completed cache entry for the next request. Polling is cooperative: a single
matrix fit, separation check, data preparation or graphics operation can delay
the next checkpoint. There is no fixed maximum cancellation-latency guarantee.

## Native plot semantics (introduced in 0.4.0)

Native `Image` results use base R graphics and cache aggregate coordinates only.
Resizing renders the saved coordinates without refitting or accessing respondent
rows. Disabled, invalidated and failed outputs clear both state and visibility.

The three fixed native images and numbered interaction-gallery images use
dynamic `results/<image>/widthScale` and `heightScale` options. Fixed-image
addresses and the host's quoted numbered-gallery addresses are explicitly
presentation-only in native invalidation handling. The 0.4.6 jamovi 28 fix
retains actual gallery resize settings through save/reopen and HTML export.
Statistical/data changes and unknown options still invalidate the fit. Small
canvases scale margins, text and axis spacing; the renderer restores only the
graphics styling it changed, avoiding invalid derived `pin`/`plt` restoration.
These display adjustments leave aggregate coordinates and fitted science intact.

Effect forests use the exact estimate and percentile bootstrap endpoints of
the selected emitted estimands. Twelve rows form a numbered page; the total
selected row/page count is displayed and no selected row is discarded. Indices
and paired contrasts retain their own units, conditioning and emitted interval.
These intervals are pointwise, not simultaneous.

Interaction graphs evaluate every declared term in the selected target equation
at the observed source range (101 grid values) or each declared source category.
The selected moderator uses its fixed original-sample probes; other moderators
use one explicitly selected probe combination. Remaining numerical equation
predictors are held at common-sample means, categorical predictors at reference.
The analytic mean/link variance is `v' V v`, with the same t or normal reference
as the fitted equation. Logistic probability intervals transform both link-scale
endpoints through the inverse logit. These are conditional equation predictions,
not individual prediction intervals, indirect associations or marginal effects.

Johnson–Neyman graphs use the existing analytic path-slope result, including all
terms involving the focal source. The grid has 201 observed-range values plus
every exact in-range root. Fitted covariance supplies pointwise slope intervals;
the existing quadratic determines significant regions and exact boundaries.
Other moderators are fixed as stated. Categorical focal sources use one declared
indicator contrast; categorical moderators have no numerical JN graph. Shading
does not imply a simultaneous band, adequate joint support or a region for an
indirect effect. Logistic path slopes remain on the log-odds coefficient scale.

## jamovi-delivered sample and file semantics (verified in 0.4.3)

Active filters remove rows before the analysis receives its data frame; native
user-missing rules mask the relevant values. `sample.source_n` counts delivered
rows, and `sample.n` is the fixed complete-case count across required graph
variables. `included_rows` and exclusion `source_row` positions index the delivered
frame. If its row names are unique positive integers, the native adapter adds
`delivered_row_numbers` and `included_row_numbers`, retaining jamovi's original
row mapping. Text row labels are omitted. The standalone R core contract is
unchanged; these two mapping fields belong to the native adapter.

The tested `.omv` round trip retains filters, user-missing rules, graph/options,
fitted cache and image sizes. HTML retains rounded displayed table values and
embedded PNGs. The tested CSV export contains filtered data rows, keeps numeric
missing codes and omits analysis/filter/missing-rule metadata. Fresh CSV import
therefore treats those codes as observed values until a rule is reapplied.
Special Import of a documented changed CSV retained the synthetic model and
definitions, changed the data fingerprint and recomputed the fitted results.
