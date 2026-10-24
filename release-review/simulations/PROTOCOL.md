# Frozen simulation protocol: 23 October 2026

Private scientific review; no publication or submission authorized. This
protocol was specified after excluded runtime/harness pilots and before the
main Monte Carlo run. Pilot jobs are not counted as additional datasets. They
use the same first-replicate seeds later regenerated in the main run, so they
are reproducibility checks rather than independent holdout datasets. The
generation equations and draw/replicate counts were not tuned to pilot coverage.

## Design and reproducibility

Installed PathLab 0.4.6, build source
`b1beae8c08413ca38bcde1e142f2d513fcb0b9b0`, Windows x64 R 4.5.0.
Use 1,000 independently generated datasets per scenario at each N = 100, 300;
2,000 paired whole-case bootstrap draws per fit. Retain the production rule
requiring at least 95% successful draws. The separation stress scenario must
reject the original fit; it has no finite logistic coefficient coverage target.

Master seed 20261023. For the scenario order below (one-based index s), dataset
r uses `20261023 + 100000*s + 100*N + r`; its bootstrap seed is that value plus
10000000. R RNG kinds: Mersenne-Twister / Inversion / Rejection. First dataset
in every cell saves the actual 1-based resample matrix; both R and Python read
those saved rows. A matching seed alone is never called an exact comparison.
The execution manifest freezes driver, runner, Python engine and installer
SHA-256 values before launching main jobs. Worker failures invalidate a run
until investigated; scientific fit failures remain in its denominators.

## Data-generating equations

Independent X, W, Z and all equation errors are standard normal, unless
specified below. Equations contain the intercepts shown. The graph is explicitly
X -> M1 -> M2 -> Y, plus X -> Y (population coefficient zero); W -> M1 and
X*W -> M1. Extra arrows/terms appear only where listed. No bypass mediator
arrows or automatic paths are inserted.

Base: M1 = .2 + .5X + .2W + .3XW + e1;
M2 = .1 + .6M1 + e2; Y = -.3 + .5M2 + 0X + eY.
The conditional indirect polynomial is .15 + .09W. Numerical probes are fixed
at W = -1, 0, 1; joint models use all nine W,Z combinations.

1. **serial_alt:** base equations.
2. **serial_null:** set X and XW coefficients in M1 to zero.
3. **serial_double_null:** additionally set M1 -> M2 to zero. This composite-null
   product case intentionally tests the known possibility of conservative
   percentile intervals; nominal coverage is measured, never assumed.
4. **repeated_w:** M1 XW coefficient .2; add .1W + .15M1W to M2 and
   .1W + .1M2W to Y. Indirect polynomial is
   .15 + .1275W + .0345W² + .003W³. Evaluate degree-aware derivatives.
5. **joint_wz:** M1 = .2 + .5X + .2W + .1Z + .2XW + .15XZ + .12WZ
   + .12XWZ + e1. Other equations as base. Full lower-order hierarchy is
   explicit. Indirect polynomial .15 + .06W + .045Z + .036WZ; mixed derivative
   .036. Includes conditional derivatives and probe contrasts.
6. **categorical:** independent X in A/B/C with probabilities .55/.30/.15,
   W in low/high with probabilities .65/.35, reference A and low. M1 = .2 +
   .5 I(X=B) + .2 I(W=high) + .3 I(X=B)I(W=high) + e1.
   M2 and Y as base; Y includes both X indicators with zero coefficients.
   B-vs-A indirect .15 + .09 I(W=high); C-vs-A indirect zero at both levels.
   Every declared level must occur in the original sample. Sparse resamples
   may fail; record them without replacing the declared levels or design.
7. **logistic_alt:** base mediator equations; Bernoulli Y with
   logit P(Y=1) = -.3 + .5M2 + 0X. Truth is the log-odds statistical path
   product .15 + .09W, not a probability or natural causal effect.
8. **logistic_null:** as logistic_alt with X and XW coefficients in M1 zero.
9. **heteroscedastic:** base with XW coefficient zero and e1 multiplied by
   sqrt(.3 + .7X²). Use HC3 normal-reference coefficient and simple-slope
   intervals. Indirect truth .15. Null interaction truth zero.
10. **separation_stress:** base mediators, Y = I(M2 > 0). Report rejection
    frequencies/codes; no finite logit truth, coverage or recovery claim.

Use classical t coefficient intervals in homoscedastic linear scenarios,
model-based normal-z logistic intervals, and production percentile bootstrap
intervals (R type 7 / NumPy linear) for effects. Nominal confidence is 95%.

## Recorded outcomes and checks

For every original dataset: source/generation/bootstrap seeds, success/error
code/message, elapsed time, requested/successful/failed draw counts. For every
successful fit, independently construct handwritten equation matrices, fit
linear models by normal equations and logistic models using stats::glm.fit,
and compare all coefficients/covariances with production (absolute tolerance
1e-7). This does not use production's graph compiler for reference designs.

Record all equation coefficients, specific indirects at every condition, direct
effects, index/derivative rows, specific-indirect condition contrasts, and the
heteroscedastic X -> M1 HC3 simple slopes. Report truth, mean estimate, bias,
RMSE, bias Monte Carlo SE, empirical SD and nominal interval coverage. For
truth-zero quantities report the probability an interval excludes zero.
Use Wilson 95% intervals and binomial Monte Carlo SE for rates. Show failures
separately, successful/attempted denominators and unconditional successful
coverage counts; never imply failed fits supplied an interval.

Selected first replicates in all 20 cells are also checked with the preserved
Python conditional-process engine, using the saved 2,000 rows. Compare original
coefficients/covariances, successful-draw counts, effects, derivatives and paired
contrasts. Check shared-draw distributions where available. Separation rejection
must agree. Keep full synthetic replay data and raw job evidence private.

Coverage/false-positive rates are pointwise. Multiple displayed rows do not
have familywise error control, and duplicated direct/index rows are not
independent simulation evidence. Investigate meaningful departures and numerical
discrepancies; do not tune the scenarios or interval method to obtain a pass.
The report must identify finite-sample limitations and any additional validation
needed before approving public-beta statistical claims.
