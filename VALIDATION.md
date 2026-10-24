# PathLab 0.4.6 validation

Proposed first public beta; not yet published or accepted by the jamovi Library.
This source records numerical, installed-package and actual Desktop checks
separately. Documentation preparation on 24 October 2026 did not change any of
170 runtime/test/example/package files from tested analysis source
`cba2741a67202521e13159967fa94c727a3ca376` (line endings normalized).

## Current Windows x64 jamovi 28.14 / R4.6

Installer: `pathlab_0.4.6-jamovi28-win-x64.jmo`.
SHA-256: `004f6eaeb55f5e59f304a5cad4e5ed059da03787ce6c6ebfe1762db8f14eb435`.
Real build and installation succeeded. All 39 installed files, 63 functions and
89 R6 methods match the recorded source. Full actual installed suite:
**147 cases / 58,289 assertions**, zero failures, errors, warnings or skips.
This includes shared-index Python fixtures and independent R/matrix references.
R4.6 package build/check: **Status: OK** with `--no-tests`; the complete installed
suite ran separately. PDF manual compilation is unverified without TeX; there
are no vignettes. R help examples are separately executable.

Actual final targeted Desktop fits passed **158 result assertions, 22 lifecycle
checks and 11 HTML-image checks**. Continuous/gallery and user-missing-rule
analyses retain exact fit ID, fingerprint, creation time, science and coordinates
on the first display edit after a complete host restart. Gallery dimensions
800×570, 760×560 and 620×370 survive save/reopen/rerender/export; decoded exported
pixels match. The safe data-only cache crosses the host's separate initialization
and fitting instances without deserializing discarded R state. Ten additional
binary/joint categorical-logistic two-instance checks pass with further
bootstrap forbidden. These are targeted final-source checks; a complete new
all-feature GUI sweep was not repeated after the last lifecycle-only fix.

Earlier source `2c04290` has a six-case broad 28 continuous/categorical/logistic/
filter sweep: 468 result assertions, 30 restart/reopen correctness and 11 HTML
checks. That run exposed cache/gallery issues fixed above; its counts are
historical and are not added to the final rerun totals. See
[current and historical 28 findings](release-review/COMPATIBILITY28.md).

## Retained Windows jamovi 2.7.39 / R4.5

The retained `pathlab_0.4.6.jmo` uses earlier analysis source
`2c04290b9815a5830bb67c3fee1d78bb440110b1`.
SHA-256: `4b2886b584f9d2211cb15de81ffbce80398543649ef6d1d987276b94f7cf1c08`.
All 38 installed files, 59 functions and 89 R6 methods match. Full test-enabled
R CMD check: **144 cases / 58,257 assertions**, no errors, warnings, notes,
failed or skipped tests; checked source unchanged. Fresh installed focused
suites: **52 cases / 23,706 assertions**. GUI missing-rule cache reuse,
keyboard/mouse single-image resizing and save/reopen pass on that source.

The latest source passes 12 focused R4.5/jmvcore2.7.35 cases / 303 assertions;
this is not a new exact-source 2.7 installer or GUI pass. Earlier broad GUI
filter/missing/import/cancellation runs are described with their own source
in [Desktop findings](release-review/DESKTOP_REGRESSION.md). Original profiles
and Store 2.7 module files were restored/verified after isolated 28 testing.

## Parameter recovery and interval calibration

The [frozen study](release-review/simulations/REPORT.md) completed 20,000 synthetic
datasets: 18,000 finite-parameter and 2,000 deliberate separation cases; 1,000
per scenario/N cell at N=100 or 300, 2,000 paired resamples per estimable fit.
17,836 fits supplied intervals. Per-estimand bias, RMSE, coverage, false positives,
fit failures, Wilson intervals and Monte Carlo SE remain in the included CSVs.
The R study driver and frozen protocol are included for reproduction.

Every successful original fit matched independently handwritten R matrices/
GLM references (maximum coefficient difference 3.19e-12; covariance 1.96e-13).
Twenty selected first-replicate cells replayed the same saved 2,000 resample
rows in Python and R: **1,714,864 scalar comparisons**, maximum absolute
difference **1.01e-11**. All selected comparisons passed, including full
bootstrap distributions, paired contrasts and both separation rejections.
Four extra sparse-data failure replays reject in both languages but do not
always fail at the same stage. Twelve aggregate-summary cells passed independent
R recalculation. These checks are not universal failure-code or calibration
agreement.

Study source is `b1beae8c08413ca38bcde1e142f2d513fcb0b9b0`. Eight scientific
files remain identical after one diagnostic-only unit-leverage message change;
cache/lifecycle changes are separately tested. The study was not rerun for
this documentation review. A new audit checked protocol/summary hashes,
20 cell accounts and all 556 estimand rows, including 172 null rows.

Important measured limits: categorical N=100 had 164/1,000 failed fits; a null
indirect excluded zero in 89/836 successful fits (10.65%; Wilson 8.73–12.92%).
Double-null serial products were conservative (0.1–0.6% false positives). There
is no uniform nominal-95%-coverage guarantee or universal minimum N. DGPs use
two serial mediators and specified moderator/coding designs; arbitrary large
graphs, missingness mechanisms and all robust-logistic settings are not calibrated.
External statistician peer review and a matched lavaan/jAMM case have not been
obtained. See METHODS.md for assumptions and pathological-reference exceptions.

## Reproducing the checks and interpreting their scope

Install DESCRIPTION dependencies in a compatible jamovi R environment. From
standalone module source, run R CMD build and R CMD check on the source package.
Where TeX is unavailable use `--no-manual`. Keep tests enabled for a complete
source check; the recorded current R4.6 `--no-tests` check is explicitly paired
with its separate full installed suite, not claimed as a test-enabled check.
Bundled synthetic fixtures replay saved resample indices; matching seeds alone
is not an exact bootstrap comparison. The private Python regeneration worker is
outside the standalone module and is not needed to run the bundled R tests.
Do not add overlapping source/installed assertion totals as independent tests.

Continuous mediators, independent observations and correctly specified equations
are required. Common complete cases do not cure missing-data bias. Logistic
indirects are statistical coefficient/log-odds products, not probability or
natural causal effects. JN concerns pointwise conditional path slopes, not
indirect-effect regions or simultaneous bands. Weights, clustering, latent SEM
and categorical mediators are unsupported. Plots do not establish adequate
joint data support.

macOS, Linux, ARM, Cloud and later host versions are unverified. A Windows JMO
is specific to its target series/runtime. Library builds and acceptance require
separate review; local success is not worldwide availability. The original
SVG/PNG branding is included, but the tested host's generic toolbar icon is
unchanged. No PROCESS source or original artwork is bundled.
