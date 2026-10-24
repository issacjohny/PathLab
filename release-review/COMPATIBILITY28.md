# jamovi 28 compatibility — 24 October 2026

Private review. No publication, submission or external message occurred.
Current analysis source: `cba2741a67202521e13159967fa94c727a3ca376`.
The two recorded jamovi 28 cache/resize lifecycle issues are now resolved on
the tested Windows host. This is bounded runtime evidence, not a claim of
universal platform compatibility or statistical calibration.

## Runtime and actual installer

The separate official portable runtime is Windows x64 Desktop **28.14.0.0**,
bundled **R/Rscript 4.6.0**, **jmvcore 28.3**; existing **jmvtools 28.4** was
reused. No Windows runtime prerequisite is missing. The Store 2.7.39 installation
and original module were preserved. Its retained installer is source `2c04290`;
this latest source has focused older-runtime checks, without a new 2.7 installer
or exact-source 2.7 GUI claim.

The current private installer is `pathlab_0.4.6-jamovi28-win-x64.jmo`,
**498,818 bytes**, SHA-256
`004f6eaeb55f5e59f304a5cad4e5ed059da03787ce6c6ebfe1762db8f14eb435`.
All **39 installed files, 63 functions and 89 R6 methods** match the committed
source. This installer targets R 4.6.0-x64; do not install it in the R4.5 host.
The completed isolated profile is archived privately after restoring the original
profile and verifying all 38 original Store module files and 39 test module files.

## Numerical and package checks

- The full suite loaded from this actual installer passed **147 cases /
  58,289 assertions**, with zero failures, errors, warnings or skips. It includes
  existing saved-resample Python references, independent linear/logistic and
  covariance references, categorical/joint interactions, plots, invalidation
  and cancellation. The 20,000-dataset simulation study was not rerun: estimator,
  resampling, inference and rejection boundaries are unchanged.
- R4.6 source build/check returned **Status: OK** for package, dependency,
  namespace, code, documentation and examples. This used `--no-tests`; the full
  installed suite ran separately. It is not a second test-enabled package check.
  No TeX/PDF-manual pass is claimed; no vignettes exist.
- The changed source passed **12 focused cases / 303 assertions** with
  R4.5/jmvcore2.7.35, without failures/errors/warnings/skips. This is source
  compatibility evidence, separate from the retained 2.7 installer.
- Ten additional binary and joint categorical-logistic checks pass the safe
  JSON roundtrip and two-instance stateless handoff, preserving fit identity,
  scientific results and plots while further bootstrap execution is forbidden.

## Cache restart cause and repair

The shipped 28 host deliberately drops serialized R state when importing a
saved analysis. It then initializes an R analysis, saves the initialized result
tree, and can fit in a separate R analysis instance. Reading the old file during
fitting is too late; retaining the text only in the first instance's private
field also loses it. Temporary synthetic diagnostics established this sequence.

PathLab now keeps a bounded **data-only JSON** copy of the successful fitted
summary in hidden result text and carries it through that intervening save.
After full data arrive, reuse requires the exact data/model/estimation/source/
runtime fingerprint and valid payload hash. The codec never evaluates text or
unserializes discarded R objects. Unsupported, malformed, oversized or mismatching
payloads safely refit. Statistical changes, data notifications and errors still
invalidate both cache copies. Older files without this text, changed source or
a different runtime legitimately refit once. See METHODS.md for bounds and scope.

The regression strips all R state, initializes and saves one instance, then
runs a second instance with bootstrap forbidden. It failed on the previous
implementation and passes on the current installer.

## Actual Desktop, save/reopen and gallery export

Two real synthetic Desktop workflows use the exact current installer: the
100-row serial moderated graph with three interaction-gallery plots, and a
user-defined-missing-rule example retaining 98 of 100 rows. Baseline fits were
saved, the complete host exited, and both files opened in a fresh host. The first
covariance-display edit now reports **reused**, retaining each original fit ID,
input fingerprint, creation time, full scientific output and plot coordinates.

These targeted final runs pass **158 saved-result assertions and 22 additional
lifecycle checks**. The native gallery resize addresses use quoted item keys
such as `interactionPlots/"1"`; the earlier matcher used an inspection-report
address instead. Corrected matching treats the actual resize as a display edit.
The user's normal mouse resizes produced **800 x 570**, **760 x 560** and
**620 x 370** native gallery sizes. They persist after the source upgrade,
save/reopen and display rerender. Fast computer-helper drags that changed only
CSS size are excluded as successful native-resize actions.

The final local HTML export passes **11 checks** for the effect plot, JN plot
and all three gallery plots: decoded RGBA pixels match the saved PNGs exactly,
and all five display dimensions match their native results. The hidden cache
text is absent from the export. PNG compression bytes may differ in this host.
No respondent data binaries were inspected by the saved-result verifier.

The broader six-case 28 Desktop/filter/categorical/logistic sweep remains
historical source-`2c04290` evidence: 468 result assertions, 30 reopen correctness
checks and 11 HTML checks. Those counts are not added to the current targeted
reruns or described as a fresh broad sweep on `cba2741`.

## Download provenance and remaining scope

The [official download page](https://www.jamovi.org/download.html) package request
needed Referer `https://www.jamovi.org/download.html`. The tested ZIP endpoint is
`https://www.jamovi.org/downloads/jamovi-28.14.0.0-win-x64.zip`, redirecting to the
official CDN. The 464,228,705-byte ZIP has locally recorded SHA-256
`7b1f9c15650f5f1da29431446638f550b3cff9ef325d1d3e8088d261a225e824`.
This is not an independently signed vendor checksum; the portable executable
reports NotSigned. A distinct Electron user-data directory avoids forwarding
commands to the Store instance. The earlier installation-forwarding incident
was repaired and all original Store files remain verified.

The recorded Windows 28 cache/gallery lifecycle gate is closed. Methods/owner
review of measured simulation limitations, source rights and bounded beta claims,
and the intended final public source/version freeze remain open. macOS, Linux,
ARM and Cloud are untested. The optional PDF manual remains unverified without
TeX. Publication and submission still require separate authorization.
