# Changelog

## 0.4.6 - first-public-beta approval preparation, 2026-10-24

* Recommend 0.4.6 public beta; no publication, submission or tag created.
* Review documented methods and simulation limitations; correct stale host
  claims and replace the public validation JSON with source-specific evidence.
* Audit unchanged runtime/test/example/package files against the tested source
  and reconcile all simulation summary rows. No statistical/interface changes
  or fresh numerical/GUI runs are claimed for this documentation-only preparation.
* Prepare the module-only source, private approval packet and SHA-256 manifests.

## 0.4.6 — private jamovi 28 lifecycle repair, 2026-10-24

* Preserve a bounded data-only fitted summary across the separate initialization
  and fitting instances used after reopening in jamovi 28. Validate the full
  input/source/runtime fingerprint and payload before reuse; never deserialize
  discarded R state. Statistical/data changes and failures still invalidate.
* Recognize native numbered-gallery resize option paths. Preserve saved native
  image dimensions while reusing the fitted model.
* Add exact typed-JSON and two-instance initialization regressions, including
  invalid payloads and a forbidden-bootstrap check. Estimators, resampling and
  inference calculations remain unchanged.

## 0.4.6 — private cache repair and final Desktop checks, 2026-10-23

* Normalize the confidence Number option's integer/double storage in the fit
  fingerprint. An unchanged 95% level can reopen as 95L; that no longer causes
  an unnecessary bootstrap. Preserve exact values and all other cache inputs.
* Add a saved-state regression that forbids a refit and verifies that changing
  the confidence value still recomputes. Scientific calculations are unchanged.
* Verify missing-rule cache reuse and native keyboard/mouse resizing through
  actual Desktop save/reopen. Refresh private release findings and limitations.

## 0.4.6 — private branding rebuild, 2026-10-23

* Replace the earlier icon with the approved original Model 59-inspired design:
  unfilled X/M/Y boxes with blue outlines and letters, and an unfilled gold W
  square box with a matching letter. All four boxes have matching rounded
  corners. W points to all three structural paths.
* Package the SVG and transparent PNG, update documentation and notices, and
  refresh the private module-only review archive. Analysis code is unchanged.
* The tested jamovi host has no module toolbar icon hook; its assets are untouched.

## 0.4.6 — module-only release candidate, 2026-10-22

* Apply the owner-approved GPL version 2 or later license to the module only.
  Include the full license text and dependency/independence notices.
* Register three existing synthetic CSV examples in jamovi's Data Library.
* Include original vector branding (superseded by the rebuild above); the tested host has no
  module-provided toolbar icon API. No host application assets are changed.
* Refresh descriptions, citation metadata and the source-only review boundary.
  No public repository, upload, submission or maintainer message is created.

## 0.4.5 — private interaction gallery, 2026-10-22

* Add selected-arrow or all-moderated-arrow interaction displays, with six
  images per page and no silent omission of later pages. Each attached moderator
  has a labelled conditional slice; duplicate joint-interaction slices collapse.
* Reuse the fitted cache across gallery mode/page changes and image resizing.
  Keep the selected-arrow Johnson–Neyman display and existing saved defaults.

## 0.4.4 — private native dropdown repair, 2026-10-22

* Resolve named NULL cells in General graph path/moderation rows to the choices
  actually displayed by native ComboBoxes. Untouched visible defaults previously
  caused a misleading "Complete every cell" error. Preserve explicit selections
  and reject missing keys, blank strings, unknown roles and malformed rows.
* Align generated target defaults with the displayed first choice, M1. Add
  regression coverage for sparse native rows and strict validation failures.

## 0.4.3 — private data/display preview, 2026-10-22

* Treat all six native image width/height scale options as presentation changes,
  preserving the completed fit and bootstrap. Unknown options still invalidate.
* Retain numeric source row mappings supplied by jamovi after filtering. Explain
  delivered-row versus common-sample counts and user-missing semantics.
* Repair base graphics restoration on small devices; effect, interaction and
  JN images render at four tested canvas sizes with identical coordinates.
* Pass a real R CMD check: 137 cases/58,176 assertions, zero errors, warnings or
  notes; PDF manual omitted. Installed-only suite passes 34 cases/11,917 assertions;
  Python regression passes 88 tests. Five new Python/R fixtures share saved indices.
* Actual Desktop checks cover filters, equality/range missing rules, off/on
  restoration, data replacement, cached reopen, native interaction-image resize,
  HTML tables/PNGs and CSV export/fresh import: 1,653 checks across the recorded
  scenarios and transitions. Detailed scope and remaining formats are in VALIDATION.
  The genuine Windows x64 installer is built/installed. Everything remains private.

## 0.4.2 — private package-quality preview, 2026-10-22

* Add cooperative bootstrap and output-page cancellation, throttled progress,
  partial-output clearing and completed-cache preservation on restart. Callbacks
  do not consume statistical RNG or change saved-index results.
* Fix missing namespace imports and source-layout assumptions in package tests;
  supply an installed citation and executable documented example. R CMD check
  is OK with zero errors, warnings or notes; PDF manual generation is omitted.
* Pass 129 source cases/57,565 assertions, 26 installed cases/11,306 assertions,
  and 88 fresh Python regression tests. Regenerate seven shared-index Python
  plot references. A 1,564-row native result cancels and recovers without refit.
* Build the genuine Windows x64 installer, side-load it into an empty jamovi
  Store profile, compute/save its synthetic analysis, and restore both original
  profiles. Verify 9,000-row Desktop cancellation and replacement results plus
  subsequent exact cached presentation. See VALIDATION.md for the full scope.
  All materials remain private and unsubmitted.

## 0.4.1 — private fit-cache preview, 2026-10-21

* Reuse one saved fitted summary and bootstrap result across all thirteen plot
  and optional-output controls, including genuine analysis save/load.
* Fingerprint full delivered data, model/inference settings and built engine;
  clear invalid/incomplete/error states, and reject stale or damaged cache data.
* Retain compressed public fitted summaries without raw case values, design
  matrices, case residuals/fitted values or bootstrap distributions. Record fit
  identity, creation time and computed/reused provenance in reproduction JSON.
* Add the existing jamovi-supplied digest dependency; use hidden result state
  without modifying the installed jmvcore runtime.
* Pass 125 source cases/57,496 assertions, 22 installed cache/plot cases/11,237
  assertions and 88 Python regression tests. Fresh Python plot references use
  the same saved resamples; comparisons consume cached summaries. Twelve actual
  Desktop scenarios pass 702 saved-file checks and nine cross-save checks,
  including separate reopen, missing/Undo, seed/model invalidation and plot edits.
  Remaining release work is recorded in VALIDATION.md. All work remains private.

## 0.4.0 — private plot preview, 2026-10-21

* Add native Image results for paginated effect forests, fitted interaction
  curves / categorical points, and analytic Johnson–Neyman path-slope regions.
* Retain exact fitted bootstrap intervals for effect rows and analytic mean/link
  intervals for interactions. Logistic response intervals transform link bounds.
* Expose arrow, moderator, fixed probe combination, focal category contrast,
  effect kind/page and logistic scale controls. Inapplicable selections explain
  their status without replacing the fitted statistical analysis.
* Cache only aggregate coordinates in Image state. Clear/hide images and their
  states when the model/data/settings change, fail, or disable the output.
* Fix the actual Desktop categorical legend failure: all-NA line types raised
  "missing value where TRUE/FALSE needed". Point-only legends now use type 0;
  every parity interaction profile is also rendered on a real PNG device.
* Final source/installed suites pass 120/57,265 and 17/11,007 cases/assertions;
  the preserved 88-test Python regression passes. Twelve actual Desktop scenarios
  pass 371 saved-file checks, plus 12 cross-save transitions and 13 HTML PNG
  export checks. Scope and remaining selection/resize/export QA are in VALIDATION.md.

## 0.3.0 — private development preview, 2026-10-21

* Add explicit indicator coding for categorical X, moderators and covariates,
  retaining typed levels, declared order, reference groups and common-sample
  counts. Numerical codes require an explicit native categorical role row.
* Expand every selected interaction across the indicator bases without changing
  the explicit graph or completing an interaction hierarchy automatically.
  Preserve separate focal-X reference contrasts in effects, slopes, derivatives,
  paired differences and cached bootstrap calculations.
* Add native categorical role/reference rows and category-aware probes. Retain
  factor level order; resolve raw text/numeric levels before common sampling.
  Keep the two-to-ten-level and 25-condition limits and existing work budgets.
* Add an optional binary logistic outcome with explicit event-to-1 mapping for
  numeric codes, text or factor labels. Mediator equations remain continuous OLS.
* Implement scaled Newton logistic maximum likelihood, model-based covariance,
  generalized HC3, normal-z inference, likelihood diagnostics and certified
  original-sample complete/quasi-separation checks. Reject uncertifiable
  numerical cases conservatively. Document three near-boundary HiGHS differences
  and pathological bootstrap failure-label differences in METHODS.md.
* Label categorical JN/derivatives as inapplicable; retain numerical derivatives
  and JN slices conditional on stated categorical levels. Show categorical
  reference conditioning and focal contrasts in native results.
* Label binary-outcome associations as log-odds statistical path products. Do not
  present them as natural causal/probability effects, odds ratios of indirect
  effects or marginal/reduced-model total coefficients.
* Pass the focused eight-case/11,364-assertion categorical/logistic suite using
  identical saved 200-draw indices, unchanged private Python output, independent
  NumPy QR/Newton/HC3 and R glm/manual information/HC3. Add wholly synthetic
  categorical linear and logistic examples with reproducible model recipes.
* Rebuild and locally install a genuine 0.3.0 Windows installer from source
  a0bd83c with explicit diagram visibility handling. Pass integrity and 14
  cases/11,734 assertions against the actual installed namespace, including
  eight parity and six native cases. Pass the final frozen source suite's 103
  cases/46,257 assertions with unchanged inputs. Pass all seven actual Desktop
  reports, 2,030/2,030 checks: categorical-linear and joint-logistic baselines and
  complete-baseline reopens, joint-logistic missing values, visible error clearing
  and restoration. The missing cell is supplied in an unrun input; an actual
  Desktop UI insertion restores it.
* Preserve first-build source 1b55510 evidence, including its cached SVG on error
  and stale lifecycle-text assertion, separately from the final repaired-build
  successes. Preserve 0.2.0
  evidence as historical. Keep all source,
  reference results, installers and release-review materials private.

## 0.2.0 — private development preview, 2026-10-21

* Add a general native graph editor with ordered lists for up to ten continuous
  mediators, three moderators and twenty covariates, plus explicit path rows.
* Preserve the original M1/M2/W editor and its default mode for saved analyses.
  Switching editors selects one complete specification without merging paths.
* Add separate and joint moderation rows with one or two moderators per arrow.
  Require all selected main effects and lower-order interactions explicitly.
* Expose separate W/Z/V custom probe fields and the existing 25-condition budget.
  Reject excessive combinations instead of changing requested probe values.
* Correct joint-interaction simple-slope diagnostics to hold other numerical
  predictors at their complete-case means; remove repeated focal-source probe
  rows and report the fixed conditions for analytic JN slices.
* Compile fixed graph/probe calculations once before bootstrap resampling.
  Keep the full effect and paired-contrast output; exact distribution tests
  compare the cached calculations with the original evaluator.
* Display general-mode effects in consecutive visible tables of at most 50 rows,
  retaining original order and every row. General R callers use effectPages$items;
  original-mode effects and the full reproduction JSON remain available.
* Label derivative/index routes with actual moderator names and orders, such as
  `[d/d w]` and `[d2/(d w d z)]`, and identify both sources of a paired contrast.
  Correct the ambiguous W/Z display labels without changing numerical arrays.
* Supply a synthetic three-mediator joint-interaction example and wrap the
  native editor's hierarchy guidance within the options pane.
* Build and locally install a genuine Windows 0.2.0 installer; verify all 23
  installed files, nine source hashes and loaded R functions/classes against
  the reviewed source. Final numerical and Desktop results are recorded
  separately in VALIDATION.md; historical 0.1.1 checks are retained explicitly.
* Pass the label-corrected source's frozen 74-case/34,160-assertion R suite and
  installed general/legacy checks. Verify actual synthetic Desktop baseline,
  invalid-role clearing/restoration, common-sample missing values and baseline
  save/reopen. Preserve earlier source stages as historical evidence.
* Keep source, examples, installer and review materials private.

## 0.1.1 — private development preview, 2026-10-20

* Replace dynamic result rows on every rerun; clear incomplete and failed models.
* Add optional mediator roles, ordinary moderation, simple mediation and native
  equation-specific continuous covariates without changing explicitly selected paths.
* Add a resolved graph, readable route labels, simple slopes and JN boundaries;
  make detailed covariance and reproduction JSON optional in the results view.
* Restore constant-index versus conditional-derivative contrasts and prevent
  ambiguous derivative identities when moderator IDs contain underscores.
* Reject active jamovi weights and preserve optional arguments in the R function.
* Verify an actual Desktop serial first-stage analysis, missing-value and path
  invalidation, saved settings/provenance and reopening of synthetic .omv files.
* Keep all development, installers and release-review materials private.

## 0.1.0 — private development preview, 2026-10-20

* Added an independent continuous linear R graph computation layer.
* Generated a native jamovi analysis with explicit arrow and moderation controls.
* Added paired bootstrap, fixed probes, degree-aware derivatives and contrasts.
* Added synthetic saved-index Python references and handwritten R references.
* Built and locally installed a Windows x64 jamovi 2.7 module using the existing
  runtime copied into a private workspace to avoid Store execution restrictions.
* Kept categorical and logistic requests disabled pending independent validation.
* Prepared module-only release review materials; no publication or submission.
