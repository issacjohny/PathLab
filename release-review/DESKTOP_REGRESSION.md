# Current cache-fix Desktop findings — 23 October 2026

Private review; nothing submitted, uploaded or made public.

Source `2c04290b9815a5830bb67c3fee1d78bb440110b1` is built and installed in
Windows x64 Store jamovi Desktop 2.7.39 / R 4.5.0. Installer SHA-256:
`4b2886b584f9d2211cb15de81ffbce80398543649ef6d1d987276b94f7cf1c08`.
The installed byte/loaded-code audit passes for 38 files, 59 functions and
89 R6 methods. The estimator, resampling, intervals and plots are unchanged.

## Two earlier Desktop gates resolved

The earlier missing-rule cache miss was reproduced exactly, including both
historical input fingerprints, by changing only confidence from double `95`
to integer `95L`. jamovi can reopen the same Number option in either form.
The fingerprint now normalizes this one storage type, preserving its value
and all other data/model/inference inputs. No missing-value attributes or
data values are discarded. A new save/load regression forbids refitting,
verifies identical science and plots, and confirms that 95.5% recomputes.

The actual 100-row synthetic file retains the `m1 < -100` missing rule and
disabled empty filter, fitting the same 98 cases. A covariance-display edit
after reopening now retains fit ID, fingerprint and creation time, as well as
all scientific output and plot coordinates.

Native keyboard resizing changes the effect image from 760 x 560 to 770 x 570.
After selecting the analysis and clicking the image again, its dotted focus
border and lower-right mouse handle appear. Dragging that visible handle
changes the image to 710 x 530. Both sizes survive actual Desktop save/reopen
and a subsequent presentation rerender. The same cached fit is retained.
The earlier unsuccessful drag attempts are not counted as passes.

Two disjoint installed suites pass **52 cases / 23,706 assertions**, with zero
failures, errors, warnings or skips. They include shared saved-index Python
references, independent R equations/covariance, native cache save/load,
categorical/logistic controls, invalidation, cancellation and real PNG renders.
These are separate from the GUI saved-file checks and earlier study evidence.
The three fresh saved-result verifications pass 102 numerical/plot assertions;
60 additional lifecycle checks cover fingerprints, dimensions, PNG assets,
metadata, archive hashes and exact science. One keyboard-state archive is an
identified byte-identical snapshot of the actual reopened Desktop save.

The new full R CMD build/check passes **144 cases / 58,257 assertions**, tests
enabled and source bytes unchanged, with zero errors, warnings, notes, failures
or skips. The PDF manual is unverified without TeX; no vignettes exist. An initial
restricted process could not see bundled R dependencies (including stats); that
failed setup attempt is retained privately and excluded. The complete retry
used the existing runtime with normal local access and no dependency bypass.

The final release gate remains open for methods/owner review of the measured
simulation limitations, public-source approval/freeze and unverified runtimes
and platforms. The working web application is preserved. No public action is
authorized by completion of these engineering checks.

## Historical diagnostic build — 23 October 2026

The remainder records the previous `6849ad9` build. Its two open Desktop items
were resolved above; its broader GUI checks remain useful historical evidence
on the unchanged estimators. They were not rerun or added to the new totals.

Private review; nothing submitted, uploaded or made public.

The Windows x64 installer `pathlab_0.4.6.jmo` from source
`6849ad9cf6c6e9072a2f11ca920fd7e05b400cd0` was installed in the existing Store
jamovi Desktop 2.7.39.0 profile using its supported installation command. The
previous module was backed up. Installer SHA-256:
`7daa8e5883c887d0fabbb4c2dac641c411d9418efe86d614b7017844b366c65d`.
All 38 installed files, 59 R functions and 89 R6 methods match the source/build.
The web application and analysis implementation were unchanged.

## Passed checks

- Two disjoint installed suites: 25 gallery/cache/plot cases, 11,279 assertions;
  26 categorical/data/cancellation cases, 12,414 assertions. Zero failures,
  errors, skips or warnings. They use installed implementation bindings, shared
  saved-index Python references and independent R equation/covariance checks.
- Actual Desktop-computed and saved continuous, categorical-linear, binary
  logistic and joint categorical-logistic examples: 1,418 saved-result assertions.
  These include full categorical native tables and independent point-polynomial
  checks; the same joint-logistic archive has separate table/plot checks, not
  two independent GUI runs. Table verification uses the same R seed as Desktop;
  cross-language bootstrap parity uses shared saved resample rows separately.
- Forty-five further lifecycle checks: saved presentation edits, exact scientific
  equality, cache identity where reused, missing rules, filter metadata, empty-
  input clearing and completed restart state. Continuous/logistic reopened fits
  retain fit ID, input fingerprint and creation time after presentation changes.
- Invalid categorical reference clears results; restoring it recomputes correctly.
  A real `x > 0` filter delivers 45 rows. The `m1 < -100` user missing rule yields
  43 complete cases. An empty filter clears tables, plots and private fit state;
  deactivating it restores 98 of 100 cases with the missing rule retained.
- Saved analyses reopen with correct role order, explicit graph, numerical output
  and plots. The missing rule and disabled filter survive another Desktop open.
  Labels containing spaces, brackets and Unicode fit and save correctly.
- A running 5,000-draw bootstrap on 9,000 synthetic rows was replaced by a
  200-draw request. Progress changed to the new request and the completed saved
  result matches a fresh same-seed native calculation. The next observation
  was about six seconds after the edit. This is one responsiveness observation,
  not a general timing benchmark or fixed interruption-latency guarantee.
- Continuous HTML export: 21 embedded-image checks passed, including all three
  declared interaction plots. PNGs match the saved analysis assets.

The 143-case / 58,244-assertion full R CMD check and 20-cell rebuilt-installer
simulation replay were completed earlier on this same analysis source. They
were not rerun or added to the fresh installed-suite totals here.

## Open checks and limitations

**Missing-rule reopen cache miss:** the 98-case analysis recomputed after reopening
and a covariance-display edit. The input fingerprint, fit ID and creation time
changed; all scientific output and plot coordinates were identical. The cause of
the changed fingerprint is not established. Correctness passed; cache reuse did
not. Reproduce and diagnose the input/state representation before deciding a fix.
Do not weaken invalidation merely to force reuse.

**GUI resizing:** attempts to drag plot corners did not establish a changed saved
image dimension. They are excluded as passing evidence. The native installed
suite covers six resize controls and three renderers at small/large sizes, and
earlier GUI resize evidence remains historical. A fresh visible handle resize
and save/reopen verification remain required for the full candidate sweep.

A private harness initially compared `-.5,.7` and `-0.5,0.7` as strings. It now
compares the exact numerical probe values. All 405 joint categorical/logistic
table assertions pass; no module analysis change was needed.

The full release gate remains open for these two Desktop items, methods/owner
review of measured simulation limitations, the intended public-source freeze,
and current-series/other-platform compatibility. TeX/PDF-manual compilation
remains unverified. Publication and submission still require separate authority.
