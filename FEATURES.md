# Feature states: proposed 0.4.6 public beta

This is the bounded observed-variable subset prepared for owner approval.
Implementation, numerical checks, Desktop evidence and public availability are
separate states. No public repository, submission or Library acceptance exists.
All runtime and test files remain unchanged from tested analysis source
`cba2741a67202521e13159967fa94c727a3ca376`; the final preparation edits documentation.

## Implemented methods and native controls

| Capability | Implemented scope | Validation / limit |
| --- | --- | --- |
| Explicit observed-variable graphs | Native Original editor and General graph use the same R compiler; selected arrows never silently disappear | Shared-index and handwritten-equation references; no latent SEM or global SEM fit |
| Simple/parallel/serial/combined mediation | Optional legacy M1/M2 and up to 10 ordered continuous general mediators | Core/general fixtures, historical larger-graph Desktop examples; maximum-size graphs not exhaustively calibrated |
| Ordinary/direct/multiple-stage moderation | Up to 3 moderators, explicit attachments, repeated W and hierarchy-complete joint terms | Classical/HC3 numerical references, polynomial derivatives and joint fixtures; no automatic lower-order completion |
| Covariates | Up to 20, assigned explicitly to individual equations | Common-sample and design/covariate tests; not implicit additional mediator routes |
| Categorical X/moderators/covariates | Exact role/reference coding; 2-10 levels, explicit indicator expansion and focal contrasts | Mixed/categorical saved-index, independent R/NumPy references; sparse-cell rank/bootstrap failure and undercoverage disclosed |
| Continuous equations | OLS with classical or HC3 covariance | R formula and manual QR/covariance references; normal HC3 coefficients/slopes, overall Wald F, block chi-square |
| Binary logistic Y | Explicit event mapping, Newton fitting, separation/rank/convergence guards; model-based or generalized HC3 covariance | Shared-index Python, independent R GLM and matrix references; 3 documented near-boundary separation-classification exceptions |
| Conditional effects and indices | Route products, totals, degree-aware derivatives and paired condition/index contrasts | Algebra and same-index full-distribution checks; logistic products remain coefficient/log-odds statistical associations |
| Probes and bootstrap | Original-sample fixed percentiles/categories/custom probes; paired case resampling and percentile intervals | Fixed saved indices in both languages; >=95% successful draws, failed draws not replaced; no BC/BCa or universal nominal coverage |
| Conditional slopes and JN | All equation terms involving focal source, declared category contrast, stated fixed conditions | Analytic/covariance and Python boundary/plot references; continuous moderator, pointwise path slopes only |
| Output | Fitted equations/model tests, coefficients, coding, conditional effects, slopes/JN, explicit model diagram, detailed JSON | Native result-class, row identity/label/escaping and saved-result checks; native human-readable interaction-block tables remain follow-up work |
| Effect/interaction/JN plots | Native resizable images, effect pages (12 rows), selected arrow or all moderated-arrow gallery (6 images/page) | Independent plot-coordinate/interval references; gallery slices hold other moderators fixed and do not prove joint support |

## Lifecycle and packaging

| Capability | Current state and evidence boundary |
| --- | --- |
| Fit cache | Display, detail, covariance and resize changes reuse a valid summary; data/model/inference edits invalidate. Safe data-only saved cache passes typed/two-instance tests and final Windows28 restart checks. |
| Missing/filtered data | One common complete-case sample after host filter/user-missing rules; numerical delivered-row mapping where available. Actual source-specific filter/missing/import checks are in VALIDATION.md. No imputation or missing-data identification claim. |
| Invalid input and cancellation | Explicit error/clearing and cooperative restart checkpoints; no partial scientific result published. Native cancellation/invariance tests and historical 9,000-row Desktop replacement evidence; no fixed maximum cancellation latency. |
| Save/reopen and resizing | Current Windows28 final first-display-edit cache checks and numbered-gallery dimensions persist through full restart/save/reopen/export, including user-missing data. |
| Export/import | Historical OMV/HTML/CSV/Special Import checks retain their exact-source scope. CSV preserves data rather than model/filter/missing-rule metadata. PDF and other formats are not claimed. |
| Windows28 x64 JMO | Real build, install and code audit on 28.14/R4.6: 39 files, 63 functions, 89 R6 methods match. Full installed suite 147 cases / 58,289 assertions; final targeted Desktop 158 result, 22 lifecycle and 11 HTML-image checks. No repeated complete all-feature GUI sweep after the last lifecycle fix. |
| Retained Windows2.7 x64 JMO | Earlier source 2c04290, tested in 2.7.39/R4.5: full test-enabled R check 144 cases / 58,257 assertions; installed focused 52 / 23,706 plus GUI evidence. Latest source has focused R4.5 tests (12 / 303), not a refreshed exact-source 2.7 installer/GUI pass. |
| Documentation/data/branding | English guides, three synthetic Data Library examples, citation, GPL >= 2 and original Model59-inspired SVG/PNG prepared. Host toolbar icon remains generic. |
| Publication | Module-only source archive and per-file hashes prepared for approval. Contact/license decision received; source rights/beta approval and later separate repository/upload/submission authorizations remain. |

See [VALIDATION.md](VALIDATION.md) for exact source/runtime scopes rather than
adding overlapping test totals. Tests include saved resample fixtures and
independent references; R/Python agreement alone is not inferential calibration.

## Measured calibration and unsupported scope

The [20,000-dataset study](release-review/simulations/REPORT.md) reports parameter
recovery, interval coverage, false positives and failures with Monte Carlo
uncertainty. In one sparse categorical N=100 design, 164/1,000 fits failed and
a selected null indirect excluded zero in 89/836 successful fits (10.65%).
Double-null serial products were conservative (0.1-0.6% false positives).
No universal N threshold or uniform nominal coverage is established. The study
uses specified two-serial-mediator DGPs at N=100/300; it is not a calibration
study of every graph accepted by the interface. External methods peer review
and a matched lavaan/jAMM case have not been obtained.

Unsupported: categorical mediators, weights, clustering/dependent-row inference,
latent measurement models, missing-data modeling and natural causal/probability
mediation effects. A drag-and-drop graph canvas, more friendly diagnostic tables,
translations and supported custom toolbar integration are follow-up interface
work, not requirements for this bounded beta. Other OS/ARM/Cloud and TeX/PDF
manual compilation are unverified. No Windows runtime prerequisite is missing.

Budgets include 150 explicit path rows and 150 interaction rows, 80 terms per
equation, 100 routes, 25 probe combinations and 8,000 result rows, plus matrix/
regression/derivative work bounds. Categorical expansion counts toward them.
Three moderators with three probes each exceed the condition budget; specify
fewer probes. Counts do not guarantee rank, estimability or adequate joint support.
No paths, coding levels or conditions are silently removed to obtain a fit.
