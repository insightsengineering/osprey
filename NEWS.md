# osprey 0.1.12.9005

### Miscellaneous
* Removed `rtables` dependency.
* Functions from imported packages are now fully specified.

# osprey 0.1.12

### Enhancements
* Modified `g_swimlane` plot to expand its y-axis range based on vertical line inputs.

### Breaking changes
* Renamed `ylab`, `yref_line` and `ytick_at` arguments to `xlab`, `xref_line` and `xtick_at` for `g_swimlane` as they refer to the `x` not `y` axis.

### Miscellaneous
* Updated R version requirement to `R >= 3.6`.
* Removed dependency on `test.nest` package.
* Removed dependency on `utils.nest` package and replaced its functions with equivalents from the `checkmate` package.

# osprey 0.1.11

* Updated `LICENCE` and `README` with new package references.
* Fixed the `sort_by` issue in `g_butterfly`.
* Update example and tests using `scda` synthetic data to replace `random.cdisc.data`.
* Added `error_on_lint: TRUE` to `.lintr`.
* Removed unneeded `importFrom` statements.
* Fixed bug in `g_swimlane` that incorrectly orders the legend in certain scenarios.

# osprey 0.1.10

* Updated and cleaned up package internals.

# osprey 0.1.9
* Added new plot function Heatmap by Grade Plot `g_heat_bygrade`.
* Added new plot function Adverse Event Category Plot `g_ae_sub`.

# osprey 0.1.8
### Miscellaneous
* Set up a `.lintr` configuration file.
* Remove NA when calculating `max_day` in the example.

# osprey 0.1.7
### Bug fixes
* Fixed the issue in `g_waterfall` with simultaneous plot facet and annotation labels.
* Fixed `g_events_term_id` for the case when no data exists fora given treatment arm and a given term are given.

### Miscellaneous
* The `right_flag` and `left_flag` arguments of `g_butterfly` should now be `logical` vectors instead of integers (1/0).
* Retired four table functions due to duplicated functionalities in `tern`:
  - `t_ae_oview`: AE overview summary table.
  - `t_ae`: AE summary table by preferred terms.
  - `t_ae_ctc`: AE summary table by highest NCI-CTCAE grade.
  - `t_ds`: Disposition table.

# osprey 0.1.6

* Refactored `g_events_term_id` to use `dplyr` and `tidyr` functions over `data.table`.

# osprey 0.1.5

* Added new plot functions:
  - Events by term plot `g_events_term_id`, which can be used to plot events (AE) by terms or AE overview.
  - Patient profile plot `g_patient_profile`.

# osprey 0.1.4

* Work on `g_swimlane`, `g_waterfall`, `t_ae_ctct_v2`, and `utils`.

# osprey 0.1.3
* Force `grade_levels` argument in `t_ae_ctc_v2` function to be a factor.

# osprey 0.1.2
* Refactor due to changes in `utils.nest`.

# osprey 0.1.1

* Refactoring package according to the NEST standards:
  - Including test.nest tests.
  - Cleaning dependencies.
  - Fix tm_g_butterfly faceting error.
  - Add sorting by right or left wing.
  - Adding g_waterfall.

# osprey 0.1.0

* First versioned release of osprey package, which include the following seven new TLG analysis functions:
  - AE overview summary table.
  - AE summary table by preferred terms.
  - AE summary table by highest NCI-CTCAE grade.
  - AE butterfly plot.
  - Disposition table.
  - Swimlane plot.
  - Spiderplot.
* Few utility functions for working in BCE:
  - Function to quickly load snapshot data from BCE.
  - Function to output graphic objects in PDF.
  - Formatting graphic objects and outputting PDF that is IDM-compatible.
