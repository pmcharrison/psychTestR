# psychTestR 2.12.0

- Added `conditional()`, which allows test elements to be displayed conditionally at run time.
- Added `order_at_run_time()`, which orders test elements at run time.
- Added `randomise_at_run_time()`, which randomises test elements at run time.
- CSV export now supports columns containing atomic vectors.

# psychTestR 2.11.0

- Allowed users to customise the text used to trigger audio/video playback.
- Minor improvements to documentation.

# psychTestR 2.10.0

- Now the test will accept URL parameters even when enable_resume_sessions = FALSE.
- Introduced allow_url_rewrite option; when FALSE, URL rewriting is disabled.

# psychTestR 2.9.0

* Added `as.list.test_element`.
* Added introductory vignette.
* Added tutorial on hosting media files.

# psychTestR 2.8.0

* Added `display_options` for greater control over display options.
* Rewrote jsPsych integration tutorial.
* Fixed CMD check complaint about `:::` calls.

# psychTestR 2.7.0

* Added randomisation tutorial.
* Deprecated `loop_while`, introduced `while_loop`.
* Added `as_tibble.results`.
* Improved the flexibility of `as.data.frame.results`,
it can now deal with atomic vectors of length > 1 in results slots.

# psychTestR 2.6.1

* Added missing documentation for `i18n_dict$edit`.

# psychTestR 2.6.0

* Added functionality for editing dictionaries.

# psychTestR 2.5.1

* Fixed encoding issue in markdown (#44).

# psychTestR 2.5.0

* Added internationalisation for `test_options()`.

# psychTestR 2.4.3

* Fixed #37.

# psychTestR 2.4.2

* Bugfix for implicit timeline combination.

# psychTestR 2.4.1

* Minor edit.

# psychTestR 2.4.0

* Created psychTestR website.

# psychTestR 2.3.0

* Changed UI panel backgrounds from grey to white.

# psychTestR 2.2.2

* Improved checks in timeline creation
* Removing unused dependencies.
* Bugfixes: #2, #3, #24, #26, #31, #32

# psychTestR 2.2.1

* Fixed a bug in test session resuming using `get_p_id()` pages.

# psychTestR 2.2.0

* Allowing extra attributes to be passed to `trigger_button()`.
* Adding an optional time delay before `trigger_button()` is activated.

# psychTestR 2.1.0

* Adding `as.timeline()`.
* If i18n() is called without a dictionary, a warning rather than an error
is thrown.
* Revising reactive dependencies in `state` objects - 
these objects now take reactive dependencies on much fewer slots.
This seemed necessary to solve some logic problems in 
potential reactive page designs.
* Adding `demo = TRUE` default to `demo_options()`.

# psychTestR 2.0.3

* Bugfix - Implicit timeline creation with `c()` no longer
throws an error when given `NULL` elements.

# psychTestR 2.0.2

* Bugfix - Allowing `NULL` `researcher_email` argument in `test_options()`.

# psychTestR 2.0.1

* Bugfix - language compatibility checks were not being performed properly

# psychTestR 2.0.0

* Internationalisation is now supported, allowing researchers
to create tests that can be taken in multiple languages
(see the function `i18n()`). This functionality is still in alpha,
as there are some loose ends to be tightened up
(in particular, certain error messages appearing in English).
* `loop_while()` now loops while the test condition is satisfied,
and stops when it is not satisfied (rather than vice versa).
* Added keyboard shortcuts for inserting `i18n()`, `psychTestR::i18n()`,
and `psychTestR::`.
* Changed default theme to 'yeti' (previous theme was 'readable')
