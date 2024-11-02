# psychTestR 2.27.2

- Throw an error when a dictionary contains a duplicate key (thanks Killian Sander!)

# psychTestR 2.27.1

- Fixed regression introduced in v2.27.0 (thanks Seb Silas!)

# psychTestR 2.27.0

- Added support for collecting IP address, geolocation, and other browser information (thanks Seb Silas!)

# psychTestR 2.26.0

- New option for adding URL to logo (thanks Seb Silas!)
- `on_start_fun` now optionally accepts two arguments: `state` and `session`.

# psychTestR 2.25.0

- Add hooks for running functions at start and end of sessions (thanks Seb Silas!).

# psychTestR 2.24.2

- Revert Klaus's changes to `audio_NAFC_page` autoplay, which had caused autoplay to stop working.

# psychTestR 2.24.1

- Bugfix in response validation, which was causing participant sessions to crash.

# psychTestR 2.24.0

- Bugfix in `audio_NAFC_page` autoplay (thanks Klaus Frieler!).
- Extra options for `checkbox_page` (thanks Klaus Frieler!).
- Better support for additional scripts (thanks Seb Silas!).
- Support for lab logos (thanks Seb Silas!).

# psychTestR 2.23.4

- Disabled `clean_session_dir` as an attempt to avoid sporadic errors of the 
  form "Error in gzfile: all connections are in use" on long-running servers.

# psychTestR 2.23.3

- Bugfix for latest version of markdown package (thanks Klaus Frieler!)
- Added video tutorial (thanks Anthony Chmiel!).

# psychTestR 2.23.2

- Fixed checkbox validation.

# psychTestR 2.23.1

- Fixed `enable_admin_panel = FALSE` option.

# psychTestR 2.23.0

- Improved CSV export (thanks Klaus Frieler!).
- Extra parameters for audio pages (thanks Klaus Frieler!).

# psychTestR 2.22.3

- Expose button_style in audio_NAFC_page.

# psychTestR 2.22.2

- Rebuild documentation.

# psychTestR 2.22.1

- Increase default session timeout parameter to 1 month.

# psychTestR 2.22.0

- Allow `problems_info` to be rendered as HTML.

# psychTestR 2.21.2

- Forcing language identifiers to be lower-case for consistency with ISO conventions.

# psychTestR 2.21.1

- Allowing navigating away on the final page.

# psychTestR 2.21.0

- Added `allow_download` argument to audio_NAFC_page.
- Improved default `btn_play_prompt` argument for audio_NAFC_page.
- Adding corresponding arguments to video_NAFC_page.

# psychTestR 2.20.0

- Added `show_controls` argument to audio_NAFC_page.

# psychTestR 2.19.1

- Adding an input check for conditional().

# psychTestR 2.19.0

- Misc improvements to website.

# psychTestR 2.18.0

- Drafted introductory paper (in `inst/`) which has now 
been deposited on PsyArXiv.
- Updated citation information in README.
- Added CITATION file.

# psychTestR 2.17.0

- Added `advance_delay` option, customising the delay before advancing 
the next page. 
This replaces the hardcoded 500 ms in previous psychTestR versions.
The new default is 0 ms.
- Created new automated tests using `shinytest` package,
which supplement and replace the original manual tests.
- Exposed the `AppTester` class for creating custom automated tests.
- Improved and documented the treatment of nested modules.
- Introduced the `slider_page` test element.
- Wrote a series of new vignettes.

# psychTestR 2.16.0

- Exposed `btn_play_prompt` in `volume_calibration_page`.
- Added dialog to check whether user is sure they want to quit the test.

# psychTestR 2.15.0

- Added option to manually show the admin panel when show_footer = FALSE.

# psychTestR 2.14.0

- Added \code{admin_panel} argument to \code{test_options},
allowing the researcher to disable the admin panel.

# psychTestR 2.13.3

- psychTestR now throws an error on nested calls to `new_timeline()`,
which sometimes worked as intended, sometimes not...

# psychTestR 2.13.2

- Fixed issue where `c()` didn't work well for all combinations 
of element types, replacing it with a new function `join()`,
a more reliable method for achieving the same result.
This in turn addresses some problems observed with `order_at_run_time()`.

# psychTestR 2.13.1

- Fixed `finish_test_and_give_code()`, which was not working properly.
- Fixed bug where `order_at_run_time()` failed when there was only one block to randomise.

# psychTestR 2.13.0

- Updated `order_at_run_time()` and `randomise_at_run_time()`
so that they can be nested arbitrarily many times.

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
