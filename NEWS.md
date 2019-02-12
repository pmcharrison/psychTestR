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
