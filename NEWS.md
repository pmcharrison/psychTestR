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
