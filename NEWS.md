# psychTestR 2.0.0

* Internationalisation is now supported, allowing researchers
to create tests that can be taken in multiple languages
(see the function `i18n()`).
* `loop_while()` now loops while the test condition is satisfied,
and stops when it is not satisfied (rather than vice versa).
* Added keyboard shortcuts for inserting `i18n()`, `psychTestR::i18n()`,
and `psychTestR::`.
* Automatic compilation of results into csv files now supports
multiple answers from the test page. 
For example, if a page labelled `A` returns `list(x = 3,  y = 4)`
then this will be parsed into two columns `A.x` and `B.y`.
