(include "__baselib.scm")

; Import an external library:
(!js-require _ underscore)

; Run some statements:
(!s "console.log(\"Running an statement!\");")
(display "This is a test text.\n")

; Call external library:
(display "Size of [1, 2, 3, 4, 5, 6] is: ")
(display (!xa _.size (!xa Array 1 2 3 4 5 6)))
(display "\n")
