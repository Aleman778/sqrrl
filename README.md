# The Sqrrl Programming Language
The main source code repository for the Sqrrl Programming Language, the core libraries, documentation and examples.
This language is aimed at creating a better C language that doesn't include all the crazyness in C++.
Improving the C syntax is not the focus with this language, a lot of new languages tries to perfect the syntax but I don't have any major issues with the C syntax
so it will stay almost the same (with some additions).
The syntax will be very close to the syntax used in this compiler which is C++ but almost C compatible, this codebase will not make use of templates (except std_ds.h uses it), classes, OOP and definitively not concepts.
Keeping a similar syntax means that it will be possible parse C headers so there won't be a need for creating wrappers to create C libraries.
The focus of this language is instead on tidying up all of the bad design decisions made in C which haven't been addressed in C++ and many other languages
(NOTE: some but NOT all of these have been addressed in C++).
