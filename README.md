An attempt at reducing creative friction between the artist and the
artwork.


Running
-------

Put this somewhere quicklisp can find it. Then,

    (ql:quickload :lgame)

Alternatively, load it using ASDF, or just manually from a REPL.

After that, get in the package,

    (in-package :lgame)

Now you're ready to roll! Just run,

    (run-game)        ; opens a window and starts the event loop
    (start-systems)   ; starts the basic systems
    (test-huge 200)   ; create some blocks!

Closing the window simply stops the event loop, the system state is
still maintained. So you can close the window and `(run-game)` again
to continue. Use `(stop-system)` to stop all systems and
`(restart-systems)` to restart them.

Check the bottom of lgame.lisp to see how the oscillator and rotator
systems are defined. You can interact with the game in real-time using
the REPL. For example, `(restart-systems)`, then run `(test)`, then
`(setf (prop =oscillator= rate *player*) 3)`.

