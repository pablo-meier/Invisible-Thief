# Invisible Thief

## A Whitespace implementation in Racket

### Why?

Because why not? [Whitespace][1] is a cute idea, and the [Racket][2] folks like
to advertise their language as a little language laboratory, as well as tauting
the power of macros.

Inspired by Danny Yoo's [F\*dging up a Racket][3] and Greg Hendershott's 
[Fear of Macros][4], I thought I'd give it a go. My original goal was to make
this into a proper Racket language (per Danny's post), but somewhere in trying
to configure Racket's Reader's default settings I decided just to make it a
command-line interpreter instead.

### How?

If you have Racket installed, just `racket main.rkt <whitespace file>`, or
`make` will run `raco exe` such that you'll have an executable named
`whitespace`, which accepts a filename as an argument.

Play with the examples in the examples/ directory and give it a try ^\_^

### Performance?

No. This is slower than a turtle racing up a hill dripping molasses.

   [1]: http://compsoc.dur.ac.uk/whitespace/
   [2]: http://racket-lang.org
   [3]: http://hashcollision.org/brainfudge/
   [4]: http://www.greghendershott.com/fear-of-macros/

