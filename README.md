This is an example of how to use Haskell to talk to an
[Open Pixel Control][1] server, such as a [FadeCandy][2], to control
strings of RGB LED lights.

If run with no arguments, `hs-opc-client` sets the string color to all
white.  If run with one argument, the argument must be one of the
color names black, blue, cyan, green, magenta, orange, purple, red,
white, or yellow.  If run with three arguments, each argument must be
an integer between 0-255, forming an RGB triple.

For more information, see [my blog post][3].

[1]: http://openpixelcontrol.org/
[2]: https://github.com/scanlime/fadecandy/
[3]: http://funwithsoftware.org/posts/2016-12-22-programmable-christmas-lights-with-haskell-and-raspberry-pi.html
