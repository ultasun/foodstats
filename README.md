# Foodstats
#### A utility for tracking animal nutrition facts

*Foodstats* is a **theme** which is appreciated by [*ultasun*](https://github.com/ultasun) when practicing a programming language (or paradigm). This version of *Foodstats*  is to practice the [*functional programming paradigm*](https://en.wikipedia.org/wiki/Functional_programming) whilst utilizing [*Common Lisp*](https://en.wikipedia.org/wiki/Common_Lisp).

If you are a subject area expert of those three topics (nutrition, *functional programming*, or *Common Lisp*), then I would appreciate any feedback! Feel free to open an issue in the issue tracker, or message me directly. 

# Installing
#### Automatic
A *Docker* image is available [on the hub](https://hub.docker.com/r/ultasun/foodstats).

`docker run -it ultasun/foodstats`

The data produced during runtime is saved in the `/app/data` directory, so it may be optionally bind-mounted wherever is desired.

***Note:*** The *Docker* tag will likely change in the future to something more specific, possibly `foodstats-cl-fp`. Stay aware!

#### Manual Installation

As of this writing, the author has written the entire system in [GNU Clisp 2.49](https://www.gnu.org/software/clisp/).  The author has also tested the program under [Armed Bear Common Lisp 1.9.0](https://armedbear.common-lisp.dev).

1. Clone the repository
2. Run
`clisp -repl foodstats.lisp`
3. Enjoy!

# Usage

After starting the program, the user is presented with a number menu. Generally, the user would start with lower-numbered options, and work their way up. Good luck!

The author uses the system to track their personal dietary habits.

# Credits / Support 

This is *alpha* quality software. Please feel free to submit bugs in the issue tracker!

The software was written by a lone author, *ultasun*. I may be reached on [Libera.Chat](https://libera.chat/), feel free to message me directly!

Thank you for reading!
