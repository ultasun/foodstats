# Foodstats
### A utility for tracking animal nutrition facts

*Foodstats* can help users track the following attributes regarding dietary habits:
- A [*UPC*](https://en.wikipedia.org/wiki/Universal_Product_Code) database to store [*Nutrition Facts*](https://en.wikipedia.org/wiki/Nutrition_facts_label) information,
- Inventory status (how many units, their price, and their expiration dates) of a *UPC*,
- A meat/deli wizard to calculate the exact *Nutrition Facts* for a temporary package of fresh food from a deli or meat counter at the grocery store,
- Calculate exact *Nutrition Facts* of a given *UPC* multiplier without affecting inventory,
- Log consumption of a given *UPC* multiplier and updating inventory,
- View leftover *UPC* units from incomplete consumption,
- Log consumption of some amount of a leftover *UPC*,
- Role-aliases for a *UPC*,
- Named recipe consisting of portions of a *UPC* and/or portions of a role-alias,
- Calculate *Nutrition Facts* of a recipe multiplier without affecting inventory,
- Simulate cooking a recipe using only new inventory (no leftovers),
- Cooking a recipe using only new inventory (without using leftovers),
- Simulate cooking a recipe using leftovers first and new inventory second,
- Cooking a recipe using leftovers first and new inventory second,
- Manage leftovers from recipes,
- Calculate recipe cook cost considering that leftovers were recycled,
- Calculate recipe cook procurement cost when using only new inventory,
- Generate report of total cumulative *Nutrition Facts* from the log between two points in time,
- Generate report of total cost of units consumed between two points in time,
- Calculate periods of inventory available of a specific nutritional attribute (such as calories or protein),
- Calculate attribute-per-penny of a *UPC* list,
- Calculate the ratio of attribute-per-penny between two *UPC*'s,
- Calculate attribute-per-penny of a recipe,
- Calculate the ratio of attribute-per-penny between two recipes,
- Know total value of all assets in inventory,
- Know the total value of all assets consumed from inventory (from the log)

Other features as requested, open an issue!

### Higher motive
*Foodstats* is a **theme** which is appreciated by [*ultasun*](https://github.com/ultasun) when practicing a programming language (or paradigm). This version of *Foodstats*  is to practice the [*functional programming paradigm*](https://en.wikipedia.org/wiki/Functional_programming) whilst utilizing [*Common Lisp*](https://en.wikipedia.org/wiki/Common_Lisp).

If you are a subject area expert of those three topics (nutrition, *functional programming*, or *Common Lisp*), then I would appreciate any feedback! Feel free to open an issue in the issue tracker, or [message me directly](#Credits). 

# Installing
### Automatic
A *Docker* image is available [on the hub](https://hub.docker.com/r/ultasun/foodstats).

`docker run -it ultasun/foodstats`

The data produced during runtime is saved in the `/app/data` directory, so it may be optionally bind-mounted wherever is desired.

***Note:*** The *Docker* tag will likely change in the future to something more specific, possibly `foodstats-cl-fp`. Stay aware!

### Manual Installation

As of this writing, the author has written the entire system in [GNU Clisp 2.49](https://www.gnu.org/software/clisp/).  The author has also tested the program under [Armed Bear Common Lisp 1.9.0](https://armedbear.common-lisp.dev).

1. Clone the repository
2. Run
`clisp -repl foodstats.lisp`
3. Enjoy!

# Usage
After starting the program, the user is presented with a number menu. Generally, the user would start with lower-numbered options, and work their way up. Good luck!

The author uses the system to track their personal dietary habits.

# Credits
This is *alpha* quality software. Please feel free to submit bugs in the issue tracker!

The software was written by a lone author, *ultasun*. I may be reached on [Libera.Chat](https://libera.chat/), feel free to message me directly!

Thank you for reading!
