# Action plan

## Algorithmic logic - F#

**RESPONSIBILITY**: Brian & Caydan

### Solving with algorithms & getting results to display/save

An abstracting interface must allow higher-level business logic to provide a problem's formulation, and compute it in one of a set of algorithms.
No implementation details should be revealed, such as the necessity of added constraints or algorithmic trickery to solve the problem.

It is the higher-level logic's responsibility to validate the problem before passing it on (review the Usability conditions for each of the algorithms below)

The following algorithms will be available:

- Primal Simplex algorithm:
    - Usability condition: No integer constraints
- Revised Primal Simplex
    - Usability condition: No integer constraints
- Revised Branch and Bound Simplex Algorithm
    - Usability condition: None
- Revised Cutting Plane Algorithm
    - Usability condition: None
- Branch and Bound knapsack algorithm
    - Usability condition: All decision variables must be binary

Every algorithm will return a result object, which can be used to generate the various displays needed for the specific algorithm.

(For simplex this means tableau, for revised simplex, this means matrix forms, etc.)

This method-return is necessary, as some display characteristics don't always match the algorithmic internals.
This also allows delaying calculating the conversion to displayable characteristics, as that also takes compute time, and doesn't necessarily have a one-long-page thing.

### Sensitivity analysis

A result object should provide an API based on the original LP formulation, so that sensitivity statistics can be dynamically calculated at display time
It should also say if the result was optimal, infeasible or unbounded

For dual programming statistics, the F# LP formulation object should allow being transformed into a dual problem.

## User interface & I/O - C#

**RESPONSIBILITY**: Keleabetswe & Qiniso

### Controversy: Framework usage

Windows Forms is the most simple way to do the front-end, as it is the most familiar.

However, I don't have Windows anymore. So, I will find it incredibly hard to help out with anything on the UI if things come down to the wire.
So, it is my preference that we would use something cross-platform so that I can help out if things break.

Otherwise, I won't even be able to verify that the program actually works without spending an hour getting AVD to work (a struggle that I think we're all familiar with).

A nice option for this that I have discovered is called Avalonia UI. There is a Visual Studio plugin for previewing it while working, and we work in XML files.

### Problem formulation interface

A user should be able to provide an LP problem formulation in one of 2 ways:

- Manual inputs
- Input file

*If the project is done, a nice feature here could be to support copy-pasting parts of the LP problem from Excel via TSV parsing*

The user interface & I/O layer is responsible for aqcuiring the LP formulation, including:

- The objective function, and whether it's min or max
- All the constraints, meaning the variable coefficients, right hand side, and their signs (<=, >=, =)
- The sign restriction for every constraint (`+`, `-`, `urs`, `int`, `bin`)

The F# module will be able to convert the LP formulation to canonical form for displaying a preview to the user for the formulation they entered.

For the input file format, consult the project file for the format specification that they are expecting from us.

*If the project is done, a nice feature here could be a more nicely set-up file format*

### Problem validation & algorithm selection

Depending on the nature of the LP formulation, it is only allowed to be passed to specific algorithms. The UI will have to filter this out, or mark those algorithms as insefficient.

For example, you can't just use primal simplex if some decision variables have to be integers, and for knapsack, all the variables have to be binary.

When the algorithm is selected and the user decides to compute the solution, the LP formulation must be passed to the F# module, where it will be computed.

### Displaying results

When the problem's computation is finished, the results should be displayed to the user. This doesn't all have to be on one screen (as the result object will be callable for different parts of the solution).

The exact things being displayed here will differ according to the algorithm being used, consult the project file for every algorithm as you develop.

The output should also be saveable as a file. The exact API between modules here will still be ironed out when we get there.

### Sensitivity analysis

On the result display screen, the user should be able to view sensitivity statistics for specific variables

## Video presentation

This I don't know yet, probably discuss first.
