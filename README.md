# gloss-modeling
Implementation of the [Nature of Code](https://natureofcode.com) book examples in Haskell using Gloss to implement the graphics.

## Usage

I recommend using [Stack](https://docs.haskellstack.org) to manage the package dependencies. To compile the whole project:

`stack build`

should be sufficient. Call each example executable code through stack as well:

`stack exec <demo>`

in which `<demo>** is one of the demos of the following list. On all demos, click `esc` to close the window and finish the simulation.

### List of available demos

**gloss-preliminaries**: Shows how to create a window and draw a static ball in it.

**example_01_01**: Creates a moving ball that bounces on the screen.

**example_02_01**: Creates a moving ball that bounces on the screen under the influence of gravity and a east directed wind.

**example_02_02**: The same as `example_02_01` but with an array of balls strating at random positions.

**example_02_03**: The same as `example_02_02` but with a correction to make gravity behave more like real world physics.

**example_02_04**: The same as `example_02_03` but now with friction.
