(defsystem :nature-of-code
  :version "0.1"
  :author "Mark Gerarts <mark.gerarts@gmail.com>"
  :depends-on (:trivial-gamekit
               :cl-bodge
               :black-tie)
  ;; We get such a messy component definition because the directory structure
  ;; is set up to be easily browsable, with a README for each example.
  :components ((:module "introduction"
                :pathname "0. Introduction"
                :components
                ((:file "nature-of-code.introduction.example-1"
                  :pathname "Example I.1: Traditional random walk/sketch")
                 (:file "nature-of-code.introduction.example-2"
                  :pathname "Example I.2: Random number distribution/sketch")
                 (:file "nature-of-code.introduction.example-3"
                  :pathname "Example I.3: Walker that tends to move to the right/sketch")
                 (:file "nature-of-code.introduction.example-4"
                  :pathname "Example I.4: Gaussian distribution/sketch")
                 (:file "nature-of-code.introduction.example-5"
                  :pathname "Example I.5: Perlin noise walker/sketch")
                 (:file "nature-of-code.introduction.example-6"
                  :pathname "Example I.6: 2D Perlin noise/sketch")
                 (:file "nature-of-code.introduction.exercise-3"
                  :pathname "Exercise I.3: Dynamic walker/sketch")
                 (:file "nature-of-code.introduction.exercise-4"
                  :pathname "Exercise I.4: Gaussian paint/sketch")
                 (:file "nature-of-code.introduction.exercise-5"
                  :pathname "Exercise I.5: Gaussian random walk/sketch")
                 (:file "nature-of-code.introduction.exercise-6"
                  :pathname "Exercise I.6: Custom distribution walk/sketch")
                 (:file "nature-of-code.introduction.exercise-7"
                  :pathname "Exercise I.7: Perlin noise step size walker/sketch")
                 (:file "nature-of-code.introduction.exercise-8"
                  :pathname "Exercise I.8: 2D Perlin noise colour/sketch")
                 (:file "nature-of-code.introduction.exercise-9"
                  :pathname "Exercise I.9: Moving 2D Perlin noise/sketch")))
               (:module "vectors"
                :pathname "01. Vectors"
                :components
                ((:file "nature-of-code.vectors.example-1"
                  :pathname "Example 1.1: Bouncing ball with no vectors/sketch")
                 (:file "nature-of-code.vectors.example-2"
                  :pathname "Example 1.2: Bouncing ball with vectors/sketch")
                 (:file "nature-of-code.vectors.example-3"
                  :pathname "Example 1.3: Vector subtraction/sketch")
                 (:file "nature-of-code.vectors.example-4"
                  :pathname "Example 1.4: Multiplying a vector/sketch")
                 (:file "nature-of-code.vectors.example-5"
                  :pathname "Example 1.5: Vector magnitude/sketch")
                 (:file "nature-of-code.vectors.example-6"
                  :pathname "Example 1.6: Normalizing a vector/sketch")
                 (:file "nature-of-code.vectors.example-7"
                  :pathname "Example 1.7: Motion 101 (velocity)/sketch")
                 (:file "nature-of-code.vectors.example-8"
                  :pathname "Example 1.8: Motion 101 (velocity and constant acceleration)/sketch")
                 (:file "nature-of-code.vectors.example-9"
                  :pathname "Example 1.9: Motion 101 (velocity and random acceleration)/sketch")
                 (:file "nature-of-code.vectors.example-10"
                  :pathname "Example 1.10: Acceleration towards the mouse/sketch")
                 (:file "nature-of-code.vectors.example-11"
                  :pathname "Example 1.11: Array of movers accelerating towards the mouse/sketch")
                 (:file "nature-of-code.vectors.exercise-4"
                  :pathname "Exercise 1.4: Vector limit/sketch")
                 (:file "nature-of-code.vectors.exercise-5"
                  :pathname "Exercise 1.5: Car acceleration simulation/sketch")
                 (:file "nature-of-code.vectors.exercise-6"
                  :pathname "Exercise 1.6: Perlin acceleration/sketch")
                 (:file "nature-of-code.vectors.exercise-8"
                  :pathname "Exercise 1.8: Variable acceleration towards the mouse/sketch")))
               (:module "forces"
                :pathname "02. Forces"
                :components
                ((:file "nature-of-code.forces.example-1"
                  :pathname "Example 2.1: Forces/sketch")
                 (:file "nature-of-code.forces.example-2"
                  :pathname "Example 2.2: Forces acting on many objects/sketch")
                 (:file "nature-of-code.forces.example-3"
                  :pathname "Example 2.3: Gravity scaled by mass/sketch")
                 (:file "nature-of-code.forces.example-4"
                  :pathname "Example 2.4: Including friction/sketch")
                 (:file "nature-of-code.forces.example-5"
                  :pathname "Example 2.5: Fluid resistance/sketch")
                 (:file "nature-of-code.forces.example-6"
                  :pathname "Example 2.6: Attraction/sketch")
                 (:file "nature-of-code.forces.exercise-1"
                  :pathname "Exercise 2.1: Helium-filled balloon/sketch")
                 (:file "nature-of-code.forces.exercise-3"
                  :pathname "Exercise 2.3: Invisible force/sketch")
                 (:file "nature-of-code.forces.exercise-4"
                  :pathname "Exercise 2.4: Pockets of friction/sketch")
                 (:file "nature-of-code.forces.exercise-5"
                  :pathname "Exercise 2.5: Fluid resistance with different heights/sketch")
                 (:file "nature-of-code.forces.exercise-6"
                  :pathname "Exercise 2.6: Falling boxes/sketch")))
               (:module "fractals"
                :pathname "08. Fractals"
                :components
                ((:file "nature-of-code.fractals.example-1"
                  :pathname "Example 8.1: Recursive circles I/sketch")
                 (:file "nature-of-code.fractals.example-2"
                  :pathname "Example 8.2: Recursion twice/sketch")
                 (:file "nature-of-code.fractals.example-3"
                  :pathname "Example 8.3: Recursion four times/sketch")
                 (:file "nature-of-code.fractals.example-4"
                  :pathname "Example 8.4: Cantor set/sketch")
                 (:file "nature-of-code.fractals.example-5"
                  :pathname "Example 8.5: Koch curve/sketch")
                 (:file "nature-of-code.fractals.example-6"
                  :pathname "Example 8.6: Recursive tree/sketch")
                 (:file "nature-of-code.fractals.example-7"
                  :pathname "Example 8.7: Dynamic recursive tree/sketch")
                 (:file "nature-of-code.fractals.exercise-1"
                  :pathname "Exercise 8.1: Custom recursion/sketch")
                 (:file "nature-of-code.fractals.exercise-2"
                  :pathname "Exercise 8.2: Koch snowflake/sketch")
                 (:file "nature-of-code.fractals.exercise-7"
                  :pathname "Exercise 8.7: Recursive thinning tree/sketch")
                 (:file "nature-of-code.fractals.exercise-8"
                  :pathname "Exercise 8.8: List tree/sketch")
                 (:file "nature-of-code.fractals.exercise-9"
                  :pathname "Exercise 8.9: Growing tree/sketch")))))
