# Exercise 1.5: Car simulation

Running this is different from most of the other examples. Because of the need
for assets, the sketch had to be defined as an asdf system. Run it like this:

```lisp
;; Make this directory available to quicklisp. If you don't want to symlink or
;; change asdf settings, use the following:
(pushnew
  (truename "/path/to/nature-of-code/01. Vectors/Exercise 1.5: Car acceleration simulation")
  ql:*local-project-directories*)
(ql:register-local-projects)

;; Then quickload & run:
(ql:quickload :vectors-car-acceleration)
(vectors-car-acceleration:start-sketch)
```

![Exercise 1.5: Car simulation](https://raw.githubusercontent.com/mark-gerarts/nature-of-code/master/screenshots/Exercise%201.5%3A%20Car%20simulation.gif)

[Sprite credits](https://opengameart.org/content/2d-car-sprite-7)
