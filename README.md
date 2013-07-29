# Gini Coefficient, Lorenz Curve

About Gini Coefficient and Lorenz Curve refer e.g. to: http://en.wikipedia.org/wiki/Gini_coefficient

## Usage

``` clojure
;; obs as [x1, x2, ..., xn] or [[x1, y1], [x2,y2], ..., [xn,yn]]
;; may also be lists, but pairs must be vectors
;; unsorted, numerical, positive values
;; otherwise garbage-in-garbage-out, probably exceptions
;;
(gini-coeff obs)
;;
(lorenz-curve obs)
;;
;; options are available to tweak sort order, sort by x or y, and legend of the curve
```

Uses Incanter for the curve graphs, Midje for the test suite.

## License

Copyright © 2013 Friedrich Boeckh

Distributed under the Eclipse Public License, the same as Clojure.
