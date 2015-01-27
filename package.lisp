(in-package #:cl-user)

(defpackage #:cl-agent
  (:use #:cl
        #:org))

(defpackage #:org
  (:use #:cl
        #:org.fipa))

(defpackage #:org.fipa
  (:use #:cl
        #:org.fipa.std))





(defpackage #:org.math.matrix
  (:use #:cl)
  (:export #:calculate-column-width
           #:correct-near-zero
           #:matrix-add
           #:matrix-apply
           #:matrix-augment
           #:matrix-column
           #:matrix-columns
           #:matrix-columnspace
           #:matrix-count-zeros
           #:matrix-determinant
           #:matrix-eigenvalues
           #:matrix-element
           #:matrix-gauss-jordan
           #:matrix-gaussian
           #:matrix-gaussian2
           #:matrix-identity
           #:matrix-inverse
           #:matrix-invertiblep
           #:matrix-lu
           #:matrix-minor
           #:matrix-multiply
           #:matrix-normalize
           #:matrix-nullspace
           #:matrix-ones
           #:matrix-orthogonalp
           #:matrix-qr
           #:matrix-random
           #:matrix-rank
           #:matrix-ref
           #:matrix-row
           #:matrix-row-swap
           #:matrix-rows
           #:matrix-rref
           #:matrix-split
           #:matrix-squarep
           #:matrix-symmetricp
           #:matrix-transpose
           #:matrix-upper-triangularp
           #:matrix-zero
           #:matrixp
           #:print-matrix
           #:print-matrix-four-subspaces
           #:standard-basis-row))
