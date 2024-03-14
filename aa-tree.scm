(import (scheme base) (scheme write))

(define-record-type aa-tree
  (make-aa-tree node size)
  aa-tree?
  (node aa-tree-node aa-tree-set-node!)
  (size aa-tree-size aa-tree-set-size!))

(define (empty)
  (make-aa-tree #f 0))

(define (skew tree)
  (let ((node (aa-tree-node tree)))
    (and
      node
      foo)))

(write (empty))
