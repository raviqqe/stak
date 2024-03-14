(import (scheme base) (scheme write))

(define-record-type aa-tree
  (make-aa-tree node size)
  aa-tree?
  (node aa-tree-node aa-tree-set-node!)
  (size aa-tree-size aa-tree-set-size!))

(define-record-type aa-tree-node
  (make-aa-tree-node value left right)
  aa-tree-node?
  (value aa-tree-node-value aa-tree-node-set-value!)
  (left aa-tree-node-left aa-tree-node-set-left!)
  (right aa-tree-node-right aa-tree-node-set-right!))

(define left aa-tree-node-left)
(define right aa-tree-node-right)

(define (empty)
  (make-aa-tree #f 0))

(define (skew tree)
  (let ((node (aa-tree-node tree)))
    (cond
      ((not node)
        #f)

      ((not (left node))
        node)

      ()

      (else
        node))))

(write (empty))
