(define-library (stak aa-tree)
  (export
    aa-tree-empty
    aa-tree?)

  (import (scheme base) (scheme write))

  (begin
    (define-record-type aa-tree
      (make-aa-tree root size)
      aa-tree?
      (root aa-tree-root aa-tree-set-root!)
      (size aa-tree-size aa-tree-set-size!))

    (define-record-type aa-tree-node
      (make-aa-tree-node value level left right)
      aa-tree-node?
      (value aa-tree-node-value aa-tree-node-set-value!)
      (level aa-tree-node-level aa-tree-node-set-level!)
      (left aa-tree-node-left aa-tree-node-set-left!)
      (right aa-tree-node-right aa-tree-node-set-right!))

    (define (aa-tree-empty)
      (make-aa-tree #f 0))

    (define (aa-tree-node-skew node)
      (and
        node
        (let ((left (aa-tree-node-left node)))
          (if (and
               (not left)
               (eq? (aa-tree-node-level node) (aa-tree-node-level left)))
            (begin
              (aa-tree-node-set-left! tree (right left))
              (aa-tree-node-set-right! left tree)
              left)
            node))))

    (define (aa-tree-node-split node)
      (and
        node
        (let ((right (aa-tree-node-right node)))
          (if (or
               (not right)
               (not (aa-tree-node-right right)))
            (begin
              (aa-tree-node-set-left! tree (right left))
              (aa-tree-node-set-right! left tree)
              left)
            node))))))
