(define-library (stak aa-tree)
  (export
    aa-tree-empty
    aa-tree?
    aa-tree-insert!)

  (import (scheme base) (scheme write))

  (begin
    (define-record-type aa-tree
      (make-aa-tree root compare)
      aa-tree?
      (root aa-tree-root aa-tree-set-root!)
      (compare aa-tree-compare aa-tree-set-compare!))

    (define-record-type aa-tree-node
      (make-aa-tree-node value level left right)
      aa-tree-node?
      (value aa-tree-node-value aa-tree-node-set-value!)
      (level aa-tree-node-level aa-tree-node-set-level!)
      (left aa-tree-node-left aa-tree-node-set-left!)
      (right aa-tree-node-right aa-tree-node-set-right!))

    (define (aa-tree-empty compare)
      (make-aa-tree #f compare))

    (define (aa-tree-insert! tree value)
      (aa-tree-set-root!
        (aa-tree-node-insert!
          (aa-tree-root tree)
          value
          (aa-tree-compare tree))))

    (define (aa-tree-node-insert! node value compare)
      (if node
        (let ((order (compare value (aa-tree-node-value node))))
          (if (= order 0)
            node
            (aa-tree-node-split!
              (aa-tree-node-skew!
                (if (< order 0)
                  (aa-tree-node-insert! (aa-tree-node-left node) value compare)

                  (aa-tree-node-insert! (aa-tree-node-right node) value compare))))))
        (make-aa-tree-node value 0 #f #f)))

    (define (aa-tree-node-skew! node)
      (let ((left (and node (aa-tree-node-left node))))
        (if (and
             left
             (eq? (aa-tree-node-level node) (aa-tree-node-level left)))
          (begin
            (aa-tree-node-set-left! tree (aa-tree-node-right left))
            (aa-tree-node-set-right! left tree)
            left)
          node)))

    (define (aa-tree-node-split! node)
      (let* ((right (and node (aa-tree-node-right node)))
             (right-right (and right (aa-tree-node-right right))))
        (if (and
             right-right
             (eq? (aa-tree-node-level node) (aa-tree-node-level right-right)))
          (begin
            (aa-tree-node-set-right! tree (aa-tree-node-left right))
            (aa-tree-node-set-left! right tree)
            (aa-tree-node-set-level! right (+ (aa-tree-node-level right) 1))
            right)
          node)))))
