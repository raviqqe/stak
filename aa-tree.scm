(define-library (stak aa-tree)
  (export
    aa-tree-empty
    aa-tree?
    aa-tree-find
    aa-tree-insert!
    aa-tree->list
    list->aa-tree)

  (import (scheme base))

  (begin
    (define-record-type aa-tree
      (make-aa-tree root less)
      aa-tree?
      (root aa-tree-root aa-tree-set-root!)
      (less aa-tree-less aa-tree-set-less!))

    (define-record-type aa-node
      (make-aa-node value level left right)
      aa-node?
      (value aa-node-value aa-node-set-value!)
      (level aa-node-level aa-node-set-level!)
      (left aa-node-left aa-node-set-left!)
      (right aa-node-right aa-node-set-right!))

    (define (aa-tree-empty less)
      (make-aa-tree #f less))

    (define (aa-tree-find tree value)
      (aa-node-find (aa-tree-root tree) value (aa-tree-less tree)))

    (define (aa-node-find node value less?)
      (and
        node
        (let ((node-value (aa-node-value node)))
          (cond
            ((less? value node-value)
              (aa-node-find (aa-node-left node) value less?))

            ((less? node-value value)
              (aa-node-find (aa-node-right node) value less?))

            (else
              node-value)))))

    (define (aa-tree-insert! tree value)
      (aa-tree-set-root!
        tree
        (aa-node-insert!
          (aa-tree-root tree)
          value
          (aa-tree-less tree))))

    (define (list->aa-tree xs less?)
      (define tree (aa-tree-empty less?))
      (for-each (lambda (x) (aa-tree-insert! tree x)) xs)
      tree)

    (define (aa-tree->list tree)
      (aa-node->list (aa-tree-root tree) '()))

    (define (aa-node->list node xs)
      (if node
        (aa-node->list
          (aa-node-left node)
          (cons
            (aa-node-value node)
            (aa-node->list (aa-node-right node) xs)))
        xs))

    (define (aa-node-insert! node value less?)
      (if node
        (let ((node-value (aa-node-value node)))
          (cond
            ((less? value node-value)
              (aa-node-set-left!
                node
                (aa-node-insert! (aa-node-left node) value less?))
              (aa-node-balance! node))

            ((less? node-value value)
              (aa-node-set-right!
                node
                (aa-node-insert! (aa-node-right node) value less?))
              (aa-node-balance! node))

            (else
              node)))
        (make-aa-node value 0 #f #f)))

    (define (aa-node-balance! node)
      (aa-node-split! (aa-node-skew! node)))

    (define (aa-node-skew! node)
      (let ((left (and node (aa-node-left node))))
        (if (and
             left
             (= (aa-node-level node) (aa-node-level left)))
          (begin
            (aa-node-set-left! node (aa-node-right left))
            (aa-node-set-right! left node)
            left)
          node)))

    (define (aa-node-split! node)
      (let* ((right (and node (aa-node-right node)))
             (right-right (and right (aa-node-right right))))
        (if (and
             right-right
             (= (aa-node-level node) (aa-node-level right-right)))
          (begin
            (aa-node-set-right! node (aa-node-left right))
            (aa-node-set-left! right node)
            (aa-node-set-level! right (+ (aa-node-level right) 1))
            right)
          node)))))
