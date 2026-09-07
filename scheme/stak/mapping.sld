; TODO Implement this as SRFI-146.
(define-library (stak mapping)
  (export
    mapping-empty
    mapping?
    mapping-find
    mapping-insert!
    mapping->list
    list->mapping)

  (import (stak base))

  (begin
    ; A mapping based on the AA tree.
    (define-record-type mapping
      (make-mapping root less)
      mapping?
      (root mapping-root mapping-set-root!)
      (less mapping-less))

    (define-record-type node
      (make-node value level left right)
      node?
      (value node-value)
      (level node-level node-set-level!)
      (left node-left node-set-left!)
      (right node-right node-set-right!))

    (define (mapping-empty less)
      (make-mapping #f less))

    (define (mapping-find tree value)
      (node-find (mapping-root tree) value (mapping-less tree)))

    (define (node-find node value less?)
      (and
        node
        (let ((node-value (node-value node)))
          (cond
            ((less? value node-value)
              (node-find (node-left node) value less?))

            ((less? node-value value)
              (node-find (node-right node) value less?))

            (else
              node-value)))))

    (define (mapping-insert! tree value)
      (mapping-set-root!
        tree
        (node-insert!
          (mapping-root tree)
          value
          (mapping-less tree))))

    (define (list->mapping xs less?)
      (define tree (mapping-empty less?))
      (for-each (lambda (x) (mapping-insert! tree x)) xs)
      tree)

    (define (mapping->list tree)
      (node->list (mapping-root tree) '()))

    (define (node->list node xs)
      (if node
        (node->list
          (node-left node)
          (cons
            (node-value node)
            (node->list (node-right node) xs)))
        xs))

    (define (node-insert! node value less?)
      (if node
        (let ((node-value (node-value node)))
          (cond
            ((less? value node-value)
              (node-set-left!
                node
                (node-insert! (node-left node) value less?))
              (node-balance! node))

            ((less? node-value value)
              (node-set-right!
                node
                (node-insert! (node-right node) value less?))
              (node-balance! node))

            (else
              node)))
        (make-node value 0 #f #f)))

    (define (node-balance! node)
      (node-split! (node-skew! node)))

    (define (node-skew! node)
      (let ((left (node-left node)))
        (if (and
             left
             (= (node-level node) (node-level left)))
          (begin
            (node-set-left! node (node-right left))
            (node-set-right! left node)
            left)
          node)))

    (define (node-split! node)
      (let* ((right (node-right node))
             (right-right (and right (node-right right))))
        (if (and
             right-right
             (= (node-level node) (node-level right-right)))
          (begin
            (node-set-right! node (node-left right))
            (node-set-left! right node)
            (node-set-level! right (+ (node-level right) 1))
            right)
          node)))))
