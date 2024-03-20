(define-library (stak aa-tree)
  (export
    aa-tree-empty
    aa-tree?
    aa-tree-insert!)

  (import (scheme base) (scheme write))

  (begin
    (define-record-type aa-tree
      (make-aa-tree root less-than)
      aa-tree?
      (root aa-tree-root aa-tree-set-root!)
      (less-than aa-tree-less-than aa-tree-set-less-than!))

    (define-record-type aa-node
      (make-aa-node value level left right)
      aa-node?
      (value aa-node-value aa-node-set-value!)
      (level aa-node-level aa-node-set-level!)
      (left aa-node-left aa-node-set-left!)
      (right aa-node-right aa-node-set-right!))

    (define (aa-tree-empty less-than)
      (make-aa-tree #f less-than))

    (define (aa-tree-insert! tree value)
      (aa-tree-set-root!
        (aa-node-insert!
          (aa-tree-root tree)
          value
          (aa-tree-less-than tree))))

    (define (aa-node-blance node)
      (aa-node-split! (aa-node-skew! node)))

    (define (aa-node-insert! node value less-than)
      (if node
        (let ((node-value ((aa-node-value node))))
          (cond
            ((less-than value node-value)
              (aa-node-set-left!
                node
                (aa-node-insert!
                  (aa-node-left node)
                  value
                  less-than)))

            ((less-than value node-value)
              (aa-node-set-right!
                node
                (aa-node-insert!
                  (aa-node-right node)
                  value
                  less-than)))

            (else
              node)))
        (make-aa-node value 0 #f #f)))

    (define (aa-node-skew! node)
      (let ((left (and node (aa-node-left node))))
        (if (and
             left
             (eq? (aa-node-level node) (aa-node-level left)))
          (begin
            (aa-node-set-left! tree (aa-node-right left))
            (aa-node-set-right! left tree)
            left)
          node)))

    (define (aa-node-split! node)
      (let* ((right (and node (aa-node-right node)))
             (right-right (and right (aa-node-right right))))
        (if (and
             right-right
             (eq? (aa-node-level node) (aa-node-level right-right)))
          (begin
            (aa-node-set-right! tree (aa-node-left right))
            (aa-node-set-left! right tree)
            (aa-node-set-level! right (+ (aa-node-level right) 1))
            right)
          node)))))
