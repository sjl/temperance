(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(:define-constant
               :set-equal
               :curry
               :rcurry
               :switch
               :ensure-boolean
               :while
               :until
               :tree-member-p
               :with-gensyms
               :once-only
               :zip
               :alist-to-hash-table
               :map-tree
               :weave
               :alist-plist
               :equivalence-classes
               :ensure-gethash
               :map-product)
  :package "TEMPERANCE.QUICKUTILS")

(quit)
