(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(

               :alist-plist
               :alist-to-hash-table
               :curry
               :define-constant
               :ensure-boolean
               :ensure-gethash
               :ensure-keyword
               :equivalence-classes
               :map-product
               :map-tree
               :once-only
               :rcurry
               :read-file-into-string
               :set-equal
               :switch
               :tree-member-p
               :until
               :weave
               :when-let
               :while
               :with-gensyms
               :zip

               )
  :package "TEMPERANCE.QUICKUTILS")

(quit)
