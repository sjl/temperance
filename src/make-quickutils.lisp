(ql:quickload 'quickutil)

(qtlc:save-utils-as
  "quickutils.lisp"
  :utilities '(:define-constant
               :set-equal
               :curry
               :switch
               :ensure-boolean
               :while
               :until
               :tree-member-p
               :tree-collect
               :with-gensyms
               :map-tree
               )
  :package "BONES.QUICKUTILS")
