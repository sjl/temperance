(ql:quickload 'quickutil)

(qtlc:save-utils-as "utils.lisp"
                    :utilities '(:define-constant
                                 :set-equal
                                 :curry
                                 :switch
                                 :ensure-boolean
                                 :while
                                 :until
                                 :tree-member-p
                                 :map-tree
                                 )
                    :package "BONES.UTILS")
