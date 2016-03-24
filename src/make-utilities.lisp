(ql:quickload 'quickutil)

(qtlc:save-utils-as "utils.lisp"
                    :utilities '(:define-constant
                                 :set-equal
                                 :curry
                                 :switch)
                    :package "BONES.UTILS")
