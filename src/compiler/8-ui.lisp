(in-package #:temperance.wam)

;;;; ,-.  .                 ,-_/     .
;;;;   |  |   ,-. ,-. ,-.   '  | ,-. |- ,-. ,-. ," ,-. ,-. ,-.
;;;;   |  | . `-. |-' |     .^ | | | |  |-' |   |- ,-| |   |-'
;;;;   `--^-' `-' `-' '     `--' ' ' `' `-' '   |  `-^ `-' `-'
;;;;                                            '

;;; The final phase wraps everything else up into a sane UI.

(defun %compile-query-into (storage query)
  (multiple-value-bind (instructions permanent-variables)
      (precompile-query query)
    (optimize-instructions instructions)
    (values permanent-variables
            (render-query-into storage instructions))))

(defun compile-query (wam query)
  "Compile `query` into the query section of the WAM's code store.

  `query` should be a list of goal terms.

  Returns the permanent variables and the size of the compiled bytecode.

  "
  (%compile-query-into (wam-code wam) query))

(defun compile-query-into (storage query)
  "Compile `query` into the given array `storage`.

  `query` should be a list of goal terms.

  Returns the permanent variables and the size of the compiled bytecode.

  "
  (%compile-query-into storage query))


(defun compile-rules (wam rules)
  "Compile `rules` into the WAM's code store.

  Each rule in `rules` should be a clause consisting of a head term and zero or
  more body terms.  A rule with no body is called a fact.

  "
  (multiple-value-bind (instructions functor arity)
      (precompile-rules rules)
    (optimize-instructions instructions)
    (render-rules wam functor arity instructions)))

