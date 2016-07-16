(in-package #:bones.wam)

;;;; ,-.  .                 ,-_/     .
;;;;   |  |   ,-. ,-. ,-.   '  | ,-. |- ,-. ,-. ," ,-. ,-. ,-.
;;;;   |  | . `-. |-' |     .^ | | | |  |-' |   |- ,-| |   |-'
;;;;   `--^-' `-' `-' '     `--' ' ' `' `-' '   |  `-^ `-' `-'
;;;;                                            '

;;; The final phase wraps everything else up into a sane UI.

(defun compile-query (wam query)
  "Compile `query` into the query section of the WAM's code store.

  `query` should be a list of goal terms.

  Returns the permanent variables.

  "
  (multiple-value-bind (instructions permanent-variables)
      (precompile-query query)
    (optimize-instructions instructions)
    (render-query wam instructions)
    permanent-variables))

(defun compile-rules (wam rules)
  "Compile `rules` into the WAM's code store.

  Each rule in `rules` should be a clause consisting of a head term and zero or
  more body terms.  A rule with no body is called a fact.

  "
  (multiple-value-bind (instructions functor arity)
      (precompile-rules rules)
    (optimize-instructions instructions)
    (render-rules wam functor arity instructions)))

