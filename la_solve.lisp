(load "./la-parse.lisp")
;(use-package :la-parse)

;; (la-parse:la-parse "/path/to/file")

(defun solve (file)
  (la-dump (la-parse file)))
