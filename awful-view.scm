(module awful-view ()

(import chicken scheme)

;; Core units
(use irregex posix files srfi-1 srfi-13 extras data-structures setup-api ports)

;; Eggs
(use awful-sql-de-lite imlib2 spiffy simple-sha1 sql-de-lite json matchable)
(use (rename awful (debug awful-debug)))

(include "src/params")
(include "src/renderers")
(include "src/image")
(include "src/utils")
(include "src/db")
(include "src/ajax-handlers")
(include "src/dispatcher")
(include "src/awful-view")

) ;; end module
