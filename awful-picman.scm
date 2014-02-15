(module awful-picman ()

(import chicken scheme)
(declare (uses chicken-syntax))

;; Core units
(use irregex posix files srfi-1 srfi-13 extras data-structures
     setup-api ports srfi-4 utils)

;; Eggs
(use awful-sql-de-lite imlib2 spiffy simple-sha1 sql-de-lite json
     free-gettext slice uri-common intarweb matchable ssql)
(use (rename awful (debug awful-debug)))

;; Module generated by this egg
(use awful-picman-params)

(include "src/conf")
(include "src/renderers/breadcrumbs")
(include "src/renderers/dynamic-input")
(include "src/renderers/common")
(include "src/renderers/folder")
(include "src/renderers/album")
(include "src/renderers/tag")
(include "src/renderers/filter")
(include "src/image")
(include "src/utils")
(include "src/db")
(include "src/db-migrations")
(include "src/ajax-handlers")
(include "src/dispatcher")
(include "src/gc")
(include "src/main")

) ;; end module
