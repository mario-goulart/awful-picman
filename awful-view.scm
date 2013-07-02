(module awful-view ()

(import chicken scheme)
(use awful-sql-de-lite imlib2 spiffy simple-sha1 sql-de-lite)
(use (rename awful (debug awful-debug)))
(use irregex posix files srfi-1 srfi-13 extras data-structures setup-api)

(include "src/renderers")
(include "src/image")
(include "src/utils")
(include "src/db")
(include "src/awful-view")

) ;; end module
