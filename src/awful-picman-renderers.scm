(module awful-picman-renderers

  (render-pics
   render-tags
   render-filters)

(import chicken scheme)
(use data-structures files ports posix srfi-1)
(use awful json slice simple-sha1 uri-common)
(use awful-picman-params awful-picman-utils awful-picman-db awful-picman-image)

(include "renderers/ajax-handlers")
(include "renderers/breadcrumbs")
(include "renderers/dynamic-input")
(include "renderers/common")
(include "renderers/folder")
(include "renderers/album")
(include "renderers/tag")
(include "renderers/filter")

) ;; end module
