(module awful-picman-export

(export-album)

(import chicken scheme)
(use data-structures files ports posix)
(use awful json slice simple-sha1 uri-common)
(use awful-picman-params
     awful-picman-utils
     awful-picman-image
     awful-picman-db)

(define (write-index dir album-title album-descr pic-filenames fancy-gallery?)
  (when fancy-gallery?
    (let ((fancy-gallery-dest-dir (make-pathname dir "fancy-gallery"))
          (fancy-gallery-assets-dir
           (make-pathname (list assets-install-dir "assets") "blueimp")))
      (create-directory (make-pathname fancy-gallery-dest-dir "js") 'recursively)
      (create-directory (make-pathname fancy-gallery-dest-dir "css") 'recursively)
      (file-copy (make-pathname (list fancy-gallery-assets-dir "js")
                                "blueimp-gallery.min.js")
                 (make-pathname (list fancy-gallery-dest-dir "js")
                                "blueimp-gallery.min.js")
                 'clobber)
      (file-copy (make-pathname (list fancy-gallery-assets-dir "css")
                                "blueimp-gallery.min.css")
                 (make-pathname (list fancy-gallery-dest-dir "css")
                                "blueimp-gallery.min.css")
                 'clobber)))
  (with-output-to-file (make-pathname dir "index.html")
    (lambda ()
      (display
       ((sxml->html)
        `((literal "<!DOCTYPE HTML>")
          (html
           (head
            (meta (@ (http-equiv "Content-Type")
                     (content "application/xhtml+xml; charset=utf-8")))
            (title ,album-title)
            ,(if fancy-gallery?
                 '((link (@ (rel "stylesheet")
                            (href "fancy-gallery/css/blueimp-gallery.min.css")))
                   (style (literal "#fancy-gallery-pics > a { margin-right: 10px; }")))
                 '(style "ul li { display: inline; list-style-type: none }")))
           (body
            ,(if fancy-gallery?
                 '((div (@ (id "blueimp-gallery")
                           (class "blueimp-gallery"))
                        (div (@ (class "slides")))
                        (h3 (@ (class "title")))
                        (a (@ (class "prev")) "‹")
                        (a (@ (class "next")) "›")
                        (a (@ (class "close")) "×")
                        (a (@ (class "play-pause")))
                        (ol (@ (class "indicator"))))
                   (script (@ (src "fancy-gallery/js/blueimp-gallery.min.js"))))
                 '())
            (h1 ,album-title)
            (blockquote ,album-descr)
            ,(if fancy-gallery?
                 `((div (@ (id "fancy-gallery-pics"))
                        ,@(map (lambda (pic-filename)
                                 `(a (@ (href ,pic-filename))
                                     (img (@ (src ,(make-pathname "thumbnails"
                                                                  pic-filename))))))
                               pic-filenames))
                   (script "
document.getElementById('fancy-gallery-pics').onclick = function (event) {
    event = event || window.event;
    var target = event.target || event.srcElement,
        link = target.src ? target.parentNode : target,
        options = {index: link, event: event},
        links = this.getElementsByTagName('a');
    blueimp.Gallery(links, options);
};"))
                 `(ul
                   ,@(map (lambda (pic-filename)
                            `(li (a (@ (href ,pic-filename))
                                    (img (@ (src ,(make-pathname "thumbnails"
                                                                 pic-filename)))))))
                          pic-filenames)))))))))))

(define (export-album album-id dir hi-res? index? fancy-gallery?)
  (create-directory dir 'recursively)
  (when (or index? fancy-gallery?)
    (create-directory (make-pathname dir "thumbnails") 'recursively))
  (let* ((album (db-get-album-by-id album-id))
         (album-title (db-album-title album))
         (album-descr (db-album-descr album))
         (pic-ids/paths (db-get-pics-id/path-by-album-id album-id))
         (pic-filenames '()))
    (for-each
     (lambda (pic-id/path)
       (let* ((pic-id (car pic-id/path))
              (pic-path (cdr pic-id/path))
              (pic-fs-path (if hi-res?
                               pic-path
                               (thumbnail-path pic-path
                                               (thumbnails/zoom-dimension))))
              (thumbnail-fs-path
               (and (or index? fancy-gallery?)
                    (thumbnail-path pic-path
                                    (thumbnails/small-dimension))))
              (extension (pathname-extension pic-path))
              (pic-id (number->string pic-id)))
         (set! pic-filenames (cons (make-pathname #f pic-id extension)
                                   pic-filenames))
         (when (and (not hi-res?)
                    (not (file-exists? pic-fs-path)))
           (image->thumbnail pic-path (thumbnails/zoom-dimension)))
         (when (and (or index? fancy-gallery?)
                    (not (file-exists? thumbnail-fs-path)))
           (image->thumbnail pic-path (thumbnails/small-dimension)))
         (when (or index? fancy-gallery?)
           (file-copy thumbnail-fs-path
                      (make-pathname (list dir "thumbnails") pic-id extension)
                      'clobber))
         (file-copy pic-fs-path
                    (make-pathname dir pic-id extension)
                    'clobber)))
     pic-ids/paths)
    (when (or index? fancy-gallery?)
      (write-index dir album-title album-descr pic-filenames fancy-gallery?))))

) ;; end module
