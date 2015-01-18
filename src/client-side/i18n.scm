(define i18n-language #f)

(define i18n-db
  `(("Save" "Salvar")
    ("Description" "Descrição")
    ("Date" "Data")
    ("Tags" "Marcadores")
    ("Albums" "Álbuns")
    ("decade" "década")
    ("picture" "foto")
    ("pictures" "fotos")
    ))

(define i18n/pt-br 0)

(define (get-text-by-language text language)
  (let ((translations (alist-ref text i18n-db)))
    (if translations
        (if (>= (length translations) language)
            (list-ref translations language)
            text)
        text)))

(define (_ text)
  (if i18n-language
      (get-text-by-language text i18n-language)
      text))
