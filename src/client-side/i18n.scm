(define i18n-language #f)

(define i18n-db
  '(("Save" "Salvar")
    ("Close" "Fechar")
    ("Cancel" "Cancelar")
    ("Description" "Descrição")
    ("Date" "Data")
    ("Edit" "Editar")
    ("Export" "Exportar")
    ("Tags" "Marcadores")
    ("Albums" "Álbuns")
    ("decade" "década")
    ("picture" "foto")
    ("pictures" "fotos")
    ("No album available.  Make albums out of pictures from "
     "Nenhum álbum disponível.  Crie álbuns a partir de fotos em ")
    ("folders" "pastas")
    ("Directory to save pictures in: "
     "Diretório aonde salvar fotos: ")
    ("Export original pics (high resolution)? "
     "Exportar fotos originais (alta resolução)? ")
    ("The target directory must be specified."
     "O Diretório de destino deve ser especificado")
    ("Album successfully exported to "
     "Álbum exportado com sucesso para ")
    ("An error occurred while exporting the album."
     "Ocorreu um erro ao exportar o álbum.")
    ))

;; Language indexes
(define i18n/en -1) ;; not really used as an index
(define i18n/pt-br 0)

(define (get-text-by-language text language)
  (if (= i18n-language i18n/en) ;; en is the default language
      text
      (let ((translations (alist-ref text i18n-db)))
        (if translations
            (if (>= (length translations) language)
                (list-ref translations language)
                text)
            text))))

(define (_ text)
  (if i18n-language
      (get-text-by-language text i18n-language)
      text))
