;; -*- lexical-binding: t; -*-

(require 'derived)
(require 'eglot)
(require 'utils)
(require 'cl-lib)
(require 'color-utils)

(defun -lsp-location-at-point ()
  (let* ((pt-loc (eglot--pos-to-lsp-position))
         (res (plist-get (eglot--TextDocumentPositionParams) :textDocument)))
    (plist-put res :range `(:start ,pt-loc :end ,pt-loc))
    res))

(defvar -link-placeholder (propertize "<...>" 'face `(:foreground ,(base16-get :bg2))))

(defun -lsp-placeholder-loc-at-point ()
  (let* ((uri (plist-get
                (plist-get (eglot--TextDocumentPositionParams) :textDocument) :uri))
         (start (eglot--pos-to-lsp-position))
         (end `(:line ,(plist-get start :line)
                :character ,(+ (plist-get start :character)
                               (length -link-placeholder)))))
    (insert -link-placeholder)
    `(:uri ,uri, :range (:start ,start :end ,end))))

(defun zk-new (title &optional path no-edit)
  (interactive (list (read-string "Title: ")))
  (let* ((path (or path (-ensure-buffer-path)))
         (ret (-lsp-cmd "zk.new" `[,path (:title ,title :edit t)])))
    (unless no-edit
      (find-file (plist-get ret :path)))
    (plist-get ret :path)))

(defun -compute-alignments (rows)
  (if (not rows) '()
    (let* ((nr-cols (length (car rows)))
           (col-idxs (number-sequence 0 (1- nr-cols)))
           (col-maxs (make-vector nr-cols 0)))
      ;; First round; find maximal widths of fields in each column
      (mapcar
        (lambda (row)
          (cl-mapcar (lambda (n field)
                       (let* ((len (length field)))
                         (when (> len (aref col-maxs n))
                           (setf (aref col-maxs n) len))))
                     col-idxs row))
        rows)
      ;; Second round; annotate padding width
      (mapcar
        (lambda (row)
          (cl-mapcar (lambda (max-width field)
                       (cons field (- max-width (length field))))
                     col-maxs row))
        rows))))

; (defvar -sep (propertize))
(defun -trunc (n str &optional from-left sep)
  (let ((len (length str))
        (sep (or sep "..."))
        (sep-len (length sep)))
    (if (>= n len) str
      (if from-left (concat
                      sep (substring str (+ (- len n) sep-len) len))
        (concat
          (substring str 0 (- n sep-len)) sep)))))

(defun -pad (n)
  (make-string n 32))

(defun -lead-to-annotation (lead)
  (cond ;((arrayp lead) (mapconcat #'identity lead ">"))
    ((not lead) "<no preview available>")
    (t (replace-regexp-in-string "\n" ">" lead))))


(defcustom zk-emacs-completion-max-path-width 7
           "Maximum width of the path displayed in the minibuffer completion for zk notes."
           :group 'zk-emacs :type 'integer)

(defcustom zk-emacs-completion-max-title-width 50
           "Maximum width of the title displayed in the minibuffer completion for zk notes."
           :group 'zk-emacs :type 'integer)


(defun -zk-list-to-options (objs)
  (let* ((path-color (base16-get :dark_green))
         (cols (mapcar
                 (lambda (obj)
                   (list
                     (-trunc
                       zk-emacs-completion-max-path-width
                       (propertize
                         (plist-get obj :path)
                         'face `(:foreground ,path-color))
                       t "$")
                     (-trunc
                       zk-emacs-completion-max-title-width
                       (plist-get obj :title))))
                 objs))
         (cols-with-alignment (-compute-alignments cols)))
    (cl-mapcar
      (lambda (row obj)
        (pcase-let ((`(,path . ,p-pad) (car row))
                    (`(,title . ,t-pad) (cadr row)))
                   (let ((annotation (concat
                                       (-pad (+ t-pad 5))
                                       (-lead-to-annotation (plist-get obj :lead))))
                         (option (concat path (-pad (1+ p-pad)) title)))
                     `(,option . (,annotation . ,obj)))))
      cols-with-alignment objs)))

(defun -ensure-buffer-path ()
  (if-let ((path (buffer-file-name))) (file-truename path)
          (error "current buffer has no file")))

(defun -ensure-eglot-server ()
  (if-let ((server (eglot-current-server))) server
          (error "no running LSP server")))

(defun -lsp-cmd (command arguments)
  (let ((server (-ensure-eglot-server)))
    (eglot-execute-command
      server command arguments)))

(defun -zk-list-get-com-tbl (path options)
  (setq options (plist-put options :select ["title" "path" "absPath" "lead"]))
  (-zk-list-to-options (-lsp-cmd "zk.list" `[,path ,options])))

(defun -zk-list (path options prompt &optional pred)
  (let* ((com-tbl (-zk-list-get-com-tbl path options))
         (choice (-zk-list-read prompt com-tbl pred)))
    (cddr (assoc choice com-tbl))))

(defun -is-open (option)
  (boolify (find-buffer-visiting (plist-get (cddr option) :path))))

(defun -edit-zk-list (obj)
  (find-file (plist-get obj :absPath)))

(defun zk-find-or-create ()
  (interactive)
  (let* ((path (-ensure-buffer-path))
         (com-tbl (-zk-list-get-com-tbl path '()))
         (choice (-zk-list-read "Title: " com-tbl nil t)))
    (if-let ((existing (assoc choice com-tbl))) (-edit-zk-list (cddr existing))
            (zk-new choice path))))

(defun zk-find-open-note ()
  (interactive)
  (let ((path (-ensure-buffer-path)))
    (-edit-zk-list
        (-zk-list
          path '() "Switch to note: " #'-is-open))))

(defun zk-find-note ()
  (interactive)
  (let ((path (-ensure-buffer-path)))
    (-edit-zk-list
          (-zk-list
            path '() "Edit note: "))))

(defun zk-find-link (&optional n)
  (interactive "p")
  (let ((path (-ensure-buffer-path)))
    (-edit-zk-list
      (-zk-list
        path `(:linkedBy [,path] :recursive t :maxDistance ,(or n 1)) "Edit linked note: "))))

(defun zk-find-backlink (&optional n)
  (interactive "p")
  (let ((path (-ensure-buffer-path)))
    (-edit-zk-list
      (-zk-list
        path `(:linkTo [,path] :recursive t :maxDistance ,(or n 1)) "Edit backlink: "))))

(defun -zk-list-read (prompt com-tbl &optional pred no-match-required)
  (let* ((completion-extra-properties
           `(:annotation-function ,(lambda (com-el) (cadr (assoc com-el com-tbl))))))
    (completing-read prompt com-tbl pred
                     (if no-match-required 'confirm t))))


(defun zk-insert-link ()
  (interactive)
  (let* ((path (-ensure-buffer-path))
         (com-tbl (-zk-list-get-com-tbl path '()))
         (choice (-zk-list-read "Note title: " com-tbl nil t))
         (link-name (read-string "Link name: "))
         (link-name (if (string-empty-p link-name) nil link-name))
         (loc (-lsp-location-at-point))
         (pt (point)))
    (if-let ((existing (assoc choice com-tbl)))
            (let ((path (plist-get (cddr existing) :path))
                  (absPath (plist-get (cddr existing) :absPath)))
              (-lsp-cmd
                "zk.link"
                (print `[,absPath (:location ,loc :path ,path :title ,link-name)]))
              )
            (-lsp-cmd
              "zk.new"
              `[,path (:title ,choice :insertLinkAtLocation ,loc)]))
    (forward-sexp)
    ))

(define-minor-mode zk-mode nil
                     :lighter " zk"
                     :keymap (define-keymap 
                               "C-c C-b" 'zk-find-backlink
                               "C-c C-l" 'zk-find-link
                               "C-c C-n" 'zk-find-or-create
                               "C-c C-o" 'zk-find-open-note
                               "C-c C-c" 'zk-new)
             (add-to-list 'eglot-server-programs '(markdown-mode . ("zk" "lsp")))
             (eglot-ensure))

(defun -enable-if-registered ()
  (when-let* ((path (buffer-file-name))
              (path (file-truename path)))
             (when (-is-zk-file path)
               (zk-mode))))

(with-eval-after-load
  'markdown-mode
  (add-hook 'markdown-mode-hook #'-enable-if-registered))

(defvar -zk-file-regexps '())

(defun -is-zk-file (path)
  (cl-some
    (lambda (regexp)
      (string-match regexp path))
    -zk-file-regexps))

(defun zk-register (base-dir)
  "Use `zk-mode` when opening markdown files in BASE-DIR."
  (let ((path-regexp (concat (regexp-quote (file-truename base-dir)) ".*\\.md")))
    (add-to-list '-zk-file-regexps path-regexp)))

(provide 'zk-emacs)

;; Local Variables:
;; read-symbol-shorthands: (("-" . "zk-emacs--"))
;; End:
