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

(defcustom zk-emacs-completion-max-title-width 30
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
  "Get buffers true path or error"
  (if-let ((path (buffer-file-name))) (file-truename path)
    (error "current buffer has no file")))

(defun -ensure-eglot-server ()
  "Get running eglot server or error"
  (if-let ((server (eglot-current-server))) server
    (error "no running LSP server")))

(defun -lsp-cmd (command arguments)
  "Execute LSP command"
  (let ((server (-ensure-eglot-server)))
    (eglot-execute-command
     server command arguments)))

(defun -get-notebook-path (abs-path)  
  (let ((found (-lsp-cmd
                "zk.list"
                `[,abs-path (:select ["path" "absPath"] :hrefs [,abs-path])])))
    (or
     (cl-some
      (lambda (el)
        (when (equal (plist-get el :absPath) abs-path)
          (plist-get el :path)))
      found)
     (error "Failed to get notebook-internal path of note"))))

(defun -zk-list-get-com-tbl (path options)
  "Execute zk.list with given PATH and OPTIONS, injecting in interpreting the :select parameter, retuning a completion table."
  (setq options (plist-put options :select ["title" "path" "absPath" "lead"]))
  (-zk-list-to-options (-lsp-cmd "zk.list" `[,path ,options])))

(defun -zk-list (path options prompt &optional pred)
  "Execute zk.list with given PATH and OPTIONS and let the user make a choice; PRED limits completion candidates."
  (let* ((com-tbl (-zk-list-get-com-tbl path options))
         (choice (-zk-list-read prompt com-tbl pred)))
    (cddr (assoc choice com-tbl))))

(defun -is-open (option)
  "Check if completion option OPTION is opened in a buffer."
  (boolify (find-buffer-visiting (plist-get (cddr option) :path))))

(defun -edit-zk-list (obj)
  "Open OBJ for editing"
  (find-file (plist-get obj :absPath)))

(defun zk-find-or-create ()
  "Open an existing zk note or create a new one."
  (interactive)
  (let* ((path (-ensure-buffer-path))
         (com-tbl (-zk-list-get-com-tbl path '()))
         (choice (-zk-list-read "Title: " com-tbl nil t)))
    (if-let ((existing (assoc choice com-tbl))) (-edit-zk-list (cddr existing))
      (zk-new choice path))))

(defun zk-find-open-note ()
  "Switch to the buffer of an already opened note by its title."
  (interactive)
  (let ((path (-ensure-buffer-path)))
    (-edit-zk-list
     (-zk-list
      path '() "Switch to note: " #'-is-open))))

(defun zk-find-note ()
  "Open an already existing note."
  (interactive)
  (let ((path (-ensure-buffer-path)))
    (-edit-zk-list
     (-zk-list
      path '() "Edit note: "))))

(defun zk-find-link (&optional n)
  "Open a note linked by the current note. Prefix arg extends this recursively."
  (interactive "p")
  (let ((path (-ensure-buffer-path)))
    (-edit-zk-list
     (-zk-list
      path `(:linkedBy [,path] :recursive t :maxDistance ,(or n 1)) "Edit linked note: "))))

(defun zk-find-backlink (&optional n)
  "Open a note linking to the current note. Prefix arg extends this recursively."
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


(defun -mk-mark ()
  (if (eobp)
      'end
    (1+ (point-marker))))

(defun -restore-mark (mark)
  (if (equal mark 'end) (end-of-buffer)
    (goto-char mark)))


(defun -insert-link-at-point (absPath path &optional link-name)
  (let ((loc (-lsp-location-at-point)))
    (-lsp-cmd
     "zk.link"
     `[,absPath (:location ,loc :path ,path :title ,link-name)])))

(defun zk-index ()
  (interactive)
  (let ((path (-ensure-buffer-path)))
    (-lsp-cmd
     "zk.index"
     `[,path ,(make-hash-table)])))

(defun zk-insert-link ()
  "Insert a link at point, querying for the note by title and optionally the link's name."
  (interactive)
  (let* ((path (-ensure-buffer-path))
         (com-tbl (-zk-list-get-com-tbl path '()))
         (choice (-zk-list-read "Note title: " com-tbl nil t))
         (link-name (read-string "Link name: "))
         (link-name (if (string-empty-p link-name) nil link-name))
         (loc (-lsp-location-at-point))
         (after-link-pos (copy-marker (point-marker) t)))
    (if-let ((existing (assoc choice com-tbl)))
        (-insert-link-at-point
         (plist-get (cddr existing) :absPath)
         (plist-get (cddr existing) :path)
         link-name)
      ;; Create the link first; creation doesn't lets us specify the title
      ;; and returns only an absolute path, while link insertion requires
      ;; a relative for whatever reason, so there are some extra steps to
      ;; resolve this.
      (let* ((res (print (-lsp-cmd "zk.new" `[,path (:title ,choice)])))
             (abs-path (plist-get res :path))
             (path (-get-notebook-path abs-path)))
        (-insert-link-at-point abs-path path link-name)))

    ;; Without this, link text is inserted by LSP response after point, which
    ;; is not what we want.
    (insert " ")))

(define-minor-mode zk-mode
    "Provides keybindings for custom commands of the zk LSP server."
  :lighter " zk"
  :keymap (define-keymap
              :name "Integration of zk language server."
            "C-c C-b" 'zk-find-backlink
            "C-c C-l" 'zk-find-link
            "C-c C-n" 'zk-find-or-create
            "C-c C-o" 'zk-find-open-note
            "C-c C-c" 'zk-new)
  ;; TODO
  (add-to-list 'eglot-server-programs '(markdown-mode . ("zk" "lsp")))
  (eglot-ensure))


(defun -enable-if-registered ()
  (when-let* ((path (buffer-file-name))
              (path (file-truename path)))
    (when (-is-zk-file path)
      (zk-mode))))

(with-eval-after-load
    'markdown-mode
  (define-minor-mode zk-auto-mode
      "Automatically enable `zk-mode' for markdown files inside directories registered via `zk-register'.'"
    :global t
    (if zk-auto-mode
        (add-hook 'markdown-mode-hook #'-enable-if-registered)
      (remove-hook 'markdown-mode-hook #'-enable-if-registered))))

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
