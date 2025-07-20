;; -*- lexical-binding: t; -*-

(require 'common)

(use-package websocket
    :straight t)


(cl-defstruct el--config
  base-dir
  entrypoint
  emanote-url
  websocket-port)


(defun el--config-rel-to-url-path (config rel-path)
  (let* ((url-path (concat (file-name-sans-extension rel-path) ".html")))
    (concat "/" url-path)))

(defun el--config-url-to-rel-path (config url-path)
  (string-remove-prefix
   "/"
   (concat (file-name-sans-extension url-path) ".md")))

(defun el--config-get-rel-path (config path)
  "If PATH is inside the base dir of CONFIG, return its relative path, nil otherwise."
  (let ((path (file-truename path))
        (base-dir (el--config-base-dir config)))
    (when (string-prefix-p (el--config-base-dir config) path)
      (thread-last
        path
        (string-remove-prefix (el--config-base-dir config))
        (string-remove-prefix "/")))))


(cl-defstruct el--session
  "An active websocket connection assocated with a config"
  ;; Dynamic parameters provided by the user or hard-coded.
  config
  ;; Dynamic state created at session-runtime
  websocket
  current-page
  connections
  window)


(defun el--connection-send (con type payload)
  (let ((encoded (json-encode-list `(:type ,type :payload ,payload))))
    (websocket-send-text con encoded)))


(defun el--session-broadcast (session type payload)
  (dolist (con (el--session-connections session))
    (el--connection-send con type payload)))


(defun el--get-json-field (obj name)
  (or
   (alist-get name obj)
   (error "object has no field %S: %S" name obj)))

(defun el--session-handle-message (session frame)
  (let* ((msg (thread-first
                frame
                (websocket-frame-text)
                (json-read-from-string)))
         (type (el--get-json-field msg 'type))
         (payload (el--get-json-field msg 'payload))
         (config (el--session-config session)))
    (pcase type
      ("note-changed"
       (unless (equal payload (el--session-current-page session))
         (let ((path (concat
                      (el--config-base-dir config) "/"
                      (el--config-url-to-rel-path config payload))))
           (when-let ((win (el--session-window session)))
             (when (window-live-p win)
               (select-window win)))
           (find-file path))))
      (_ (error "unknown type %S" type)))))


(defun el--session-init (config)
  (let* ((session (make-el--session
                   :websocket nil
                   :current-page nil
                   :connections '()
                   :window nil
                   :config config))
         (websocket (websocket-server
                     (el--config-websocket-port config)
                     :host 'local
                     :on-open (lambda (con)
                                (push con (el--session-connections session))
                                ;; Propagate current note if known
                                (when-let (url-path (el--session-current-page session))
                                  (el--connection-send
                                   con "set-note"
                                   (concat (el--config-emanote-url config) url-path)))
                                (message "emanote-live connected"))
                     :on-close (lambda (con)
                                 (cl-callf2 remove con
                                     (el--session-connections session))
                                 (message "emanote-live disconnected"))
                     :on-message (lambda (_ws frame)
                                   (el--session-handle-message
                                    session frame)))))
    (setf (el--session-websocket session) websocket)
    session))


(defun el--close-session (session)
  (el--session-broadcast session "close" nil)
  (websocket-server-close (el--session-websocket session)))


(defun el--session-process-path (session path &optional interactive)
  "Check if SESSION is applicable for PATH and update the preview url accordingly if this is the case. If not, error iff INTERACTIVE."
  ;; Check path is a markdown file
  (if (not (equal (file-name-extension path) "md"))
      (when interactive
        (error "Buffer is not a markdown file"))
    ;; Check it's inside the configured Zettelkasten
    (if-let* ((config (el--session-config session))
              (rel-path (el--config-get-rel-path config path))
              (url-path (el--config-rel-to-url-path config rel-path))
              (_guard (not (or
                            (equal url-path (el--config-entrypoint config))
                            (equal url-path (el--session-current-page session)))))
              (url (concat (el--config-emanote-url config) url-path)))
        ;; Update web page accordingly
        (progn
          (el--session-broadcast session "set-note" url)
          (setf (el--session-current-page session) url-path)
          )
      (when interactive
        (error "Buffer is not associated with Zettelkasten")))))


;; Entrypoints

(defvar el--config nil)
(defvar el--session nil)

;; TODO: package ~/init.html and start at init
(defvar emanote-live-open-in-browser (lambda (path)
                                       (call-process "firefox" nil 0 nil "--new-tab" path)))

(defun emanote-live-configure (base-dir &optional emanote-url websocket-port)
  "Sets the global emanote-live configuration."
  (setq el--config (make-el--config
                    :base-dir (file-truename base-dir)
                    :entrypoint "/preview.html"
                    :emanote-url (or emanote-url "http://localhost:8080")
                    :websocket-port (or websocket-port 8081))))


(defun el--config-entry-url (config)
  (concat (el--config-emanote-url config) (el--config-entrypoint config)))


(defun emanote-live-start-preview ()
  (interactive)
  (unless el--config
    (error "emanote-live is not configured"))
  ;; Shutdown possible existing session
  (emanote-live-stop-preview)
  ;; Create new preview session
  (setq el--session (el--session-init el--config))
  ;; Open browser entrypoint, connecting to websocket
  (funcall emanote-live-open-in-browser (el--config-entry-url el--config))
  ;; Initial sync; Not a race condition as we save the last page shown and use it to initialize new connections.
  (emanote-live-sync-buffer))


(defun emanote-live-sync-buffer (&optional interactive)
  (interactive '(t))
  (unless el--session
    (user-error "No active emanote-live session"))
  (if-let ((path (buffer-file-name)))
      (when (el--session-process-path el--session (buffer-file-name) interactive)
        (setf (el--session-window el--session) (selected-window)))
    (when interactive
      (user-error "Buffer has no filename"))))


(defun emanote-live-stop-preview (&optional interactive)
  (interactive '(t))
  (if (not el--session)
      (when interactive
        (error "No active emanote-live session"))
    (el--close-session el--session)
    (setq el--session nil)))


(define-minor-mode emanote-live-mode
    "Live emanote preview"
  :global t
  :lighter " [live]"
  (let ((hooks '(window-selection-change-functions
                 window-buffer-change-functions
                 ;; Newly created notes are synced this way as soon as they are saved the first time.
                 after-save-hook)))
    (if emanote-live-mode
        (progn
          (emanote-live-start-preview)
          (dolist (hook hooks)
            (add-hook hook #'el--sync-buffer-hook)))
      (emanote-live-stop-preview)
      (dolist (hook hooks)
        (remove-hook hook #'el--sync-buffer-hook)))))

(defun el--sync-buffer-hook (&rest _)
  (emanote-live-sync-buffer))


(provide 'emanote-live)
