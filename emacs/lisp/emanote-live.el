;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(use-package websocket
  :ensure t)


(cl-defstruct el--config
  base-dir
  emanote-url
  websocket-port)


(defun el--config-resolve-url-path (config path)
  "Check if PATH is part of the zettelkasten defined by CONFIG and return the corresponding url path if yes" 
  (let ((path (file-truename path)))
    (when (string-prefix-p (el--config-base-dir config) path)
      (let* ((rel-path (string-remove-prefix (el--config-base-dir config) path))
             (url-path (concat (file-name-sans-extension rel-path) ".html")))
        ;; Handle the case where the configured base-dir has a trailing slash
        (if (string-prefix-p "/" url-path)
            url-path
          (concat "/" url-path))))))


(cl-defstruct el--session
  "An active websocket connection assocated with a config"
  config
  websocket
  current-page
  connections)


(defun el--session-update-url (session url-path)
  "Update the current URL-PATH of the session and notify all active connections."
  (let ((url (concat (el--config-emanote-url (el--session-config session)) url-path)))
    (when (not (equal url (el--session-current-page session)))
      (dolist (con (el--session-connections session))
        (websocket-send-text con (el--encode-url url)))
      (setf (el--session-current-page session) url))))


(defun el--encode-url (url)
  (json-encode-list `(:url ,url)))


(defun el--init-session (config)
  (let ((session (make-el--session
                  :websocket nil
                  :current-page nil
                  :connections '()
                  :config config)))
    (setf (el--session-websocket session)
          (websocket-server
           (el--config-websocket-port config)
           :host 'local
           :on-open (lambda (con)
                      (push con (el--session-connections session))
                      (when-let (url (el--session-current-page session))
                        (websocket-send-text con (el--encode-url url)))
                      (message "emanote-live connected"))
           :on-close (lambda (con)
                       (setf (el--session-connections session) (remove con (el--session-connections session))))
                                        ; :on-message (lambda (_ws frame) (print "msg") (print frame))
           ))
    session))


(defun el--close-session (session)
  (websocket-server-close (el--session-websocket session)))


(defun el--session-process-path (session path &optional interactive)
  "Check if SESSION is applicable for PATH and update the preview url accordingly if this is the case. If not, error iff INTERACTIVE."
  ;; Check path is a markdown file
  (if (not (equal (file-name-extension path) "md"))
      (when interactive
        (error "Buffer is not a markdown file"))
    ;; Check it's inside the configured Zettelkasten
    (if-let ((url-path (el--config-resolve-url-path (el--session-config session) path)))
        ;; Broadcast update
        (el--session-update-url el--session url-path)
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
                    :emanote-url (or emanote-url "http://localhost:8080")
                    :websocket-port (or websocket-port 8081))))



(defun emanote-live-start-preview ()
  (interactive)
  (unless el--config
    (error "emanote-live is not configured"))
  ;; Shutdown possible existing session
  (emanote-live-stop-preview)
  ;; Create new preview session
  (setq el--session (el--init-session el--config))
  ;; Open browser entrypoint, connecting to websocket
  (funcall emanote-live-open-in-browser "~/init.html")
  ;; Initial sync; Not a race condition as we save the last page shown and use it to initialize new connections.
  (emanote-live-sync-buffer)
  )


(defun emanote-live-sync-buffer (&optional interactive)
  (interactive '(t))
  (unless el--session
    (error "No active emanote-live session"))
  (if-let ((path (buffer-file-name)))
      (el--session-process-path el--session (buffer-file-name) interactive)
    (when interactive
      (error "Buffer has no filename")))
  )


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
                 ;; Newly created notes are synced this way as soon as they are safed the first time.
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
