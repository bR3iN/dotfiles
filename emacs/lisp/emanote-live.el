;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(use-package websocket
             :ensure t)
(require 'websocket)

; (defvar -server nil)
; (defvar -emanote-base-url "http://localhost:8080")
; (defvar -ws-url "ws://localhost:8088")

(cl-defstruct -config
              base-dir
              emanote-url
              websocket-port
              websocket
              current-page
              connections)

(defalias '-make-config 'make-emanote-live--config)



(defvar -configs '())

(defvar -default-config 'default)

(defvar -websocket-server nil)
(defvar -client-alist '())

(defun emanote-live-register (base-dir &optional name emanote-url websocket-port)
  (let ((name (or name -default-config)))
    (when-let ((existing-config (alist-get name -configs)))
              (-shutdown-config (cdr existing-config)))
    (setf (alist-get name -configs)
          (-make-config
            :base-dir (file-truename base-dir)
            :emanote-url (or emanote-url "http://localhost:8080")
            :current-page nil
            :websocket-port (or websocket-port 8081)
            :websocket nil :connections '()))))

(defun -shutdown-config (config)
  (when-let ((server (-config-websocket config)))
            (websocket-server-close server)
            (setf (-config-websocket config) nil
                  (-config-connections config) '()
                  (-config-current-page config) nil)))


(defun -start-config (config)
  (setf (-config-current-page config) "/index.html"
        (-config-websocket config) (websocket-server (-config-websocket-port config)
                                                     :host 'local
                                                     :on-open (lambda (con)
                                                                (push con (-config-connections config))
                                                                (-send-url con (concat (-config-emanote-url config)
                                                                                       (-config-current-page config))))
                                                     :on-close (lambda (con)
                                                                 (setf (-config-connections config) (remove con (-config-connections config))))
                                                     ; :on-message (lambda (_ws frame) (print "msg") (print frame))
                                                     )))


(defun -get-config (name)
  (if-let* ((config (alist-get name -configs))) config
           (error (format "No config with name %s found" name))))


(defun emanote-live-start-preview (&optional name)
  (interactive)
  (let ((config (-get-config (or name -default-config))))
    (-start-config config)))

(defun emanote-live-shutdown-preview (&optional name)
  (interactive)
  (let ((config (-get-config (or name -default-config))))
    (-shutdown-config config)))

(defun -send-url (con url)
  ;; (print "sent")
  (websocket-send-text con (json-encode-list `(:url ,url))))

(defun -update-iframe (config url-path)
  (when (not (equal url-path (-config-current-page config)))
    (let ((url (concat (-config-emanote-url config) url-path)))
      (dolist (con (-config-connections config))
        (-send-url con url)))
    (setf (-config-current-page config) url-path)))

(defun -detect-zk-note-selected (&rest _)
  (when-let* ((curr-path (buffer-file-name))
              (curr-path (file-truename curr-path))
              (ext (file-name-extension curr-path))
              (_is-mark-down-file (equal "md" ext)))
             (dolist (named-config -configs)
               (let ((config (cdr named-config)))
                 (when (and
                         (-config-websocket config)
                         (string-prefix-p (-config-base-dir config) curr-path))
                   (let* ((rel-path (string-remove-prefix (-config-base-dir config) curr-path))
                          (url-path (concat (file-name-sans-extension rel-path) ".html")))
                     (-update-iframe config (if (string-prefix-p "/" url-path) url-path
                                              (concat "/" url-path)))))))))

(add-hook 'window-selection-change-functions #'-detect-zk-note-selected)
(add-hook 'window-buffer-change-functions #'-detect-zk-note-selected)

(provide 'emanote-live)

;; Local Variables:
;; read-symbol-shorthands: (("-" . "emanote-live--"))
;; End:
