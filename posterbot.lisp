;;;; posterbot.lisp

(in-package #:posterbot)

(defclass posterbot (client auto-joiner) ())

(defvar *posterbot* nil
  "Dynamic variable holding the bot instance. Bound by HANDLE-EVENT.")

(defparameter +image-link-regex+
  (ppcre:create-scanner "http.+\.(png|gif|jpeg|bmp|jpg)"
                        :case-insensitive-mode t))

(defun download-link (link)
  "Downloads the file at LINK to a temporary file. Returns the path to
the downloaded file."
  (cl-fad:with-output-to-temporary-file (out :element-type '(unsigned-byte 8))
    (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8)))
          (file-stream  (drakma:http-request link :want-stream t)))
      (loop :for bytes = (read-sequence buffer file-stream)
         :while (plusp bytes) :do (write-sequence buffer out)))))

(defun filename-from-link (link)
  "Extracts the filename of a link. I.e. everything after the last / character."
  (first (last (ppcre:split "/" link))))

(defun make-mime-type (filename)
  "Given a string FILENAME, returns a string representing a sensible guess for a mimetype."
  (format nil "image/~a"
          (let ((type  (string-downcase (pathname-type filename))))
            (cond ((member type '("jpg" "jpeg") :test #'equal) "jpeg")
                  ((equal type "svg") "svg+xml")
                  (t type)))))

(defun handle-link-candiate (word)
  "Checks if WORD is an HTTP URI pointing to an image resource. If it
is, downloads the image and posts it to the current room."
  (let ((link (ppcre:scan-to-strings +image-link-regex+ word)))
    (when link
      (let* ((file-path (download-link link))
             (file-name (filename-from-link link))
             (mxc-uri 
              (upload *posterbot*
                      file-name
                      (alexandria:read-file-into-byte-vector file-path) 
                      :content-type (make-mime-type word))))
        (if mxc-uri
            (send-image-message *posterbot* *room-id* file-name mxc-uri
                                :info (list :|mimetype| (make-mime-type word)))
            (send-text-message *posterbot* *room-id* "I have failed you :("))))))

;; look for links to images, one word at a time, downloading and
;; posting any images found to the room at the current *ROOM-ID*
(defmethod handle-event :after ((*posterbot* posterbot) (event text-message-event))
  (mapc #'handle-link-candiate (ppcre:split " " (msg-body event))))


(defun start-posterbot ()
  "A start function to pass in as the :toplevel to SAVE-LISP-AND-DIE"
  (let* ((config (if (uiop:file-exists-p "posterbot.config")
                     (with-open-file (input "posterbot.config")
                       (read input))
                     (progn (format  t "I think you need a posterbot.config~^")
                            (return-from start-posterbot))))
         (bot (make-instance 'posterbot
                             :ssl (if (member :ssl config)
                                      (getf config :ssl)
                                      t)
                             :hardcopy (getf config :hardcopy)
                             :user-id (getf config :user-id)
                             :homeserver (getf config :homeserver))))
    (when (not (logged-in-p bot))
      (login bot (getf config :user-id) (getf config :password)))
    (start bot)))
