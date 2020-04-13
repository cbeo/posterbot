;;;; posterbot.lisp

(in-package #:posterbot)

;;; An Example Bot with Granolin:
;;; 
;;; A bot is a class that extends granolin:client.  There are several
;;; "plug-in" classes that you can also extend, each one implements a
;;; self-contained functionality that might be useful.  For example,
;;; the posterbot class that is defined below extends the
;;; granolin:auto-joiner plugin.  An instance of a class extending
;;; auto-joiner will join any room to which it is invited.

(defclass posterbot (client auto-joiner) ())

(defvar *posterbot* nil
  "Dynamic variable holding the bot instance. Bound by HANDLE-EVENT.")

;;; All you need to do to plug your bot into granolin's logic is
;;; specialize HANDLE-EVENT :AFTER or HANDLE-EVENT :BEFORE methods on
;;; your bot class.

;;; Granolin defines a number of event types, and you can specialize
;;; HANDLE-EVENT for each one. The folling handler is specialized for
;;; TEXT-MESSAGE-EVENT.
;;;
;;; Within the extent of a HANDLE-EVENT call, the special variable
;;; *ROOM-ID* is bound to the id of the matrix room where the EVENT
;;; occurred.

(defmethod handle-event :after ((*posterbot* posterbot) (event text-message-event))
  (mapc #'handle-link-candiate (ppcre:split " " (msg-body event))))



;;; Finally, when you want to use a bot - you make an instance of your
;;; class and call the GRANOLIN:LOGIN method on it, and, assuming the
;;; login is successful, GRANOLIN:START, which will begin polling the
;;; homeserver for messages.
;;;
;;; Instantiating a bot instance accepts a few paramters, like
;;; :USER-ID and :HOMESERVER, which should be self explanatory.
;;; 
;;; The :HARDCOPY parameter is the name of a file where bot state will
;;; be saved if it is stopped or if it dies.  You can use this
;;; "hardcopy" to resume your bot.

(defun start-posterbot ()
  "A start function to pass in as the :toplevel to SAVE-LISP-AND-DIE"
  (let* ((config (if (uiop:file-exists-p "posterbot.config")
                     (with-open-file (input "posterbot.config")
                       (read input))
                     (progn (format  t "I think you need a posterbot.config~%~%")
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


(defparameter +image-link-regex+
  (ppcre:create-scanner "http.+\\\.(png|gif|jpeg|bmp|jpg)$"
                        :case-insensitive-mode t))

(defparameter +giphy-link-regex+
  (ppcre:create-scanner "https://giphy.com/gifs/.+-([a-zA-Z0-9]+)"
                        :case-insensitive-mode t))

(defun download-link (link)
  "Downloads the file at LINK to a temporary file. Returns the path to
the downloaded file.  If there is an error thrown at any point, returns NIL."
  (handler-case 
      (cl-fad:with-output-to-temporary-file (out :element-type '(unsigned-byte 8))
        (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8)))
              (file-stream  (drakma:http-request link :want-stream t)))
          (loop :for bytes = (read-sequence buffer file-stream)
             :while (plusp bytes) :do (write-sequence buffer out))))
    (error () nil)))


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


(defun check-word-for-link (word)
  (cond ((ppcre:scan-to-strings +image-link-regex+ word)
         (ppcre:scan-to-strings +image-link-regex+ word))
        ((ppcre:scan-to-strings +giphy-link-regex+ word)
         (multiple-value-bind (string matches)
             (ppcre:scan-to-strings +giphy-link-regex+ word)
           (declare (ignore string))
           (when (plusp (length matches))
             (format nil "https://media.giphy.com/media/~a/giphy.gif" (elt matches 0)))))
        (t nil)))

(defun handle-link-candiate (word)
  "Checks if WORD is an HTTP URI pointing to an image resource. If it
is, downloads the image and posts it to the current room."
  (let ((link (check-word-for-link word)))
    (when link
      (let* ((file-path (download-link link))
             (file-name (filename-from-link link))
             (mxc-uri 
              (and file-path
                   (upload *posterbot*
                           file-name
                           (alexandria:read-file-into-byte-vector file-path) 
                           :content-type (make-mime-type file-name)))))
        (if mxc-uri
            (progn
              (send-image-message *posterbot* *room-id* file-name mxc-uri
                                  :info (list :|mimetype| (make-mime-type file-name)))
              (uiop:delete-file-if-exists file-path))
            (send-text-message *posterbot* *room-id* "I have failed you :("))))))

