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
  (let ((link (ppcre:scan-to-strings +image-link-regex+ word)))
    (when link
      (let* ((file-path (download-link link))
             (file-name (filename-from-link link))
             (mxc-uri 
              (upload *posterbot*
                      file-name
                      (alexandria:read-file-into-byte-vector file-path)
                      :content-type (make-mime-type word))))
        (send-image-message *posterbot* *room-id* file-name mxc-uri
                            :info (list :|mimetype| (make-mime-type word)))))))

;; look for links to images, one word at a time, downloading and
;; posting any images found to the room at the current *ROOM-ID*
(defmethod handle-event :after ((*posterbot* posterbot) (event text-message-event))
  (mapc #'handle-link-candiate (ppcre:split " " (msg-body event))))
