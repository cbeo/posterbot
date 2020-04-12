;;;; posterbot.lisp

(in-package #:posterbot)

(defclass posterbot (client auto-joiner) ())

(defvar *posterbot* nil "dynamic variable holding the bot so I dont have to pass it around")

(defparameter +image-link-regex+
  (ppcre:create-scanner "http.+\.(png|gif|jpeg|bmp|jpg)"
                        :case-insensitive-mode t))

(defun download-link (link)
  (cl-fad:with-output-to-temporary-file (out :element-type '(unsigned-byte 8))
    (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8)))
          (file-stream  (drakma:http-request link :want-stream t)))
      (loop :for bytes = (read-sequence buffer file-stream)
         :while (plusp bytes) :do (write-sequence buffer out)))))


(defun filename-from-link (link)
  (first (last (ppcre:split "/" link))))

(defun make-mime-type (filename)
  (format nil "image/~a" (pathname-type filename)))

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
