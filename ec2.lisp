;;; The EC2 protocol interface.

;;;  Copyright (c) 2010 Saffron Technology (www.saffrontech.com)

;;;  Permission is hereby granted, free of charge, to any person
;;;  obtaining a copy of this software and associated documentation
;;;  files (the "Software"), to deal in the Software without
;;;  restriction, including without limitation the rights to use,
;;;  copy, modify, merge, publish, distribute, sublicense, and/or sell
;;;  copies of the Software, and to permit persons to whom the
;;;  Software is furnished to do so, subject to the following
;;;  conditions:

;;;  The above copyright notice and this permission notice shall be
;;;  included in all copies or substantial portions of the Software.

;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;  HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;  WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;  OTHER DEALINGS IN THE SOFTWARE.

(in-package :ec2)

(defvar *cl-ec2-version* "0.3")
(defvar *api-version* "2009-11-30")
(defvar *xmlns* (format nil "http://ec2.amazonaws.com/doc/~A/" *api-version*))
(defvar *ec2-namespace* (s-xml:register-namespace *xmlns* "EC2" :ec2))
(defvar *host-header* "ec2.amazonaws.com")
(defvar *url* (concatenate 'string "http://" *host-header*))

(defun cl-ec2-version ()
  *cl-ec2-version*)
  
(defun issue-request (specific-params &key (protocol :lxml) (parse-body t))
  (let* ((raw-params (make-parameters specific-params *api-version*))
         (params (append raw-params
                         `(("Signature" . ,(make-signature (canonicalize-params raw-params) *host-header*))))))
    (multiple-value-bind (body status headers uri stream must-close reason-phrase)
        (drakma:http-request *url* :parameters params)
      (declare (ignore must-close))
      (when (ec2-error-p status)
        (make-ec2-error :body body :status status :headers headers :uri uri :reason reason-phrase))
      (close stream)
      (if parse-body
          (parse-http-body body protocol)
        body))))

(defun make-entity-list (tag entities)
  (loop for entity in entities
       for i from 1 to (length entities)
       collect `(,(format nil "~A.~A" tag i) . ,entity)))

(defgeneric make-volume-size (value)
  (:method ((value integer))
    (if (<= 1 value 1024)
        (format nil "~A" value)
      (error 'volume-size-error :requested-size value)))
  (:method ((value string))
    (make-volume-size (parse-integer value)))
  (:method ((value t))
    (error 'volume-size-error :requested-size value)))

(defmethod encode-user-data (udata)
  (flet ((format-udata ()
           (let ((pairs (loop for key being the hash-key of udata using (hash-value value)
                             nconc `(,key ,value))))
             (format nil  "~{~A=~A~^,~}" pairs))))
    (with-output-to-string (strm)
      (s-base64:encode-base64-bytes (ironclad:ascii-string-to-byte-array (format-udata)) strm))))

(defun encode-group-description (raw-description)
  (format nil "'~A'" raw-description))
