;;; An implementation of Signature Version 2, an EC2 signing protocol.

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

;;; The following copyright notice covers our importation and modification of URL-ENCODE
;;; from the Drakma package:

;;; Copyright (c) 2006-2009, Dr. Edmund Weitz.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :ec2-protocol)

(defun signature-method ()
  (case (digest-method)
    (:sha1 "HmacSHA1")
    (:sha256 "HmacSHA256")
    (t (error "Unsupported digest type ~A" (digest-method)))))

(defun url-encode (string)
  (with-output-to-string (strm)
    (loop for octet across (flexi-streams:string-to-octets (or string "") :external-format :utf-8)
       for char = (code-char octet)
       do (cond ((or (char<= #\0 char #\9)
                     (char<= #\a char #\z)
                     (char<= #\A char #\Z)
                     (find char "$-_.!*'()," :test #'char=))
                 (write-char char strm))
                ;; according to the Amazon docs, spaces must be encoded as %20, not '+' as the Drakma version does; thus the test for #\Space has
                ;; been removed.
                (t (format strm "%~2,'0x" (char-code char)))))))

(defmethod sort-params (params)
  (sort (copy-list params) (lambda (a b) (string< (car a) (car b)))))

(defmethod make-parameters (command-params api-version)
  (let ((access-params `(("SignatureVersion" . "2") ("SignatureMethod" . ,(signature-method))
                         ("Timestamp" . ,(make-timestamp)) ("AWSAccessKeyId" . ,aws:*access-key*)
                         ("Version" . ,api-version))))
    (sort-params (append command-params access-params))))

(defun make-signing-string (params host-header)
  (let* ((pairs (loop for (key . value) in params
                   collect (format nil "~A=~A" key (url-encode value))))
         (encoded-params (format nil "~{~A~^&~}" pairs)))
    (format nil "GET~%~A~%/~%~A" host-header encoded-params)))

(defmethod make-signature (params host-header)
  (with-output-to-string (strm)
    (s-base64:encode-base64-bytes (make-digest (make-signing-string params host-header) :digest (digest-method)) strm)))

#+ignore
(defmethod make-digest :after (signing-string &key &allow-other-keys)
  (format t "signing-string: ~A~%" signing-string))

#+ignore
(defmethod make-signature :around (params host-header)
  (let ((sig (call-next-method)))
    (format t "signature = ~A~%" sig)
    sig))

(defmethod canonicalize-params (params)
  (sort (copy-list params) (lambda (a b) (string< (car a) (car b)))))
