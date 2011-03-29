;;; An implementation of Signature Version 1, a (deprecated) EC2 signing protocol.

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

(in-package :ec2-protocol)

(defmethod sort-params (params)
  (sort (copy-list params) (lambda (a b) (string< (string-downcase (car a)) (string-downcase (car b))))))

(defmethod canonicalize-params (params)
  (with-output-to-string (strm)
    (loop for (key . value) in params do
         (write-string key strm)
         (write-string value strm))))

(defmethod make-signature (data-string host-header)
  (declare (ignore host-header))
  (flet ((encode-sha1 (bytes)
           (with-output-to-string (strm)
             (s-base64:encode-base64-bytes bytes strm))))
    (encode-sha1 (make-digest data-string :digest (digest-method)))))

(defmethod make-parameters (command-params api-version)
  (let ((access-params `(("AWSAccessKeyId" . ,(aws:access-key)) ("SignatureVersion" . "1")
                         ("Timestamp" . ,(make-timestamp)) ("Version" . ,api-version))))
    (sort-params (append command-params access-params))))
