;;; A generic, configurable interface to the EC2 signing protocol.

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

(defvar *digest* :sha256)

(defgeneric sort-params (params))
(defgeneric make-signature (data-string host-header))
(defgeneric make-parameters (command-params api-version))
(defgeneric canonicalize-params (params))
(defgeneric encode-user-data (udata))
(defgeneric make-digest (data-string &key digest))

(defun digest-method ()
  #+:ec2-signature-version=2
  *digest*
  #+ec2-signature-version=1
  :sha1)

(defun signing-key ()
  #+:ec2-signature-version=2
  (aws:secret-key)
  #+:ec2-signature-version=1
  (aws:access-key))

(defmethod make-digest (data-string &key (digest :sha1))
  (let ((hmac (ironclad:make-hmac (ironclad:ascii-string-to-byte-array (signing-key)) digest)))
    (ironclad:update-hmac hmac (ironclad:ascii-string-to-byte-array data-string))
    (ironclad:hmac-digest hmac)))
