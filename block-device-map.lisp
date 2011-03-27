;;; Support for the latest EC2 block-device-mappings.

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

(defclass block-device-map ()
  ((device-name :initarg :device-name
                :reader get-device-name)
   (device :initarg :device
           :reader get-device)))

(defmethod print-object ((self block-device-map) strm)
  (print-unreadable-object (self strm :identity t :type t)
    (format strm "~A; volume: ~A" (get-device-name self) (block-device-volume-id (get-device self)))))

(defstruct block-device
  volume-id status attach-time termination-behavior)

(defun make-block-device-descriptor (ebs)
  (flet ((map-termination-behavior (val)
           (if (equal val "true") :delete :keep)))
    (make-block-device :volume-id (getattr '|volumeId| ebs)
                       :status (getattr '|status| ebs)
                       :attach-time (getattr '|attachTime| ebs)
                       :termination-behavior (map-termination-behavior (getattr '|deleteOnTermination| ebs)))))

(defun make-block-device-mapping (body)
  (make-instance 'block-device-map
                 :device-name (getattr '|deviceName| body)
                 :device (make-block-device-descriptor (find-element '|ebs| body))))
