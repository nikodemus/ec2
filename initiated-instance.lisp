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

(defclass initiated-instance ()
  ((request-id :initarg :request-id
               :reader get-request-id)
   (reservation-id :initarg :reservation-id
                   :reader get-reservation-id)
   (virtual-name :initarg :virtual-name
                 :initform nil
                 :reader get-virtual-name)
   (instance-id :initarg :instance-id
                :reader get-instance-id)
   (launch-time :initarg :launch-time
                :reader get-launch-time)))

(defmethod print-object ((self initiated-instance) strm)
  (print-unreadable-object (self strm :identity t :type t)
    (format strm "~A (~A); reservation: ~A" (get-instance-id self) (get-virtual-name self) (get-reservation-id self))))

(defun get-initiated-instance-data (body)
  (with-element-children (item (find-element '|instancesSet| body))
    (return (values (getattr '|instanceId| item) (getattr '|launchTime| item)))))

(defun make-initiated-instance (body &key (virtual-name nil))
  (multiple-value-bind (inst-id launch-time)
      (get-initiated-instance-data body)
    (make-instance 'initiated-instance
                   :request-id (getattr '|requestId| body)
                   :reservation-id (getattr '|reservationId| body)
                   :virtual-name virtual-name
                   :instance-id inst-id
                   :launch-time launch-time)))
