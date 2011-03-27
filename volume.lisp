;;; A class describing an EBS Volume entity as returned by the EC2 DescribeVolumes request, along with supporting functions.

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

(defclass volume ()
  ((volume-id :initarg :volume-id
             :reader get-volume-id)
   (size :initarg :size
         :reader get-size)
   (snapshot-id :initarg :snapshot-id
                :initform nil
                :reader get-snapshot-id)
   (zone :initarg :zone
         :reader get-zone)
   (creation-time :initarg :creation-time
                  :reader get-creation-time)
   (attachments :initarg :attachments
                :initform nil
                :reader get-attachments)
   (status :initarg :status
           :reader get-status)))

(defun volume-status (vol)
  (let ((attachments (get-attachments vol)))
    (if attachments
        (cdr (assoc "status" attachments :test #'equal))
      "unattached")))

(defun volume-attached-p (vol)
  (get-attachments vol))

(defun volume-available-p (vol)
  (equal (get-status vol) "available"))

(defmethod print-object ((self volume) strm)
  (print-unreadable-object (self strm :identity t :type t)
    (format strm "id: ~A; zone: ~A; status: ~A" (get-volume-id self) (get-zone self) (get-status self))))

(defun make-attachment-set (desc)
  (let ((alist (list)))
    (dolist (attr (rest desc) alist)
      (setf alist (acons (symbol-name (first attr)) (second attr) alist)))))

(defun make-volume (node)
  (make-instance 'volume :volume-id (getattr '|volumeId| node)
                 :size (getattr '|size| node)
                 :snapshot-id (getattr '|snapshotId| node)
                 :zone (getattr '|availabilityZone| node)
                 :status (getattr '|status| node)
                 :creation-time (getattr '|createTime| node)
                 :attachments (make-attachment-set (getattr '|attachmentSet| node))))

(defun make-volume-set (doc)
  (collecting-element-children (volume (find-element '|volumeSet| doc))
    (make-volume volume)))
