;;; Class representing a volume snapshot.

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

(defclass snapshot ()
  ((snapshot-id :initarg :snapshot-id
                :reader get-snapshot-id)
   (volume-id :initarg :volume-id
              :reader get-volume-id)
   (status :initarg :status
           :reader get-status)
   (progress :initarg :progress
             :initform nil
             :reader get-progress)
   (start-time :initarg :start-time
               :reader get-start-time)))

(defmethod print-object ((self snapshot) strm)
  (print-unreadable-object (self strm :identity t :type t)
    (format strm "~A; volume ~A; status ~A" (get-snapshot-id self) (get-volume-id self) (get-status self))))

(defun make-snapshot (body)
  (make-instance 'snapshot
                 :snapshot-id (getattr '|snapshotId| body)
                 :status (getattr '|status| body)
                 :volume-id (getattr '|volumeId| body)
                 :start-time (getattr '|startTime| body)))

(defun collect-snapshots (body)
  (collecting-element-children (snap (find-element '|snapshotSet| body))
    (make-instance 'snapshot
                   :snapshot-id (getattr '|snapshotId| snap)
                   :status (getattr '|status| snap)
                   :volume-id (getattr '|volumeId| snap)
                   :start-time (getattr '|startTime| snap)
                   :progress (getattr '|progress| snap))))
