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

(defclass availability-zone ()
  ((zone-name :initarg :zone-name
              :reader get-zone-name)
   (state :initarg :state
          :reader get-state)
   (region :initarg :region
           :reader get-region)))

(defmethod print-object ((self availability-zone) strm)
  (print-unreadable-object (self strm :identity t :type t)
    (format strm "zone: ~A" (get-zone-name self))))

(defun make-availability-zone (node)
  (make-instance 'availability-zone
                 :zone-name (getattr '|zoneName| node)
                 :state (getattr '|zoneState| node)
                 :region (getattr '|regionName| node)))

(defun make-availability-zone-set (doc)
  (collecting-element-children (zone (find-element '|availabilityZoneInfo| doc))
    (make-availability-zone zone)))
