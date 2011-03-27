;;; A class describing an AMI entity as returned by the EC2 DescribeInstances request, along with supporting functions.

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

(defclass ami ()
  ((image-id :initarg :image-id
             :reader get-image-id)
   (manifest :initarg :manifest
             :reader get-manifest)
   (state :initarg :state
          :reader get-state)
   (owner-id :initarg :owner-id
             :reader get-owner-id)
   (visibility :initarg :visibility
               :reader get-visibility)
   (architecture :initarg :architecture
                 :reader get-architecture)
   (image-type :initarg :image-type
               :reader get-image-type)))

(defmethod print-object ((self ami) strm)
  (print-unreadable-object (self strm :identity t :type t)
    (format strm "id: ~A (~A)" (get-image-id self) (get-manifest self))))

(defun make-ami (node)
  (flet ((parse-visibility (state)
           (if (equal state "false")
               :private
             :public)))
    (make-instance 'ami :image-id (getattr '|imageId| node)
                   :manifest (getattr '|imageLocation| node)
                   :state (getattr '|imageState| node)
                   :owner-id (getattr '|imageOwnerId| node)
                   :architecture (getattr '|architecture| node)
                   :image-type (getattr '|imageType| node)
                   :visibility (parse-visibility (getattr '|isPublic| node)))))

(defun make-ami-set (doc)
  (collecting-element-children (image (find-element '|imagesSet| doc))
    (make-ami image)))
