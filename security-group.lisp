;;; Security group support.

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

(defclass security-group ()
  ((name :initarg :name
         :reader get-name)
   (description :initarg :description
                :reader get-description)
   (owner-id :initarg :owner-id
             :reader get-owner-id)
   (ip-permissions :initarg :ip-permissions
                   :initform (list)
                   :reader get-ip-permissions)))

(defstruct ip-protocol
  protocol from-port to-port ranges)

(defun gather-ip-ranges (rule)
  (collecting-element-children (range (find-element '|ipRanges| rule))
    (getattr '|cidrIp| range)))

(defun make-ip-permissions (group)
  (collecting-element-children (rule (find-element '|ipPermissions| group))
    (make-ip-protocol :protocol (getattr '|ipProtocol| rule)
                      :from-port (getattr '|fromPort| rule)
                      :to-port (getattr '|toPort| rule)
                      :ranges (gather-ip-ranges rule))))

(defun collect-security-groups (body)
  (collecting-element-children (group (find-element '|securityGroupInfo| body))
    (make-instance 'security-group
                   :name (getattr '|groupName| group)
                   :description (getattr '|groupDescription| group)
                   :owner-id (getattr '|ownerId| group)
                   :ip-permissions (make-ip-permissions group))))
