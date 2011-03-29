;;; A class describing an EBS AMI instance as returned by the EC2 DescribeInstances request, along with supporting functions.

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

(defclass ami-instance ()
  ((instance-id :initarg :instance-id
                :reader get-instance-id)
   (reservation-id :initarg :reservation-id
                   :reader get-reservation-id)
   (image-id :initarg :image-id
             :reader get-image-id)
   (groups :initarg :groups
           :reader get-groups)
   (state :initarg :state
          :reader get-state)
   (architecture :initarg :architecture
                 :reader get-architecture)
   (private-dns :initarg :private-dns
                :initform nil
                :reader get-private-dns)
   (private-ip-address :initarg :private-ip-address
                       :reader get-private-ip-address)
   (public-dns :initarg :public-dns
               :initform nil
               :reader get-public-dns)
   (public-ip-address :initarg :public-ip-address
                      :reader get-public-ip-address)
   (instance-type :initarg :instance-type
                  :reader get-instance-type)
   (launch-time :initarg :launch-time
                :reader get-launch-time)
   (launch-index :initarg :launch-index
                 :reader get-launch-index)
   (key-name :initarg :key-name
             :reader get-key-name)
   (monitoring-state :initarg :monitoring-state
                     :initform "disabled"
                     :reader get-monitoring-state)
   (root-device-type :initarg :root-device-type
                     :reader get-root-device-type)
   (block-device-map :initarg :block-device-map
                     :initform nil
                     :reader get-block-device-map)
   (zone :initarg :zone
         :reader get-zone)))

(defun runningp (instance)
  (string= (get-state instance) "running"))

(defun terminatedp (instance)
  (string= (get-state instance) "terminated"))

(defun pendingp (instance)
  (string= (get-state instance) "pending"))

(defmethod print-object ((self ami-instance) strm)
  (print-unreadable-object (self strm :identity t :type t)
    (format strm "~A; ~A; ~A"
            (get-instance-id self) (get-image-id self) (get-state self))
    (let ((dns (get-public-dns self)))
      (when dns
        (format strm "; dns: ~A" dns)))))

(defun find-instance-state (instance)
  ;; instance-state looks like ((|code| "16") (|name| "running"))
  (getattr '|name| (getattrs '|instanceState| instance)))

(defun find-groups (reservation)
  (collecting-element-children (group (find-element '|groupSet| reservation))
    (getattr '|groupId| group)))

(defun find-availability-zone (instance)
  (second (getattr '|placement| instance)))

(defun find-block-device-maps (instance)
  (collecting-element-children (bd (find-element '|blockDeviceMapping| instance))
    (make-block-device-mapping bd)))

(defun make-ami-instance (reservation)
  (collecting-element-children (instance (find-element '|instancesSet| reservation))
    (make-instance 'ami-instance
                   :reservation-id (getattr '|reservationId| reservation)
                   :instance-id (getattr '|instanceId| instance)
                   :image-id (getattr '|imageId| instance)
                   :state (find-instance-state instance)
                   :architecture (getattr '|architecture| instance)
                   :private-dns (getattr '|privateDnsName| instance)
                   :private-ip-address (getattr '|privateIpAddress| instance)
                   :public-dns (getattr '|dnsName| instance)
                   :public-ip-address (getattr '|ipAddress| instance)
                   :instance-type (getattr '|instanceType| instance)
                   :launch-time (getattr '|launchTime| instance)
                   :launch-index (getattr '|amiLaunchIndex| instance)
                   :key-name (getattr '|keyName| instance)
                   :monitoring-state (second (getattr '|monitoring| instance))
                   :groups (find-groups reservation)
                   :root-device-type (getattr '|rootDeviceType| instance)
                   :block-device-map (find-block-device-maps instance)
                   :zone (find-availability-zone instance))))

(defun make-ami-instance-set (doc)
  (collecting-element-children (reservation (find-element '|reservationSet| doc))
    (let ((resid (getattr '|reservationId| reservation)))
      `(,resid ,(make-ami-instance reservation)))))

(defun make-instance-state-change-set (response)
  (collecting-element-children (node (find-element '|instancesSet| response))
    (list :id (getattr '|instanceId| node)
          :current-state (getattr '|name| (getattrs '|currentState| node))
          :previous-state (getattr '|name| (getattrs '|previousState| node)))))
