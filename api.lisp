;;; The EC2 API as currently supported.

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

(defun describe-images (&rest ami-ids)
  (let ((basic-params (append '(("Action" . "DescribeImages") ("Owner.1" . "self"))
                              (make-entity-list "ImageId" ami-ids))))
    (make-ami-set (issue-request basic-params))))

(defun describe-image (ami-id)
  (first (describe-images ami-id)))

(defun describe-instances (&key (keep-reservations nil))
  (flet ((coalesce-instances (set)
           (loop for reservation in set
                nconc (second reservation))))
    (let* ((raw-params '(("Action" . "DescribeInstances")))
           (set (make-ami-instance-set (issue-request raw-params))))
      (if keep-reservations
          set
        (coalesce-instances set)))))

(defun terminate-instances (&rest inst-ids)
  (let ((params (append '(("Action" . "TerminateInstances"))
                        (make-entity-list "InstanceId" inst-ids))))
    (issue-request params)
    inst-ids))

(defun describe-zones ()
  (let ((raw-params '(("Action" . "DescribeAvailabilityZones"))))
    (make-availability-zone-set (issue-request raw-params))))

(defun get-console-output (instid)
  (let ((params `(("Action" . "GetConsoleOutput") ("InstanceId" . ,instid))))
    (make-console-output (issue-request params))))

(defun run-instances (image-id key-name &key (virtual-name nil) (instance-type "m1.large") (mincount "1")
                      (maxcount "1") (user-data nil) (zone (aws:default-zone)) (monitor-instance nil)
                      (security-group nil))
  (let ((params `(("Action" . "RunInstances") ("ImageId" . ,image-id) ("KeyName" . ,key-name)
                  ("InstanceType" . ,instance-type) ("MinCount" . ,mincount)
                  ("MaxCount" . ,maxcount) ("Placement.AvailabilityZone" . ,zone)
                  ("Monitoring.Enabled" . ,(if monitor-instance "true" "false"))
                  ("UserData" . ,(encode-user-data user-data))
                  ,@(when security-group
                      `(("SecurityGroup" . ,security-group))))))
    (make-initiated-instance (issue-request params) :virtual-name virtual-name)))

(defun describe-volumes ()
  (let ((raw-params '(("Action" . "DescribeVolumes"))))
    (make-volume-set (issue-request raw-params))))

(defun create-volume (&key (size 5) (zone (aws:default-zone)))
  (let ((params `(("Action" . "CreateVolume") ("Size" . ,(make-volume-size size)) ("AvailabilityZone" . ,zone))))
    (create-volume-response (issue-request params))))

(defun delete-volume (volid)
  (let ((params `(("Action" . "DeleteVolume") ("VolumeId" . ,volid))))
    (issue-request params)
    volid))

(defun create-security-group (name description)
  (let ((params `(("Action" . "CreateSecurityGroup") ("GroupName" . ,name)
                  ("GroupDescription" . ,(encode-group-description description)))))
    (issue-request params)
    name))

(defun delete-security-group (name)
  (let ((params `(("Action" . "DeleteSecurityGroup") ("GroupName" . ,name))))
    (issue-request params)
    name))

(defun describe-security-groups (&rest names)
  (let ((params (append '(("Action" . "DescribeSecurityGroups")) (make-entity-list "GroupName" names))))
    (collect-security-groups (issue-request params))))

(defgeneric create-snapshot (volume)
  (:method ((self volume))
    (let ((params `(("Action" . "CreateSnapshot") ("VolumeId" . ,(get-volume-id self)))))
      (make-snapshot (issue-request params)))))

(defun delete-snapshot (snapshot-id)
  (let ((params `(("Action" . "DeleteSnapshot") ("SnapshotId" . ,snapshot-id))))
    (issue-request params)
    snapshot-id))

(defun describe-snapshots (&rest snapshot-ids)
  (let ((params (append '(("Action" . "DescribeSnapshots")) (make-entity-list "SnapshotId" snapshot-ids))))
    (collect-snapshots (issue-request params))))

(defun attach-volume (volid instid device)
  (let ((params `(("Action" . "AttachVolume") ("VolumeId" . ,volid) ("InstanceId" . ,instid)
                  ("Device" . ,device))))
    (make-attached-volume (issue-request params))))

(defun monitor-instances (&rest instance-ids)
  (let ((params (append '(("Action" . "MonitorInstances")) (make-entity-list "InstanceId" instance-ids))))
    (cloudwatch:collect-monitored-instances (issue-request params))))

(defun unmonitor-instances (&rest instance-ids)
  (let ((params (append '(("Action" . "UnmonitorInstances")) (make-entity-list "InstanceId" instance-ids))))
    (cloudwatch:make-monitored-instance (issue-request params))))

(defmacro with-header-stream ((&optional (stream *standard-output*)) &body body)
  `(let ((drakma:*header-stream* ,stream))
     ,@body))

;;; Keypairs

(defun create-keypair (name)
  (let* ((params `(("Action" . "CreateKeyPair")
                   ("KeyName" . ,name)))
         (response (issue-request params)))
    (make-instance 'private-key
                   :name (car (find-element '|keyName| response))
                   :fingerprint (car (find-element '|keyFingerprint| response))
                   :material (car (find-element '|keyMaterial| response)))))

(defun delete-keypair (name)
  (let ((params `(("Action" . "DeleteKeyPair")
                  ("KeyName" . ,name))))
    (string= "true"
             (car (find-element '|return| (issue-request params))))))

(defun describe-keypairs ()
  (let ((params `(("Action" . "DescribeKeyPairs")))
        (keys nil))
    (with-element-children (key (find-element '|keySet| (issue-request params)))
      (push (make-instance 'key
                           :name (getattr '|keyName| key)
                           :fingerprint (getattr '|keyFingerprint| key))
            keys))
    keys))
