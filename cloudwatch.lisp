;;; The Amazon CloudWatch API.

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

(in-package :cloudwatch)

(defvar *api-version* "2009-05-15")
(defvar *xmlns* (format nil "http://monitoring.amazonaws.com/doc/~A/" *api-version*))
(defvar *namespace* (s-xml:register-namespace *xmlns* "Cloudwatch" :cloudwatch))
(defvar *default-cloudwatch-namespace* "AWS/EC2")
(defvar *host-header* "monitoring.amazonaws.com")
(defvar *url* (concatenate 'string "http://" *host-header*))

(defstruct monitored-instance
  id state)

(defclass datapoint ()
  ((samples :initarg :samples
            :initform nil
            :reader get-samples)
   (average :initarg :average
            :initform nil
            :reader get-average)
   (maximum :initarg :maximum
            :initform nil
            :reader get-maximum)
   (minimum :initarg :minimum
            :initform nil
            :reader get-minimum)
   (sum :initarg :sum
        :initform nil
        :reader get-sum)
   (unit :initarg :unit
         :initform nil
         :reader get-unit)
   (timestamp :initarg :timestamp
              :initform nil
              :reader get-timestamp)))

(defclass metric ()
  ((label :initarg :label
          :initform nil
          :reader get-label)
   (datapoints :initarg :datapoints
               :initform '()
               :reader get-datapoints)))

(defclass stored-metric ()
  ((measure :initarg :measure
            :initform nil
            :reader get-measure)
   (namespace :initarg :namespace
              :initform nil
              :reader get-namespace)
   (dimensions :initarg :dimensions
               :initform '()
               :reader get-dimensions)))

(defmethod print-object ((self stored-metric) strm)
  (print-unreadable-object (self strm :identity t :type t)
    (format strm "~A; namespace ~A" (get-measure self) (get-namespace self))))

(defmethod print-object ((self metric) strm)
  (print-unreadable-object (self strm :identity t :type t)
    (format strm "~S" (get-label self))))

(defun tostr(obj)
  (format nil "~A" obj))

(defgeneric tofloat (obj)
  (:method ((obj string))
    (read-from-string obj))
  (:method ((obj float))
    obj)
  (:method ((obj t))
    nil))

(defun collect-datapoints (body)
  (collecting-element-children (dp body)
    (make-instance 'datapoint
                   :unit (getattr '|Unit| dp)
                   :timestamp (getattr '|Timestamp| dp)
                   :samples (tofloat (getattr '|Samples| dp))
                   :sum (tofloat (getattr '|Sum| dp))
                   :minimum (tofloat (getattr '|Minimum| dp))
                   :maximum (tofloat (getattr '|Maximum| dp))
                   :average (tofloat (getattr '|Average| dp)))))

(defun make-metric (body)
  (let ((result (find-element '|GetMetricStatisticsResult| body)))
    (make-instance 'metric
                   :label (getattr '|Label| result)
                   :datapoints (collect-datapoints (find-element '|Datapoints| result)))))

(defun get-monitoring-state (instance)
  (getattr '|state| (getattrs '|monitoring| instance)))

(defun collect-monitored-instances (body)
  (collecting-element-children (instance (find-element '|instancesSet| body))
    (make-monitored-instance :id (getattr '|instanceId| instance)
                             :state (get-monitoring-state instance))))

(defun issue-request (specific-params &key (protocol :lxml) (parse-body t))
  (let* ((raw-params (make-parameters specific-params *api-version*))
         (params (append raw-params
                         `(("Signature" . ,(make-signature (canonicalize-params raw-params) *host-header*))))))
    (multiple-value-bind (body status headers uri stream must-close reason-phrase)
        (drakma:http-request *url* :parameters params)
      (declare (ignore must-close))
      (when (ec2-error-p status)
        (ec2:make-ec2-error :body body :status status :headers headers :uri uri :reason reason-phrase))
      (close stream)
      (if parse-body
          (parse-http-body body protocol)
        body))))

(defun make-dimensions-list (entities)
  (loop for (name . value) in entities
       for i from 1 to (length entities)
       nconc `((,(format nil "Dimensions.member.~A.Name" i) . ,name)
               (,(format nil "Dimensions.member.~A.Value" i) . ,value))))

(defun get-metric-statistics (measure-name start-time end-time &key (statistics '("Average")) (period 60) (namespace *default-cloudwatch-namespace*) (dimensions '()))
  (let ((params (append `(("Action" . "GetMetricStatistics") ("Period" . ,(tostr period)) ("Namespace" . ,namespace)
                          ("StartTime" . ,(iso-8601-timestamp start-time :local))
                          ("EndTime" . ,(iso-8601-timestamp end-time :local))
                          ("MeasureName" . ,measure-name))
                        (make-entity-list "Statistics.member" statistics)
                        (make-dimensions-list dimensions))))
    (make-metric (issue-request params))))

(defun collect-stored-dimensions (metric)
  (collecting-element-children (dim (getattrs '|Dimensions| metric))
    `(,(getattr '|Name| dim) . ,(getattr '|Value| dim))))

(defun collect-stored-metrics (body)
  (let ((stored-metrics '())
        (result (find-element '|ListMetricsResult| body)))
    (with-element-children (metrics result)
      (with-element-children (metric metrics)
        (push (make-instance 'stored-metric
                             :measure (getattr '|MeasureName| metric)
                             :namespace (getattr '|Namespace| metric)
                             :dimensions (collect-stored-dimensions metric))
              stored-metrics)))
    (nreverse stored-metrics)))

(defun list-metrics ()
  (let ((params '(("Action" . "ListMetrics"))))
    (collect-stored-metrics (issue-request params))))
