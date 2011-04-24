;;;  Copyright (c) 2011 Nikodemus Siivola <nikodemus@sb-studio.net>

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

(in-package :elastic-load-balancing)

(defparameter *elb-api-version* "2009-05-15")
(defparameter *elb-host-header* "elasticloadbalancing.amazonaws.com")
(defparameter *elb-xmlns*
  (format nil "http://elasticloadbalancing.amazonaws.com/doc/~A/"
          *elb-api-version*))
(defparameter *elb-namespace*
  (s-xml:register-namespace *elb-xmlns* "ELB" :elastic-load-balancing))
(defparameter *elb-url* (concatenate 'string "http://" *elb-host-header*))

(defun elb-request (params)
  (let ((ec2::*api-version* *elb-api-version*)
        (ec2::*host-header* *elb-host-header*)
        (ec2::*url* *elb-url*))
    (ec2::issue-request params)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun string-camelcase (string)
    (with-output-to-string (s)
      (let* ((string (string string))
             (length (length string)))
        (loop with start = 0
              while (and start (< start length))
              for p = (position #\- string :start start)
              for slice = (subseq string start p)
              do (write-string (string-capitalize slice) s)
                 (setf start (when p (1+ p)))))))
  (defun make-keyword (string)
    (intern (string string) :keyword))
  (defun expand-parser (spec result)
    (if (consp spec)
      (flet ((getter (thing type)
               (ecase type
                 (:element `(find-element ',thing ,result))
                 (:attribute `(getattr ',thing ,result)))))
        (destructuring-bind (action &rest more) spec
         (ecase action
           (:collect
             (let ((kid (gensym "KID"))
                   (type (pop more))
                   (thing (pop more)))
               `(collecting-element-children (,kid ,(getter thing type))
                  ,(expand-parser more kid))))
           ((:element :attribute)
              (let ((kid (gensym "KID"))
                    (thing (pop more)))
                `(let ((,kid ,(getter thing action)))
                   ,(expand-parser more kid))))
           (:plist
              (let ((plist (pop more)))
                (assert (not more))
                `(list ,@(loop while plist
                               for key = (pop plist)
                               for thing = (pop plist)
                               append (list key (expand-parser thing result))))))
           (:list
              (let ((list (pop more)))
                (assert (not more))
                `(list ,@(loop for elt in list
                               collect (expand-parser elt result)))))
           (:eval
              (let ((thing (pop more)))
                (assert (not more))
                thing))
           (:values
             (let ((values (pop more)))
               (assert (not more))
               `(values ,@(loop for value in values
                                collect (expand-parser value result)))))
           (:translate
              (let ((translations (pop more))
                    (value (pop more))
                    (tmp (gensym "TMP"))
                    (cell (gensym "CELL")))
                `(let* ((,tmp ,(expand-parser value result))
                        (,cell (assoc ,tmp ,translations :test #'equal)))
                   (cond (,cell
                          (cdr ,cell))
                         (t
                          (with-simple-restart
                              (continue "Return ~S without translation."
                                        ,tmp)
                            (error "No translation for ~S in ~S."
                                   ,tmp ',translations))
                          ,tmp)))))
           (:atom
              `(destructuring-bind (atom) ,result
                 atom)))))
      (if spec
        `(getattr ',spec ,result)
        result))))

(defun ensure-list (x)
  (if (listp x)
    x
    (list x)))

(defun make-parameter (name value type)
  (ecase type
    (string
       (list (cons name (the string value))))
    ((load-balancer-names availability-zones)
       (make-member-list name (ensure-list value)))
    (instances
       (make-member-list name (ensure-list value) "InstanceId"))
    (listeners
       (make-listener-list name value))))

(defun make-member-list (tag members &optional type)
  (loop for member in members
        for i from 1
        append (if (consp member)
                   (loop for part in member
                         collect (cons (format nil "~A.member.~A.~A" tag i (car part))
                                       (princ-to-string (cdr part))))
                   (list (cons (format nil "~A.member.~A~@[.~A~]" tag i type)
                               (princ-to-string member))))))

(defun make-listener-list (name listeners)
  (unless (consp (car listeners))
    (setf listeners (list listeners)))
  (make-member-list
   name
   (mapcar
    (lambda (listener)
      (destructuring-bind (protocol load-balancer-port instance-port) listener
        (list (cons "Protocol" protocol)
              (cons "LoadBalancerPort" load-balancer-port)
              (cons "InstancePort" instance-port))))
    (if (consp (car listeners))
      listeners
      (list listeners)))))

(defun missing-parameter (param action)
  (error "Required parameter ~S missing from call to ~S"
         param action))

(defmacro defaction (action parameters &optional parser)
  (let (required-parameters optional-parameters keyword-parameters aggregates)
    (dolist (spec parameters)
      (destructuring-bind (name type
                           &key
                           optional
                           required
                           required-key
                           required-optional
                           required-aggregate) spec
        (declare (ignore type))
        (cond (required
               (push name required-parameters))
              (required-aggregate
               (let (parts)
                 (dolist (part required-aggregate)
                   (destructuring-bind (var type &key default) part
                     (declare (ignore type))
                     (push `(cons ,(format nil "~A.~A"
                                           (string-camelcase name)
                                           (string-camelcase var))
                                  (princ-to-string
                                   (or ,var
                                       (missing-parameter
                                        ,(make-keyword var)
                                        ',action))))
                           parts)
                     (if default
                       (push `(,var ,default) keyword-parameters)
                       (push var keyword-parameters))))
                 (push `(,name (list ,@parts)) aggregates)))
              (required-key
               (push `(,name ,required-key) keyword-parameters))
              (optional
               (push name optional-parameters))
              (required-optional
               (push `(,name ,required-optional) optional-parameters))
              (t
               (push name keyword-parameters)))))
    (when (and optional-parameters keyword-parameters)
      (error "Both &OPTIONAL and &KEY? Think again."))
    `(defun ,action (,@(reverse required-parameters)
                     ,@(when optional-parameters '(&optional))
                     ,@(reverse optional-parameters)
                     ,@(when keyword-parameters '(&key))
                     ,@keyword-parameters)
       (let ,aggregates
         (let ((result (elb-request
                        (cons '("Action" . ,(string-camelcase action))
                              (append
                               ,@(mapcar
                                  (lambda (spec)
                                    (destructuring-bind (name type
                                                         &key
                                                         required
                                                         optional
                                                         required-optional
                                                         required-aggregate
                                                         required-key)
                                        spec
                                      (if required-aggregate
                                        name
                                        (let ((parameter-form
                                               `(make-parameter
                                                 ,(string-camelcase name)
                                                 ,name
                                                 ',type)))
                                          (cond
                                            (required
                                             parameter-form)
                                            ((or required-key required-optional)
                                             `(if ,name
                                                ,parameter-form
                                                (missing-parameter ,(if required-key
                                                                      (make-keyword name)
                                                                      name)
                                                                   ',action)))
                                            (t
                                             `(when ,name ,parameter-form)))))))
                                  parameters))))))
           (declare (ignorable result))
           ,(expand-parser parser 'result))))))

;;;; API

(defaction configure-health-check
    ((load-balancer-name string :required t)
     (health-check
      health-check
      :required-aggregate
      ((healthy-threshold healthy-threshold :default 2)
       (interval health-check-interval :default 30)
       (target string)
       (timeout health-check-timeout :default 5)
       (unhealthy-threshold unhealthy-threshold :default 5))))
  (:attribute |ConfigureHealthCheckResult|
              :plist (:interval |Interval|
                      :target |Target|
                      :healthy-threshold |HealthyThreshold|
                      :timeout |Timeout|
                      :unhealthy-threshold |UnhealthyThreshold|)))

(defaction create-load-balancer
    ((load-balancer-name string :required t)
     (listeners listeners :required t)
     (availability-zones availability-zones
                         :required-key (list (aws:default-zone))))
  (:element |CreateLoadBalancerResult|
            :values ((:eval load-balancer-name) |DNSName|)))

(defaction delete-load-balancer
    ((load-balancer-name string :required t))
  (:values))

(defaction deregister-instances-from-load-balancer
    ((load-balancer-name string :required t)
     (instances instances :required t))
  (:values))

(defaction describe-load-balancers
    ((load-balancers load-balancer-names :optional t))
  (:collect :attribute |DescribeLoadBalancersResult|
            :plist (:name |LoadBalancerName|
                    :dns |DNSName|
                    :listeners (:collect :element |Listeners|
                                         :list (|Protocol|
                                                |LoadBalancerPort|
                                                |InstancePort|))
                    :instances (:collect :element |Instances|
                                         :attribute |InstanceId|))))

(defparameter *instance-health-states*
  '(("OutOfService" . :out-of-service)
    ("InService" . :in-service)))

(defaction describe-instance-health
    ((load-balancer-name string :required t)
     (instances instances :optional t))
  (:element |DescribeInstanceHealthResult|
            :collect :element |InstanceStates|
            :plist (:instance |InstanceId|
                    :state (:translate *instance-health-states* |State|)
                    :reason |ReasonCode|
                    :description |Description|)))

(defaction disable-availability-zones-for-load-balancer
    ((load-balancer-name string :required t)
     (availability-zones
      availability-zones
      :required-optional (aws:default-zone)))
  (:element |DisableAvailabilityZonesForLoadBalancerResult|
            :collect :element |AvailabilityZones|
            :atom))

(defaction enable-availability-zones-for-load-balancer
    ((load-balancer-name string :required t)
     (availability-zones
      availability-zones
      :required-optional (aws:default-zone)))
  (:element |EnableAvailabilityZonesForLoadBalancerResult|
            :collect :element |AvailabilityZones|
            :atom))

(defaction register-instances-with-load-balancer
    ((load-balancer-name string :required t)
     (instances instances :required t))
  (:collect :attribute |RegisterInstancesWithLoadBalancerResult|
            :attribute |InstanceId|))
