;;; Code that offers an interface, of sorts, to the response returned by an EC2 request.

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

(in-package :ec2-parser)

(defun getattr-aux (tag element)
  (find-if (lambda (obj) (and (consp obj) (eq tag (first obj)))) element))

(defun getattr (tag element)
  (second (getattr-aux tag element)))

(defun getattrs (tag element)
  (rest (getattr-aux tag element)))

(defun find-element (tag doc)
  (rest (getattr-aux tag doc)))

#+ignore
(defun ensure-compound-element (element)
  (unless (and (consp element) (eq (first element) ec2:*compound-element-tag*))
    (error (make-condition 'element-usage-error :element element))))
    
(defmacro with-element-children ((var element) &body body)
  (let ((child (gensym))
        (elements (gensym)))
    `(let ((,elements ,element))
       (dolist (,child ,elements)
         (when (consp ,child)
           (let ((,var (rest ,child)))
             ,@body))))))

(defmacro collecting-element-children ((var element) &body body)
  (let ((elements (gensym))
        (collection (gensym)))
    `(let ((,collection (list))
           (,elements ,element))
       (with-element-children (,var ,elements)
         (push (progn ,@body) ,collection))
       (nreverse ,collection))))

;;; test code from here down
#|
(defvar *sample-doc*
  '((|DescribeInstancesResponse| :|xmlns|
     "http://ec2.amazonaws.com/doc/2009-04-04/")
    (|requestId| "2ea0f764-4f57-469d-925e-939683c0c3a4")
    (|reservationSet|
     (|item| (|reservationId| "r-c3f243ab") (|ownerId| "046403095304")
      (|groupSet| (|item| (|groupId| "default")))
      (|instancesSet|
       (|item| (|instanceId| "i-f9d78091") (|imageId| "ami-9c17f5f5")
               (|instanceState| (|code| "16") (|name| "running"))
               (|privateDnsName| "ip-10-212-111-223.ec2.internal")
               (|dnsName| "ec2-75-101-235-251.compute-1.amazonaws.com") |reason|
               (|keyName| "smbami-keypair") (|amiLaunchIndex| "0") |productCodes|
               (|instanceType| "m1.large") (|launchTime| "2009-12-17T18:47:32.000Z")
               (|placement| (|availabilityZone| "us-east-1d"))
               (|monitoring| (|state| "disabled"))))))))

(defun parser-test ()
  (let ((rset (find-element '|reservationSet| *sample-doc*)))
    (with-element-children (reservation rset)
      (format t "reservationId: ~A~%" (getattr '|reservationId| reservation))
      (with-element-children (groups (find-element '|groupSet| reservation))
        (format t "groupId: ~A~%" (getattr '|groupId| groups)))
      (with-element-children (instance (find-element '|instancesSet| reservation))
        (format t "instanceId: ~A~%" (getattr '|instanceId| instance))
        (format t "instanceState: ~A~%" (find-element '|instanceState| instance))
        (format t "privateDnsName: ~A~%" (getattr '|privateDnsName| instance))
        (format t "dnsName: ~A~%" (getattr '|dnsName| instance))
        (format t "keyName: ~A~%" (getattr '|keyName| instance))
        (format t "amiLaunchIndex: ~A~%" (getattr '|amiLaunchIndex| instance))
        (format t "instanceType: ~A~%" (getattr '|instanceType| instance))
        (format t "placement: ~A~%" (find-element '|placement| instance))
        nil))))
|#

