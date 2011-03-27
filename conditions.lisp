;;; Conditions that may be raised by the EC2 package.

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

(define-condition ec2-error (error)
  ((status :initarg :status
           :reader ec2-error-status)
   (headers :initarg :headers
            :reader ec2-error-headers)
   (uri :initarg :uri
        :reader ec2-error-uri)
   (body :initarg :body
         :reader ec2-error-body)
   (reason :initarg :reason
           :reader ec2-error-reason))
  (:report (lambda (condition strm)
             (format strm "Amazon EC2 Error: ~A:~%" (ec2-error-reason condition))
             (format strm "  status: ~A~%" (ec2-error-status condition))
             (format strm "  uri: ~A~%" (ec2-error-uri condition))
             (format strm "  headers: ~A~%" (ec2-error-headers condition))
             (format strm "  body: ~A~%" (ec2-error-body condition)))))

(define-condition element-usage-error (error)
  ((element :initarg :element
            :reader element-usage-error-element))
  (:report (lambda (condition strm)
             (format strm "Element usage error:~%")
             (format strm "  element: ~S~%" (element-usage-error-element condition)))))

(define-condition volume-size-error (error)
  ((requested-size :initarg :requested-size
                   :reader volume-requested-size))
  (:report (lambda (condition strm)
             (format strm "Volume size error: request was ~A, but must be between 1 and 1024" (volume-requested-size condition)))))

(define-condition insufficient-capacity-error (ec2-error)
  ()
  (:report (lambda (condition strm)
             (format strm "EC2: Insufficient capacity.~%")
             (format strm "  body: ~A~%" (ec2-error-body condition)))))

(defmacro make-ec2-error (&key body status headers uri reason)
  (let ((response-body (gensym)))
    `(let ((,response-body ,body))
       (if (search "Insufficient capacity" ,response-body)
           (error 'insufficient-capacity-error :body (parse-http-body ,response-body :lxml) :status ,status :headers ,headers :uri ,uri :reason ,reason)
         (error 'ec2-error :body (parse-http-body ,response-body :lxml) :status ,status :headers ,headers :uri ,uri :reason ,reason)))))
