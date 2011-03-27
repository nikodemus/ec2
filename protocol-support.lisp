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

(in-package :ec2-protocol)

(defgeneric parse-http-body (body protocol)
  (:method ((body string) protocol)
    (s-xml:parse-xml-string body :output-type protocol))
  (:method ((body vector) protocol)
    (parse-http-body (flexi-streams:octets-to-string body) protocol))
  (:method ((body t) protocol)
    body))

(defgeneric iso-8601-timestamp (source timezone)
  (:method ((source string) timezone)
    (declare (ignore timezone))
    source)
  (:method ((source number) (timezone (eql :local)))
    (multiple-value-bind (sec min hour date mon year dow dstp tz)
        (decode-universal-time source)
      (declare (ignore dow dstp))
      (format nil "~A-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D+~2,'0D:00" year mon date hour min sec tz)))
  (:method ((source number) (timezone (eql :zulu)))
    (multiple-value-bind (sec min hour date mon year dow dstp tz)
        (decode-universal-time source 0)
      (declare (ignore tz dow dstp))
      (format nil "~A-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ" year mon date hour min sec)))
  (:method ((source number) (timezone t))
    (iso-8601-timestamp source :zulu)))

(defun make-timestamp()
  (iso-8601-timestamp (get-universal-time) :zulu))

(defun ec2-error-p (status)
  (not (= status 200)))
