;;;; -*- Mode: Lisp -*-

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

(in-package :asdf)

(push :ec2-signature-version=2 *features*)
;;; (push :ec2-signature-version=1 *features*)

(defsystem :ec2
  :name "EC2"
  :author "David E. Young <dyoung@saffrontech.com>"
  :version "1"
  :maintainer "David E. Young <dyoung@saffrontech.com>"
  :license "MIT"
  :description "Common Lisp EC2 Package"
  :long-description "The beginnings of an EC2 interface for Common Lisp"
  :perform (load-op :after (op ec2)
                    (pushnew :ec2 cl:*features*))
  :components ((:file "package")
               (:file "compat" :depends-on ("package"))
               (:file "aws" :depends-on ("compat"))
               (:file "conditions" :depends-on ("package"))
               (:file "protocol-support" :depends-on ("package"))
               (:file "signing" :depends-on ("aws" "protocol-support"))
               #+:ec2-signature-version=2
               (:file "signature-version-two" :depends-on ("signing" "conditions" "aws"))
               #+:ec2-signature-version=1
               (:file "signature-version-one" :depends-on ("signing" "conditions" "aws"))
               (:file "parser" :depends-on ("conditions"))
               (:file "ami" :depends-on ("parser"))
               (:file "volume" :depends-on ("parser"))
               (:file "create-volume" :depends-on ("parser"))
               (:file "attach-volume" :depends-on ("parser"))
               (:file "block-device-map" :depends-on ("parser"))
               (:file "ami-instance" :depends-on ("block-device-map"))
               (:file "availability-zone" :depends-on ("parser"))
               (:file "console-output" :depends-on ("parser"))
               (:file "security-group" :depends-on ("parser"))
               (:file "cloudwatch" :depends-on ("aws" "signing" "parser"))
               (:file "snapshot" :depends-on ("parser"))
               (:file "initiated-instance" :depends-on ("parser"))
               (:file "key" :depends-on ("package"))
               (:file "ec2" :depends-on ("aws" "signing" "ami" "volume" "ami-instance" "availability-zone"
                                         "console-output" "security-group" "create-volume" "attach-volume"
                                         "initiated-instance" "cloudwatch" "snapshot" "conditions" "package"
                                         "key"))
               (:file "api" :depends-on ("ec2"))
               (:file "load-balancing" :depends-on ("api")))
  :depends-on (:s-xml :s-base64 :ironclad :drakma))
