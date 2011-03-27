;;; The beginnings of a comptibility layer for EC2

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

(in-package :compat)

(define-condition not-implemented (error)
  ((proc :initarg :proc
         :initform nil
         :reader get-proc))
  (:report (lambda (condition strm)
             (format strm "Missing implementation: ~A" (get-proc condition)))))

;;; Borrowed from uffi-compat.lisp
;;; Copyright (C) 2005-2006, James Bielman  <jamesjb@jamesjb.com>
;;; Copyright (C) 2005-2007, Luis Oliveira  <loliveira@common-lisp.net>

(defun getenv-aux (var)
  #+allegro (sys::getenv (string var))
  #+clisp (sys::getenv (string var))
  #+lispworks (lw:environment-variable (string var))
  #+sbcl (sb-posix:getenv (string var))
  #-(or allegro clisp lispworks sbcl)
  (error 'not-implemented :proc (list 'getenv var)))

(defun getenv (var &optional (default nil))
  "Return the value of the environment variable."
  (let ((env (getenv-aux var)))
    (if env env default)))
