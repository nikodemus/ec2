;;; Package definitions for EC2

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

(defpackage "COMPAT"
  (:use "COMMON-LISP")
  (:export "GETENV"
           "NOT-IMPLEMENTED"
           "GET-PROC"))

(defpackage "AWS"
  (:use "COMMON-LISP")
  (:import-from "COMPAT"
                "GETENV")
  (:export "*ACCESS-KEY*"
           "*SECRET-KEY*"
           "*DEFAULT-ZONE*"
            "ACCESS-KEY"
            "SECRET-KEY"
            "DEFAULT-ZONE"))

(defpackage "EC2-PARSER"
  (:use "COMMON-LISP")
  (:export "GETATTR"
           "GETATTRS"
           "FIND-ELEMENT"
           "WITH-ELEMENT-CHILDREN"
           "COLLECTING-ELEMENT-CHILDREN"))

(defpackage "EC2-PROTOCOL"
  (:use "COMMON-LISP")
  (:export "MAKE-TIMESTAMP"
           "SORT-PARAMS"
           "MAKE-PARAMETERS"
           "MAKE-SIGNATURE"
           "CANONICALIZE-PARAMS"
           "ENCODE-USER-DATA"
           "EC2-ERROR-P"
           "ISO-8601-TIMESTAMP"
           "PARSE-HTTP-BODY"
           "MAKE-ENTITY-LIST"))

(defpackage "EC2"
  (:use "COMMON-LISP" "EC2-PARSER" "EC2-PROTOCOL")
  (:export "DESCRIBE-VOLUMES"
           "DESCRIBE-ZONES"
           "ISSUE-REQUEST"
           "AMI-INSTANCE"
           "GET-INSTANCE-ID"
           "GET-RESERVATION-ID"
           "GET-IMAGE-ID"
           "GET-GROUPS"
           "GET-STATE"
           "GET-PRIVATE-DNS"
           "GET-PUBLIC-DNS"
           "GET-PRIVATE-IP-ADDRESS"
           "GET-PUBLIC-IP-ADDRESS"
           "GET-INSTANCE-TYPE"
           "GET-LAUNCH-TIME"
           "GET-LAUNCH-INDEX"
           "GET-KEY-NAME"
           "GET-ZONE"
           "VOLUME"
           "GET-VOLUME-ID"
           "GET-SIZE"
           "GET-SNAPSHOT-ID"
           "GET-CREATION-TIME"
           "GET-ATTACHMENTS"
           "GET-STATUS"
           "AMI"
           "GET-MANIFEST"
           "GET-OWNER-ID"
           "GET-VISIBILITY"
           "GET-ARCHITECTURE"
           "GET-IMAGE-TYPE"
           "AVAILABILITY-ZONE"
           "GET-ZONE-NAME"
           "GET-REGION"
           "GET-CONSOLE-OUTPUT"
           "CREATE-VOLUME"
           "DELETE-VOLUME"
           "RUNNINGP"
           "RUN-INSTANCES"
           "START-INSTANCES"
           "START-INSTANCE"
           "STOP-INSTANCES"
           "STOP-INSTANCE"
           "WITH-HEADER-STREAM"
           "SECURITY-GROUP"
           "DESCRIBE-SECURITY-GROUPS"
           "IP-PROTOCOL"
           "GET-NAME"
           "GET-DESCRIPTION"
           "GET-OWNER-ID"
           "GET-IP-PERMISSIONS"
           "IP-PROTOCOL-PROTOCOL"
           "IP-PROTOCOL-FROM-PORT"
           "IP-PROTOCOL-TO-PORT"
           "IP-PROTOCOL-RANGES"
           "TERMINATEDP"
           "INITIATED-INSTANCE"
           "GET-REQUEST-ID"
           "GET-VIRTUAL-NAME"
           "CREATE-SNAPSHOT"
           "DELETE-SNAPSHOT"
           "DESCRIBE-SNAPSHOTS"
           "SNAPSHOT"
           "GET-SNAPSHOT-ID"
           "GET-PROGRESS"
           "GET-DEVICE-NAMES"
           "GET-DEVICE"
           "GET-DEVICE-NAME"
           "BLOCK-DEVICE"
           "BLOCK-DEVICE-MAP"
           "GET-BLOCK-DEVICE-MAP"
           "GET-ROOT-DEVICE-TYPE"
           "BLOCK-DEVICE-VOLUME-ID"
           "BLOCK-DEVICE-STATUS"
           "BLOCK-DEVICE-ATTACH-TIME"
           "BLOCK-DEVICE-TERMINATION-BEHAVIOR"
           "GET-VOLUMES"
           "ATTACH-VOLUME"
           "ATTACHED-VOLUME-RESPONSE"
           "ATTACHED-VOLUME-RESPONSE-VOLUME-ID"
           "ATTACHED-VOLUME-RESPONSE-INSTANCE-ID"
           "ATTACHED-VOLUME-RESPONSE-DEVICE"
           "ATTACHED-VOLUME-RESPONSE-STATUS"
           "ATTACHED-VOLUME-RESPONSE-ATTACH-TIME"
           "CREATED-VOLUME"
           "CREATED-VOLUME-ID"
           "CREATED-VOLUME-SIZE"
           "CREATED-VOLUME-ZONE"
           "CREATED-VOLUME-STATUS"
           "CREATED-VOLUME-CREATE-TIME"
           "MONITOR-INSTANCES"
           "UNMONITOR-INSTANCES"
           "EC2-ERROR"
           "INSUFFICIENT-CAPACITY-ERROR"
           "EC2-ERROR-STATUS"
           "EC2-ERROR-HEADERS"
           "EC2-ERROR-URI"
           "EC2-ERROR-REASON"
           "MAKE-EC2-ERROR"
           "VOLUME-ATTACHED-P"
           "VOLUME-AVAILABLE-P"
           "CL-EC2-VERSION"
           "GET-MONITORING-STATE"
           "PENDINGP"
           ;; Keypairs
           "CREATE-KEYPAIR"
           "DELETE-KEYPAIR"
           "DESCRIBE-KEYPAIRS"
           "GET-FINGERPRINT"
           "GET-MATERIAL"
           ;; Images
           "CREATE-IMAGE"
           "DESCRIBE-IMAGE"
           "DESCRIBE-IMAGES"
           ;; Instances
           "DESCRIBE-INSTANCES"
           "DESCRIBE-INSTANCE"
           "TERMINATE-INSTANCES"
           "RUN-INSTANCES"
           ))

(defpackage "CLOUDWATCH"
  (:use "COMMON-LISP" "EC2-PARSER" "EC2-PROTOCOL")
  (:nicknames "CW")
  (:export "MONITORED-INSTANCE"
           "MONITORED-INSTANCE-ID"
           "MONITORED-INSTANCE-STATE"
           "COLLECT-MONITORED-INSTANCES"
           "GET-METRIC-STATISTICS"
           "MAKE-MONITORED-INSTANCE"
           "DATAPOINT"
           "GET-SAMPLES"
           "GET-AVERAGE"
           "GET-MAXIMUM"
           "GET-MINIMUM"
           "GET-AVERAGE"
           "GET-SUM"
           "GET-UNIT"
           "GET-TIMESTAMP"
           "METRIC"
           "GET-LABEL"
           "GET-DATAPOINTS"
           "STORED-METRIC"
           "GET-MEASURE"
           "GET-NAMESPACE"
           "GET-DIMENSIONS"
           "LIST-METRICS"))

(defpackage "EC2-USER"
  (:use "COMMON-LISP" "EC2" "CLOUDWATCH")
  (:documentation "A convenience package for users of EC2"))
