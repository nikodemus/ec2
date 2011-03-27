
(in-package :cloudwatch)

(defparameter *cpu* '((|GetMetricStatisticsResponse| :|xmlns|
                       "http://monitoring.amazonaws.com/doc/2009-05-15/")
                      (|GetMetricStatisticsResult|
                       (|Datapoints|
                        (|member| (|Timestamp| "2010-02-04T15:00:00Z") (|Unit| "Percent")
                                  (|Minimum| "0.33") (|Maximum| "42.16") (|Samples| "21.0")
                                  (|Average| "4.019523809523808"))
                        (|member| (|Timestamp| "2010-02-04T17:00:00Z") (|Unit| "Percent")
                                  (|Minimum| "0.33") (|Maximum| "0.67") (|Samples| "60.0")
                                  (|Average| "0.43283333333333357"))
                        (|member| (|Timestamp| "2010-02-04T16:00:00Z") (|Unit| "Percent")
                                  (|Minimum| "0.33") (|Maximum| "54.83") (|Samples| "60.0")
                                  (|Average| "1.5139999999999998"))
                        (|member| (|Timestamp| "2010-02-04T18:00:00Z") (|Unit| "Percent")
                                  (|Minimum| "0.33") (|Maximum| "2.43") (|Samples| "50.0")
                                  (|Average| "0.4709999999999999")))
                       (|Label| "CPUUtilization"))
                      (|ResponseMetadata| (|RequestId| "48ea7e75-11be-11df-b919-75431137f2b3"))))

(defparameter *netin* '((|GetMetricStatisticsResponse| :|xmlns|
                         "http://monitoring.amazonaws.com/doc/2009-05-15/")
                        (|GetMetricStatisticsResult|
                         (|Datapoints|
                          (|member| (|Timestamp| "2010-02-04T18:00:00Z") (|Unit| "Bytes")
                                    (|Samples| "46.0") (|Average| "17.347826086956523"))
                          (|member| (|Timestamp| "2010-02-04T17:00:00Z") (|Unit| "Bytes")
                                    (|Samples| "60.0") (|Average| "10.1"))
                          (|member| (|Timestamp| "2010-02-04T16:00:00Z") (|Unit| "Bytes")
                                    (|Samples| "60.0") (|Average| "26.966666666666665"))
                          (|member| (|Timestamp| "2010-02-04T15:00:00Z") (|Unit| "Bytes")
                                    (|Samples| "21.0") (|Average| "983.3333333333334")))
                         (|Label| "NetworkIn"))
                        (|ResponseMetadata| (|RequestId| "c9e87560-11bd-11df-8fe2-e1d1b6d62e49"))))

(defparameter *netout* '((|GetMetricStatisticsResponse| :|xmlns|
                          "http://monitoring.amazonaws.com/doc/2009-05-15/")
                         (|GetMetricStatisticsResult|
                          (|Datapoints|
                           (|member| (|Timestamp| "2010-02-04T18:00:00Z") (|Unit| "Bytes")
                                     (|Minimum| "0.0") (|Maximum| "292.0") (|Samples| "49.0"))
                           (|member| (|Timestamp| "2010-02-04T17:00:00Z") (|Unit| "Bytes")
                                     (|Minimum| "0.0") (|Maximum| "292.0") (|Samples| "60.0"))
                           (|member| (|Timestamp| "2010-02-04T16:00:00Z") (|Unit| "Bytes")
                                     (|Minimum| "0.0") (|Maximum| "386.0") (|Samples| "60.0"))
                           (|member| (|Timestamp| "2010-02-04T15:00:00Z") (|Unit| "Bytes")
                                     (|Minimum| "0.0") (|Maximum| "14428.0") (|Samples| "21.0")))
                          (|Label| "NetworkOut"))
                         (|ResponseMetadata| (|RequestId| "2a736c0b-11be-11df-ac83-9b8e49f844b3"))))
