(in-package :cl-meter-readings)

(defun string-starts-with (s pfx &key (start1 0) (end1 (length s)) (start2 0) (end2 (length pfx)))
  (let ((prefix-length (- end2 start2)))
    (if (> prefix-length (- end1 start1))
	nil
	(string= s pfx :start1 start1 :end1 (+ start1 prefix-length) :start2 start2 :end2 end2))))

(defclass smart-meter-report ()
  ((timestamp :initarg :timestamp :accessor reading-timestamp :documentation "Seconds since UNIX epoch [s]")
   (peak-hour-consumption-kWh :initarg :peak-hour-consumption-kWh :initform nil :accessor peak-hour-consumption-kWh :documentation "1.8.1 Peak hour consumption [kWh]")
   (off-hour-consumption-kWh :initarg :off-hour-consumption-kWh :initform nil :accessor off-hour-consumption-kWh :documentation "1.8.2 Off hour consumption [kWh]")
   (peak-hour-injection-kWh :initarg :peak-hour-injection-kWh :initform nil :accessor peak-hour-injection-kWh :documentation "2.8.1 Peak hour injection [kWh]")
   (off-hour-injection-kWh :initarg :off-hour-injection-kWh :initform nil :accessor off-hour-injection-kWh :documentation "2.8.2 Off hour injection [kWh]")))

(defmethod print-object ((x smart-meter-report) stream)
  (format stream "#<SMART-METER-REPORT")
  (loop for slot-name in '(timestamp peak-hour-consumption-kWh
                           off-hour-consumption-kWh peak-hour-injection-kWh
                           off-hour-injection-kWh)
        when (slot-boundp x slot-name) do
        (let ((value (slot-value x slot-name)))
          (when value
            (format stream " :~A ~S" slot-name value))))
  (write-string ">" stream))

(defun consume-smart-meter-output (dev-ttyusb0)
  (let (data)
    (labels
        ((detect-timestamp (line)
           (let* ((prefix "0-0:1.0.0(")
                  (cursor (length prefix)))
             (when (string-starts-with line prefix :end2 cursor)
               (let ((year (+ 2000 (parse-integer line :start cursor :end (incf cursor 2))))
                     (month (parse-integer line :start cursor :end (incf cursor 2)))
                     (day (parse-integer line :start cursor :end (incf cursor 2)))
                     (hour (parse-integer line :start cursor :end (incf cursor 2)))
                     (minute (parse-integer line :start cursor :end (incf cursor 2)))
                     (second (parse-integer line :start cursor :end (incf cursor 2)))
                     ;; (summer-time-p (eql (char line cursor) #\S))
                     )
                 (setq data
                       (list :timestamp (- (encode-universal-time second minute hour day month year)
                                           +unix-epoch+)))))))
         (detect-value (line prefix cursor key)
           (when (and data
                      (string-starts-with line prefix :end2 cursor))
             (let* ((first-digit (position-if #'digit-char-p line :start cursor))
                    (dot (and first-digit (position #\. line :start first-digit)))
                    (star (and dot (position #\* line :start dot)))
                    (integer-part (and star
                                       (parse-integer line :start first-digit :end dot :junk-allowed t)))
                    (frac-part (and integer-part
                                    (parse-integer line :start (1+ dot) :end star :junk-allowed t))))
               (when frac-part
                 (setq data
                       (list* key (+ integer-part (/ frac-part (expt 10.0 (- star dot 1)))) data))))))
         (detect-peak-power-consumption (line)
           (detect-value line "1-0:1.8.1(" 10 :peak-hour-consumption-kWh))
         (detect-off-power-consumption (line)
           (detect-value line "1-0:1.8.2(" 10 :off-hour-consumption-kWh))
         (detect-peak-power-injection (line)
           (detect-value line "1-0:2.8.1(" 10 :peak-hour-injection-kWh))
         (detect-off-power-injection (line)
           (detect-value line "1-0:2.8.2(" 10 :off-hour-injection-kWh))
         (detect (line)
           (or (detect-timestamp line)
               (detect-peak-power-consumption line)
               (detect-off-power-consumption line)
               (detect-peak-power-injection line)
               (detect-off-power-injection line))))
      (with-open-file (stream dev-ttyusb0)
        (loop :for _ :below 50
              :for line := (read-line stream)
              :when (and (detect line)
                        (every (lambda (k) (getf data k))
                               '(:timestamp
                                 :peak-hour-consumption-kWh
                                 :off-hour-consumption-kWh 
                                 :peak-hour-injection-kWh
                                 :off-hour-injection-kWh)))
              :return (apply #'make-instance 'smart-meter-report data))))))
