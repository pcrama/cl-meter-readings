(in-package :cl-meter-readings)


(defun parse-float (s)
  (unless (or (null s) (string= s ""))
    (multiple-value-bind (integer-part index) (parse-integer s :junk-allowed t)
      (let ((separator (when (< index (length s)) (char s index))))
        (cond ((or (eql separator #\.) (eql separator #\,))
               (let ((fractional-part (parse-integer s :start (1+ index))))
                 (* (signum integer-part)
                    (+ (abs integer-part) (/ fractional-part
                                             (expt 10.0d0 (- (length s) index 1)))))))
              ((null separator) (coerce integer-part 'double-float))
              (t (error "Could not parse ~S" s)))))))


(defclass meter-reading-202303 ()
  ((timestamp :initarg :timestamp :accessor reading-timestamp :documentation "Seconds since UNIX epoch [s]")
   (pv-2012-prod-kWh :initarg :pv-2012-prod-kWh :initform nil :accessor pv-2012-prod-kWh :documentation "PV 2012 production [kWh]")
   (pv-2022-prod-kWh :initarg :pv-2022-prod-kWh :initform nil :accessor pv-2022-prod-kWh :documentation "PV 2022 production [kWh]")
   (peak-hour-consumption-kWh :initarg :peak-hour-consumption-kWh :initform nil :accessor peak-hour-consumption-kWh :documentation "1.8.1 Peak hour consumption [kWh]")
   (off-hour-consumption-kWh :initarg :off-hour-consumption-kWh :initform nil :accessor off-hour-consumption-kWh :documentation "1.8.2 Off hour consumption [kWh]")
   (peak-hour-injection-kWh :initarg :peak-hour-injection-kWh :initform nil :accessor peak-hour-injection-kWh :documentation "2.8.1 Peak hour injection [kWh]")
   (off-hour-injection-kWh :initarg :off-hour-injection-kWh :initform nil :accessor off-hour-injection-kWh :documentation "2.8.2 Off hour injection [kWh]")
   (gas-m3 :initarg :gas-m3 :initform nil :accessor gas-m3 :documentation "Gas [m³]")
   (water-m3 :initarg :water-m3 :initform nil :accessor water-m3 :documentation "Water [m³]")))


(defmethod print-object ((x meter-reading-202303) stream)
  (format stream "#<METER-READING-202303")
  (loop for slot-name in '(timestamp pv-2012-prod-kWh pv-2022-prod-kWh
                           peak-hour-consumption-kWh off-hour-consumption-kWh
                           peak-hour-injection-kWh off-hour-injection-kWh
                           gas-m3 water-m3)
        when (slot-boundp x slot-name) do
        (let ((value (slot-value x slot-name)))
          (when value
            (format stream " :~A ~S" slot-name value))))
  (write-string ">" stream))

(defun start-process-and-pipe-text (command text)
  ;; Start the external process
  (let ((process (uiop:launch-program command
                                      :input :stream
                                      :output :stream
                                      :error :stream)))
    (when process
      ;; Get the process's standard input stream
      (let ((process-input (uiop:process-info-input process)))
        ;; Write the text to the process's standard input
        (format process-input "~a~%" text)
        ;; Close the input stream to signal the end of input
        (close process-input))
      ;; Read and return the process's output
      (loop with process-output = (uiop:process-info-output process)
            for line = (read-line process-output nil nil)
            while line
            collect line into result
            finally
               (progn
                 (uiop:close-streams process)
                 ;; Wait for the process to complete
                 (let ((return-code (uiop:wait-process process)))
                   (unless (zerop return-code)
                     (error "Process returned ~A" return-code)))
                 (return result))))))


(defvar *sql-program*
  (or (uiop:getenv "CL_METER_READINGS_SQL_PROGRAM")
      "sqlite3 db.db"))


(defgeneric insert-meter-reading-in-sql-table (sql-program meter-reading))


(defmethod insert-meter-reading-in-sql-table
    ((sql-program string) (meter-reading meter-reading-202303))
  (start-process-and-pipe-text sql-program
                               (format nil
                                       "INSERT INTO data_202303 VALUES (~S~{, ~F~})"
                                       (reading-timestamp meter-reading)
                                       (mapcar (lambda (accessor)
                                                 (let ((value (funcall accessor meter-reading)))
                                                   (if (null value) "NULL" value)))
                                               '(pv-2012-prod-kWh
                                                 pv-2022-prod-kWh
                                                 peak-hour-consumption-kWh
                                                 off-hour-consumption-kWh
                                                 peak-hour-injection-kWh
                                                 off-hour-injection-kWh
                                                 gas-m3
                                                 water-m3)))))


(defun parse-sql-line-to-meter-reading-202303 (line)
  (flet ((parse-optional-float (x)
           (if (string= x "")
               nil
               (parse-float x))))
    (destructuring-bind
        (timestamp pv2012kWh pv2022kWh peak-hour-consumption-kWh off-hour-consumption-kWh peak-hour-injection-kWh off-hour-injection-kWh gas-m3 water-m3)
        (uiop:split-string line :separator "|")
      (make-instance 'meter-reading-202303
                     :timestamp (parse-integer timestamp)
                     :pv-2012-prod-kWh (parse-optional-float pv2012kWh)
                     :pv-2022-prod-kWh (parse-optional-float pv2022kWh)
                     :peak-hour-consumption-kWh (parse-optional-float peak-hour-consumption-kWh)
                     :off-hour-consumption-kWh (parse-optional-float off-hour-consumption-kWh)
                     :peak-hour-injection-kWh (parse-optional-float peak-hour-injection-kWh)
                     :off-hour-injection-kWh (parse-optional-float off-hour-injection-kWh)
                     :gas-m3 (parse-optional-float gas-m3)
                     :water-m3 (parse-optional-float water-m3)))))


(defun get-count-and-most-recent-reading ()
  (destructuring-bind (line)
      (start-process-and-pipe-text
       *sql-program*
       "SELECT MAX(timestamp), MAX(pv2012_kWh), MAX(pv2022_kWh), MAX(peak_conso_kWh), MAX(off_conso_kWh), MAX(peak_inj_kWh), MAX(off_inj_kWh), MAX(gas_m3), MAX(water_m3), COUNT(*) FROM data_202303;")
    (destructuring-bind (meter-reading-line count)
        (uiop:split-string line :max 2 :separator "|")
      (values (parse-integer count) (parse-sql-line-to-meter-reading-202303 meter-reading-line)))))


(defun save-reading (meter-reading)
  (insert-meter-reading-in-sql-table *sql-program* meter-reading))


(defun get-meter-reading-202303 ()
  (let ((data-lines
          (start-process-and-pipe-text
           *sql-program*
           "SELECT timestamp, pv2012_kWh, pv2022_kWh, peak_conso_kWh, off_conso_kWh, peak_inj_kWh, off_inj_kWh, gas_m3, water_m3 FROM data_202303;")))
    (mapcar #'parse-sql-line-to-meter-reading-202303 data-lines)))
