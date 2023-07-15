(defmacro with-data-file ((stream &rest rest &key (direction :input) if-exists if-does-not-exist) &body body)
  (declare (ignorable direction if-exists if-does-not-exist))
  `(with-open-file (,stream
                    (or (uiop:getenv "CL_METER_READINGS_DATA_FILE")
                        "meter-readings.data")
                    ,@rest)
     ,@body))


(defun get-count-and-most-recent-reading ()
  (with-data-file (stream :direction :input :if-does-not-exist :create)
    (loop with most-recent = nil
          for count from 0
          for reading = (read-meter-reading-from-stream stream)
          unless reading
            return (if most-recent
                       (values count most-recent)
                       (values 0 nil))
          when (or (null most-recent)
                   (>= (reading-timestamp reading) (reading-timestamp most-recent)))
            do (setf most-recent reading))))


(defun read-meter-reading-from-stream (stream)
  (let* ((stream-data (ignore-errors (read stream)))
         (type (getf stream-data :type))
         (data (getf stream-data :data))
         (timestamp (getf data :timestamp)))
    (unless (or (null type) (null data) (null timestamp))
      (case type
        (:meter-readings-20220815
         (apply #'make-instance 'meter-readings-20220815 data))))))


(defun save-reading-to-stream (stream meter-reading)
  (format stream
          "(:type ~S :data (:timestamp ~S"
          :meter-readings-20220815
          (reading-timestamp meter-reading))
  (dolist (accessor '(pv-2022-prod-kWh
                      pv-2012-prod-kWh
                      peak-hour-consumption-kWh
                      off-hour-consumption-kWh
                      peak-hour-injection-kWh
                      off-hour-injection-kWh
                      gas-m3
                      water-m3))
    (let ((value (funcall accessor meter-reading)))
      (unless (null value)
        (format stream " :~A ~S" (symbol-name accessor) value))))
  (format stream "))~&")
  meter-reading)


(defun save-reading (meter-reading)
  (with-data-file (stream
                   :direction :output
                   :if-exists :append
                   :if-does-not-exist :create)
    (save-reading-to-stream stream meter-reading)))
