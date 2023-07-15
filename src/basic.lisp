(ql:quickload "uiop")
(ql:quickload "cl-who")
(ql:quickload "hunchentoot")

(eval-when (:compile-toplevel :execute :load-toplevel)
  (setf (cl-who:html-mode) :html5))

(defconstant +css-styling+
  "/* CSS styles for the form */
form {
    display: grid;
    gap: 10px;
}

.input-row {
    display: grid;
    grid-template-columns: 1fr 1fr;
    gap: 10px;
    align-items: center;
}

.input-row label {
    text-align: right;
}

/* CSS styles for responsive design */
@media screen and (max-width: 600px) {
    .input-row {
        grid-template-columns: 1fr;
    }
}

.input-row:last-child {
    grid-template-columns: 1fr;
}

.input-row:last-child input[type=\"submit\"] {
    grid-column: 1 / -1;
    width: 100%;
}
")

(hunchentoot:define-easy-handler (form-handler :uri "/cl-meter-readings/form") ()
  (cl-who:with-html-output-to-string (*standard-output* nil :prologue t)
    (:html
     (:head
      (:meta :charset "UTF-8")
      (:title "Input meter readings")
      (:link :rel "apple-touch-icon" :sizes "180x180" :href "/cl-meter-readings/apple-touch-icon.png")
      (:link :rel "icon" :type "image/png" :sizes "32x32" :href "/cl-meter-readings/favicon-32x32.png")
      (:link :rel "icon" :type "image/png" :sizes "16x16" :href "/cl-meter-readings/favicon-16x16.png")
      (:link :rel "manifest" :href "/cl-meter-readings/site.webmanifest")
      (:style (cl-who:str +css-styling+)))
    (:body
     (:form
      :action "/cl-meter-readings/submit" :method "POST"
      (labels ((input-row (name label placeholder &key value pattern)
                 (cl-who:htm
                  (:div
                   :class "input-row"
                   (:label :for name (cl-who:str label))
                   (:input :type "text" :id name :name name :placeholder placeholder :value value :pattern pattern))))
               (meter-reading (name label &key value)
                 (input-row name label "Empty field or positive number with at most one decimal" :value value :pattern "^ *(|\\d+([.,]\\d)?) *$")))
        (input-row "timestamp"
                   "Timestamp"
                   "dd/mm/yyyy HH:MM[:SS] (in your local timezone)"
                   :value (multiple-value-bind (_seconds minutes hours day month year)
	                      (get-decoded-time)
                            (declare (ignore _seconds))
	                    (format nil "~2,'0D/~2,'0D/~4,'0D ~2,'0D:~2,'0D" day month year hours minutes))
                   :pattern " *(3[01]|[0-2]?[0-9])/(1[0-2]|0?[0-9])/2[01][0-9][0-9] (2[0-3]|[01]?[0-9]):[0-5][0-9](:[0-5][0-9])? *")
        (meter-reading "pv_2022_prod_kWh" "PV 2022 production [kWh]" :value "1484.3")
        (meter-reading "pv_2012_prod_kWh" "PV 2012 production [kWh]")
        (meter-reading "peak_hour_consumption_kWh" "1.8.1 Peak hour consumption [kWh]")
        (meter-reading "off_hour_consumption_kWh" "1.8.2 Off hour consumption [kWh]")
        (meter-reading "peak_hour_injection_kWh" "2.8.1 Peak hour injection [kWh]")
        (meter-reading "off_hour_injection_kWh" "2.8.2 Off hour injection [kWh]")
        (meter-reading "gas_m3" "Gas [m³]")
        (meter-reading "water_m3" "Water [m³]"))
      (:div
       :class "input-row"
       (:input :type "submit" :value "submit" :value "Confirm readings")))))))


(defclass meter-readings-20220815 ()
  ((timestamp :initarg :timestamp :accessor reading-timestamp :documentation "Seconds since UNIX epoch [s]")
   (pv-2022-prod-kWh :initarg :pv-2022-prod-kWh :initform nil :accessor pv-2022-prod-kWh :documentation "PV 2022 production [kWh]")
   (pv-2012-prod-kWh :initarg :pv-2012-prod-kWh :initform nil :accessor pv-2012-prod-kWh :documentation "PV 2012 production [kWh]")
   (peak-hour-consumption-kWh :initarg :peak-hour-consumption-kWh :initform nil :accessor peak-hour-consumption-kWh :documentation "1.8.1 Peak hour consumption [kWh]")
   (off-hour-consumption-kWh :initarg :off-hour-consumption-kWh :initform nil :accessor off-hour-consumption-kWh :documentation "1.8.2 Off hour consumption [kWh]")
   (peak-hour-injection-kWh :initarg :peak-hour-injection-kWh :initform nil :accessor peak-hour-injection-kWh :documentation "2.8.1 Peak hour injection [kWh]")
   (off-hour-injection-kWh :initarg :off-hour-injection-kWh :initform nil :accessor off-hour-injection-kWh :documentation "2.8.2 Off hour injection [kWh]")
   (gas-m3 :initarg :gas-m3 :initform nil :accessor gas-m3 :documentation "Gas [m³]")
   (water-m3 :initarg :water-m3 :initform nil :accessor water-m3 :documentation "Water [m³]")))


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


(defconstant +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0))


(defun parse-user-timestamp (s &optional time-zone)
  "Parse S yyyy/mm/dd hh:mm[:ss] with optional TIME-ZONE into offset from Unix epoch"
  (setq s (string-right-trim '(#\space #\tab) s))
  (flet ((check-bounds (name lower-incl value upper-incl)
           (unless (and (integerp value) (<= lower-incl value upper-incl))
             (error "Bad ~A: ~A is not in [~A, ~A]" name value lower-incl upper-incl)))
         (check-separator (next expected)
           (unless (eql (char s next) expected)
             (error "Bad separator ~S at index ~A: expected ~S" (char s next) next expected))))
    (multiple-value-bind (day next)
        (parse-integer s :junk-allowed t)
      (check-bounds "day" 1 day 31)
      (check-separator next #\/)
      (multiple-value-bind (month next)
          (parse-integer s :start (1+ next) :junk-allowed t)
        (check-bounds "month" 1 month 12)
        (check-separator next #\/)
        (multiple-value-bind (year next)
            (parse-integer s :start (1+ next) :junk-allowed t)
          (check-bounds "year" 1970 year 2199)
          (unless (<= day (ecase month
                            ((1 3 5 7 8 10 12) 31)
                            ((4 6 9 11) 30)
                            (2 (if (or (plusp (mod year 4))
                                       (and (zerop (mod year 100)) (plusp (mod year 400))))
                                   28
                                   29))))
            (error "Bad date ~4'0D-~2,'0D-~2,'0D" year month day))
          (check-separator next #\space)
          (multiple-value-bind (hour next)
              (parse-integer s :start (1+ next) :junk-allowed t)
            (check-bounds "hour" 0 hour 23)
            (check-separator next #\:)
            (multiple-value-bind (minute next)
                (parse-integer s :start (1+ next) :junk-allowed t)
              (check-bounds "minute" 0 minute 59)
              (let ((second
                      (if (>= next (length s))
                          0
                          (progn
                            (check-separator next #\:)
                            (multiple-value-bind (second next)
                                (parse-integer s :start (1+ next) :junk-allowed t)
                              (check-bounds "second" 0 second 59)
                              (unless (>= next (length s))
                                (error "No data after second allowed at index ~A" next))
                              second)))))
                (- (encode-universal-time second minute hour day month year time-zone)
                   +unix-epoch+)))))))))


;; use drakma as HTTP client?

(defun handle-form-request (request)
  (let* ((timestamp (parse-user-timestamp (hunchentoot:parameter "timestamp" request)))
         (pv-2022-prod-kWh (parse-float (hunchentoot:parameter "pv_2022_prod_kWh" request)))
         (pv-2012-prod-kWh (parse-float (hunchentoot:parameter "pv_2012_prod_kWh" request)))
         (peak-hour-consumption-kWh (parse-float (hunchentoot:parameter "peak_hour_consumption_kWh" request)))
         (off-hour-consumption-kWh (parse-float (hunchentoot:parameter "off_hour_consumption_kWh" request)))
         (peak-hour-injection-kWh (parse-float (hunchentoot:parameter "peak_hour_injection_kWh" request)))
         (off-hour-injection-kWh (parse-float (hunchentoot:parameter "off_hour_injection_kWh" request)))
         (gas-m3 (parse-float (hunchentoot:parameter "gas_m3" request)))
         (water-m3 (parse-float (hunchentoot:parameter "water_m3" request)))
         (meter-reading (make-instance
                         'meter-readings-20220815
                         :timestamp timestamp
                         :pv-2022-prod-kWh pv-2022-prod-kWh
                         :pv-2012-prod-kWh pv-2012-prod-kWh
                         :peak-hour-consumption-kWh peak-hour-consumption-kWh
                         :off-hour-consumption-kWh off-hour-consumption-kWh
                         :peak-hour-injection-kWh peak-hour-injection-kWh
                         :off-hour-injection-kWh off-hour-injection-kWh
                         :gas-m3 gas-m3
                         :water-m3 water-m3)))
    ;; Process the reading (store it in a database, etc.)
    (cl-who:with-html-output-to-string (*standard-output*)
      (:head
       (:meta :charset "UTF-8")
       (:title "Input meter readings")
       (:link :rel "apple-touch-icon" :sizes "180x180" :href "/cl-meter-readings/apple-touch-icon.png")
       (:link :rel "icon" :type "image/png" :sizes "32x32" :href "/cl-meter-readings/favicon-32x32.png")
       (:link :rel "icon" :type "image/png" :sizes "16x16" :href "/cl-meter-readings/favicon-16x16.png")
       (:link :rel "manifest" :href "/cl-meter-readings/site.webmanifest")
       (:style (cl-who:str +css-styling+)))
      (:body
       (:h1 "Reading")
       (:p (cl-who:str
            (flet ((time-xsor (mr)
                     (multiple-value-bind (ss mm hh dy mo ye)
                         (decode-universal-time (+ (reading-timestamp mr) +unix-epoch+) -2)
                       (format nil "~2,'0D/~2,'0D/~D ~2,'0D:~2,'0D:~2,'0D" dy mo ye hh mm ss))))
              (format nil
                      "Received reading: ~a"
                      (mapcar (lambda (accessor) (funcall accessor meter-reading))
                              (list #'time-xsor #'pv-2022-prod-kWh #'pv-2012-prod-kWh
                                    #'peak-hour-consumption-kWh
                                    #'off-hour-consumption-kWh
                                    #'peak-hour-injection-kWh
                                    #'off-hour-injection-kWh
                                    #'gas-m3 #'water-m3))))))))))


(hunchentoot:define-easy-handler (submit-handler :uri "/cl-meter-readings/submit") ()
  (handle-form-request hunchentoot:*request*))


(defvar *static-assets-directory*
  (or (uiop:getenv "CL_METER_READINGS_STATIC_DIRECTORY")
      (let (parent-dir)
        (when (and *load-truename*
                   (setq parent-dir (butlast (pathname-directory *load-truename*))))
          (make-pathname :name nil
                         :type nil
                         :version nil
                         :directory (append parent-dir '("static"))
                         :defaults *load-truename*)))
      #P"static/"))

(defvar *acceptor* (make-instance 'hunchentoot:easy-acceptor
                                  :port 4242
                                  :document-root *static-assets-directory*))

(hunchentoot:start *acceptor*)
