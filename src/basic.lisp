(ql:quickload "uiop")
(ql:quickload "cl-who")
(ql:quickload "hunchentoot")
(ql:quickload "drakma")
(ql:quickload "cl-json")

(defpackage :cl-meter-readings
  (:use :cl))

(in-package :cl-meter-readings)

(defconstant +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0))

(defvar *version-comment*
  (or (ignore-errors
       (with-open-file (stream "version-comment.txt")
         (read-line stream nil "empty version-comment")))
      "dev"))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (setf (cl-who:html-mode) :html5))

(load "data")
(load "interpol")
(load "smart-meter")


(defmacro with-timed-logged-ignored-error ((info &body handler) &body body)
  (let ((start (gensym "start"))
        (err (gensym "error")))
    `(let ((,start (get-universal-time)))
       (handler-case (progn ,@body)
         (error (,err)
           (format *error-output* "~&~A: ~A after ~Ds~%" ,info ,err (- (get-universal-time) ,start))
           ,@handler)))))

(defvar *smart-meter* nil
  "Initialized from CL_METER_READINGS_SMART_METER")

(defvar *sma-inverter-host* nil
  "Initialized from CL_METER_READINGS_SMA_INVERTER_HOST")

(defvar *sma-inverter-path* nil
  "Initialized from CL_METER_READINGS_SMA_INVERTER_PATH")

(defun descend-json (json path)
  (if (null path)
      json
      (destructuring-bind (head &rest tail) path
        (if (functionp head)
            (descend-json (funcall head json) tail)
            (descend-json (cdr (assoc head json)) tail)))))

(defun get-sma-inverter-total-production ()
  "Return produced kWh (rounded to Wh) of SMA inverter"
  (when (and (stringp *sma-inverter-host*) (stringp *sma-inverter-path*))
    (multiple-value-bind (response status-code)
         ;; TODO: SSL
         (drakma:http-request (format nil "https://~A/~A" *sma-inverter-host* *sma-inverter-path*) :decode-content :utf-8)
       (when (= status-code 200)
         (let* ((json (json:decode-json-from-string (flexi-streams:octets-to-string response)))
                (total-production (descend-json json (list :result :0199-XXXXX-9-+BD+ :+6400-00260100+ :|1| #'first :val))))
           (when (numberp total-production)
             (/ (round total-production) 1000.0)))))))

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

.input-row label.with-auto-fill-enabled:hover {
    cursor: copy;
    color: blue;
}

/* CSS styles for responsive design */
@media screen and (max-width: 600px) {
    .input-row {
        grid-template-columns: 1fr;
    }
    .input-row label {
        font-size: 6mm;
        text-align: left;
    }
    .input-row input {
        font-size: 6mm;
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
      (:meta :name "viewport" :content "width=device-width,initial-scale=1")
      (:title "Input meter readings")
      (:link :rel "apple-touch-icon" :sizes "180x180" :href "/cl-meter-readings/apple-touch-icon.png")
      (:link :rel "icon" :type "image/png" :sizes "32x32" :href "/cl-meter-readings/favicon-32x32.png")
      (:link :rel "icon" :type "image/png" :sizes "16x16" :href "/cl-meter-readings/favicon-16x16.png")
      (:link :rel "manifest" :href "/cl-meter-readings/site.webmanifest")
      (:style (cl-who:str +css-styling+)))
     (:body
      (:form
       :action "/cl-meter-readings/submit" :method "POST"
       (labels ((input-row (name label placeholder &key value pattern auto-fill)
                  (cl-who:htm
                   (:div
                    :class "input-row"
                    (if auto-fill
                        (cl-who:htm
                         (:label :for name
                                 :class "with-auto-fill-enabled"
                                 :onclick (format nil
                                                  "{ let elt = document.getElementById(~S); elt.value = elt.value.trim() || ~S; }"
                                                  name
                                                  auto-fill)
                                 (cl-who:esc label)
                                 " →⬜"))
                        (cl-who:htm (:label :for name (cl-who:esc label))))
                    (:input :type "text" :id name :name name :placeholder placeholder :value value :pattern pattern))))
                (meter-reading (name label accessor &key value)
                  (let* ((base-help-text "Empty field or positive number")
                         (last-known-value (and *data-points* (funcall accessor (find-if accessor *data-points* :from-end t))))
                         (last-known-formatted (if (typep last-known-value 'double-float)
                                                   (let ((formatted (format nil "~A" last-known-value)))
                                                     (subseq formatted 0 (- (length formatted) 2)))
                                                   last-known-value))
                         (help-text
                           (if last-known-value
                               (format nil
                                       "~A (~A)"
                                       last-known-formatted
                                       base-help-text)
                               base-help-text)))
                    (input-row name label help-text
                               :value value
                               :pattern "^ *(|\\d+([.,]\\d+)?) *$"
                               :auto-fill last-known-formatted))))
         (input-row "timestamp"
                    "Timestamp"
                    "dd/mm/yyyy HH:MM[:SS] (in your local timezone)"
                    :value (multiple-value-bind (_seconds minutes hours day month year)
                               (get-decoded-time)
                             (declare (ignore _seconds))
                             (format nil "~2,'0D/~2,'0D/~4,'0D ~2,'0D:~2,'0D" day month year hours minutes))
                    :pattern " *(3[01]|[0-2]?[0-9])/(1[0-2]|0?[0-9])/2[01][0-9][0-9] (2[0-3]|[01]?[0-9]):[0-5][0-9](:[0-5][0-9])? *")
         (meter-reading "pv_2022_prod_kWh"
                        "PV 2022 production [kWh]"
                        #'pv-2022-prod-kWh
                        :value (with-timed-logged-ignored-error ("Getting SMA production data failed")
                                 (get-sma-inverter-total-production)))
         (meter-reading "pv_2012_prod_kWh" "PV 2012 production [kWh]" #'pv-2012-prod-kWh)
         (let ((smart-meter-reading (ignore-errors (consume-smart-meter-output *smart-meter*))))
           (meter-reading "peak_hour_consumption_kWh" "1.8.1 Peak hour consumption [kWh]" #'peak-hour-consumption-kWh
                          :value (ignore-errors (peak-hour-consumption-kWh smart-meter-reading)))
           (meter-reading "off_hour_consumption_kWh" "1.8.2 Off hour consumption [kWh]" #'off-hour-consumption-kWh
                          :value (ignore-errors (off-hour-consumption-kWh smart-meter-reading)))
           (meter-reading "peak_hour_injection_kWh" "2.8.1 Peak hour injection [kWh]" #'peak-hour-injection-kWh
                          :value (ignore-errors (peak-hour-injection-kWh smart-meter-reading)))
           (meter-reading "off_hour_injection_kWh" "2.8.2 Off hour injection [kWh]" #'off-hour-injection-kWh
                          :value (ignore-errors (off-hour-injection-kWh smart-meter-reading))))
         (meter-reading "gas_m3" "Gas [m³]" #'gas-m3)
         (meter-reading "water_m3" "Water [m³]" #'water-m3))
       (:div
        :class "input-row"
        (:input :type "submit" :value "submit" :value "Confirm readings")))
      (:hr)
      (:p "Version: " (cl-who:esc *version-comment*))))))


(defun parse-user-timestamp (s &optional time-zone)
  "Parse S dd/mm/yyyy hh:mm[:ss] with optional TIME-ZONE into offset from Unix epoch"
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
                         'meter-reading-202303
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
    (save-reading meter-reading))
  ;; view: generate response
  (hunchentoot:redirect "/cl-meter-readings/main"))


(hunchentoot:define-easy-handler (submit-handler :uri "/cl-meter-readings/submit") ()
  (handle-form-request hunchentoot:*request*))


(hunchentoot:define-easy-handler (main-page :uri "/cl-meter-readings/main")
    ((reading :init-form "pv-prod"))
  (with-timed-logged-ignored-error ("Loading database cache failed")
    (unless *data-points* (load-database-cache)))
  (cl-who:with-html-output-to-string (*standard-output* nil :prologue t)
    (:html
     (:head
      (:meta :charset "UTF-8")
      (:meta :name "viewport" :content "width=device-width,initial-scale=1")
      (:title "Input meter readings")
      (:link :rel "apple-touch-icon" :sizes "180x180" :href "/cl-meter-readings/apple-touch-icon.png")
      (:link :rel "icon" :type "image/png" :sizes "32x32" :href "/cl-meter-readings/favicon-32x32.png")
      (:link :rel "icon" :type "image/png" :sizes "16x16" :href "/cl-meter-readings/favicon-16x16.png")
      (:link :rel "manifest" :href "/cl-meter-readings/site.webmanifest")
      (when *data-points*
        (cl-who:htm
         (:script :src "/cl-meter-readings/chart.js")
         (:script :src "/cl-meter-readings/luxon.js")
         (:script :src "/cl-meter-readings/chartjs-adapter-luxon.js")))
      (:style (cl-who:str +css-styling+)))
     (:body
      (:h1 "Reading")
      (with-timed-logged-ignored-error ("Getting summary info from DB failed"
                                         (cl-who:htm "Unable to fetch data"))
        (multiple-value-bind (readings-count most-recent)
            (get-count-and-most-recent-reading)
          (if (zerop readings-count)
              (cl-who:htm (:p "No data yet"))
              (cl-who:htm (:p (cl-who:str
                               (flet ((time-xsor (mr)
                                        (multiple-value-bind (ss mm hh day mon yer)
                                            (decode-universal-time (+ (reading-timestamp mr) +unix-epoch+))
                                          (format nil "~2,'0D/~2,'0D/~D ~2,'0D:~2,'0D:~2,'0D" day mon yer hh mm ss))))
                                 (format nil
                                         "Most recent reading of ~A reading(s): ~A"
                                         readings-count
                                         (mapcar (lambda (accessor) (funcall accessor most-recent))
                                                 (list #'time-xsor
                                                       #'pv-2022-prod-kWh
                                                       #'pv-2012-prod-kWh
                                                       #'peak-hour-consumption-kWh
                                                       #'off-hour-consumption-kWh
                                                       #'peak-hour-injection-kWh
                                                       #'off-hour-injection-kWh
                                                       #'gas-m3
                                                       #'water-m3))))))))))
      (:p (:a :href "/cl-meter-readings/form" "Enter new meter readings") ".")
      (when *data-points*
        (let* ((graphs '(("pv-prod" pv-prod "Total PV production")
                         ("usage" usage "Electricity usage💡")
                         ("gas-m3" gas-m3 "Gas")
                         ("water-m3" water-m3 "Water")
                         ("pv-2022-prod-kWh" pv-2022-prod-kWh "PV production (panels placed in 2022)")
                         ("pv-2012-prod-kWh" pv-2012-prod-kWh "PV production (panels placed in 2012)")
                         ("consumption" consumption "Electricity consumption💰")
                         ("injection" injection "Electricity injection"))))
          (cl-who:htm
           (:div (:canvas :id "myChart"))
           (:script "const ctx = document.getElementById('myChart');
                     const config = {
                        type: 'line',
                        data: "
                    (cl-who:str (with-output-to-string (stream)
                                  (let ((xsor (or (cadar (member reading
                                                                 graphs
                                                                 :key #'car
                                                                 :test #'equal))
                                                  'pv-prod)))
                                    (write-chart-config *data-points* xsor stream))))
                    ", options: {
                            responsive: true,
                            interaction: {intersect: false, axis: 'x'},
                            plugins: {title: {display: true, text: 'Meter Readings'}},
                            scales: {x: {'type': 'time'}}}};
                    new Chart(ctx, config);")
           (:ul
            (loop :for (reading _ label) :in graphs :do
              (cl-who:htm (:li (:a :href (format nil "/cl-meter-readings/main?reading=~A" reading) (cl-who:esc label)))))))))
      (:p (:a :href "/cl-meter-readings/monthly" "See monthly consumption statistics") ".")
      (:hr)
      (:p "Version: " (cl-who:esc *version-comment*))))))

(hunchentoot:define-easy-handler (monthly-page :uri "/cl-meter-readings/monthly")
    ()
  (with-timed-logged-ignored-error ("Loading database cache failed")
    (unless *data-points* (load-database-cache)))
  (cl-who:with-html-output-to-string (*standard-output* nil :prologue t)
    (:html
     (:head
      (:meta :charset "UTF-8")
      (:meta :name "viewport" :content "width=device-width,initial-scale=1")
      (:title "Input meter readings")
      (:link :rel "apple-touch-icon" :sizes "180x180" :href "/cl-meter-readings/apple-touch-icon.png")
      (:link :rel "icon" :type "image/png" :sizes "32x32" :href "/cl-meter-readings/favicon-32x32.png")
      (:link :rel "icon" :type "image/png" :sizes "16x16" :href "/cl-meter-readings/favicon-16x16.png")
      (:link :rel "manifest" :href "/cl-meter-readings/site.webmanifest")
      (:style (cl-who:str +css-styling+)))
     (:body
      (if *data-points*
        (let* ((monthly-timestamps (monthly-timestamps (- (get-universal-time) +unix-epoch+ (* (+ 95 366) 86400))))
               (monthly-results (mapcar (lambda (xsor) (tabulate-interpolations monthly-timestamps *data-points* xsor))
                                        '(pv-2012-prod-kWh pv-2022-prod-kWh consumption injection gas-m3 water-m3 pv-prod usage))))
          (cl-who:htm
           (:h1 "Monthly consumption")
           (:table :style "border-collapse: collapse; border-bottom: solid 1px; border-top: solid 1px;"
            (:thead :style "padding-bottom: 0px; padding-top: 0px;"
             (:tr :style "border-bottom: solid 1.5px;"
              (:th :style "border-right: solid 1px; border-left: solid 1px;" "Date")
              (:th :style "border-right: solid 1px" :colspan 2 "PV 2012 [kWh]")
              (:th :style "border-right: solid 1px" :colspan 2 "PV 2022 [kWh]")
              (:th :style "border-right: solid 1px" :colspan 2 "Consumption [kWh]")
              (:th :style "border-right: solid 1px" :colspan 2 "Injection [kWh]")
              (:th :style "border-right: solid 1px" :colspan 2 "Gas [m³]")
              (:th :style "border-right: solid 1px" :colspan 2 "Water [m³]")
              (:th :style "border-right: solid 1px" :colspan 2 "PV prod [kWh]")
              (:th :style "border-right: solid 1px" :colspan 2 "Usage [kWh]")))
            (:tbody :style "text-align: right; padding-bottom: 0px; padding-top: 0px;"
             (loop :for ts :in (cdr monthly-timestamps)
                   :for (month . year) = (multiple-value-bind (_s _m _h _d mon yer)
                                             (decode-universal-time (+ ts +unix-epoch+) 0)
                                           (declare (ignore _s _m _h _d))
                                           (cons mon yer))
                   :for cursors = (mapcar #'cdr monthly-results) :then (mapcar #'cdr cursors)
                   :do
                      (cl-who:htm
                       (:tr
                        (:td :style "border-left: solid 1px; border-right: solid 1px;" (cl-who:str (format nil "01/~2,'0D/~D" month year)))
                        (loop :for (val . diff) :in (mapcar #'car cursors)
                              :do
                                 (cl-who:htm (:td (cl-who:str (if val (format nil "~,1F" val) "?")))
                                             (:td :style "border-right: solid 1px; border-left: dotted 1px grey;"
                                                  (cl-who:str (if diff (format nil "~,1F" diff) "?"))))))))))))
        (cl-who:htm (:p "No data available")))
      (:hr)
      (:p "Version: " (cl-who:esc *version-comment*))))))


(defvar *static-assets-directory* nil
  "Initialized from CL_METER_READINGS_STATIC_DIRECTORY")

(defvar *acceptor* nil
  "Hunchentoot acceptor instance, started by `main'.")


(defun main ()
  (setf *static-assets-directory*
        (or (uiop:getenv "CL_METER_READINGS_STATIC_DIRECTORY")
            (let (parent-dir)
              (when (and *load-truename*
                         (setq parent-dir (butlast (pathname-directory *load-truename*))))
                (make-pathname :name nil
                               :type nil
                               :version nil
                               :directory (append parent-dir '("static"))
                               :defaults *load-truename*)))
            #P"static/")
        *sma-inverter-host* (uiop:getenv "CL_METER_READINGS_SMA_INVERTER_HOST")
        *sma-inverter-path* (uiop:getenv "CL_METER_READINGS_SMA_INVERTER_PATH")
        *smart-meter* (or (uiop:getenv "CL_METER_READINGS_SMART_METER") "/dev/ttyUSB0")
        sb-ext:*debug-print-variable-alist* (list* '(*print-length* . 10)
                                                   '(*print-level* . 6)
                                                   '(*print-pretty* . nil)
                                                   sb-ext:*debug-print-variable-alist*)
        *sql-program* (or (uiop:getenv "CL_METER_READINGS_SQL_PROGRAM") "sqlite3 db.db"))
  (with-timed-logged-ignored-error ("Loading cache of DB at startup") (load-database-cache))
  (format t
          "~&*sma-inverter-host*=~S~
           ~&*sma-inverter-path*=~S~
           ~&*smart-meter*=~S~
           ~&*static-assets-directory*=~S~
           ~&*sql-program*=~S~
           ~&*version-comment*=~S~
           ~&~D data points~&"
          *sma-inverter-host*
          *sma-inverter-path*
          *smart-meter*
          *static-assets-directory*
          *sql-program*
          *version-comment*
          (length *data-points*))
  (hunchentoot:start (setf *acceptor*
                           (make-instance 'hunchentoot:easy-acceptor
                                          :address "127.0.0.1"
                                          :port 4242
                                          :document-root *static-assets-directory*))))

