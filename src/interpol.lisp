(in-package :cl-meter-readings)

(defun interpolate-meter-readings (data-points x xsor &key (start 0) end)
  (loop :with end = (or end (length data-points))
        :with prev-idx
        :with prev-x
        :with prev-y
        :with x0
        :for idx :from start :below end
        :for elt = (aref data-points idx)
        :for y0 = (funcall xsor elt)
        :when y0
          :do (setf x0 (reading-timestamp elt))
          :and :if (= x0 x) :return (values y0 idx end)
                 :else :if (< x0 x) do (setf prev-x x0 prev-y y0 prev-idx idx)
                         :else :if (null prev-x) :do (error "First usable index ~S with abscissa ~S > ~S" idx x0 x)
                                 :else :return (values (+ prev-y (/ (* 1d0 (- x prev-x) (- y0 prev-y))
                                                                    (- x0 prev-x 0d0)))
                                                       prev-idx
                                                       end)
        :finally (error "Last usable index ~S with abscissa ~S but not enough info to interpolate for ~S"
                        idx
                        (reading-timestamp elt)
                        x)))

(defun make-chart-data-raw (data-points xsor scale &key (step 7200) (period (* 30 86400)))
  "Resample DATA-POINTS (XSOR readings) and SCALE them to a consumption rate

The &key STEP and PERIOD are expressed in seconds

Returns (CL:VALUES number-of-10s-since-unix-epoch
                   xsor-values
                   first-xsor-value
                   last-xsor-value)

Useful scale values:
| xsor             |        scale | resulting unit |
| gas-m3           |        86400 | m3/day         |
| water-m3         | 1000 * 86400 | l/day          |
| pv-2012-prod-kWh |         3600 | kW             |"
  (loop :with start = (position-if xsor data-points)
        :with last-idx
        :with previous-value
        :with last-ts
        :with first-ts
        :with first-value
        :with last-value
        :with end
          :initially
             (unless start
               (error "No data to plot"))
             (setf last-idx (position-if xsor data-points :from-end t))
             (unless (and last-idx (< start last-idx))
               (error "No data to plot"))
             (let ((last (aref data-points last-idx)))
               (setf last-value (funcall xsor last)
                     last-ts (reading-timestamp last)))
             (setf end (1+ last-idx))
             (let ((first (aref data-points start))
                   (first-ts-if-enough-data (- last-ts period)))
               (setf first-ts (reading-timestamp first))
               (if (< first-ts-if-enough-data first-ts)
                   ;; Not enough data
                   (setf first-value (funcall xsor first))
                   ;; enough data
                   (multiple-value-bind (value next-start _)
                       (interpolate-meter-readings data-points first-ts-if-enough-data xsor :start start :end end)
                     (declare (ignore _))
                     (setf start next-start
                           first-value value
                           first-ts first-ts-if-enough-data))))
             (unless (< first-ts last-ts)
               (error "No data to plot"))
             (setf previous-value first-value)
        :for previous-ts = first-ts :then ts
        :for ts = (+ first-ts step) :then (min (+ ts step) last-ts)
        :for (abscissa . ordinate) = (multiple-value-bind (value next-start _)
                                         (interpolate-meter-readings data-points ts xsor :start start :end end)
                                       (declare (ignore _))
                                       (prog1
                                           (cons ts (/ (- value previous-value 0.0d0) (- ts previous-ts 0.0d0)))
                                         (setf start next-start
                                               previous-value value)))
        :collect (round abscissa 10) into timestamps
        :collect (coerce (* ordinate scale) 'single-float) into values
        :while (< ts last-ts)
        :finally (return (values (cons (round first-ts 10) timestamps) (cons (car values) values) first-value last-value))))


(defun make-multi-accessor (data-points fun &rest inputs)
  (let ((accessors (mapcar (lambda (xsor)
                             (let (start end)
                               (lambda (meter-reading)
                                 (or (funcall xsor meter-reading)
                                     (let ((timestamp (reading-timestamp meter-reading)))
                                       (multiple-value-bind (result new-start new-end)
                                           (interpolate-meter-readings data-points timestamp xsor
                                                                       :start (or start 0) :end end)
                                         (setq start new-start end new-end)
                                         result))))))
                           inputs)))
    (lambda (meter-reading)
      (ignore-errors
       (apply fun (mapcar (lambda (xsor) (funcall xsor meter-reading)) accessors))))))


(defun test-multi-accessor ()
  (let* ((data-points (make-array
                       6
                       :initial-contents
                       (list (make-instance 'meter-reading-202303 :timestamp 0 :pv-2012-prod-kWh 1d0 :pv-2022-prod-kWh 2d0)
                             (make-instance 'meter-reading-202303 :timestamp 1000 :pv-2012-prod-kWh 3d0 :pv-2022-prod-kWh 4d0)
                             (make-instance 'meter-reading-202303 :timestamp 2000 :pv-2012-prod-kWh nil :pv-2022-prod-kWh 5d0)
                             (make-instance 'meter-reading-202303 :timestamp 3000 :pv-2012-prod-kWh 7d0 :pv-2022-prod-kWh nil)
                             (make-instance 'meter-reading-202303 :timestamp 4000 :pv-2012-prod-kWh 8d0 :pv-2022-prod-kWh 9d0)
                             (make-instance 'meter-reading-202303 :timestamp 5000 :pv-2012-prod-kWh 10d0 :pv-2022-prod-kWh nil))))
         (fun (make-multi-accessor data-points #'+ #'pv-2012-prod-kWh #'pv-2022-prod-kWh)))
    (assert (equal (funcall fun (make-instance 'meter-reading-202303 :timestamp 0)) 3d0))
    (assert (equal (funcall fun (make-instance 'meter-reading-202303 :timestamp 500)) 5d0))
    (assert (equal (funcall fun (make-instance 'meter-reading-202303 :timestamp 1000)) 7d0))
    (assert (equal (funcall fun (make-instance 'meter-reading-202303 :timestamp 1500)) 8.5d0))
    (assert (equal (funcall fun (make-instance 'meter-reading-202303 :timestamp 2000)) 10d0))
    (assert (equal (funcall fun (make-instance 'meter-reading-202303 :timestamp 2250)) 11d0))
    (assert (equal (funcall fun (make-instance 'meter-reading-202303 :timestamp 3000)) 14d0))
    (assert (equal (funcall fun (make-instance 'meter-reading-202303 :timestamp 4000)) 17d0))
    (assert (equal (funcall fun (make-instance 'meter-reading-202303 :timestamp 5000)) nil))))

(defun get-xsor (xsor data-points)
  (ecase xsor
    (gas-m3 (values "Gas [m³/day] ~,1Fm³ → ~Fm³" #'gas-m3 86400))
    (water-m3 (values "Water [l/day] ~,1Fm³ → ~Fm³" #'water-m3 86400e3))
    (pv-2022-prod-kWh (values "PV 2022 [kW] ~,1FkWh → ~FkWh" #'pv-2022-prod-kWh 3600))
    (pv-2012-prod-kWh (values "PV 2012 [kW] ~,1FkWh → ~FkWh" #'pv-2012-prod-kWh 3600))
    (pv-prod (values "PV [kW] ~,1FkWh → ~,1FkWh"
                     (make-multi-accessor data-points #'+ #'pv-2022-prod-kWh #'pv-2012-prod-kWh)
                     3600))
    (usage (values "Usage💡 [kW] ~,1FkWh → ~,1FkWh"
                   (make-multi-accessor data-points
                                        (lambda (pv-2022 pv-2012 peak-conso off-conso peak-inj off-inj)
                                          (- (+ pv-2022 pv-2012 peak-conso off-conso)
                                             (+ peak-inj off-inj)))
                                        #'pv-2022-prod-kWh #'pv-2012-prod-kWh
                                        #'peak-hour-consumption-kWh #'off-hour-consumption-kWh
                                        #'peak-hour-injection-kWh #'off-hour-injection-kWh)
                   3600))
    (consumption (values "Consumption💰 [kW] ~,1FkWh → ~,1FkWh"
                         (make-multi-accessor data-points #'+ #'peak-hour-consumption-kWh #'off-hour-consumption-kWh)
                         3600))
    (injection (values "Injection [kW] ~,1FkWh → ~,1FkWh"
                       (make-multi-accessor data-points #'+ #'peak-hour-injection-kWh #'off-hour-injection-kWh)
                       3600))
    (peak-hour-injection-kWh (values "Peak Hour Injection [kW] ~,1FkWh → ~FkWh"
                                     #'peak-hour-injection-kWh
                                     3600))
    (off-hour-injection-kWh (values "Off Hour Injection [kW] ~,1FkWh → ~FkWh"
                                    #'off-hour-injection-kWh
                                    3600))))

(defun write-chart-config (data-points xsor &optional (stream *standard-output*))
  (multiple-value-bind (format-string xsor-fun scale)
      (get-xsor xsor data-points)
    (multiple-value-bind (timestamps-10s values first-value last-value)
        (make-chart-data-raw data-points xsor-fun scale)
      (write-string "{labels:[" stream)
      (format stream "~{~Ae4,~}" timestamps-10s) ; from 10s steps to ms
      (write-string "],datasets:[{label: '" stream)
      (format stream format-string first-value last-value)
      (write-string "',data:[" stream)
      (format stream "~{~F,~}" values)
      (write-string "],borderWidth:1,filled:false,stepped:'after',borderColor:'blue'}]}" stream)
      (values))))

(defun test-interpolate-meter-readings ()
  (let ((data-points
          (make-array 4
                      :initial-contents
                      (list
                       (make-instance 'meter-reading-202303 :timestamp 1698230488 :pv-2022-prod-kWh 1d3)
                       (make-instance 'meter-reading-202303 :timestamp 1698231488 :pv-2012-prod-kWh 1d3)
                       (make-instance 'meter-reading-202303 :timestamp 1698232488 :pv-2022-prod-kWh 2d3 :pv-2012-prod-kWh 3d3 :water-m3 0d0)
                       (make-instance 'meter-reading-202303 :timestamp 1698233488 :pv-2022-prod-kWh 3d3 :pv-2012-prod-kWh 4d3)))))
    (assert (equal (multiple-value-list (interpolate-meter-readings data-points 1698230488 'pv-2022-prod-kWh))
                   (list 1d3 0 4)))
    (assert (equal (multiple-value-list (interpolate-meter-readings data-points 1698230489 'pv-2022-prod-kWh))
                   (list 1000.5d0 0 4)))
    (assert (equal (multiple-value-list (interpolate-meter-readings data-points 1698231487 'pv-2022-prod-kWh))
                   (list 1499.5d0 0 4)))
    (assert (equal (multiple-value-list (interpolate-meter-readings data-points 1698231488 'pv-2022-prod-kWh))
                   (list 1.5d3 0 4)))
    (assert (equal (multiple-value-list (interpolate-meter-readings data-points 1698231489 'pv-2022-prod-kWh))
                   (list 1500.5d0 0 4)))
    (assert (equal (multiple-value-list (interpolate-meter-readings data-points 1698232487 'pv-2022-prod-kWh))
                   (list 1999.5d0 0 4)))
    (assert (equal (multiple-value-list (interpolate-meter-readings data-points 1698232488 'pv-2022-prod-kWh))
                   (list 2d3 2 4)))
    (assert (equal (multiple-value-list (interpolate-meter-readings data-points 1698232490 'pv-2022-prod-kWh))
                   (list 2002d0 2 4)))
    (assert (equal (multiple-value-list (interpolate-meter-readings data-points 1698233480 'pv-2022-prod-kWh))
                   (list 2992d0 2 4)))
    (assert (equal (multiple-value-list (interpolate-meter-readings data-points 1698233488 'pv-2022-prod-kWh))
                   (list 3d3 3 4)))
    (assert (equal (multiple-value-list (interpolate-meter-readings data-points 1698231488 'pv-2012-prod-kWh))
                   (list 1d3 1 4)))
    (assert (equal (multiple-value-list (interpolate-meter-readings data-points 1698231489 'pv-2012-prod-kWh))
                   (list 1002d0 1 4)))
    (assert (equal (multiple-value-list (interpolate-meter-readings data-points 1698232487 'pv-2012-prod-kWh))
                   (list 2998d0 1 4)))
    (assert (equal (multiple-value-list (interpolate-meter-readings data-points 1698232488 'pv-2012-prod-kWh))
                   (list 3d3 2 4)))
    (assert (equal (multiple-value-list (interpolate-meter-readings data-points 1698232490 'pv-2012-prod-kWh))
                   (list 3002d0 2 4)))
    (assert (equal (multiple-value-list (interpolate-meter-readings data-points 1698233480 'pv-2012-prod-kWh))
                   (list 3992d0 2 4)))
    (assert (equal (multiple-value-list (interpolate-meter-readings data-points 1698233488 'pv-2012-prod-kWh))
                   (list 4d3 3 4)))
    (assert (handler-case (interpolate-meter-readings data-points 1698230487 #'pv-2022-prod-kWh) (simple-error () t)))
    (assert (handler-case (interpolate-meter-readings data-points 1698230489 #'pv-2012-prod-kWh) (simple-error () t)))
    (assert (handler-case (interpolate-meter-readings data-points 1698233489 #'pv-2012-prod-kWh) (simple-error () t)))
    (assert (handler-case (interpolate-meter-readings data-points 1698232488 #'gas-m3) (simple-error () t)))
    (assert (handler-case (interpolate-meter-readings data-points 1698232487 #'water-m3) (simple-error () t)))
    (assert (equal (multiple-value-list (interpolate-meter-readings data-points 1698232488 'water-m3))
                   (list 0d0 2 4)))
    (assert (handler-case (interpolate-meter-readings data-points 1698232489 #'water-m3) (simple-error () t)))))

(defun test-make-chart-data-raw ()
  (let* ((data '((0    nil nil)
                 (500  nil 5d0)
                 (3500 1d0 8d0)
                 (4000 nil nil)
                 (4500 2d0 nil)
                 (5000 nil 8d0)
                 (6000 nil nil)))
         (data-points (make-array
                       (length data)
                       :initial-contents
                       (loop :for (ts v w) in data
                             :collect (make-instance 'meter-reading-202303
                                                     :timestamp (+ ts 10000)
                                                     :pv-2012-prod-kWh v
                                                     :pv-2022-prod-kWh w)))))
    (multiple-value-bind (timestamps values first-val last-val)
        (make-chart-data-raw data-points
                             #'pv-2022-prod-kWh
                             1000
                             :step 2000)
      (assert (equal timestamps '(1050 1250 1450 1500)))
      (assert (equal values '(1.0f0 1.0f0 0.5f0 0.0f0)))
      (assert (equal first-val 5d0))
      (assert (equal last-val 8d0)))
    (multiple-value-bind (timestamps values first-val last-val)
        (make-chart-data-raw data-points
                             #'pv-2022-prod-kWh
                             1000
                             :step 2000
                             :period 4000)
      (assert (equal timestamps '(1100 1300 1500)))
      (assert (equal values '(1f0 1f0 0.25f0)))
      (assert (equal first-val 5.5d0))
      (assert (equal last-val 8d0)))
    (multiple-value-bind (timestamps values first-val last-val)
        (make-chart-data-raw data-points
                             (make-multi-accessor data-points #'+ #'pv-2022-prod-kWh #'pv-2012-prod-kWh)
                             1000
                             :step 2000
                             :period 4000)
      (assert (equal timestamps '(1100 1300 1500)))
      (assert (equal values '(1f0)))
      (assert (equal first-val 9d0))
      (assert (equal last-val 10d0)))))


(defun monthly-timestamps (minimum-timestamp)
  (loop :with max-timestamp = (reading-timestamp (aref *data-points* (1- (length *data-points*))))
        :with year = 2013
        :for month = 1 :then (if (= month 12) (progn (incf year) 1) (1+ month))
        :for timestamp = (- (encode-universal-time 0 0 0 1 month year 0) +unix-epoch+)
        :while (<= timestamp max-timestamp)
        :when (>= timestamp minimum-timestamp)
          :collect timestamp))


(defun tabulate-interpolations (timestamps data-points xsor)
  (loop :with xsor = (if (fboundp xsor)
                         xsor
                         (multiple-value-bind (_ multi-xsor)
                             (get-xsor xsor data-points)
                           (declare (ignore _))
                           multi-xsor))
        :with start = (position-if xsor data-points)
        :with last-idx
        :with previous-value
        :with last-ts
        :with first-ts = (reading-timestamp (aref data-points start))
        :with end
          :initially
             (unless start
               (error "No data to tabulate"))
             (setf last-idx (position-if xsor data-points :from-end t))
             (unless (and last-idx (< start last-idx))
               (error "No data to tabulate"))
             (setf last-ts (reading-timestamp (aref data-points last-idx))
                   end (1+ last-idx))
        :for ts in timestamps
        :collect (multiple-value-bind (value next-start _)
                     (handler-case
                         (interpolate-meter-readings data-points ts xsor :start start :end end)
                       (error (_) (declare (ignore _)) (values nil start)))
                   (declare (ignore _))
                   (prog1
                       (cons value (when (and value previous-value) (- value previous-value)))
                     (setf start next-start
                           previous-value value)))))
