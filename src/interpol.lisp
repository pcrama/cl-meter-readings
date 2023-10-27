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

(defun make-chart-data-raw (data-points xsor scale)
  "Resample DATA-POINTS (XSOR readings) and SCALE them to a consumption rate

Returns (CL:VALUES number-of-10s-since-unix-epoch
                   xsor-values
                   first-xsor-value
                   last-xsor-value)

Useful scale values:
| xsor             |        scale | resulting unit |
| gas-m3           |        86400 | m3/day         |
| water-m3         | 1000 * 86400 | l/day          |
| pv-2012-prod-kWh |         3600 | kW             |"
  (loop :with step = 7200               ; seconds
        :with period = 30               ; days
        :with start = (position-if xsor data-points)
        :with last-idx
        :with previous-ts
        :with previous-value
        :with start-ts
        :with last-ts
        :with first-value
        :with last-value
        :with end
          :initially
             (unless start
               (error "No data to plot"))
             (setf last-idx (position-if xsor data-points :from-end t))
             (unless (and last-idx (< start last-idx))
               (error "No data to plot"))
             (let ((previous (aref data-points start)))
               (setf first-value (funcall xsor previous)
                     previous-value first-value
                     previous-ts (reading-timestamp previous)))
             (let ((last (aref data-points last-idx)))
               (setf last-value (funcall xsor last)
                     last-ts (reading-timestamp last)))
             (setf end (1+ last-idx)
                   start-ts (max (+ previous-ts step) (- last-ts (* period 86400))))
             (unless (< start-ts last-ts)
               (error "No data to plot"))
        :for ts = start-ts :then (min (+ ts step) last-ts)
        :for (abscissa . ordinate) = (multiple-value-bind (value next-start _)
                                         (interpolate-meter-readings data-points ts xsor :start start :end end)
                                       (declare (ignore _))
                                       (prog1
                                           (cons ts (/ (- value previous-value 0.0d0) (- ts previous-ts 0.0d0)))
                                         (setf start next-start
                                               previous-value value
                                               previous-ts ts)))
        :collect (round abscissa 10) into timestamps
        :collect (coerce (* ordinate scale) 'single-float) into values
        :while (< ts last-ts)
        :finally (return (values timestamps values first-value last-value))))

(defun write-chart-config (data-points xsor &optional (stream *standard-output*))
  (multiple-value-bind (format-string xsor-fun scale)
      (ecase xsor
        (gas-m3 (values "Gas [m³/day] ~Fm³ → ~Fm³" #'gas-m3 86400))
        (water-m3 (values "Water [l/day] ~Fm³ → ~Fm³" #'water-m3 86400e3))
        (pv-2022-prod-kWh (values "PV 2022 [kW] ~FkWh → ~FkWh" #'pv-2022-prod-kWh 3600))
        (pv-2012-prod-kWh (values "PV 2012 [kW] ~FkWh → ~FkWh" #'pv-2012-prod-kWh 3600)))
    (multiple-value-bind (timestamps-10s values first-value last-value)
        (make-chart-data-raw data-points xsor-fun scale)
      (write-string "{labels:[" stream)
      (format stream "~{~Ae4,~}" timestamps-10s) ; from 10s steps to ms
      (write-string "],datasets:[{label: '" stream)
      (format stream format-string first-value last-value)
      (write-string "',data:[" stream)
      (format stream "~{~F,~}" values)
      (write-string "],borderWidth:1,filled:false,stepped:'before',borderColor:'blue'}]}" stream)
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
