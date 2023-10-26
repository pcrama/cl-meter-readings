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

