;; -*- lisp -*-
;;
;; Stupid calendar tricks for computing Discardia.
;; http://www.aigeek.com/discardia/ 
;; Seth Golub <seth@aigeek.com>
;; 25 Dec 2002

(require 'cl)
(require 'lunar)
(require 'solar)
(defmacro month (date) `(first  ,date))
(defmacro day   (date) `(floor (second ,date)))
(defmacro year  (date) `(third  ,date))
(defun date>= (d1 d2)
  (or (> (year d1) (year d2))
      (and (= (year d1) (year d2))
           (or (> (month d1) (month d2))
               (and (= (month d1) (month d2))
                    (>= (day d1) (day d2)))))))

(defun date== (d1 d2)
  (and (date>= d1 d2)
       (date>= d2 d1)))

(defun next-equinoxes/solstices (date k)
  "Returns dates of the next k equinoxes/solstices following date.
Each date is of the form (month day year)."
  (let ((l nil)
        (q 0)
        (year (year date)))
    (while (< (length l) k)
      (let ((nextdate (solar-equinoxes/solstices q year)))
        (if (date>= nextdate date)
            (setq l (append l (list nextdate)))))
      (if (= 0 (setq q (mod (+ 1 q) 4))) ; q=++q%4, and if we wrap
          (setq year (+ 1 year))))       ; then year++
    l))

(defun lunar-phase-after-date (date phase)
  "Returns the date of the next specified lunar phase.
Lunar phases are specified by number.  0 is full moon, etc."
  (caar (delete-if-not (lambda (p)  ;; p looks like ((12 3 2002) "11:36pm (PST)" 0)
                         (and (= phase (third p))
                              (date>= (car p) date)))
                       (lunar-phase-list (month date) (year date)))))

(defun previous-equinox/solstice (date)
  "The single equinox/solstice prior to date"
  (let ((year-ago (list (month date) (day date) (- (year date) 1))))
    (car (last (delete-if (lambda (d) (date>= d date))
                          (next-equinoxes/solstices year-ago 5))))))

(defun previous-or-current-equinox/solstice (date)
  "The single equinox/solstice prior to or equal to date"
  (let ((year-ago (list (month date) (day date) (- (year date) 1))))
    (car (last (delete-if-not (lambda (d) (date>= date d))
                          (next-equinoxes/solstices year-ago 5))))))

(defun discardia-p (date)
  "True iff date falls during Discardia (the period from an equinox/solstice
until the following full moon)."
  (date>= (lunar-phase-after-date (previous-or-current-equinox/solstice date) 0)
          date))

(defun next-discardias (date k)
  "Returns a list of (soldate moondate) of the k next equinoxes/solstices
and their following full moons"
  (mapcar (lambda (sol) (list sol (lunar-phase-after-date sol 0)))
          (next-equinoxes/solstices date k)))

(defun next-or-current-discardias (date k)
  "Returns a list of (soldate moondate) of the k next equinoxes/solstices
and their following full moons"
    (if (discardia-p date)
        (list (let ((sol (previous-or-current-equinox/solstice date)))
                (list sol (lunar-phase-after-date sol 0)))
              (next-discardias date (- k 1)))
      (next-discardias date k)))

(provide 'discardia)