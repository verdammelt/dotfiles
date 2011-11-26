;;;
;;; Calendar/Diary setup
;;;
;;; Time-stamp: <2011-03-05 13:05:52 mark>


(require 'calendar)

(setq 
 calendar-latitude +42.358056
 calendar-location-name "Cambridge, MA"
 calendar-longitude -71.113056
 calendar-time-display-form '(24-hours ":" minutes (if time-zone " (") time-zone (if time-zone ")"))
 diary-display-hook 'fancy-diary-display
 diary-file "~/.diary"
 diary-mail-days 10
 european-calendar-style t
 mark-diary-entries-hook '(mark-included-diary-files)
 mark-diary-entries-in-calendar t
 mark-holidays-in-calendar t
 number-of-diary-entries 10
 view-calendar-holidays-initially nil
 view-diary-entries-initially t)

(setq 
 other-holidays '((holiday-fixed 1 23 "National Pie Day")
		  (holiday-fixed 1 27 "Holocaust Commemoration Day")
		  (holiday-fixed 3 14 "Pi Day")
		  (holiday-fixed 3 17 "Evacuation Day")
		  (holiday-fixed 4 15 "Tax Day")
		  (holiday-fixed 4 30 "Walpurgis Nacht")
		  (holiday-fixed 5 1  "May Day")
		  (holiday-fixed 5 25 "Towel Day") 
		  (holiday-fixed 9 19 "Talk Like a Pirate Day")
		  (holiday-fixed 9 22 "Car Free Day")
		  (holiday-fixed 11 5 "Guy Fawkes Day")
		  (holiday-fixed 11 11 "Armistice Day")
		  (holiday-fixed 11 23 "Dr. Who Day (1963)"))
 calendar-holidays (append general-holidays local-holidays other-holidays
			   christian-holidays hebrew-holidays islamic-holidays
			   oriental-holidays solar-holidays))

(add-hook 'list-diary-entries-hook #'include-other-diary-files)
(add-hook 'list-diary-entries-hook #'sort-diary-entries)
(add-hook 'list-diary-entries-hook #'mark-included-diary-files)
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)


(provide 'init-calendar)
