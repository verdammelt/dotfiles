;; time display the way i like it
(with-eval-after-load 'time
  (setq display-time-24hr-format t
        display-time-day-and-date t
        display-time-use-mail-icon t
        display-time-mail-face 'cursor ; (only background color used)
        display-time-format "%Y-%m-%dT%R"))

;; calendar
(setq holiday-hebrew-holidays nil
      holiday-islamic-holidays nil
      holiday-oriental-holidays nil
      holiday-bahai-holidays nil)
(setq holiday-other-holidays
      '((holiday-sexp '(if (zerop (% year 4))
                           (calendar-gregorian-from-absolute
                            (1+ (calendar-dayname-on-or-before
                                 1 (+ 6 (calendar-absolute-from-gregorian
                                         (list 11 1 year)))))))
                      "US Presidential Election")))
(with-eval-after-load 'calendar
  (add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
  (add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files)

  (setq
   calendar-latitude +40.72541
   calendar-longitude -73.70928
   calendar-location-name "Floral Park, NY"
   calendar-time-display-form '(24-hours ":" minutes (if time-zone " (") time-zone (if time-zone ")"))
   diary-file "~/.diary"
   calendar-mark-diary-entries-flag t
   calendar-mark-holidays-flag t
   calendar-view-diary-initially-flag t
   calendar-view-holidays-initially-flag t)
  (calendar-set-date-style 'iso))

(display-time)
