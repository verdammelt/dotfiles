(use-package time
  :ensure nil
  :init
  (add-hook 'after-init-hook 'display-time t)
  :config
  (progn
    (defvar display-time-use-mail-icon)
    (defvar display-time-format)
    (setq display-time-use-mail-icon t
          display-time-default-load-average nil
          display-time-format "%FT%R"
          display-time-mail-face
          (defface display-time-mail-face
            '((t :foreground "blue" :background "red" :weight bold))
            "Face for showing mail indicator on mode line"
            :group 'mjs-faces))))

(use-package diary
  :ensure nil
  :init
  (progn
    (add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
    (add-hook 'diary-list-entries-hook 'diary-sort-entries t)
    (add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files)))

(use-package holiday
  :ensure nil
  :init
  (setq holiday-hebrew-holidays nil
        holiday-islamic-holidays nil
        holiday-oriental-holidays nil
        holiday-bahai-holidays nil
        holiday-local-holidays ;; Def Method Holidays
        '()

        holiday-other-holidays
        '((holiday-sexp '(if (zerop (% year 4))
                             (calendar-gregorian-from-absolute
                              (1+ (calendar-dayname-on-or-before
                                   1 (+ 6 (calendar-absolute-from-gregorian
                                           (list 11 1 year)))))))
                        "US Presidential Election"))))

(use-package calendar
  :ensure nil
  :commands (calendar-set-date-style)
  :init (setq
         diary-file "~/.diary"
         calendar-mark-diary-entries-flag t
         calendar-mark-holidays-flag t
         calendar-view-diary-initially-flag t
;         calendar-view-holidays-initially-flag t
         )
  :config (progn
            (calendar-set-date-style 'iso)
            (add-hook 'calendar-today-visible-hook 'calendar-mark-today)))

(use-package solar
  :ensure nil
  :config
  (setq calendar-latitude +40.869486
        calendar-longitude -73.428620
        calendar-location-name "Huntington, NY"
        calendar-time-display-form
        '(24-hours ":" minutes
                   (if time-zone " (") time-zone (if time-zone ")"))))

(use-package gcal-sync
  :ensure nil
  :commands (gcal-sync-calendars-to-diary)
  :defines (gcal-diary-file)
  :config
  (setq gcal-diary-file (expand-file-name "~/.diary.imported")))

(defun diary-schedule (m1 d1 y1 m2 d2 y2 dayname)
  "Entry applies if date is between dates on DAYNAME.
    Order of the parameters changes according to `calendar-date-style`.

DAYNAME can also be a list of DAYNAMEs"
  (let ((dayname (if (atom dayname) (list dayname) dayname))
        (date1 (calendar-absolute-from-gregorian (diary-make-date m1 d1 y1)))
        (date2 (calendar-absolute-from-gregorian (diary-make-date m2 d2 y2)))
        (d (calendar-absolute-from-gregorian date)))
    (when (and (<= date1 d date2)
               (memq (calendar-day-of-week date) dayname)
               (not (calendar-check-holidays date)))
      entry)))
