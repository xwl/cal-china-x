;;; cal-china-x.el --- Chinese calendar extras

;; Copyright (C) 2004 Charles Wang
;; Copyright (C) 2006 William Xu

;; Author: Charles Wang <charleswang@peoplemail.com.cn>
;; Author: William Xu <william.xwl@gmail.com>

;; $Id: cal-china-x.el, v 0.1 2006/05/11 15:14:02 xwl Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This package localized calendar display for chinese users, which
;; means it will make calendar display Chinese characters.

;; Put this file into your load-path and the following into your
;; ~/.emacs:
;;
;;   (require 'chinese-calendar)
;;   (cal-china-x-setup)

;;; History:

;; This package is based on chinese-calendar.el written by Charles Wang
;; <charleswang@peoplemail.com.cn>. Major changes i've made including:

;; - Reorgize code layout and modify some codes to be more clear
;; - Remove some unnecessary hooks and functions
;; - Apply Emacs package prefix naming style
;; - Add some new functions

;;; TODO:

;; - Display week day(the first line of each month) in chinese properly
;; - Make `cal-china-x-birthday-from-chinese' interactive.

;;; Code:

(require 'calendar)
(require 'cal-china)

;;; Variables

(defvar cal-china-x-celestial-stem
  ["甲" "乙" "丙" "丁" "戊" "已" "庚" "辛" "壬" "癸"])

(defvar cal-china-x-terrestrial-branch
  ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"])

(defvar cal-china-x-days
  ["天" "一" "二" "三" "四" "五" "六"])

;; (defvar solar-n-hemi-seasons
;;   '("春分" "夏至" "秋分" "冬至"))

;; (defvar solar-s-hemi-seasons
;;   '("秋分" "夏至" "春分" "冬至"))

(defvar cal-china-x-month-name
  ["正月" "二月" "三月" "四月" "五月" "六月" "七月" "八月" "九月" "十月"
   "十一月" "腊月"])

(defvar cal-china-x-day-name
  ["初一" "初二" "初三" "初四" "初五" "初六" "初七" "初八" "初九" "初十"
   "十一" "十二" "十三" "十四" "十五" "十六" "十七" "十八" "十九"  "廿"
   "廿一" "廿二" "廿三" "廿四" "廿五" "廿六" "廿七" "廿八" "廿九" "三十"
   "卅一" "卅二" "卅三" "卅四" "卅五" "卅六" "卅七" "卅八" "卅九" "卅十"])

(defvar chinese-date-diary-pattern
  '((year " *年" month " *月" day "日 *[^/年0-9]")
    (year "-" month "-" day "[^0-9]")
    (day "/" month "[^/0-9]")
    (day "/" month "/" year "[^0-9]")
    (backup day " *" monthname "\\W+\\<\\([^*0-9]\\|\\([0-9]+[:aApP]\\)\\)")
    (day " *" monthname " *" year "[^0-9]")
    (dayname "\\W")))


;;; High Level Functions

;; TODO: write an interactive version
(defun cal-china-x-birthday-from-chinese (birthday-chinese)
  "Return birthday date this year in Gregorian form.

BIRTHDAY-CHINESE is a list.  e.g.  '(5 11)"
  (let* ((current-chinese-date (calendar-chinese-from-absolute
				(calendar-absolute-from-gregorian
				 (calendar-current-date))))
	 (cycle (car current-chinese-date))
	 (year (cadr current-chinese-date))

	 (birthday-chinese-full `(,cycle ,year ,@birthday-chinese))
	 (birthday-gregorian-full (calendar-gregorian-from-absolute
				   (calendar-absolute-from-chinese
				    birthday-chinese-full))))

    (setq birthday-gregorian `(,(car birthday-gregorian-full)
			       ,(cadr birthday-gregorian-full)))))

(defun holiday-chinese (cmonth cday string)
  "Chinese calendar holiday, month and day in Chinese calendar (CMONTH, CDAY).

If corresponding MONTH and DAY in gregorian calendar is visible,
the value returned is the list \(((MONTH DAY year) STRING)).
Returns nil if it is not visible in the current calendar window."
  (let* ((m displayed-month)
         (y displayed-year)
         (gdate (calendar-gregorian-from-absolute
                 (+ (cadr (assoc cmonth (chinese-year y))) (1- cday))))
         (gm (car gdate))
         (gd (cadr gdate))
         (gy (caddr gdate)))
    (increment-calendar-month m y (- 11 gm))
    (if (> m 9)
	(list (list (list gm gd gy) string)))))


;;; Low Level Functions

(defun cal-china-x-day-name (date)
  "Chinese day name in a week, like `星期一'."
  (concat "星期"
	  (aref cal-china-x-days (calendar-day-of-week date))))

(defun cal-china-x-day-short-name (num)
  "Short chinese day name in a week, like `一'. NUM is from 0..6
in a week."
  (aref cal-china-x-days num))

(defun cal-china-x-calendar-display-form (date)
  (format "%4d年%2d月%2d日 %s"
	  (extract-calendar-year date)
	  (extract-calendar-month date)
	  (extract-calendar-day date)
	  (cal-china-x-day-name date)))

(defun cal-china-x-chinese-date-string (date)
  (let* ((a-date (calendar-absolute-from-gregorian date))
         (c-date (calendar-chinese-from-absolute a-date))
         (cycle (car c-date))
         (year (car (cdr c-date)))
         (month (car (cdr (cdr c-date))))
         (day (car (cdr (cdr (cdr c-date)))))
         (this-month (calendar-absolute-from-chinese
                      (list cycle year month 1)))
         (next-month (calendar-absolute-from-chinese
                      (list (if (= year 60) (1+ cycle) cycle)
                            (if (= (floor month) 12) (1+ year) year)
                            (calendar-mod (1+ (floor month)) 12)
                            1)))
         (m-cycle (% (+ (* year 5) (floor month)) 60)))
    (format "农历%s年%s%s%s"
            ;;cycle
            ;;year
            (calendar-chinese-sexagesimal-name year)
            (if (not (integerp month))
                "润"
              (if (< 30 (- next-month this-month))
                  ""
                ""))
            (aref cal-china-x-month-name (1- (floor month)))
            (aref cal-china-x-day-name (1- day)))))

(defun holiday-chinese-new-year ()
  "Date of Chinese New Year."
  (let ((m displayed-month)
        (y displayed-year))
    (increment-calendar-month m y 1)
    (if (< m 5)
        (let ((chinese-new-year
               (calendar-gregorian-from-absolute
                (car (cdr (assoc 1 (chinese-year y)))))))
          (if (calendar-date-is-visible-p chinese-new-year)
	      (list
	       (list
		chinese-new-year
		(format "%s年春节"
			(calendar-chinese-sexagesimal-name
			 (+ y 57))))))))))


;;; Modifications to Standard Functions

;; These functions(from calendar.el, cal-china.el) have been modified
;; for localization.

(defun calendar-chinese-sexagesimal-name (n)
  "The N-th name of the Chinese sexagesimal cycle.
N congruent to 1 gives the first name, N congruent to 2 gives the second name,
..., N congruent to 60 gives the sixtieth name."
  ;; "%s-%s" -> "%s%s", since Chinese characters are tight one by one,
  ;; no extra `-' needed.
  (format "%s%s"
          (aref chinese-calendar-celestial-stem (% (1- n) 10))
          (aref chinese-calendar-terrestrial-branch (% (1- n) 12))))

(defun generate-calendar-month (month year indent)
  "Produce a calendar for MONTH, YEAR on the Gregorian calendar.
The calendar is inserted in the buffer starting at the line on which point
is currently located, but indented INDENT spaces.  The indentation is done
from the first character on the line and does not disturb the first INDENT
characters on the line."
  (let* ((blank-days ;; at start of month
          (mod
           (- (calendar-day-of-week (list month 1 year))
              calendar-week-start-day)
           7))
	 (last (calendar-last-day-of-month month year)))
    (goto-char (point-min))
    (calendar-insert-indented
     (calendar-string-spread
      (list (format "%d年%2d月" year month)) ?  20)
     indent t)
    (calendar-insert-indented "" indent) ;; Go to proper spot
    (calendar-for-loop i from 0 to 6 do
       (insert
	(let ((string
	       (calendar-day-name (mod (+ calendar-week-start-day i) 7) nil t)))
	  (if enable-multibyte-characters
	      (truncate-string-to-width string 2)
	    (substring string 0 2)))
	" "))
    ;; FIXME: Seems it's uneasy to make chinese align correctly
;;     (calendar-for-loop i from 0 to 6 do
;;        (insert
;; 	(let ((string
;; 	       (cal-china-x-day-short-name i)))
;; 	  string)
;; 	"  "))
    (calendar-insert-indented "" 0 t)	 ;; Force onto following line
    (calendar-insert-indented "" indent) ;; Go to proper spot
    ;; Add blank days before the first of the month
    (calendar-for-loop i from 1 to blank-days do (insert "   "))
    ;; Put in the days of the month
    (calendar-for-loop i from 1 to last do
       (insert (format "%2d " i))
       (add-text-properties
	(- (point) 3) (1- (point))
	'(mouse-face highlight
		     help-echo "mouse-2: menu of operations for this date"))
       (and (zerop (mod (+ i blank-days) 7))
	    (/= i last)
           (calendar-insert-indented "" 0 t)    ;; Force onto following line
           (calendar-insert-indented "" indent)))));; Go to proper spot


;;; Setup

(defun cal-china-x-setup ()
  "Localized calendar to Chinese."
  (setq calendar-date-display-form
	'((cal-china-x-calendar-display-form date)))

  (setq diary-date-forms chinese-date-diary-pattern)

  ;; TODO: show holiday one mode line if it's ! in blue?
  (setq calendar-mode-line-format
	(list
	 (propertize (substitute-command-keys
		      "\\<calendar-mode-map>\\[scroll-calendar-left]")
		     'help-echo "mouse-2: scroll left"
		     'mouse-face 'mode-line-highlight
		     'keymap (make-mode-line-mouse-map 'mouse-2
						       'mouse-scroll-calendar-left))
	 "Calendar"
	 (concat
	  (propertize
	   (substitute-command-keys
	    "\\<calendar-mode-map>\\[calendar-goto-info-node] info")
	   'help-echo "mouse-2: read Info on Calendar"
	   'mouse-face 'mode-line-highlight
	   'keymap (make-mode-line-mouse-map 'mouse-2 'calendar-goto-info-node))
	  "/"
	  (propertize
	   (substitute-command-keys
	    "\\<calendar-mode-map>\\[calendar-other-month] other")
	   'help-echo "mouse-2: choose another month"
	   'mouse-face 'mode-line-highlight
	   'keymap (make-mode-line-mouse-map
		    'mouse-2 'mouse-calendar-other-month))
	  "/"
	  (propertize
	   (substitute-command-keys
	    "\\<calendar-mode-map>\\[calendar-goto-today] today")
	   'help-echo "mouse-2: go to today's date"
	   'mouse-face 'mode-line-highlight
	   'keymap (make-mode-line-mouse-map 'mouse-2 #'calendar-goto-today)))
	 '(calendar-date-string date t)
	 '(cal-china-x-chinese-date-string date)
	 (propertize (substitute-command-keys
		      "\\<calendar-mode-map>\\[scroll-calendar-right]")
		     'help-echo "mouse-2: scroll right"
		     'mouse-face 'mode-line-highlight
		     'keymap (make-mode-line-mouse-map
			      'mouse-2 'mouse-scroll-calendar-right))
	 ;; FIXME: why should i add this in order to display right '>' ?
	 ""))

  (add-hook 'calendar-move-hook 'update-calendar-mode-line)

  (setq chinese-calendar-celestial-stem
	cal-china-x-celestial-stem
	chinese-calendar-terrestrial-branch
	cal-china-x-terrestrial-branch)

  (setq local-holidays
	'((holiday-fixed 1  1  "元旦")
	  (holiday-chinese-new-year)
	  (holiday-fixed 3  8  "妇女节")
	  (holiday-fixed 3  12 "植树节")
	  (holiday-fixed 5  1  "劳动节")
	  (holiday-fixed 5  4  "青年节")
	  (holiday-fixed 6  1  "儿童节")
	  (holiday-fixed 9  10 "教师节")
	  (holiday-fixed 10 1  "国庆节")
	  (holiday-fixed 12 25 "圣诞节")

	  (holiday-chinese 1 15 "元宵节")
	  (holiday-chinese 5 5  "端午节")
	  (holiday-chinese 9 9  "重阳节")
	  (holiday-chinese 8 15 "中秋节")))

  (setq calendar-holidays
	(append general-holidays local-holidays))

  (message "cal-china-x setup done"))

(provide 'cal-china-x)

;;; chinese-calendar.el ends here
