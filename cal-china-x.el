;;; cal-china-x.el --- Chinese calendar extras     -*- coding: utf-8; -*-

;; Copyright (C) 2006, 2007, 2008 William Xu

;; Author: William Xu <william.xwl@gmail.com>
;; Version: 0.9
;; Url: http://williamxu.net9.org/ref/cal-china-x.el

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

;; This package adds more chinese calendar supports and others,
;; including:
;;
;;   - Chinese localizations
;;   - Display holiday, lunar, horoscope, zodiac, solar term info on mode line
;;   - Define holidays using `holiday-lunar', `holiday-solar-term'
;;   - Highlight holidays based on different priorities
;;   - Add `cal-china-x-chinese-holidays', `cal-china-x-japanese-holidays'.
;;
;; To use, add the following in your .emacs:
;;     (require 'cal-china-x)

;;; History

;; This is an early derived work from `chinese-calendar.el' written by
;; Charles Wang <charleswang@peoplemail.com.cn>.

;;; TODO:

;; - Display week day(the first line of each month) in chinese properly

;;; Code:

(require 'calendar)
(require 'holidays)
(require 'cal-china)
(eval-when-compile
  (require 'cl))

;;; Variables

(defconst cal-china-x-celestial-stem
  ["甲" "乙" "丙" "丁" "戊" "已" "庚" "辛" "壬" "癸"])

(defconst cal-china-x-terrestrial-branch
  ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"])

(defconst cal-china-x-days
  ["日" "一" "二" "三" "四" "五" "六"])

(defconst cal-china-x-month-name
  ["正月" "二月" "三月" "四月" "五月" "六月" "七月" "八月" "九月" "十月"
   "十一月" "腊月"])

(defconst cal-china-x-day-name
  ["初一" "初二" "初三" "初四" "初五" "初六" "初七" "初八" "初九" "初十"
   "十一" "十二" "十三" "十四" "十五" "十六" "十七" "十八" "十九"  "廿"
   "廿一" "廿二" "廿三" "廿四" "廿五" "廿六" "廿七" "廿八" "廿九" "三十"
   "卅一" "卅二" "卅三" "卅四" "卅五" "卅六" "卅七" "卅八" "卅九" "卅十"])

(defvar chinese-date-diary-pattern
  '((year " *年" month " *月" day " *日 *[^\年0-9]") ; " *星期")
    (year "-" month "-" day "[^0-9]")
    (day "/" month "[^/0-9]")
    (day "/" month "/" year "[^0-9]")
    (backup day " *" monthname "\\W+\\<\\([^*0-9]\\|\\([0-9]+[:aApP]\\)\\)")
    (day " *" monthname " *" year "[^0-9]")
    (dayname "\\W")))

(defconst cal-china-x-horoscope-name
  '(((3  21) (4  19) "白羊")
    ((4  20) (5  20) "金牛")
    ((5  21) (6  21) "双子")
    ((6  22) (7  22) "巨蟹")
    ((7  23) (8  22) "狮子")
    ((8  23) (9  22) "处女")
    ((9  23) (10 23) "天秤")
    ((10 24) (11 22) "天蝎")
    ((11 23) (12 21) "射手")
    ((12 22) (1  19) "摩羯")
    ((1  20) (2  18) "水瓶")
    ((2  19) (3  20) "双鱼")))

(defconst cal-china-x-zodiac-name
  ["鼠" "牛" "虎" "兔" "龙" "蛇" "马" "羊" "猴" "鸡" "狗" "猪"]
  "The zodiac(Sheng Xiao) when you were born.")

;; for ref, http://www.geocities.com/calshing/chinesecalendar.htm
(defconst cal-china-x-solar-term-name
  ["小寒" "大寒" "立春" "雨水" "惊蛰" "春分"
   "清明" "谷雨" "立夏" "小满" "芒种" "夏至"
   "小暑" "大暑" "立秋" "处暑" "白露" "秋分"
   "寒露" "霜降" "立冬" "小雪" "大雪" "冬至"]
  "24 solar terms(节气, in chinese).
\"小寒\" is the first solar term in a new year. e.g., 2007-01-06.
There is a short poem for remembering,

    春雨惊春清谷天，夏满芒夏暑相连，
    秋处露秋寒霜降，冬雪雪冬小大寒。")

(defconst cal-china-x-japanese-holidays
  '((holiday-fixed 1 1 "元旦")
    (holiday-fixed 1 2 "公务员法定休息日")
    (holiday-fixed 1 3 "公务员法定休息日")
    (holiday-fixed 1 4 "公务员法定休息日")
    (holiday-float 1 1 1 "成人の日")
    (holiday-fixed 2 11 "建国記念の日")
    (holiday-solar-term "春分" "春分の日")
    (holiday-fixed 4 29 "みどりの日")
    (holiday-fixed 5 3 "憲法記念日")
    (holiday-fixed 5 4 "国民の休日")
    (holiday-fixed 5 5 "こどもの日")
    (holiday-fixed 7 20 "海の日")
    (holiday-fixed 9 15 "敬老の日")
    (holiday-solar-term "秋分" "秋分の日")
    (holiday-float 10 1 0 "体育の日")
    (holiday-fixed 11 3 "文化の日")
    (holiday-fixed 11 23 "勤労感謝の日")
    (holiday-fixed 12 23 "天皇誕生日")
    (holiday-fixed 12 28 "公务员法定休息日")
    (holiday-fixed 12 29 "公务员法定休息日")
    (holiday-fixed 12 30 "公务员法定休息日")
    (holiday-fixed 12 31 "公务员法定休息日"))
  "Pre-defined japanese public holidays.
You can add this to your `calendar-holidays'.")

(defconst cal-china-x-chinese-holidays
  '((holiday-fixed 1 1 "元旦")
    (holiday-lunar 12 30 "春节" 0)
    (holiday-lunar 1 1 "春节" 0)
    (holiday-lunar 1 2 "春节" 0)
    (holiday-solar-term "清明" "清明节")
    (holiday-fixed 5 1 "劳动节")
    (holiday-lunar 5 5 "端午节" 0)
    (holiday-lunar 8 15 "中秋节" 0)
    (holiday-fixed 10 1 "国庆节")
    (holiday-fixed 10 2 "国庆节")
    (holiday-fixed 10 3 "国庆节"))
  "Pre-defined chinese public holidays.
You can add this to your `calendar-holidays'.")


;;; Interfaces

(defgroup cal-china-x nil
  "China calendar extentions and more."
  :group 'calendar)

(defcustom cal-china-x-priority1-holidays '()
  "Highlighted by `cal-china-x-priority1-holiday-face'."
  :type 'symbol
  :group 'cal-china-x)

(defcustom cal-china-x-priority2-holidays '()
  "Highlighted by `cal-china-x-priority2-holiday-face'."
  :type 'symbol
  :group 'cal-china-x)

(defface cal-china-x-priority1-holiday-face
  '((((class color) (background light))
     :background "red")
    (((class color) (background dark))
     :background "red")
    (t
     :inverse-video t))
  "Face for indicating `cal-china-x-priority1-holidays'."
  :group 'cal-china-x)

(defface cal-china-x-priority2-holiday-face
  '((((class color) (background light))
     :background "green")
    (((class color) (background dark))
     :background "green")
    (t
     :inverse-video t))
  "Face for indicating `cal-china-x-priority2-holidays'."
  :group 'cal-china-x)

;;;###autoload
(defun cal-china-x-birthday-from-chinese (lunar-month lunar-day)
  "Return birthday date this year in Gregorian form.

LUNAR-MONTH and LUNAR-DAY are date number used in chinese lunar
calendar."
  (interactive "nlunar month: \nnlunar day: ")
  (let* ((birthday-chinese (list lunar-month lunar-day))
	 (current-chinese-date (calendar-chinese-from-absolute
				(calendar-absolute-from-gregorian
				 (calendar-current-date))))
	 (cycle (car current-chinese-date))
	 (year (cadr current-chinese-date))
	 (birthday-chinese-full `(,cycle ,year ,@birthday-chinese))
	 (birthday-gregorian-full (calendar-gregorian-from-absolute
				   (calendar-absolute-from-chinese
				    birthday-chinese-full))))
    (message "Your next birthday in gregorian is on %s"
	     (calendar-date-string birthday-gregorian-full))))

;;;###autoload
(defun holiday-lunar (lunar-month lunar-day string &optional num)
  "Like `holiday-fixed', but with LUNAR-MONTH and LUNAR-DAY.
When there are multiple days(like Run Yue or 闰月, e.g.,
2006-08-30), we use NUM to define which day(s) as holidays. The
rules are:

NUM = 0, only the earlier day.
NUM = 1, only the later day.
NUM with other values(default), all days(maybe one or two)."
  (unless (integerp num)
    (setq num 2))
  (let* ((cn-years (chinese-year displayed-year))
         (ret '()))
    (setq ret (append ret (holiday-lunar-1 (assoc lunar-month cn-years)
                                           lunar-day
                                           string)))
    (when (and (> (length cn-years) 12) (not (zerop num)))
      (let ((run-yue '())
            (years cn-years)
            (i '()))
        (while years
          (setq i (car years)
                years (cdr years))
          (unless (integerp (car i))
            (setq run-yue i)
            (setq years nil)))
        (when (= lunar-month (floor (car run-yue)))
          (setq ret (append ret (holiday-lunar-1 run-yue
                                                 lunar-day
                                                 string))))))
    (cond ((= num 0)
           (when (car ret)
             (list (car ret))))
          ((= num 1)
           (if (cadr ret)
               (list (cadr ret))
             ret))
          (t
           ret))))

(defun holiday-lunar-1 (run-yue lunar-day string)
  (let* ((date (calendar-gregorian-from-absolute
                (+ (cadr run-yue) (1- lunar-day))))
         (holiday (holiday-fixed (car date) (cadr date) string)))
    ;; Same year?
    (when (and holiday (= (nth 2 (caar holiday)) (nth 2 date)))
      holiday)))

;;;###autoload
(defun holiday-solar-term (solar-term str)
  "A holiday(STR) on SOLAR-TERM day.
See `cal-china-x-solar-term-name' for a list of solar term names ."
  (cal-china-x-sync-solar-term displayed-year)
  (let ((l cal-china-x-solar-term-alist)
        date)
    (dolist (i l)
      (when (string= (cdr i) solar-term)
        (setq l '()
              date (car i))))
    (holiday-fixed (car date) (cadr date) str)))

(defun cal-china-x-calendar-display-form (date)
  (if (equal date '(0 0 0))
      ""
    (format "%04d年%02d月%02d日 %s"
            (extract-calendar-year date)
            (extract-calendar-month date)
            (extract-calendar-day date)
            (cal-china-x-day-name date))))

(defun cal-china-x-chinese-date-string (date)
  (let* ((cn-date (calendar-chinese-from-absolute
                   (calendar-absolute-from-gregorian date)))
         (cn-year  (cadr   cn-date))
         (cn-month (caddr  cn-date))
         (cn-day   (cadddr cn-date)))
    (format "%s%s年%s%s%s(%s)%s"
            (calendar-chinese-sexagesimal-name cn-year)
            (cal-china-x-get-zodiac date)
            (aref cal-china-x-month-name (1-  (floor cn-month)))
            (if (integerp cn-month) "" "(闰月)")
            (aref cal-china-x-day-name (1- cn-day))
            (cal-china-x-get-horoscope (car date) (cadr date))
            (cal-china-x-get-solar-term date))))

(defun cal-china-x-setup ()
  (setq calendar-date-display-form
	'((cal-china-x-calendar-display-form
           (mapcar (lambda (el) (string-to-number el))
                   (list month day year)))))

  (setq diary-date-forms chinese-date-diary-pattern)

  ;; chinese month and year
  (setq calendar-font-lock-keywords
        (append calendar-font-lock-keywords
                '(("[0-9]+年\\ *[0-9]+月"
                   . font-lock-function-name-face))))

  (setq calendar-mode-line-format
        (list
         (concat
          (propertize "<"
                      'help-echo "mouse-1: previous month"
                      'mouse-face 'mode-line-highlight
                      'keymap (make-mode-line-mouse-map 'mouse-1
                                                        'calendar-scroll-right))
          " " calendar-buffer)

         '(cal-china-x-get-holiday date) ; FIXME: how to ignore this column when it is "" ?
         '(calendar-date-string date t)
         '(cal-china-x-chinese-date-string date)

         (concat
          (propertize
           (substitute-command-keys
            "\\<calendar-mode-map>\\[calendar-goto-info-node] info")
           'help-echo "mouse-1: read Info on Calendar"
           'mouse-face 'mode-line-highlight
           'keymap (make-mode-line-mouse-map 'mouse-1 'calendar-goto-info-node))
          " / "
          (propertize
           (substitute-command-keys
            " \\<calendar-mode-map>\\[calendar-other-month] other")
           'help-echo "mouse-1: choose another month"
           'mouse-face 'mode-line-highlight
           'keymap (make-mode-line-mouse-map
                    'mouse-1 'mouse-calendar-other-month))
          " / "
          (propertize
           (substitute-command-keys
            "\\<calendar-mode-map>\\[calendar-goto-today] today")
           'help-echo "mouse-1: go to today's date"
           'mouse-face 'mode-line-highlight
           'keymap (make-mode-line-mouse-map 'mouse-1 #'calendar-goto-today)))

         ;; FIXME: This right `>' can not be displayed correctly. Also,
         ;; it looks like if i don't append an additional "" at end,
         ;; even more right partial info will disappear.
         (propertize ">"
                     'help-echo "mouse-1: next month"
                     'mouse-face 'mode-line-highlight
                     'keymap (make-mode-line-mouse-map
                              'mouse-1 'calendar-scroll-left))
         ""))

  (cond
   ((fboundp 'update-calendar-mode-line) ; < emacs 23
    (add-hook 'calendar-move-hook 'update-calendar-mode-line))
   ((fboundp 'calendar-update-mode-line) ; >= emacs 23
    (add-hook 'calendar-move-hook 'calendar-update-mode-line)))

  (setq chinese-calendar-celestial-stem cal-china-x-celestial-stem
	chinese-calendar-terrestrial-branch cal-china-x-terrestrial-branch))

(if (fboundp 'calendar-mark-holidays)
    (defadvice calendar-mark-holidays (around mark-different-holidays activate)
      "Mark extra `xwl-important-holidays'."
      (let ((calendar-holiday-marker 'cal-china-x-priority1-holiday-face)
            (calendar-holidays cal-china-x-priority1-holidays))
        ad-do-it)
      (let ((calendar-holiday-marker 'cal-china-x-priority2-holiday-face)
            (calendar-holidays cal-china-x-priority2-holidays))
        ad-do-it)
      (let ((calendar-holidays
             (remove-if (lambda (i)
                          (or (member i cal-china-x-priority1-holidays)
                              (member i cal-china-x-priority2-holidays)))
                        calendar-holidays)))
        ad-do-it))

  (defadvice mark-calendar-holidays (around mark-different-holidays activate)
    "Mark extra `xwl-important-holidays'."
    (let ((calendar-holiday-marker 'cal-china-x-priority1-holiday-face)
          (calendar-holidays cal-china-x-priority1-holidays))
      ad-do-it)
    (let ((calendar-holiday-marker 'cal-china-x-priority2-holiday-face)
          (calendar-holidays cal-china-x-priority2-holidays))
      ad-do-it)
    (let ((calendar-holidays
           (remove-if (lambda (i)
                        (or (member i cal-china-x-priority1-holidays)
                            (member i cal-china-x-priority2-holidays)))
                      calendar-holidays)))
      ad-do-it)))


;;; Implementations

(defun cal-china-x-day-name (date)
  "Chinese day name in a week, like `星期一'."
  (concat "星期" (aref cal-china-x-days (calendar-day-of-week date))))

(defun cal-china-x-day-short-name (num)
  "Short chinese day name in a week, like `一'. NUM is from 0..6
in a week."
  (aref cal-china-x-days num))

(defun cal-china-x-get-horoscope (month day)
  "Return horoscope on MONTH(1-12) DAY(1-31)."
  (catch 'return
    (mapc
     (lambda (el)
       (let ((start (car el))
             (end (cadr el)))
         (when (or (and (= month (car start))
                        (>= day (cadr start)))
                   (and (= month (car end))
                        (<= day (cadr end))))
           (throw 'return (caddr el)))))
     cal-china-x-horoscope-name)))

(defun holiday-chinese-new-year ()
  "Date of Chinese New Year."
  (let ((m displayed-month)
        (y displayed-year))
    (increment-calendar-month m y 1)
    (if (< m 5)
        (let ((chinese-new-year
               (calendar-gregorian-from-absolute
                (cadr (assoc 1 (chinese-year y))))))
          (if (calendar-date-is-visible-p chinese-new-year)
	      `((,chinese-new-year
                 ,(format "%s年春节"
                          (calendar-chinese-sexagesimal-name
                           (+ y 57))))))))))

(defun cal-china-x-get-zodiac (&optional date)
  "Get zodiac(Sheng Xiao) on DATE."
  (let ((n (cadr (calendar-chinese-from-absolute
                  (calendar-absolute-from-gregorian
                   (or date (calendar-current-date)))))))
    (aref cal-china-x-zodiac-name (% (1- n) 12))))

(defun cal-china-x-get-solar-term (&optional date)
  (unless date
    (setq date (calendar-current-date)))
  (let ((year (extract-calendar-year date)))
    (cal-china-x-sync-solar-term year)
    (or (cdr (assoc date cal-china-x-solar-term-alist))
        "")))

(defun cal-china-x-solar-term-alist-new (year)
  "Return a solar-term alist for YEAR."
  (loop for i from 0 upto 23

        for date = (cal-china-x-next-solar-term `(1 1 ,year))
        then (setq date (cal-china-x-next-solar-term date))

        with solar-term-alist = '()

        collect (cons date (aref cal-china-x-solar-term-name i))
        into solar-term-alist

        finally return solar-term-alist))

(defun cal-china-x-gregorian-from-astro (a)
  (calendar-gregorian-from-absolute
   (floor (calendar-absolute-from-astro a))))

(defun cal-china-x-astro-from-gregorian (g)
  (calendar-astro-from-absolute
   (calendar-absolute-from-gregorian g)))

(defun cal-china-x-next-solar-term (date)
  "Return next solar term's data after DATE.
Each solar term is separated by 15 longtitude degrees or so, plus an
extra day appended."
  (cal-china-x-gregorian-from-astro
    (solar-date-next-longitude
     (cal-china-x-astro-from-gregorian
      (calendar-gregorian-from-absolute
       (1+ (calendar-absolute-from-gregorian date))))
     15)))

(defun cal-china-x-get-holiday (date)
  (when (and (boundp 'displayed-month)
             (boundp 'displayed-year))
    (let ((holidays (calendar-holiday-list))
          (str ""))
      (dolist (i holidays)
        (when (equal (car i) date)
          (setq str (concat str " " (cadr i)))))
      str)))

;; cached solar terms in a year
(defvar cal-china-x-solar-term-alist nil) ; e.g., '(((1 20 2008) "春分") ...)
(defvar cal-china-x-solar-term-year nil)

(defun cal-china-x-sync-solar-term (year)
  "Sync `cal-china-x-solar-term-alist' and `cal-china-x-solar-term-year' to YEAR."
  (unless (and cal-china-x-solar-term-year
               (= cal-china-x-solar-term-year year))
      (setq cal-china-x-solar-term-alist
            (cal-china-x-solar-term-alist-new year))
      (setq cal-china-x-solar-term-year
            (extract-calendar-year
             (caar cal-china-x-solar-term-alist)))))


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


;; setup
(cal-china-x-setup)

(provide 'cal-china-x)

;;; Local Variables: ***
;;; coding: utf-8 ***
;;; End: ***

;;; cal-china-x.el ends here
