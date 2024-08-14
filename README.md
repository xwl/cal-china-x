chinese calendar
================

This extension mainly adds the following extra features:
  - Chinese localizations
  - Display holiday, lunar(农历), horoscope(星座), zodiac(属相、十二生肖), solar term(节气) info on mode line
  - Define holidays using `holiday-lunar`, `holiday-solar-term`
  - Highlight holidays based on different priorities
  - Add `cal-china-x-chinese-holidays`, `cal-china-x-japanese-holidays`.
  - custom week diary(like weeks in school)

To use, add something like the following to your .emacs:

    (require 'cal-china-x)
    (setq calendar-mark-holidays-flag t)
    (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
    (setq cal-china-x-general-holidays '((holiday-lunar 1 15 "元宵节")))
    (setq calendar-holidays
          (append cal-china-x-important-holidays
                  cal-china-x-general-holidays
                  other-holidays))

Here is a screenshot:

![chinese calendar](/../screenshot/cal-china-x.png?raw=true "")

Also available in [melpa](http://melpa.org) .

For aligning Chinese and English font, [chinese-fonts-setup](https://github.com/tumashu/chinese-fonts-setup) may be helpful.
