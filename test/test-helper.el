;;; test-helper.el --- Helper functions to test ros  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Max Beutelspacher

;; Author: Max Beutelspacher <max.beutelspacher@mailbox.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(declare-function undercover "undercover")

(when (require 'undercover nil t)
  (undercover "ros.el"))

(defun wait-for-roscore (&optional iteration-limit)
  "Wait until roscore is available or ITERATION-LIMIT reached."
  (let ((iteration 0))
    (while (and (not (ros-process-roscore-running-p))
                (or (not iteration-limit)
                    (< iteration iteration-limit)))
      (progn
        (setq iteration (1+ iteration))
        (sleep-for 0.1)))))

(defmacro with-roscore (&rest forms)
  "Run FORMS with running roscore."
  `(let ((process (when (not (ros-process-roscore-running-p))(start-process-shell-command "roscore" "roscore"
                                                                                          "roscore"))))
     (when process (wait-for-roscore 50))
     ,@forms
     ))



;;; test-helper.el ends here
