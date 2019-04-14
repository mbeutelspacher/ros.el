;;; ros-test.el --- Tests for ros.el

;; Copyright (C) 2013 Max Beutelspacher

;; Author: Max Beutelspacher <max.beutelspacher@mailbox.org>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Tests for ros.el

;;; Code:

(require 'ert)
(require 'ros)

(ert-deftest ros-test-default-workspace-is-used-when-no-workspace-is-set ()
  (let ((ros-current-workspace nil)
        (ros-default-workspace "~/catkin_ws"))
    (should (string= (ros-current-workspace) ros-default-workspace))))

(ert-deftest ros-test-current-workspace-is-used-if-set ()
  (let ((ros-default-workspace "~/catkin_ws")
        (ros-current-workspace "~/catkin_ws2"))
    (should (string= (ros-current-workspace) ros-current-workspace)))
  )

(ert-deftest ros-test-new-workspace-can-be-selected ()
  (let ((ros-current-workspace "~/catkin_ws"))
    (ros-select-workspace "~/catkin_ws2")
    (should (string= (ros-current-workspace) "~/catkin_ws2")))
  )


(ert-deftest ros-test-shell-output-as-list ()
  (should (cl-every 'string=
                    (ros-shell-output-as-list "for VAR in 1 2 3 4 5; do; echo $VAR;done;")
                    '("1" "2" "3" "4" "5"))))

(ert-deftest ros-test-package-list ()
  (should (member "roscpp" (ros-packages))))

(ert-deftest ros-sourcing-command-for-zsh()
  (skip-unless (string= (getenv "SHELL") "/usr/bin/zsh"))
  (should (string= (ros-source-workspace-command "/opt/ros/melodic") "source /opt/ros/melodic/setup.zsh")))

(ert-deftest ros-sourcing-command-for-zsh()
  (skip-unless (string= (getenv "SHELL") "/bin/bash"))
  (should (string= (ros-source-workspace-command "/opt/ros/melodic") "source /opt/ros/melodic/setup.bash")))

(ert-deftest ros-test-generic-list-msg  ()
  (should (member "std_msgs/String" (ros-generic-list "msg")))
  )

(ert-deftest ros-generic-get-msg-returns-msg ()
  (should (string= (s-trim (ros-generic-get-info "msg" "std_msgs/String")) "string data")))





(provide 'ros-test)

;;; ros-test.el ends here
