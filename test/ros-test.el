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

;; (ert-deftest ros-source-workspace-command-is-correct()
;; (let ((ros-current-workspace "~/git/catkin/ipa")
;;       (ros-current-profile "debug"))
;;   (should (string= (ros-source-workspace-command) "source ~/git/catkin/ipa/devel_debug/setup.bash"))))

(ert-deftest ros-shell-command-to-string-on-host-machine
    nil
  (should (string= (s-trim (shell-command-to-string "ls /"))
                   (ros-shell-command-to-string "ls /"))))

;; (ert-deftest ros-shell-command-to-string-on-remote-machine
;;     nil
;;   (let ((ros-current-tramp-prefix "/ssh:kontron:")
;;         (ros-current-workspace "~/"))
;;     (should (string= (ros-shell-command-to-string "hostname")
;;                      "agv-57856.agv"))))

(ert-deftest ros-shell-command-can-source-ros
    nil
  (should (string= (ros-shell-command-to-string "source /opt/ros/melodic/setup.bash")
                   "")))

(ert-deftest ros-catkin-locate-devel-works
    nil
  (let ((ros-current-workspace "~/git/catkin/ipa")
        (ros-current-profile "debug"))
    (should (string= (ros-catkin-locate-devel)
                     (expand-file-name "~/git/catkin/ipa/devel_debug")))))

(ert-deftest ros-shell-source-command nil
  (let ((ros-current-workspace "~/git/catkin/ipa")
        (ros-current-profile "debug"))
    (should (string= (ros-shell-source-command)
                     (format "source %s" (expand-file-name "~/git/catkin/ipa/devel_debug/setup.bash"))))))

(ert-deftest ros-shell-source-command-default nil
  (let ((ros-current-workspace nil))
    (should (string= (ros-shell-source-command)
                     (format "source %s" (expand-file-name "/opt/ros/melodic/setup.bash"))))))

(ert-deftest ros-catkin-list-profiles nil
    (should (member "default" (ros-catkin-list-profiles "~/git/catkin/ipa"))))

(ert-deftest ros-list-packages nil
  (should (member "move_base" (ros-packages-list))))

(ert-deftest ros-list-packages nil
  (let ((ros-current-workspace "~/git/catkin/ipa"))
    (should (member "ipa_navigation_config" (ros-packages-list)))))

(ert-deftest ros-list-package-locations nil
  (let ((package-location-list (ros-packages-location-list)))
    (should (member "move_base" (kvalist->keys package-location-list)))
    (should (string= (cdr (assoc "move_base" package-location-list)) "/opt/ros/melodic/share/move_base"))))

(ert-deftest ros-current-package nil
  (let ((default-directory "/opt/ros/melodic/share/move_base/cmake"))
    (should (string= (ros-current-package) "move_base"))))

(ert-deftest ros-catkin-dump-action nil
  (let* ((tramp-prefix "/ssh:remote:")
        (workspace "~/git/catkin/ipa")
        (profile "default")
        (verb "build")
        (flags "-c")
        (args "move_base")
        (post-cmd "echo Test")
        (action (ros-catkin-dump-action :tramp-prefix tramp-prefix :workspace workspace :profile profile :verb verb :flags flags :args args :post-cmd post-cmd)))
    (should (string= (cdr (assoc "tramp-prefix" action)) tramp-prefix))
    (should (string= (cdr (assoc "workspace" action)) workspace))
    (should (string= (cdr (assoc "profile" action)) profile))
    (should (string= (cdr (assoc "verb" action)) verb))
    (should (string= (cdr (assoc "flags" action)) flags))
    (should (string= (cdr (assoc "args" action)) args))
    (should (string= (cdr (assoc "post-cmd" action)) post-cmd))))

(ert-deftest ros-catkin-compile-command-from-action ()
    (let* ((tramp-prefix nil)
           (workspace "~/git/catkin/ipa")
           (profile "default")
           (verb "build")
           (flags "-c")
           (args "move_base")
           (post-cmd "echo Test")
           (action (ros-catkin-dump-action :tramp-prefix tramp-prefix :workspace workspace :profile profile :verb verb :flags flags :args args :post-cmd post-cmd)))
      (should (string= (ros-catkin-load-action action) (format "source %s && catkin build --profile default -c move_base && echo Test" (expand-file-name "~/git/catkin/ipa/devel/setup.bash"))))))

(ert-deftest ros-catkin-display-action ()
  (let* ((tramp-prefix nil)
         (workspace "~/git/catkin/ipa")
         (profile "default")
         (verb "build")
         (flags "-c")
         (args "move_base")
         (post-cmd "echo Test")
         (action (ros-catkin-dump-action :tramp-prefix tramp-prefix :workspace workspace :profile profile :verb verb :flags flags :args args :post-cmd post-cmd)))
    (should (string= (ros-catkin-display-action action) (format "localhost | %s | %s | catkin %s %s %s && %s" workspace profile verb flags args post-cmd))))
  )

(ert-deftest ros-catkin-push-action-to-history ()
  (let* ((tramp-prefix nil)
         (workspace "~/git/catkin/ipa")
         (profile "default")
         (verb "build")
         (flags "-c")
         (args "move_base")
         (post-cmd "echo Test")
         (action (ros-catkin-dump-action :tramp-prefix tramp-prefix :workspace workspace :profile profile :verb verb :flags flags :args args :post-cmd post-cmd)))
    (should (eq (length ros-catkin-action-history) 0))
    (ros-catkin-push-action-to-history action)
    (should (eq (length ros-catkin-action-history) 1))
    (ros-catkin-push-action-to-history action)
    (should (eq (length ros-catkin-action-history) 1))
    (let ((action2 (ros-catkin-dump-action :tramp-prefix tramp-prefix :workspace workspace :profile "debug" :verb verb :flags flags :args args :post-cmd post-cmd)))
      (ros-catkin-push-action-to-history action2)
      (should (eq (length ros-catkin-action-history) 2))
      (ros-catkin-push-action-to-history action)
      (should (eq (length ros-catkin-action-history) 2))
      (should (eq (car ros-catkin-action-history) action)))))


    (provide 'ros-test)

;;; ros-test.el ends here
