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

(ert-deftest ros-list-packages nil
  (let ((ros-current-workspace "~/git/catkin/ipa"))
    (should (member "ipa_navigation_config" (ros-catkin-packages-list)))
    (should (not(member "move_base" (ros-catkin-packages-list))))))

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
        (flags '("-c"))
        (args "move_base")
        (post-cmd "echo Test")
        (action (ros-catkin-dump-action :tramp-prefix tramp-prefix :workspace workspace :profile profile :verb verb :flags flags :args args :post-cmd post-cmd)))
    (should (string= (cdr (assoc "tramp-prefix" action)) tramp-prefix))
    (should (string= (cdr (assoc "workspace" action)) workspace))
    (should (string= (cdr (assoc "profile" action)) profile))
    (should (string= (cdr (assoc "verb" action)) verb))
    (should (string= (car(cdr (assoc "flags" action))) (car flags)))
    (should (string= (cdr (assoc "args" action)) args))
    (should (string= (cdr (assoc "post-cmd" action)) post-cmd))))

(ert-deftest ros-catkin-compile-command-from-action ()
    (let* ((tramp-prefix nil)
           (workspace "~/git/catkin/ipa")
           (profile "default")
           (verb "build")
           (flags '("-c"))
           (args "move_base")
           (post-cmd "echo Test")
           (action (ros-catkin-dump-action :tramp-prefix tramp-prefix :workspace workspace :profile profile :verb verb :flags flags :args args :post-cmd post-cmd)))
      (should (string= (ros-catkin-load-action action) (format "source %s && catkin build --profile default -c move_base && echo Test" (expand-file-name "~/git/catkin/ipa/devel/setup.bash"))))))

(ert-deftest ros-catkin-display-action ()
  (let* ((tramp-prefix nil)
         (workspace "~/git/catkin/ipa")
         (profile "default")
         (verb "build")
         (flags '("-c"))
         (args "move_base")
         (post-cmd "echo Test")
         (action (ros-catkin-dump-action :tramp-prefix tramp-prefix :workspace workspace :profile profile :verb verb :flags flags :args args :post-cmd post-cmd)))
    (should (string= (ros-catkin-display-action action) (format "localhost | %s | %s | catkin %s %s %s && %s" workspace profile verb (string-join flags " ") args post-cmd))))
  )

(ert-deftest ros-catkin-push-action-to-history ()
  (let* ((ros-catkin-action-history)
         (tramp-prefix nil)
         (workspace "~/git/catkin/ipa")
         (profile "default")
         (verb "build")
         (flags '("-c"))
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


(ert-deftest ros-catkin-build-action
    ()
  (let* ((ros-current-tramp-prefix "/ssh:remote:")
         (ros-current-workspace "~/git/catkin/ipa")
         (ros-current-profile "default")
         (action (ros-catkin-build-action :package "ipa_eband"
                                          :flags '("-c"))))
    (should (string= (cdr (assoc "tramp-prefix" action)) ros-current-tramp-prefix))
    (should (string= (cdr (assoc "workspace" action)) ros-current-workspace))
    (should (string= (cdr (assoc "profile" action)) ros-current-profile))
    (should (string= (cdr (assoc "verb" action)) "build"))
    (should (string= (cdr (assoc "args" action)) "ipa_eband"))
    (should (string= (car(cdr (assoc "flags" action))) "-c"))
    (should (eq (cdr (assoc "post-cmd" action)) nil))))

(ert-deftest ros-catkin-clean-action
    ()
  (let* ((ros-current-tramp-prefix "/ssh:remote:")
         (ros-current-workspace "~/git/catkin/ipa")
         (ros-current-profile "default")
         (action (ros-catkin-clean-action :package "ipa_eband"
                                          :flags '("-c"))))
    (should (string= (cdr (assoc "tramp-prefix" action)) ros-current-tramp-prefix))
    (should (string= (cdr (assoc "workspace" action)) ros-current-workspace))
    (should (string= (cdr (assoc "profile" action)) ros-current-profile))
    (should (string= (cdr (assoc "verb" action)) "clean"))
    (should (string= (cdr (assoc "args" action)) "ipa_eband"))
    (should (string= (car(cdr (assoc "flags" action))) "-c"))
    (should (eq (cdr (assoc "post-cmd" action)) nil))))

(ert-deftest ros-catkin-test-action
    ()
  (let* ((ros-current-tramp-prefix "/ssh:remote:")
         (ros-current-workspace "~/git/catkin/ipa")
         (ros-current-profile "default")
         (action (ros-catkin-test-action :package "ipa_eband" :flags '("-j 10"))))
    (should (string= (cdr (assoc "tramp-prefix" action)) ros-current-tramp-prefix))
    (should (string= (cdr (assoc "workspace" action)) ros-current-workspace))
    (should (string= (cdr (assoc "profile" action)) ros-current-profile))
    (should (string= (cdr (assoc "verb" action)) "build"))
    (should (string= (cdr (assoc "args" action)) "ipa_eband --no-deps --catkin-make-args run_tests"))
    (should (string= (car(cdr (assoc "flags" action))) "-j 10"))
    (should (string= (cdr (assoc "post-cmd" action)) "catkin_test_results build/ipa_eband"))))

(ert-deftest ros-generic-list-type-correct()
  (should-error (ros-generic-list "foo")))

(ert-deftest ros-generic-list-msg ()
    (should (member "std_msgs/String" (ros-generic-list "msg"))))

(ert-deftest ros-generic-list-srv ()
  (should (member "std_srvs/Empty" (ros-generic-list "srv"))))

(ert-deftest ros-generic-list-topic ()
  (should (member "/rosout" (ros-generic-list "topic"))))

(ert-deftest ros-generic-list-service ()
  (should (member "/rosout/get_loggers" (ros-generic-list "service"))))

(ert-deftest ros-generic-list-node ()
  (should (member "/rosout" (ros-generic-list "node"))))

(ert-deftest ros-generic-inf-type-correct()
  (should-error (ros-generic-info "foo" "foobar")))

(ert-deftest ros-generic-info-msg ()
  (should (string-match-p (regexp-quote "string data") (ros-generic-info "msg" "std_msgs/String"))))

(ert-deftest ros-generic-info-srv ()
  (should (string-match-p (regexp-quote "bool data") (ros-generic-info "srv" "std_srvs/SetBool"))))

(ert-deftest ros-generic-info-srv-with-flags ()
  (should (string-match-p (regexp-quote "bool data # e.g. for hardware enabling / disabling") (ros-generic-info "srv" "std_srvs/SetBool" '("-r")))))

(ert-deftest ros-generic-info-topic ()
  (should (string-match-p (regexp-quote "Type: rosgraph_msgs/Log") (ros-generic-info "topic" "/rosout"))))

(ert-deftest ros-generic-info-node ()
  (should (string-match-p (regexp-quote " * /rosout_agg [rosgraph_msgs/Log]") (ros-generic-info "node" "/rosout"))))

(ert-deftest ros-generic-info-service ()
  (should (string-match-p (regexp-quote "Type: roscpp/GetLoggers") (ros-generic-info "service" "/rosout/get_loggers"))))



    (provide 'ros-test)

;;; ros-test.el ends here
