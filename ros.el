;;; ros.el --- Package to interact with and write code for ROS systems -*- lexical-binding: t -*-

;; Copyright (C) 2019 Max Beutelspacher

;; Author: Max Beutelspacher <max.beutelspacher@mailbox.org>
;; URL: https://github.com/DerBeutlin/ros.el
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))

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

;; A package to ease the interaction ROS nodes and the development of ROS software.
;; This includes the interaction with the catkin build system, interactively exploring ROS nodes,
;; messages, topics, and services, as well as some convenience functions when coding for ROS in
;; C++ and Python.

;;; Code:

(require 'with-shell-interpreter)
(require 's)
(require 'subr-x)
(require 'kv)
(require 'cl-lib)
(require 'transient)

(defgroup ros nil "Related to the Robot Operating System."
  :group 'external)

(defvar ros-current-workspace nil "Path to binary/devel directory of current catkin workspace.")

(defvar ros-version "melodic" "Name of the ros version.")

(defvar ros-current-profile "default" "Name of the current profile in `ros-current-workspace'.")

(defvar ros-current-tramp-prefix nil "Prefix to all paths such that tramp can be used to run commands on remote systems.")

(defvar ros-workspaces '("localhost" . nil) "Assoc list of candidates for `ros-current-workspace' grouped by `ros-current-tramp-prefix'.")

(defun ros-shell-command-to-string (cmd &optional source)
  "Source `ros-current-workspace' if SOURCE run CMD and return output as string.

Run in `ros-current-workspace' on `ros-current-tramp-prefix'
or the host system if `ros-current-tramp-prefix' is nil."
  (let ((command (if source (format "%s && %s" (ros-shell-source-command) cmd) cmd)))
  (s-trim (with-shell-interpreter :path (concat ros-current-tramp-prefix ros-current-workspace):form
            (shell-command-to-string (format "/bin/bash -c \"%s\"" command))))))

(defun ros-shell-command-to-list (cmd &optional source)
  "Source `ros-current-workspace' if SOURCE run CMD and return output as list.

Run in `ros-current-workspace' on `ros-current-tramp-prefix'
or the host system if `ros-current-tramp-prefix' is nil."
    (split-string (ros-shell-command-to-string cmd source)))


(defun ros-catkin-locate-devel ()
  "Return the path to the devel folder in `ros-current-workspace' with profile `ros-current-profile'."
  (s-trim (ros-shell-command-to-string (concat "catkin locate -d --profile " ros-current-profile))))

(defun ros-shell-source-command ()
  "Return the source command to source workspace.

Source `ros-current-workspace' with `ros-current-profile' or
if `ros-current-workspace' is nil, source /opt/ros/`ros-version'/setup.bash instead."
  (if ros-current-workspace
      (format "source %s/setup.bash"
              (ros-catkin-locate-devel))
    (format "source /opt/ros/%s/setup.bash" ros-version)))

(defun ros-catkin-list-profiles (workspace)
  "Get list of profiles for WORKSPACE."
  (let ((ros-current-workspace workspace))
    (ros-shell-command-to-list "catkin profile list -u")))

(defun ros-set-workspace ()
  "Read `ros-current-tramp prefix', `ros-current-workspace' and `ros-current-profile' from `ros-workspaces' and set the corresponding variables."
  (interactive)
  (let* ((tramp-prefix (completing-read "Tramp prefix: " (kvalist->keys ros-workspaces) nil t nil nil (if ros-current-tramp-prefix ros-current-tramp-prefix "localhost")))
         (workspace (completing-read "Workspace: " (cdr (assoc tramp-prefix ros-workspaces)) nil t nil nil ros-current-workspace))
         (profile (completing-read "Profile: " (ros-catkin-list-profiles workspace) nil t nil nil ros-current-profile)))
    (setq ros-current-tramp-prefix (when (not(string= tramp-prefix "localhost")) tramp-prefix))
    (setq ros-current-workspace workspace)
    (setq ros-current-profile profile)))

(defun ros-packages-list ()
  "List all the available Ros packages."
  (ros-shell-command-to-list "rospack list-names" t))

(defun ros-packages-location-list ()
  "Return assocation list of all the available Ros packages and their paths."
  (kvplist->alist (split-string (ros-shell-command-to-string "rospack list" t))))

(defun ros-completing-read-ros-package()
  "Completing read function for `ros-packages-list'."
  (completing-read "Package: " (ros-packages-list) nil t nil nil (ros-current-package)))

(defun ros-completing-read-ros-package-path()
  "Completing read function for `ros-packages-location-list' locations."
  (let* ((locations (ros-packages-location-list))
         (package (completing-read "Package: " (kvalist->keys locations) nil t)))
    (cdr (assoc package locations))))

(defun ros-packages-go-to-package(path)
  "Read package and open PATH to package in file manager."
  (interactive (list (ros-completing-read-ros-package-path)))
  (find-file (concat ros-current-tramp-prefix path)))

(defun ros-current-package ()
  "Return the name of the ROS package the current buffer lies in.
If the current buffer does not lie in a ROS package return nil."
  (let* ((package-path (locate-dominating-file default-directory "package.xml")))
    (when package-path (ros-parse-package-xml-for-package (concat package-path "package.xml")))))

(defun ros-parse-package-xml-for-package (path)
  "Parse package.xml in PATH for package name."
  (with-temp-buffer
    (insert-file-contents path)
    (string-match  "<name>\\(.*\\)</name>" (buffer-string))
    (match-string 1 (buffer-string))))

(cl-defun ros-catkin-dump-action (&key tramp-prefix workspace profile verb flags args post-cmd)
  "Dump action keys in a association list."
  (list (cons "tramp-prefix" tramp-prefix)
   (cons "workspace"  workspace)
   (cons "profile" profile)
   (cons "verb" verb)
   (cons "flags" flags)
   (cons "args" args)
   (cons "post-cmd" post-cmd)))

(defun ros-catkin-load-action (action)
  "Generate catkin command from ACTION."
  (let* ((ros-current-tramp-prefix (cdr(assoc "tramp-prefix" action)))
         (ros-current-workspace (cdr(assoc "workspace" action)))
         (ros-current-profile (cdr(assoc "profile" action)))
         (verb (cdr(assoc "verb" action)))
         (flags (cdr(assoc "flags" action)))
         (args (cdr(assoc "args" action)))
         (post-cmd (cdr(assoc "post-cmd" action)))
         (source-command (ros-shell-source-command)))
    (concat source-command " && catkin " verb " --profile " ros-current-profile " " (when flags (concat (string-join flags " ") " ")) args (when post-cmd (concat " && " post-cmd)))))

(defvar ros-catkin-action-history '() "List of catkin actions sorted by recenctness.")

(defun ros-catkin-display-action (action)
  "Display string which describes the ACTION."
  (let ((tramp-prefix (cdr(assoc "tramp-prefix" action)))
         (workspace (cdr(assoc "workspace" action)))
         (profile (cdr(assoc "profile" action)))
         (verb (cdr(assoc "verb" action)))
         (flags (cdr(assoc "flags" action)))
         (args (cdr(assoc "args" action)))
         (post-cmd (cdr(assoc "post-cmd" action))))
    (format "%s | %s | %s | catkin %s %s %s %s" (if tramp-prefix tramp-prefix "localhost") workspace profile verb (string-join flags " ") args (if post-cmd (concat "&& " post-cmd) ""))))

(defun ros-catkin-compare-actions (action1 action2)
  "Comparison function to compare ACTION1 and ACTION2."
  (string= (ros-catkin-display-action action1) (ros-catkin-display-action action2)))

(defun ros-catkin-push-action-to-history (action)
  "Push ACTION to the front of `ros-catkin-action-history'.

Further occurrences are removed."
  (when (member action ros-catkin-action-history) (setq ros-catkin-action-history (remove action ros-catkin-action-history)))
  (push action ros-catkin-action-history))

(defun ros-catkin-completing-read-action-from-history ()
  "Completing read function for `ros-catkin-action-history'."
  (let* ((history-strings (cl-mapcar 'ros-catkin-display-action ros-catkin-action-history))
         (action-string (completing-read "Action: " history-strings nil t))
         (index (seq-position history-strings action-string)))
    (nth index ros-catkin-action-history)))

(defun ros-catkin-compile (action)
  "Compile ACTION and push it to `ros-catkin-action-history'.

If called interactively prompt for action from history."
  (interactive (list(ros-catkin-completing-read-action-from-history)))
  (let* ((tramp-prefix (cdr(assoc "tramp-prefix" action)))
         (workspace (cdr(assoc "workspace" action)))
         (default-directory (concat tramp-prefix workspace)))
    (ros-catkin-push-action-to-history action)
    (compile (format "/bin/bash -c \"%s\""(ros-catkin-load-action action)))))

(cl-defun ros-catkin-build-action (&key package flags)
"Generate a build action to build PACKAGE with FLAGS."
(ros-catkin-dump-action :tramp-prefix ros-current-tramp-prefix :workspace ros-current-workspace :profile ros-current-profile :verb "build" :args package :flags flags :post-cmd nil))

(defun ros-catkin-run-build (package &optional flags)
  "Run a build action to build PACKAGE with FLAGS."
  (interactive (list (ros-completing-read-ros-package) (transient-args 'ros-catkin-build-transient)))
  (ros-catkin-compile (ros-catkin-build-action :package package :flags flags)))

(defun ros-catkin-run-build-current-workspace (&optional flags)
  "Run a build action to build the current workspace with FLAGS."
  (interactive (list (transient-args 'ros-catkin-build-transient)))
  (ros-catkin-run-build " " flags))

(cl-defun ros-catkin-test-action (&key package flags)
  "Generate a test action to test PACKAGE with FLAGS."
  (ros-catkin-dump-action :tramp-prefix ros-current-tramp-prefix :workspace ros-current-workspace :profile ros-current-profile :verb "build" :args (format "%s --no-deps --catkin-make-args run_tests" package) :flags flags :post-cmd (concat "catkin_test_results build/" package)))

(defun ros-catkin-run-test (package &optional flags)
  "Run a-test action to-test PACKAGE with FLAGS."
  (interactive (list (ros-completing-read-ros-package) (transient-args 'ros-catkin-build-transient)))
  (ros-catkin-compile (ros-catkin-test-action :package package :flags flags)))

(define-infix-argument ros-catkin-build-transient:--jobs()
  :description "Jobs"
  :class 'transient-option
  :shortarg "-j"
  :argument "--jobs "
  :reader 'transient-read-number-N+)

(define-infix-argument ros-catkin-build-transient:--limit-status-rate()
  :description "Limit of the update rate of the status"
  :class 'transient-option
  :shortarg "-lr"
  :argument "--limit-status-rate "
  :reader 'transient-read-number-N+)

(define-transient-command ros-catkin-build-transient ()
  "Transient command for catkin build."
  ["Arguments"
   ("-c" "continue" "--continue")
   ("-fc" "force-cmake" "--force-cmake")
   ("-v" "verbose" "--verbose")
   (ros-catkin-build-transient:--limit-status-rate)
   (ros-catkin-build-transient:--jobs)
   ]
  ["Actions"
   ("p" "Build a package" ros-catkin-run-build)
   ("w" "Build current workspace" ros-catkin-run-build-current-workspace)
   ("t" "Test a package" ros-catkin-run-test)
   ])

(cl-defun ros-catkin-clean-action (&key package flags)
  "Generate a clean action to clean PACKAGE with FLAGS."
  (ros-catkin-dump-action :tramp-prefix ros-current-tramp-prefix :workspace ros-current-workspace :profile ros-current-profile :verb "clean" :args package :flags flags :post-cmd nil))

(defun ros-catkin-run-clean (package &optional flags)
  "Run a clean action to clean PACKAGE with FLAGS."
  (interactive (list (ros-completing-read-ros-package) (transient-args 'ros-catkin-clean-transient)))
  (ros-catkin-compile (ros-catkin-clean-action :package package :flags flags)))

(defun ros-catkin-run-clean-current-workspace (&optional flags)
  "Run a clean action to clean the current workspace with FLAGS."
  (interactive (list (transient-args 'ros-catkin-clean-transient)))
  (ros-catkin-run-clean " " flags))

(define-transient-command ros-catkin-clean-transient ()
  "Transient command for catkin clean."
  ["Arguments"
   ("-v" "verbose" "--verbose")
   ("-n" "dry-run" "--dry-run")
   ("-l" "restrict to log space" "--logs")
   ("-b" "restrict to build space" "--build")
   ("-d" "restrict to devel space" "--devel")
   ("-i" "restrict to install space" "--install")
   ("-o" "remove orphans" "--orphans")
   ]
  ["Actions"
   ("p" "Clean a package" ros-catkin-run-clean)
   ("w" "Clean current workspace" ros-catkin-run-clean-current-workspace)
   ])




(provide 'ros)

;;; ros.el ends here
