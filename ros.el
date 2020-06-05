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
(require 'f)
(require 'subr-x)
(require 'kv)
(require 'cl-lib)
(require 'transient)
(require 'yaml-mode)
(require 'hydra)

(defgroup ros nil "Related to the Robot Operating System."
  :group 'external)

(defvar ros-current-workspace nil "Path to binary/devel directory of current catkin workspace.")

(defvar ros-version "melodic" "Name of the ros version.")

(defvar ros-current-profile "default" "Name of the current profile in `ros-current-workspace'.")

(defvar ros-current-tramp-prefix nil "Prefix to all paths such that tramp can be used to run commands on remote systems.")

(defvar ros-workspaces '("localhost" . nil) "Assoc list of candidates for `ros-current-workspace' grouped by `ros-current-tramp-prefix'.")

(defun ros-shell-command-to-string (cmd &optional not-source)
  "Source workspace unless NOT-SOURCE run CMD, return output as string.

Use`ros-current-workspace' as sourced workspace.
Run in `ros-current-workspace' on `ros-current-tramp-prefix'
or the host system if `ros-current-tramp-prefix' is nil."
  (let ((command (if not-source  cmd (format "%s && %s" (ros-shell-source-command) cmd))))
  (s-trim (with-shell-interpreter :path (concat ros-current-tramp-prefix ros-current-workspace):form
            (shell-command-to-string (format "/bin/bash  -c \"%s\" | sed -r \"s/\x1B\\[([0-9]{1,3}(;[0-9]{1,2})?)?[mGK]//g\"" command))))))

(defun ros-shell-command-to-list (cmd &optional not-source)
  "Source workspace unless NOT-SOURCE run CMD and return output as list.

Use`ros-current-workspace' as sourced workspace.
Run in `ros-current-workspace' on `ros-current-tramp-prefix'
or the host system if `ros-current-tramp-prefix' is nil."
  (split-string (ros-shell-command-to-string cmd not-source) "\n" t "[\f\t\n\r\v\\]+"))

(defun ros-process-run (cmd buffer &optional not-source)
  "Source workspace unless NOT-SOURCE run CMD and print output in BUFFER.

Use`ros-current-workspace' as sourced workspace.
Run in `ros-current-workspace' on `ros-current-tramp-prefix'
or the host system if `ros-current-tramp-prefix' is nil."
  (let* ((command (if not-source cmd (format "%s && %s" (ros-shell-source-command) cmd) ))
         (process (with-shell-interpreter :path (concat ros-current-tramp-prefix ros-current-workspace):form
                                          (start-process-shell-command buffer buffer (format "/bin/bash  -c \"%s\"" command)))))

    (view-buffer-other-window (process-buffer process))
    (ros-process-mode)))

(defun ros-process-kill-buffer-process (buffer)
  "Kill the process associated with BUFFER."
  (interactive (list (current-buffer)))
  (let ((process (get-buffer-process buffer)))
    (kill-process process)))


(defun ros-catkin-locate-devel ()
  "Return the path to the devel folder in `ros-current-workspace' with profile `ros-current-profile'."
  (s-trim (ros-shell-command-to-string (concat "catkin locate -d --profile " ros-current-profile) t)))

(defun ros-shell-source-command ()
  "Return the source command to source workspace.

Source `ros-current-workspace' with `ros-current-profile' or
if `ros-current-workspace' is nil, source /opt/ros/`ros-version'/setup.bash instead."
  (if ros-current-workspace
      (format "source %s/setup.bash"
              (ros-catkin-locate-devel))
    (format "source /opt/ros/%s/setup.bash" ros-version)))

(defun ros-catkin-list-profiles (workspace tramp-prefix)
  "Get list of profiles for WORKSPACE and TRAMP-PREFIX."
  (let ((ros-current-workspace workspace)
        (ros-current-tramp-prefix tramp-prefix))
    (ros-shell-command-to-list "catkin profile list -u" t)))

(defun ros-set-workspace ()
  "Read `ros-current-tramp prefix', `ros-current-workspace' and `ros-current-profile' from `ros-workspaces' and set the corresponding variables."
  (interactive)
  (let* ((tramp-prefix-user (completing-read "Tramp prefix: " (kvalist->keys ros-workspaces) nil t nil nil (if ros-current-tramp-prefix ros-current-tramp-prefix "localhost")))
         (tramp-prefix (when (not (string= tramp-prefix-user "localhost")) tramp-prefix-user))
         (workspace (completing-read "Workspace: " (cdr (assoc tramp-prefix-user ros-workspaces)) nil t nil nil ros-current-workspace))
         (profile (completing-read "Profile: " (ros-catkin-list-profiles workspace tramp-prefix) nil t nil nil ros-current-profile)))
    (setq ros-current-tramp-prefix tramp-prefix)
    (setq ros-current-workspace workspace)
    (setq ros-current-profile profile)))

(defun ros-packages-list ()
  "List all the available Ros packages."
  (ros-shell-command-to-list "rospack list-names"))

(defun ros-packages-location-list ()
  "Return assocation list of all the available Ros packages and their paths."
  (kvplist->alist (split-string (ros-shell-command-to-string "rospack list"))))

(defun ros-packages-locate-package (package)
  "Return path to PACKAGE."
  (ros-shell-command-to-string (concat "rospack find " package)))

(defun ros-list-files-in-directory (directory)
  "Return list of all files in DIRECTORY and its subdirectories."
  (ros-shell-command-to-list (format "find %s" directory)))

(defun ros-packages-list-files-in-package (package)
  "Return list of files in PACKAGE.

Paths will be relative to root of package."
  (let ((path (ros-packages-locate-package package)))
    (mapcar (lambda (f) (file-relative-name f path)) (seq-filter (lambda (f) (not (s-ends-with-p ".idx" f))) (ros-list-files-in-directory path)))))

(defun ros-packages-find-file-in-package (package)
  "Find file in PACKAGE."
  (interactive (list (ros-completing-read-ros-package)))
  (let ((path (ros-packages-locate-package package))
        (file (completing-read "File: " (ros-packages-list-files-in-package package) nil t)))
    (find-file (concat ros-current-tramp-prefix (file-name-as-directory path) file))))

(defun ros-packages-find-file-in-current-package ()
  "Find file in PACKAGE."
  (interactive)
  (ros-packages-find-file-in-package (ros-current-package)))

(defun ros-completing-read-ros-package()
  "Completing read function for `ros-packages-list'."
  (let ((collection (ros-packages-list))
        (current-package (ros-current-package)))
    (completing-read "Package: " collection nil t nil nil (when (member current-package collection) current-package))))

(defun ros-completing-read-ros-package-path()
  "Completing read function for `ros-packages-location-list' locations."
  (let* ((locations (ros-packages-location-list))
         (collection (kvalist->keys locations))
         (current-package (ros-current-package))
         (package (completing-read "Package: " collection nil t nil nil (when (member current-package collection) current-package))))
    (cdr (assoc package locations))))

(defun ros-catkin-packages-list ()
  "List all the Ros packages in `ros-current-workspace'."
  (ros-shell-command-to-list "catkin list --unformatted --quiet"))

(defun ros-catkin-completing-read-ros-package()
  "Completing read function for `ros-catkin-packages-list'."
  (let ((collection (ros-catkin-packages-list))
        (current-package (ros-current-package)))
    (completing-read "Package: " collection nil t nil nil (when (member current-package collection) current-package))))

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
  (interactive (list (ros-catkin-completing-read-ros-package) (transient-args 'ros-catkin-build-transient)))
  (ros-catkin-compile (ros-catkin-build-action :package package :flags flags)))

(defun ros-catkin-run-build-current-workspace (&optional flags)
  "Run a build action to build the current workspace with FLAGS."
  (interactive (list (transient-args 'ros-catkin-build-transient)))
  (ros-catkin-run-build " " flags))

(cl-defun ros-catkin-build-test-action (&key package flags)
  "Generate a test action to build tests for PACKAGE with FLAGS."
  (ros-catkin-dump-action :tramp-prefix ros-current-tramp-prefix :workspace ros-current-workspace :profile ros-current-profile :verb "build" :args (format "%s --no-deps --catkin-make-args tests" package) :flags flags :post-cmd nil ))

(cl-defun ros-catkin-test-action (&key package flags)
  "Generate a test action to test PACKAGE with FLAGS."
  (ros-catkin-dump-action :tramp-prefix ros-current-tramp-prefix :workspace ros-current-workspace :profile ros-current-profile :verb "build" :args (format "%s --no-deps --catkin-make-args run_tests" package) :flags flags :post-cmd (concat "catkin_test_results build/" package)))

(cl-defun ros-catkin-test-action-single-rostest (&key package flags rostest)
  "Generate a test action to build and run ROSTEST in PACKAGE with FLAGS."
  (ros-catkin-dump-action :tramp-prefix ros-current-tramp-prefix :workspace ros-current-workspace :profile ros-current-profile :verb "build" :args (format "%s --no-deps --make-args %s" package (file-name-sans-extension rostest)) :flags flags :post-cmd (format "rostest %s %s" package rostest)))

(cl-defun ros-catkin-test-action-single-gtest (&key package flags gtest regexp)
  "Generate a test action to build and run GTEST matching REGEXP in PACKAGE with FLAGS."
  (ros-catkin-dump-action :tramp-prefix ros-current-tramp-prefix :workspace ros-current-workspace :profile ros-current-profile :verb "build" :args (format "%s --no-deps --make-args %s" package gtest) :flags flags :post-cmd (format "rosrun %s %s %s" package gtest (if regexp (concat "--gtest_filter=" regexp) ""))))

(defun ros-catkin-run-test (package &optional flags)
  "Run a test action to-test PACKAGE with FLAGS."
  (interactive (list (ros-catkin-completing-read-ros-package) (transient-args 'ros-catkin-build-transient)))
  (ros-catkin-compile (ros-catkin-test-action :package package :flags flags)))

(defun ros-catkin-build-test (package &optional flags)
  "Build a test action to-test PACKAGE with FLAGS."
  (interactive (list (ros-catkin-completing-read-ros-package) (transient-args 'ros-catkin-build-transient)))
  (ros-catkin-compile (ros-catkin-build-test-action :package package :flags flags)))

(defun ros-catkin-run-single-rostest (package &optional flags)
  "Run a test action to run single rostest in PACKAGE with FLAGS."
  (interactive (list (ros-catkin-completing-read-ros-package) (transient-args 'ros-catkin-build-transient)))
  (let ((rostest (completing-read "Rostest: " (ros-catkin-list-rostests package) nil t)))
    (ros-catkin-compile (ros-catkin-test-action-single-rostest :package package :flags flags :rostest rostest))))

(defun ros-catkin-run-single-gtest (package &optional flags)
  "Run a test action to run single gtest in PACKAGE with FLAGS."
  (interactive (list (ros-catkin-completing-read-ros-package) (transient-args 'ros-catkin-build-transient)))
  (let ((gtest (completing-read "Gtest: " (ros-catkin-list-executables package) nil t)))
    (ros-catkin-compile (ros-catkin-test-action-single-gtest :package package :flags flags :gtest gtest))))

(define-infix-argument ros-catkin-build-transient:--jobs()
  :description "maximal number of jobs"
  :class 'transient-option
  :shortarg "-j"
  :argument "--jobs "
  :reader 'transient-read-number-N+)

(define-infix-argument ros-catkin-build-transient:--limit-status-rate()
  :description "limit of the update rate of the status"
  :class 'transient-option
  :shortarg "-lr"
  :argument "--limit-status-rate "
  :reader 'transient-read-number-N+)

(define-transient-command ros-catkin-build-transient ()
  "Transient command for catkin build."
  ["Arguments"
   ("-c" "continue on failure" "--continue-on-failure")
   ("-fc" "runs cmake explicitly for each catkin package" "--force-cmake")
   ("-v" "verbose" "--verbose")
   (ros-catkin-build-transient:--limit-status-rate)
   (ros-catkin-build-transient:--jobs)
   ]
  ["Actions"
   ("p" "Build a package" ros-catkin-run-build)
   ("w" "Build current workspace" ros-catkin-run-build-current-workspace)
   ("t" "Test a package" ros-catkin-run-test)
   ("T" "Build tests for a package" ros-catkin-build-test)
   ("r" "Build and run a single rostest" ros-catkin-run-single-rostest)
   ("g" "Build and run a single gtest" ros-catkin-run-single-gtest)
   ])

(cl-defun ros-catkin-clean-action (&key package flags)
  "Generate a clean action to clean PACKAGE with FLAGS."
  (ros-catkin-dump-action :tramp-prefix ros-current-tramp-prefix :workspace ros-current-workspace :profile ros-current-profile :verb "clean" :args package :flags flags :post-cmd nil))

(defun ros-catkin-clean-flags ()
  "Parse flags for ros-catkin-clean action."
  (cons "--yes" (transient-args  'ros-catkin-clean-transient)))

(defun ros-catkin-run-clean (package &optional flags)
  "Run a clean action to clean PACKAGE with FLAGS."
  (interactive (list (ros-catkin-completing-read-ros-package) (ros-catkin-clean-flags)))
  (ros-catkin-compile (ros-catkin-clean-action :package package :flags flags)))

(defun ros-catkin-run-clean-current-workspace (&optional flags)
  "Run a clean action to clean the current workspace with FLAGS."
  (interactive (list (ros-catkin-clean-flags)))
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

(cl-defun ros-catkin-config-action (&key flags)
  "Generate a config action to config `ros-current-workspace' with FLAGS."
  (ros-catkin-dump-action :tramp-prefix ros-current-tramp-prefix :workspace ros-current-workspace :profile ros-current-profile :verb "config" :args "" :flags flags :post-cmd nil))

(defun ros-catkin-run-config(&optional flags)
  "Run a clean action to clean the current workspace with FLAGS."
  (interactive (list (transient-args 'ros-catkin-config-transient)))
  (ros-catkin-compile (ros-catkin-config-action :flags flags)))

(define-infix-argument ros-catkin-config-transient:--DCMAKE_EXPORT_COMPILE_COMMANDS()
  :description "-DCMAKE_EXPORT_COMPILE_COMMANDS"
  :class 'transient-switches
  :key "-ec"
  :argument-format "--cmake-args -DCMAKE_EXPORT_COMPILE_COMMANDS=%s"
  :argument-regexp "--cmake-args -DCMAKE_EXPORT_COMPILE_COMMANDS=\\(ON\\|OFF\\)"
  :choices '("ON" "OFF"))

(define-infix-argument ros-catkin-config-transient:--DCMAKE_BUILD_TYPE()
  :description "-DCMAKE_BUILD_TYPE"
  :class 'transient-switches
  :key "-bt"
  :argument-format "--cmake-args -DCMAKE_BUILD_TYPE=%s"
  :argument-regexp "--cmake-args -DCMAKE_BUILD_TYPE=\\(Release\\|Debug\\|RelWithDebInfo\\|MinSizeRel\\)"
  :choices '("Release" "Debug" "RelWithDebInfo" "MinSizeRel"))

(define-transient-command ros-catkin-config-transient ()
  "Transient command for catkin config."
  ["Arguments"
   (ros-catkin-config-transient:--DCMAKE_EXPORT_COMPILE_COMMANDS)
   (ros-catkin-config-transient:--DCMAKE_BUILD_TYPE)
   ]
  ["Actions"
   ("c" "Clean a package" ros-catkin-run-config)
   ])

(defun ros-generic-assert-type (type)
  "Assert that TYPE is a valid type."
  (let ((candidates '("msg" "srv" "topic" "node" "service")))
    (when (not(member type candidates))
      (error (format "%s is not element of %s" type (string-join candidates ", "))))))

(defun ros-generic-list (type)
  "Return list of Ros TYPE.

TYPE can be \"msg\", \"srv\", \"topic\", \"node\",\"service\"."
  (ros-generic-assert-type type)
  (ros-shell-command-to-list (format "ros%s list" type)))

(defun ros-generic-completing-read (type)
  "Prompts for Ros TYPE.

TYPE can be \"msg\", \"srv\", \"topic\", \"node\",\"service\"."
  (let ((collection (ros-generic-list type))
        (thing-at-point (symbol-name(symbol-at-point))))
    (completing-read (format "%s: " type) collection nil t nil nil (when (member thing-at-point collection) thing-at-point))))

(defun ros-generic-info (type name &optional flags)
  "Return info about NAME of type TYPE with FLAGS.

TYPE can be \"msg\", \"srv\", \"topic\", \"node\",\"service\"."
  (ros-generic-assert-type type)
  (let ((command (if (or(string= type "msg")(string= type "srv")) "show" "info")))
    (ros-shell-command-to-string (format "ros%s %s %s %s" type command (string-join flags " ") name))))

(defun ros-generic-show-info (type name &optional flags)
  "Show info about NAME of type TYPE in new buffer."
  (let ((buffer-name (format "* ros-%s: %s" type name)))
    (when (get-buffer buffer-name) (kill-buffer buffer-name))
    (pop-to-buffer buffer-name))
  (erase-buffer)
  (insert (ros-generic-info type name flags))
  (ros-info-mode))

(defun ros-msg-show (msg &optional flags)
  "Prompt for MSG and show structure with FLAGS."
  (interactive (list (ros-generic-completing-read "msg") (transient-args 'ros-msg-srv-show-transient)))
  (ros-generic-show-info "msg" msg flags))

(define-infix-argument ros-msg-srv-show-transient:--bag()
  :description "show messages from .bag file"
  :class 'transient-option
  :shortarg "-b"
  :argument "--bag="
  :reader 'ros-transient-read-existing-file)

(define-transient-command ros-msg-srv-show-transient ()
  "Transient command for ros-topic-echo."
  ["Arguments"
   ("-r" "show raw message text, including comments" "--raw")
   (ros-msg-srv-show-transient:--bag)
   ]
  ["Actions"
   ("m" "show messages" ros-msg-show)
   ("s" "show srv" ros-srv-show)
   ])

(defun ros-topic-show (topic)
  "Prompt for TOPIC and show subscribers and publishers."
  (interactive (list (ros-generic-completing-read "topic")))
  (ros-generic-show-info "topic" topic))


(defun ros-service-show (service)
  "Prompt for active SERVICE and show structure."
  (interactive (list (ros-generic-completing-read "service")))
  (ros-generic-show-info "service" service))

(defun ros-srv-show (service &optional flags)
  "Prompt for (not necessarily active) SERVICE and show structure with FLAGS."
  (interactive (list (ros-generic-completing-read "srv") (transient-args 'ros-msg-srv-show-transient)))
  (ros-generic-show-info "srv" service flags))

(defun ros-node-show (node)
  "Prompt for NODE and show published and subscribed topics and provided services."
  (interactive (list (ros-generic-completing-read "node")))
  (ros-generic-show-info "node" node))


(define-derived-mode ros-info-mode messages-buffer-mode "ros-info-mode"
  "major mode for displaying ros info messages")

(define-derived-mode ros-process-mode fundamental-mode "ros-process-mode"
  "major mode when executing ros processes"
  (let ((process (get-buffer-process (current-buffer))))
    (when process (set-process-filter process 'ros-process-filter))))

(defun ros-process-filter (process string)
  "Function to automatically scroll in the ros-proces-mode.
The STRING of the output of PROCESS will be inserted into the buffer
and the point will be kept at the latest output."
  (let ((buffer (process-buffer process))
        (mark (process-mark process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char mark)
        (insert string)
        (set-marker mark (point))))))

(defun ros-topic-echo (topic &optional flags)
  "Prompt for TOPIC and echo this topic with FLAGS."
  (interactive (list (ros-generic-completing-read "topic") (transient-args 'ros-topic-echo-transient)))
  (let ((command (format "rostopic echo %s %s" (string-join flags " ") topic)))
    (message command)
    (ros-process-run command (format "*%s*" command))))

(defun ros-topic-echo-filtered ( &optional flags)
  "Prompt for TYPE and then for topic and echo this topic with FLAGS."
  (interactive (list (transient-args 'ros-topic-echo-transient)))
  (let*((type-topic-list (ros-topic-list-by-type))
        (type (completing-read "Type: " (delq nil(delete-dups(kvalist->keys type-topic-list))) nil t))
        (topic (completing-read "Topic: " (ros-topic-filter-by-type type type-topic-list) nil t)))
    (ros-topic-echo topic flags)))

(defun ros-transient-read-existing-file (prompt _initial-input _history)
  "Read an existing file with PROMPT."
  (expand-file-name (read-file-name prompt nil nil t)))


(define-infix-argument ros-topic-echo-transient:--bag()
  :description "echo messages from .bag file"
  :class 'transient-option
  :shortarg "-b"
  :argument "--bag="
  :reader 'ros-transient-read-existing-file)

(define-infix-argument ros-topic-echo-transient:--num_width()
  :description "fixed width for numeric values"
  :class 'transient-option
  :shortarg "-w"
  :argument "-w "
  :reader 'transient-read-number-N+)

(define-infix-argument ros-topic-echo-transient:--count()
  :description "number of messages to echo"
  :class 'transient-option
  :shortarg "-n"
  :argument "-n "
  :reader 'transient-read-number-N+)

(define-transient-command ros-topic-echo-transient ()
  "Transient command for ros-topic-echo."
  ["Arguments"
   (ros-topic-echo-transient:--bag)
   ("-p" "echo in a plotting friendly format" "-p")
   (ros-topic-echo-transient:--num_width)
   ("-ns" "omit strings" "--nostr")
   ("-na" "omit arrays" "--noarr")
   ("-c" "clear screen before printing message" "--clear")
   ("-a" "display all message in bag, only valid with -b option" "--all")
   (ros-topic-echo-transient:--count)
   ("-o" "display time as offsets from current time (in seconds)" "--offset")
   ]
  ["Actions"
   ("e" "echo" ros-topic-echo)
   ("f" "echo filtered" ros-topic-echo-filtered)
   ])

(defun ros-node-pid (node)
  "Get PID for Ros NODE."
  (ros-shell-command-to-string (format"rosnode info %s 2>/dev/null | grep Pid| cut -d' ' -f2" node)))

(defun ros-topic-service-assert-type (type)
  "Assert that TYPE is a valid type."
  (let ((candidates '("topic" "service")))
    (when (not(member type candidates))
      (error (format "%s is not element of %s" type (string-join candidates ", "))))))

(defun ros-topic-service-get-type (type name)
  "Get type of topic or service (TYPE) with NAME."
  (ros-topic-service-assert-type type)
  (ros-shell-command-to-string (format "ros%s type %s" type name)))

(defun ros-topic-list-by-type()
  "Return list of type-topic associations."
  (let* ((output (ros-shell-command-to-string (format "rostopic list -v")))
         (matches (s-match-strings-all "\\* \\(/.*\\) \\[\\(.*\\)]*\]" output)))
    (mapcar (lambda (x) (cons (cl-second(cdr x)) (cl-first(cdr x)))) matches)))

(defun ros-topic-filter-by-type (type &optional type-topic-list)
  "Return topics with type TYPE, TYPE-TOPIC-LIST can be reused."
  (let ((type-topic-list (if type-topic-list type-topic-list (ros-topic-list-by-type))))
    (mapcar 'cdr(kvalist->filter-keys type-topic-list type))))

(defun ros-catkin-list-executables(package)
  "List the names of all executables in PACKAGE."
  (let* ((path (concat (ros-catkin-locate-devel) "/lib/" package))
         (files (directory-files path t))
         (executables (seq-filter (lambda (x) (and (f-file-p x)(f-executable-p x))) files)))
    (mapcar 'file-name-nondirectory executables)))

(defun ros-catkin-list-rostests(package)
  "List the names of all rostests in PACKAGE."
  (let* ((path (concat(ros-packages-locate-package package) "/test")))
    (directory-files path nil ".*\\.xml")))
(defun ros-msg-insert-import (message)
  "Prompt for MESSAGE and include it in file."
  (interactive (list (ros-generic-completing-read "msg")))
  (ros-msg-srv-insert-import "msg" message))

(defun ros-srv-insert-import (service)
  "Prompt for SERVICE and include it in file."
  (interactive (list (ros-generic-completing-read "srv")))
  (ros-msg-srv-insert-import "srv" service))

(defun ros-msg-srv-insert-import (type name)
  "Insert TYPE (either msg or srv) definition for NAME in the current buffer."
  (let ((package (car (split-string name "/")))
        (item-name (car (cdr(split-string name "/")))))
    (cond ((string= major-mode "python-mode") (ros-msg-srv-insert-import-python type package item-name))
          ((string= major-mode "c++-mode") (ros-msg-srv-insert-import-cpp type package item-name))
          (t (message "Only works in Python and C++ mode")))))

(defun ros-topic-insert (topic)
  "Prompt for TOPIC and insert it at point."
  (interactive (list (ros-generic-completing-read "topic")))
  (insert topic))

(defun ros-msg-insert(name)
  "Insert  definition for msg NAME in the current buffer."
  (interactive (list (ros-generic-completing-read "msg")))
  (ros-msg-srv-insert name))

(defun ros-srv-insert (name)
  "Insert  definition for srv NAME in the current buffer."
  (interactive (list (ros-generic-completing-read "srv")))
  (ros-msg-srv-insert name))

(defun ros-msg-srv-insert (name)
  "Insert (either msg or srv) definition for NAME in the current buffer."
  (let ((package (car (split-string name "/")))
        (item-name (car (cdr (split-string name "/")))))
    (insert (format " %s::%s" package item-name))))

(defun ros-msg-srv-insert-import-python (type package name)
  "Insert TYPE (either msg or srv) definition for NAME which is part of PACKAGE in the current python buffer."
  (let ((start-import-statement (format "from %s.%s import" package type)))
    (when (not (ros-msg-srv-import-is-included-python-p type package name))
      (if (ros-msg-srv-import-search-same-package-import-python type package)
          (progn
            (goto-char (ros-msg-srv-import-search-same-package-import-python type package))
            (move-end-of-line nil)
            (insert (format ", %s" name)))
        (goto-char (ros-msg-srv-insert-import-python-best-import-location type))
        (end-of-line)
        (newline-and-indent)
        (insert (format "%s %s" start-import-statement name))))))

(defun ros-msg-srv-insert-import-python-best-import-location (type)
  "Return the best location for a python import of TYPE.
TYPE can be either msg or srv.
The best location would be another import of this TYPE,
the second best another import and lastly the beginning of the buffer."
  (or (ros-string-in-buffer (format "from .*\.%s import .*" type)) (ros-string-in-buffer "import") (point-min)))

(defun ros-string-in-buffer (string)
  "Return point where STRING is in the current buffer, nil otherwise."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward string nil t)))

(defun ros-msg-srv-import-is-included-python-p (type package name)
  "Return t if NAME in PACKAGE of TYPE is already included in the current python buffer."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (format "from %s.%s import .*%s[, \n]" package type name) nil t)))

(defun ros-msg-srv-import-search-same-package-import-python (type package)
  "Search for import of TYPE  of PACKAGE in the current buffer.
TYPE can be either msg or srv.
Return nil if there is None and the point of the first import if there is one."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (format "from %s.%s import" package type) nil t)))


(defun ros-msg-srv-insert-import-cpp (type package name)
  "Insert TYPE (either msg or srv) definition for NAME which is part of PACKAGE in the current cpp buffer."
  (when (not (ros-msg-srv-import-is-included-cpp-p package name))
    (progn
      (goto-char  (ros-msg-srv-insert-import-cpp-best-import-location type package))
      (end-of-line)
      (newline-and-indent)
      (insert (format "#include <%s/%s.h>" package name)))))

(defun ros-msg-srv-import-is-included-cpp-p (package name)
  "Return t if NAME in PACKAGE of TYPE is already included in the current cpp buffer."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (format "#include <%s\/%s.h>" package name) nil t)))

(defun ros-msg-srv-insert-import-cpp-best-import-location (type package)
  "Return the best location for an cpp include of TYPE.
TYPE can be either msg or srv.
The best location would be another import of the same PACKAGE,
the second best another import of this TYPE
the third best another include
and lastly the beginning of the buffer."
  (or (ros-string-in-buffer (format "#include <%s/.*>" package)) (ros-string-in-buffer (format "#include <.*%ss/.*>" type)) (ros-string-in-buffer "#include") (point-min)))

(defun ros-msg-srv-assert-type (type)
  "Assert that TYPE is a valid type."
  (let ((candidates '("msg" "srv")))
    (when (not(member type candidates))
      (error (format "%s is not element of %s" type (string-join candidates ", "))))))

(defun ros-msg-srv-generate-prototype(type name)
  "Generate prototype of type NAME for TYPE (msg or srv)."
  (ros-msg-srv-assert-type type)
  (string-trim (ros-shell-command-to-string (format "rosmsg-proto %s %s" type name)) "\"" "\""))

(defun ros-topic-pub (topic)
  "Prompt for TOPIC and compose message of right type in new buffer."
  (interactive (list (ros-generic-completing-read "topic")))
  (let ((buffer-name (format "*%s*" topic)))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    (pop-to-buffer buffer-name)
    (erase-buffer)
    (insert (ros-msg-srv-generate-prototype "msg" (ros-topic-service-get-type "topic" topic)))
    (ros-topic-pub-mode)))

(define-derived-mode ros-topic-pub-mode yaml-mode "ros-topic-pub-mode"
  "major mode for publishing ros msg")




(defhydra hydra-ros-main (:color blue :hint nil :foreign-keys warn)
  "
_c_: Catkin              _d_: Debug        _e_: Environment     _m_: Messages
_n_: Nodes               _p_: Packages     _P_: Parameters      _s_: Services
_S_: Active Services     _t_: Topics
"
  ("c" hydra-ros-catkin/body)
  ("e" hydra-ros-environment/body)
  ("m" hydra-ros-messages/body)
  ("n" hydra-ros-nodes/body)
  ("p" hydra-ros-packages/body)
  ("s" hydra-ros-services/body)
  ("S" hydra-ros-active-services/body)
  ("t" hydra-ros-topics/body)
  ("d" nil)
  ("P" nil)
  ("q" nil "quit" :color blue))


(defhydra hydra-ros-catkin (:color blue :hint nil :foreign-keys warn)
  "
_b_: build  _c_: config  _r_: redo  _x_: clean
"
  ("b" ros-catkin-build-transient)
  ("c" ros-catkin-config-transient)
  ("r" ros-catkin-compile)
  ("x" ros-catkin-clean-transient)
  ("q" nil "quit hydra")
  ("^" hydra-ros-main/body "Go back"))

(defhydra hydra-ros-environment (:color blue :hint nil :foreign-keys warn)
  "
_w_: Set Workspace
"
  ("w" ros-set-workspace)
  ("q" nil "quit hydra")
  ("^" hydra-ros-main/body "Go back"))

(defhydra hydra-ros-messages (:color blue :hint nil :foreign-keys warn)
  "
_i_: Insert message type at point               _s_: Show message
_I_: Insert import statement for message type
"
  ("i" ros-msg-insert)
  ("I" ros-msg-insert-import)
  ("s" ros-msg-srv-show-transient)
  ("q" nil "quit hydra")
  ("^" hydra-ros-main/body "Go back"))

(defhydra hydra-ros-packages (:color blue :hint nil :foreign-keys warn)
  "
_g_: Go to package _f_:  Find file current package  _F_: Find file in a package
_s_:  Search in current package  _S_: Search in a package
"
  ("g" ros-packages-go-to-package)
  ("F" ros-packages-find-file-in-package)
  ("f" ros-packages-find-file-in-current-package)
  ("s" nil)
  ("S" nil)
  ("q" nil "quit hydra")
  ("^" hydra-ros-main/body "Go back"))

(defhydra hydra-ros-nodes (:color blue :hint nil :foreign-keys warn)
  "
_s_: Show node
"
  ("s" ros-node-show)
  ("q" nil "quit hydra")
  ("^" hydra-ros-main/body "Go back"))

(defhydra hydra-ros-services (:color blue :hint nil :foreign-keys warn)
  "
_i_: Insert service type at point               _s_: Show service
_I_: Insert import service for message type
"
  ("i" ros-srv-insert)
  ("I" ros-srv-insert-import)
  ("s" ros-msg-srv-show-transient)
  ("q" nil "quit hydra")
  ("^" hydra-ros-main/body "Go back"))

(defhydra hydra-ros-active-services (:color blue :hint nil :foreign-keys warn)
  "
_s_: Show active Service    _c_: Call active service
"
  ("s" ros-service-show)
  ("c" nil)
  ("q" nil "quit hydra")
  ("^" hydra-ros-main/body "Go back"))

(defhydra hydra-ros-topics (:color blue :hint nil :foreign-keys warn)
  "
_e_: Echo Topic                   _s_: Show Topic                   _p_: Publish message on topic                   _i_: Insert name of topic at point
"
  ("e" ros-topic-echo-transient)
  ("s" ros-topic-show)
  ("p" nil)
  ("i" ros-topic-insert)
  ("q" nil "quit hydra")
  ("^" hydra-ros-main/body "Go back"))

(provide 'ros)

;;; ros.el ends here
