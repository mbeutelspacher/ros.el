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

(defun ros-shell-command-to-string (cmd &optional not-source)
  "Source `ros-current-workspace' if NOT—SOURCE, run CMD, return output as string.

Run in `ros-current-workspace' on `ros-current-tramp-prefix'
or the host system if `ros-current-tramp-prefix' is nil."
  (let ((command (if not-source  cmd (format "%s && %s" (ros-shell-source-command) cmd))))
  (s-trim (with-shell-interpreter :path (concat ros-current-tramp-prefix ros-current-workspace):form
            (shell-command-to-string (format "/bin/bash  -c \"%s\" | sed -r \"s/\x1B\\[([0-9]{1,3}(;[0-9]{1,2})?)?[mGK]//g\"" command))))))

(defun ros-shell-command-to-list (cmd &optional not-source)
  "Source `ros-current-workspace' unless NOT-SOURCE run CMD and return output as list.

Run in `ros-current-workspace' on `ros-current-tramp-prefix'
or the host system if `ros-current-tramp-prefix' is nil."
  (split-string (ros-shell-command-to-string cmd not-source) "\n" t "[\f\t\n\r\v\\]+"))

(defun ros-process-run (cmd buffer &optional not-source)
  "Source `ros-current-workspace' unless NOT-SOURCE run CMD and print output in BUFFER.

Run in `ros-current-workspace' on `ros-current-tramp-prefix'
or the host system if `ros-current-tramp-prefix' is nil."
  (let* ((command (if not-source cmd (format "%s && %s" (ros-shell-source-command) cmd) ))
        (process (with-shell-interpreter :path (concat ros-current-tramp-prefix ros-current-workspace):form
                   (start-process-shell-command buffer buffer (format "/bin/bash  -c \"%s\"" command)))))
    
    (view-buffer-other-window (process-buffer process))
    (ros-process-mode)))

;;;###autoload
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

(defun ros-catkin-list-profiles (workspace)
  "Get list of profiles for WORKSPACE."
  (let ((ros-current-workspace workspace))
    (ros-shell-command-to-list "catkin profile list -u" t)))

;;;###autoload
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
  (ros-shell-command-to-list "rospack list-names"))

(defun ros-packages-location-list ()
  "Return assocation list of all the available Ros packages and their paths."
  (kvplist->alist (split-string (ros-shell-command-to-string "rospack list"))))

(defun ros-packages-locate-package (package)
  "Return path to PACKAGE."
  (ros-shell-command-to-string (concat "rospack find " package)))


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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun ros-catkin-run-build (package &optional flags)
  "Run a build action to build PACKAGE with FLAGS."
  (interactive (list (ros-catkin-completing-read-ros-package) (transient-args 'ros-catkin-build-transient)))
  (ros-catkin-compile (ros-catkin-build-action :package package :flags flags)))

;;;###autoload
(defun ros-catkin-run-build-current-workspace (&optional flags)
  "Run a build action to build the current workspace with FLAGS."
  (interactive (list (transient-args 'ros-catkin-build-transient)))
  (ros-catkin-run-build " " flags))

(cl-defun ros-catkin-test-action (&key package flags)
  "Generate a test action to test PACKAGE with FLAGS."
  (ros-catkin-dump-action :tramp-prefix ros-current-tramp-prefix :workspace ros-current-workspace :profile ros-current-profile :verb "build" :args (format "%s --no-deps --catkin-make-args run_tests" package) :flags flags :post-cmd (concat "catkin_test_results build/" package)))

(cl-defun ros-catkin-test-action-single-rostest (&key package flags rostest)
  (ros-catkin-dump-action :tramp-prefix ros-current-tramp-prefix :workspace ros-current-workspace :profile ros-current-profile :verb "build" :args (format "%s --no-deps --make-args %s" package (file-name-sans-extension rostest)) :flags flags :post-cmd (format "rostest %s %s" package rostest)))

(cl-defun ros-catkin-test-action-single-gtest (&key package flags gtest regexp)
  (ros-catkin-dump-action :tramp-prefix ros-current-tramp-prefix :workspace ros-current-workspace :profile ros-current-profile :verb "build" :args (format "%s --no-deps --make-args %s" package gtest) :flags flags :post-cmd (format "rosrun %s %s %s" package gtest (if regexp (concat "--gtest_filter=" regexp) ""))))

;;;###autoload
(defun ros-catkin-run-test (package &optional flags)
  "Run a test action to-test PACKAGE with FLAGS."
  (interactive (list (ros-catkin-completing-read-ros-package) (transient-args 'ros-catkin-build-transient)))
  (ros-catkin-compile (ros-catkin-test-action :package package :flags flags)))

;;;###autoload
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

;;;###autoload
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
   ("r" "Build and run a single rostest" ros-catkin-run-single-rostest)
   ("g" "Build and run a single gtest" ros-catkin-run-single-gtest)
   ])

(cl-defun ros-catkin-clean-action (&key package flags)
  "Generate a clean action to clean PACKAGE with FLAGS."
  (ros-catkin-dump-action :tramp-prefix ros-current-tramp-prefix :workspace ros-current-workspace :profile ros-current-profile :verb "clean" :args package :flags flags :post-cmd nil))

;;;###autoload
(defun ros-catkin-run-clean (package &optional flags)
  "Run a clean action to clean PACKAGE with FLAGS."
  (interactive (list (ros-catkin-completing-read-ros-package) (transient-args 'ros-catkin-clean-transient)))
  (ros-catkin-compile (ros-catkin-clean-action :package package :flags flags)))

;;;###autoload
(defun ros-catkin-run-clean-current-workspace (&optional flags)
  "Run a clean action to clean the current workspace with FLAGS."
  (interactive (list (transient-args 'ros-catkin-clean-transient)))
  (ros-catkin-run-clean " " flags))

;;;###autoload
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

;;;###autoload
(defun ros-msg-show (msg &optional flags)
  "Prompt for MSG and show structure."
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

;;;###autoload
(defun ros-topic-show (topic)
  "Prompt for TOPIC and show subscribers and publishers."
  (interactive (list (ros-generic-completing-read "topic")))
  (ros-generic-show-info "topic" topic))


;;;###autoload
(defun ros-service-show (service)
  "Prompt for active SERVICE and show structure."
  (interactive (list (ros-generic-completing-read "service")))
  (ros-generic-show-info "service" service))

;;;###autoload
(defun ros-srv-show (service &optional flags)
  "Prompt for (not necessarily active) SERVICE and show structure."
  (interactive (list (ros-generic-completing-read "srv") (transient-args 'ros-msg-srv-show-transient)))
  (ros-generic-show-info "srv" service flags))

;;;###autoload
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

;;;###autoload
(defun ros-topic-echo (topic &optional flags)
  "Prompt for TOPIC and echo this topic with FLAGS."
  (interactive (list (ros-generic-completing-read "topic") (transient-args 'ros-topic-echo-transient)))
  (let ((command (format "rostopic echo %s %s" (string-join flags " ") topic)))
    (message command)
    (ros-process-run command (format "*%s*" command))))

;;;###autoload
(defun ros-topic-echo-filtered ( &optional flags)
  "Prompt for TYPE and then for topic and echo this topic with FLAGS."
  (interactive (list (transient-args 'ros-topic-echo-transient)))
  (let*((type-topic-list (ros-topic-list-by-type))
        (type (completing-read "Type: " (delq nil(delete-dups(kvalist->keys type-topic-list))) nil t))
        (topic (completing-read "Topic: " (ros-topic-filter-by-type type type-topic-list) nil t)))
    (ros-topic-echo topic flags)))

(defun ros-transient-read-existing-file (prompt _initial-input _history)
  "Read an existing file."
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

;;;###autoload
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
  "Return topics with type TYPE, TYPE-TOPIC—LIST can be reused."
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

(provide 'ros)

;;; ros.el ends here
