;;; ros.el --- Package to interact with and write code for ROS systems -*- lexical-binding: t -*-

;; Copyright (C) 2019 Max Beutelspacher

;; Author: Max Beutelspacher <max.beutelspacher@mailbox.org>
;; URL: https://github.com/DerBeutlin/ros.el
;; Version: 0.1
;; Package-Requires: ((emacs "24.4"))

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
(require 'dash)
(require 'dired)
(require 'json)
(require 's)
(require 'subr-x)
(require 'hydra)

(defgroup ros nil "Related to the Robot Operating System." :group 'external)

(defcustom ros-distro (getenv "ROS_DISTRO") "Name of ROS Distribution." :type 'string :group 'ros)

(defcustom ros-default-workspace nil
  "Path to binary/devel directory of default catkin workspace."
  :group 'ros-workspace
  :type 'directory)

(defcustom ros-env-host-directory "~/" "Directory from which all ros shell commands should be executed.":type 'directory)

(defcustom ros-env-saved-host-directory '("~/") "List of directories from which to choose the variable `ros-env-host-directory'." :type (list 'directory))

(defvar ros-current-workspace nil "Path to binary/devel directory of current catkin workspace.")

(defcustom ros-catkin-build-limited-status-rate nil "Limit update rate of status when compiling to this rate. If nil no restriction" :type 'integer)

(defun ros-current-workspace ()
  "Return path to binary/devel directory of current catkin workspace or to default workspace if not set."
  (if ros-current-workspace ros-current-workspace ros-default-workspace))

(defvar ros-current-profile "default" "Profile in current workspace.")

(defcustom ros-workspaces (list ros-default-workspace)
  "List of paths to binary/devel directories of catkin workspaces."
  :group 'ros-workspace
  :type 'sexp)

(defun ros-setup-file-extension ()
  "Return the file extension of the source file used by the current shell."
  (let  ((inferior-shell (s-trim(shell-command-to-string "echo $0"))))
    (cond
     ((s-suffix-p "zsh" inferior-shell) ".zsh")
     ((s-suffix-p "bash" inferior-shell) ".bash")
     (t ".sh"))))


(defun ros-catkin-source-workspace-command (workspace &optional profile)
  "Return the right sourcing command for WORKSPACE. If PROFILE is not nil, this profile is used, otherwise the default profile is used."
  (if (not workspace)
      (format "source /opt/ros/%s/setup%s" ros-distro (ros-setup-file-extension))
    (let* ((wspace (shell-quote-argument (expand-file-name workspace)))
           (source-file (concat (ros-catkin-locate-command wspace "d" profile) "/setup" (ros-setup-file-extension))))
      (unless (file-exists-p source-file)
        (let* ((extended-devel-space (ros-catkin-extended-devel-space wspace profile))
              (extended-source-file (concat extended-devel-space "/setup" (ros-setup-file-extension))))
          (message extended-source-file)
          (if (and (file-exists-p extended-source-file) (y-or-n-p (concat source-file " does not exist, do you want to source " extended-source-file" instead?")))
              (setq source-file extended-source-file)
              (error "%s could not be sourced" source-file))))
      (concat "source " source-file))))

(defun ros-catkin-extended-devel-space (workspace &optional profile)
  "Return the path to the devel space that WORKSPACE with optional PROFILE or default profile extends."
  (let ((workspace (shell-quote-argument (expand-file-name workspace)))
        (profile-flag (if profile (concat "--profile " (shell-quote-argument profile)) "")))
    (s-trim (car (split-string (car (cdr (split-string (shell-command-to-string (format "cd %s && catkin --no-color config %s | awk '{if ($1 == \"Extending:\"){print $3}}'" workspace profile-flag)) "\n"))) ":")))))

(defun ros-completing-read-workspace ()
  "Read a workspace from the minibuffer."
  (completing-read "Workspace: " ros-workspaces nil t nil nil (ros-current-workspace)))

;;;###autoload
(defun ros-select-workspace (workspace &optional profile)
  "Select current ROS workspace.
Set the variable `ros-current-workspace' to WORKSPACE
and the variable `ros-current-profile' to PROFILE.
If called interactively prompt for WORKSPACE and PROFILE."
  (interactive (list (ros-completing-read-workspace)))
  (setq ros-current-workspace workspace)
  (if profile
      (setq ros-current-profile profile)
      (setq ros-current-profile (ros-completing-read-catkin-profiles workspace))))

;;;###autoload
(defun ros-select-profile(profile)
  "Set the variable `ros-current-profile' to PROFILE.  If called interactively prompt for PROFILE."
  (interactive (list (ros-completing-read-catkin-profiles (ros-current-workspace))))
  (ros-select-workspace (ros-current-workspace) profile))

(defun ros-catkin-locate-command (workspace flag &optional profile)
  "Locate subfolders of catkin workspace.
Returns catkin command to locate subfolders of WORKSPACE with optional PROFILE.
The FLAG can be:
\"s\" : Get the path to the source space
\"b\" : Get the path to the build space
\"d\" : Get the path to the devel space
\"i\" : Get the path to the install space"
  (let ((workspace (shell-quote-argument (expand-file-name workspace)))
        (profile-str (if profile (format "--profile %s" (shell-quote-argument profile)) "")))
    (if (member flag '("s" "b" "d" "i"))
        (s-trim(shell-command-to-string (format "cd %s && catkin locate -%s %s" workspace flag profile-str)))
      (error "Catkin locate flag can only be s,b,d or i"))))

(defun ros-list-catkin-profiles (workspace)
  "Return list of all catkin profiles for WORKSPACE."
  (ros-shell-output-as-list (format "cd %s && catkin profile list -u" workspace)))

(defun ros-completing-read-catkin-profiles (workspace)
  "Completing read function for catkin profiles of WORKSPACE."
  (let ((profiles (ros-list-catkin-profiles workspace)))
    (completing-read (format "Profile: " ) profiles nil t nil nil (if (member ros-current-profile profiles) ros-current-profile "default"))))

(defun ros-shell-prepend-ros-environment-commands (cmd &optional workspace profile)
  "Return CMD with prepended sourcing and environment commands.
These consists of setting the ROS_MASTER_URI, the ROS_IP
and sourcing WORKSPACE with PROFILE.
If PROFILE and WORKSPACE are not provided use the settings
in the variables `ros-current-workspace' and `ros-current-profile'."
  (let ((workspace (if workspace workspace (ros-current-workspace)))
         (prof (if profile profile ros-current-profile))
        (export-master-uri (if (ros-env-ros-master-uri) (format "export ROS_MASTER_URI=%s" (ros-env-ros-master-uri)) "true"))
        (export-ros-ip (if (ros-env-ros-ip) (format "export ROS_IP=%s" (ros-env-ros-ip)) "true")))
    (format "%s && %s && %s && %s" export-master-uri export-ros-ip (ros-catkin-source-workspace-command workspace prof) cmd)))

(defun ros-shell-command-to-string (cmd &optional workspace profile)
  "Run CMD after sourcing workspace and return output as a string.
If WORKSPACE or PROFILE are nil the ones specified in
the variables `ros-current-workspace'and `ros-current-profile' are used."
  (let ((default-directory ros-env-host-directory))
    (s-trim (shell-command-to-string (ros-shell-prepend-ros-environment-commands cmd workspace profile)))))

(defun ros-shell-output-as-list (cmd &optional workspace)
  "Run CMD after sourcing workspace and return output lines as a list.
The WORKSPACE or if nil the one returned by the function
`ros-current-workspace' is sourced."
    (split-string (ros-shell-command-to-string cmd workspace) "\n"))

(defun ros-packages ()
  "List all available ROS packages in the current workspace."
  (ros-shell-output-as-list "rospack list-names"))

(defun ros-depend-on-other-package( dependency)
  "Let current package depend on DEPENDENCY package."
  (interactive (list (ros-completing-read-packages)))
  (ros-depend-on-other-package-package-xml dependency)
  (ros-depend-on-other-package-cmakelists-find-package dependency)
  (ros-depend-on-other-package-cmakelists-catkin-package dependency))


(defun ros-depend-on-other-package-package-xml( dependency)
  "Write DEPENDENCY in package.xml file in current file."
  (let ((packagexml (concat (locate-dominating-file (ros-current-directory) "package.xml") "package.xml"))
        (line (format "<depend>%s</depend>" dependency)))
    (with-current-buffer (find-file-noselect packagexml)
      (goto-char 1)
      (when (not (search-forward line nil t))
        (goto-char 1)
        (when (search-forward "<buildtool_depend>catkin</buildtool_depend>" nil t)
          (move-end-of-line nil)
          (open-line 1)
          (forward-line)
          (indent-for-tab-command)
          (insert line)
          (save-buffer))))))


(defun ros-depend-on-other-package-cmakelists-find-package( dependency)
  "Write DEPENDENCY in CMakeLists.txt file in current file."
  (let (( cmakefile (concat (locate-dominating-file (ros-current-directory) "CMakeLists.txt") "CMakeLists.txt")))
    (with-current-buffer (find-file-noselect cmakefile)
      (goto-char 1)
        (when (search-forward "find_package" nil t)
          (let ((begin-components (search-forward "COMPONENTS"))
                (end-components (search-forward ")")))
            (goto-char begin-components)
            (when (not (search-forward dependency end-components t))
              (goto-char begin-components)
              (move-end-of-line nil)
              (open-line 1)
              (forward-line)
              (indent-for-tab-command)
              (insert dependency)
              (save-buffer)))))))


(defun ros-depend-on-other-package-cmakelists-catkin-package( dependency)
  "Write DEPENDENCY in CMakeLists.txt file in current file."
  (let (( cmakefile (concat (locate-dominating-file (ros-current-directory) "CMakeLists.txt") "CMakeLists.txt")))
    (with-current-buffer (find-file-noselect cmakefile)
      (goto-char 1)
      (when (search-forward "catkin_package" nil t)
        (let ((begin-components (search-forward "CATKIN_DEPENDS"))
              (end-components (search-forward ")")))
          (goto-char begin-components)
          (when (not (search-forward dependency end-components t))
            (goto-char begin-components)
            (move-end-of-line nil)
            (open-line 1)
            (forward-line)
            (indent-for-tab-command)
            (insert dependency)
            (save-buffer)))))))

(defun ros-completing-read-packages ()
  "Completing read function for ROS packages."
  (completing-read "Package: " (ros-packages) nil t ))

(defun ros-current-package ()
  "Return the name of the ROS package the current buffer lies in.
If the current buffer does not lie in a ROS package return nil."
  (let* ((package-path (locate-dominating-file (ros-current-directory) "package.xml")))
    (when package-path (ros-parse-package-xml-for-package (concat package-path "package.xml")))))

(defun ros-parse-package-xml-for-package (path)
  "Parse package.xml in PATH for package name."
  (with-temp-buffer
    (insert-file-contents path)
    (string-match  "<name>\\(.*\\)</name>" (buffer-string))
    (match-string 1 (buffer-string))))

(defun ros-current-directory ()
  "Return the directory of the current buffer or the current dired directory."
  (if (buffer-file-name) (file-name-directory (buffer-file-name)) (dired-current-directory)))

(defvar ros-catkin-compile-history nil)

(defun ros-catkin-compile-command (verb args workspace &optional profile additional_cmd)
  "Run catkin VERB with ARGS  after sourcing WORKSPACE with optional PROFILE.
If ADDITIONAL_CMD is not nil, run it after the command."
  (let* ((default-directory workspace)
         (compilation-buffer-name-function (lambda (_) "*catkin*"))
         (profile-flag (if profile (format "--profile %s" profile) ""))
         (add-cmd (if additional_cmd additional_cmd "true"))
         (compile-command (format "catkin %s %s %s && %s" verb profile-flag args add-cmd))
         (triplet (list compile-command workspace profile)))
    (ros-catkin-insert-triplet-to-front-of-history-and-delete-duplicates triplet)
    (compile (ros-shell-prepend-ros-environment-commands  compile-command workspace profile))))

(defun ros-catkin-generate-string-from-triplet (triplet index)
  "Convert TRIPLET consisting of compile command, workspace and profile to description string and prepend it with the INDEX."
  (format "%04d: %s IN %s WITH PROFILE %s" index (first triplet) (second triplet) (third triplet)))

(defun ros-catkin-insert-triplet-to-front-of-history-and-delete-duplicates (triplet)
  "Push TRIPLET to the front of `ros-catkin-compile-history' and remove any duplicates."
  (when (member triplet ros-catkin-compile-history)
    (setq ros-catkin-compile-history (remove triplet ros-catkin-compile-history)))
  (push triplet ros-catkin-compile-history))

(defun ros-catkin-parse-triplet-from-string (string)
  "Convert description STRING to triplet consisting of compile command, workspace and profile."
  (let ((command)
        (workspace)
        (profile))
    (string-match "[0-9]*: \\\(.*\\\) IN \\\(.*\\\) WITH PROFILE \\\(.*\\\)" string)
    (setq command (match-string 1 string))
    (setq workspace (match-string 2 string))
    (setq profile (match-string 3 string))
    (list command workspace profile)))

(defun ros-catkin-compile-history-indexes(compile-history)
  "Return a sequence of numbers from 1 to the length of COMPILE-HISTORY."
  (number-sequence 1 (length compile-history)))

(defun ros-catkin-completing-read-compile-history()
  "Completing-read function for `ros-catkin-compile-history'."
(completing-read "Compile Command: " (mapcar* 'ros-catkin-generate-string-from-triplet ros-catkin-compile-history (ros-catkin-compile-history-indexes ros-catkin-compile-history)) nil t))

(defun ros-catkin-compile-from-history (command)
  "Prompt for past COMMAND and rerun it in the same workspace and the same profile."
  (interactive (list (ros-catkin-completing-read-compile-history)))
  (let* ((triplet (ros-catkin-parse-triplet-from-string command))
        (default-directory (second triplet))
        (compilation-buffer-name-function (lambda (_) "*catkin*")))
    (ros-catkin-insert-triplet-to-front-of-history-and-delete-duplicates triplet)
    (compile (ros-shell-prepend-ros-environment-commands (first triplet) (second triplet) (third triplet)))))



(defun ros-catkin-build-command (args workspace &optional profile additional_cmd)
  "Run catkin build with ARGS  after sourcing WORKSPACE with optional PROFILE.
If ADDITIONAL_CMD is not nil, run it after the command."
  (let ((limit-status (when ros-catkin-build-limited-status-rate (concat " --limit-status-rate " (number-to-string ros-catkin-build-limited-status-rate)))))
    (ros-catkin-compile-command (concat "build" limit-status) args workspace profile additional_cmd)))

;;;###autoload
(defun ros-catkin-build-workspace(workspace &optional profile)
  "Build the WORKSPACE with PROFILE or default if not provided.
If called interactively prompt for WORKSPACE and PROFILE."
  (interactive (list (ros-completing-read-workspace)))
  (let ((prof (if profile profile (ros-completing-read-catkin-profiles workspace))))
    (ros-catkin-build-command "" workspace prof)))

;;;###autoload
(defun ros-catkin-build-current-workspace()
  "Build the workspace with profile specified in the variables `ros-current-workspace' and `ros-current-profile'."
  (interactive)
  (ros-catkin-build-workspace (ros-current-workspace) ros-current-profile))

;;;###autoload
(defun ros-catkin-build-package(package)
  "Build the ROS package PACKAGE.
The packages will be built in the workspace specified
in the variable `ros-current-workspace' and with the profile
specified in the variable`ros-current-profile'."
  (interactive (list (ros-completing-read-packages)))
  (ros-catkin-build-command package (ros-current-workspace) ros-current-profile))

;;;###autoload
(defun ros-catkin-build-current-package ()
  "If the current buffer is part of a ROS package in the workspace specified by the variable `ros-current-workspace', build it."
  (interactive)
  (ros-catkin-build-package (ros-current-package)))

;;;###autoload
(defun ros-catkin-clean-workspace (workspace &optional profile)
  "Run `catkin clean' in WORKSPACE with optional PROFILE."
  (interactive (list (ros-completing-read-workspace)))
  (let ((prof (if profile profile (ros-completing-read-catkin-profiles workspace))))
    (when (y-or-n-p (concat "Do you really want to clean " workspace " %s with profile " prof " ?"))
        (ros-catkin-compile-command "clean" "-y" workspace profile))))

;;;###autoload
(defun ros-catkin-clean-current-workspace()
  "Run `catkin clean' in workspace with profile specified in the variables `ros-current-workspace' and `ros-current-profile'."
  (interactive)
  (ros-catkin-clean-workspace (ros-current-workspace) ros-current-profile))

;;;###autoload
(defun ros-catkin-clean-package (package)
  "Clean the ROS PACKAGE."
  (interactive (list (ros-completing-read-packages)))
    (when (y-or-n-p (concat "Do you really want to clean " package "?"))
      (ros-catkin-compile-command "clean" (concat "-y " package) (ros-current-workspace) ros-current-profile)))

;;;###autoload
(defun ros-catkin-clean-current-package()
  "If the current buffer is part of a ROS package in the workspace specified by  in the variable `ros-current-workspace', clean it."
  (interactive)
  (ros-catkin-clean-package (ros-current-package)))

;;;###autoload
(defun ros-catkin-test-package(package)
  "Build and run all unittests in PACKAGE."
  (interactive (list (ros-completing-read-packages)))
  (ros-catkin-build-command (concat package " --catkin-make-args run_tests") (ros-current-workspace) ros-current-profile (concat "catkin_test_results build/" package)))

;;;###autoload
(defun ros-catkin-test-current-package()
  "Build and run all unittests in the package the current buffer lies in."
  (interactive)
  (ros-catkin-test-package (ros-current-package)))

;;;###autoload
(defun ros-catkin-test-workspace(workspace &optional profile)
  "Build and run all unittests in WORKSPACE with profile PROFILE."
  (interactive (list (ros-completing-read-workspace)))
  (let ((prof (if profile profile (ros-completing-read-catkin-profiles workspace))))
    (ros-catkin-build-command "--catkin-make-args run_tests" workspace prof)))

;;;###autoload
(defun ros-catkin-test-current-workspace()
  "Build and run all unittests in the current workspace.
The workspace and the profile are specified in the variables
`ros-current-workspace' and `ros-current-profile'."
  (interactive)
  (ros-catkin-test-workspace (ros-current-workspace) ros-current-profile))

(defun ros-catkin-test-files-in-package (package regexp)
  "List test files matching REGEXP in the test directory of PACKAGE."
  (let* ((test-directory (concat (file-name-as-directory (ros-get-package-path package)) "test"))
         (test-files (directory-files test-directory nil regexp)))
    (mapcar 'file-name-sans-extension test-files)))


(defun ros-catkin-test-code-files-in-package (package)
  "List test files matching *.cpp and *.py in test directory of PACKAGE."
  (ros-catkin-test-files-in-package package  ".*test.*\\\(\\\.cpp\\\|\\\.py\\\)"))

(defun ros-catkin-completing-read-test-file-in-package(package)
  "Completing read function for test code files in PACKAGE."
  (completing-read "Test: " (ros-catkin-test-code-files-in-package package) nil t))

;;;###autoload
(defun ros-catkin-test-file-in-package(package)
  "Build and run all unittests of a prompted file in PACKAGE."
  (interactive (list (ros-completing-read-packages)))
  (let ((test-file (ros-catkin-completing-read-test-file-in-package package)))
    (ros-catkin-test-file-in-package2 package test-file)))


(defun ros-catkin-test-file-in-package2 (package test)
  "Build and run all unittests in TEST file in PACKAGE."
  (let ((corresponding-ros-test (ros-catkin-test-files-in-package package (concat test "\\\.xml"))))
    (if corresponding-ros-test (ros-catkin-test-run-single-rostest package test) (ros-catkin-test-run-single-gtest package test))))

(defun ros-catkin-test-run-single-rostest(package test)
  "Build and run a single rostest called TEST in PACKAGE."
  (ros-catkin-build-command (concat  package " --no-deps --make-args " test) (ros-current-workspace) ros-current-profile (concat "rostest " package " " test ".xml")))

(defun ros-catkin-test-run-single-gtest (package test &optional regexp)
  "Build and run a single gtest called TEST in PACKAGE.
If REGEXP is not nil filter tests for REGEXP."
  (ros-catkin-build-command (concat package " --no-deps --make-args " test) (ros-current-workspace) ros-current-profile (concat "rosrun " package " " test (when regexp (concat " --gtest_filter=" regexp)))))

;;;###autoload
(defun ros-catkin-test-file-in-current-package ()
  "Prompt for test file in current package and build and run this test file."
  (interactive)
  (ros-catkin-test-file-in-package (ros-current-package)))

;;;###autoload
(defun ros-catkin-test-current-test-file ()
  "Build and run the test file in the current buffer."
  (interactive)
  (let ((test-candidate (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
        (package (ros-current-package)))
    (if (member test-candidate (ros-catkin-test-code-files-in-package package))
        (ros-catkin-test-file-in-package2 package test-candidate)
        (message "Current file is not a test file"))))

;;;###autoload
(defun ros-catkin-test-at-point()
  "If current file is a gtest test file, build and run the test at point."
  (interactive)
  (let* ((test-candidate (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
       (package (ros-current-package))
       (corresponding-ros-test (ros-catkin-test-files-in-package package (concat test-candidate "\\\.xml"))))
  (if (not(member test-candidate (ros-catkin-test-code-files-in-package package)))
    (message "Current file is not a test file.")
    (if corresponding-ros-test
        (message "This file is a ros test and can therefore not be filtered.")
      (ros-catkin-test-run-single-gtest package test-candidate (concat "\"*" (symbol-name(symbol-at-point)) "*\""))))))


(defun ros-generic-list (type)
  "Return result from rosTYPE list.

  TYPE can be any of the following \"node\", \"topic\", \"service\" \"msg\""
  (ros-shell-output-as-list (format "ros%s list" type)))

(defun ros-generic-completing-read (type)
  "Prompts for ros TYPE.

  TYPE can be any of the following \"node\", \"topic\", \"service\" \"msg\""
  (completing-read (format "%s: " type) (ros-generic-list type) nil t))

(defun ros-generic-get-info (type name)
  "Return info about NAME of type TYPE.
TYPE can be any of the following \"node\", \"topic\", \"service\" \"msg\""
  (let ((command (cond ((string= type "msg") "show") (t "info"))))
    (ros-shell-command-to-string (format "ros%s %s %s" type command name))))


(defun ros-generic-show-info (type name)
  "Show info about NAME of type TYPE in new buffer."
  (let ((buffer-name (format "* ros-%s: %s" type name)))
    (when (get-buffer buffer-name) (kill-buffer buffer-name))
    (pop-to-buffer buffer-name))
  (erase-buffer)
  (insert (ros-generic-get-info type name))
  (ros-info-mode))

(define-derived-mode ros-info-mode messages-buffer-mode "ros-info-mode"
  "major mode for displaying ros info messages")

(define-key ros-info-mode-map (kbd "RET") 'ros-show-thing-at-point)
(define-key ros-info-mode-map (kbd "E") 'ros-echo-topic-at-point)
(define-key ros-info-mode-map (kbd "C") 'ros-call-service-at-point)
(define-key ros-info-mode-map (kbd "K") 'ros-kill-node-at-point)

;;;###autoload
(defun ros-msg-show (msg)
  "Prompt for MSG and show structure."
  (interactive (list (ros-generic-completing-read "msg")))
  (ros-generic-show-info "msg" msg))

;;;###autoload
(defun ros-topic-show (topic)
  "Prompt for TOPIC and show subscribers and publishers."
  (interactive (list (ros-generic-completing-read "topic")))
  (ros-generic-show-info "topic" topic))

;;;###autoload
(defun ros-topic-show-filtered (topic)
  "Prompt for TOPIC filtered by type and show subscribers and publishers."
  (interactive (list (ros-topic-completing-read-topic-filtered)))
  (ros-topic-show topic))

;;;###autoload
(defun ros-service-show (service)
  "Prompt for active SERVICE and show structure."
  (interactive (list (ros-generic-completing-read "service")))
  (ros-generic-show-info "service" service))

;;;###autoload
(defun ros-srv-show (service)
"Prompt for (not necessarily active) SERVICE and show structure."
(interactive (list (ros-generic-completing-read "srv")))
(ros-generic-show-info "srv" service))

;;;###autoload
(defun ros-node-show (node)
  "Prompt for NODE and show published and subscribed topics and provided services."
  (interactive (list (ros-generic-completing-read "node")))
  (ros-generic-show-info "node" node))

;;;###autoload
(defun ros-show-thing-at-point ()
  "Get thing at point and try to describe it."
  (interactive)
  (let ((thing (thing-at-point 'symbol))
        (section (ros-info-get-section))
        (type nil))
    (cond
     ((member section '("Publishers" "Subscribers" "Node"))
      (setq type "node"))
     ((member section '("Subscriptions" "Publications"))
      (setq type "topic"))
     ((member section '("Services"))
      (setq type "service"))
     ((member section '("Type"))
      (cond
       ((member thing (ros-generic-list "msg"))
        (setq type "msg"))
       ((member thing (ros-generic-list "srv"))
        (setq type "srv"))))
     (t (message "Section not recognized")))
    (when type
      (ros-generic-show-info type thing))))

;;;###autoload
(defun ros-echo-topic-at-point ()
  "Get thing at point and if it is a topic echo it."
  (interactive)
  (let ((thing (thing-at-point 'symbol)))
    (if (member thing (ros-generic-list "topic"))
        (ros-topic-echo thing)
        (message (format "%s is not an active topic" thing)))))

;;;###autoload
(defun ros-call-service-at-point ()
  "Get thing at point and if it is a service call it."
  (interactive)
  (let ((thing (thing-at-point 'symbol)))
    (if (member thing (ros-generic-list "service"))
        (ros-service-call thing)
      (message (format "%s is not an active service" thing)))))

;;;###autoload
(defun ros-kill-node-at-point ()
  "Get thing at point and if it is a node kill it."
  (interactive)
  (let ((thing (thing-at-point 'symbol)))
    (if (member thing (ros-generic-list "node"))
        (ros-node-kill thing)
      (message (format "%s is not an active node" thing)))))

(defun ros-node-kill (node)
  "Kill NODE if active node."
  (if (member node (ros-generic-list "node"))
      (when (yes-or-no-p (format "Do you really want to kill node %s"
                                 node))
          (ros-shell-command-to-string (format "rosnode kill %s" node))
          (if (member node (ros-generic-list "node"))
              (message (format "Failed to kill node %s" node))
            (message (format "Killed node %s successfully" node))))
    (message (format "There is no node %s to kill" node))))

;;;###autoload
(defun ros-topic-echo (topic)
  "Prompt for TOPIC and echo it."
  (interactive (list (ros-generic-completing-read "topic")))
  (let* ((topic-full-name (if (string-match "^/" topic) topic (concat "/" topic)))
         (buffer-name (concat "*rostopic:" topic-full-name "*")))
    (ros-process-start-process buffer-name (concat "rostopic echo " topic-full-name))))

;;;###autoload
(defun ros-topic-echo-filtered (topic)
  "Prompt for TOPIC filtered by type and echo it."
  (interactive (list (ros-topic-completing-read-topic-filtered)))
  (ros-topic-echo topic))

(defun ros-process-start-process (buffer-name cmd)
  "Run CMD in buffer BUFFER-NAME.
In this environment the environment variables `ROS_MASTER_URI'
and `ROS_IP' are set according to the current settings.
Additionally the current workspace is sourced."
  (let* ((process (start-process-shell-command buffer-name buffer-name (ros-shell-prepend-ros-environment-commands cmd))))
    (view-buffer-other-window (process-buffer process))
    (ros-process-mode)))

(defun ros-info-get-section ()
  "Get the section of ros info of the symbol at point.
These sections help to identify the type of the symbol at point e.g. Topic, Node etc."
  (save-excursion
    (let* ((start (re-search-backward "Services:\\|Subscriptions:\\|Publications:\\|Publishers:\\|Subscribers:\\|Node:\\|Type:"))
                 (end (if start (re-search-forward ":"))))
      (when (and start end) (buffer-substring-no-properties start (- end 1))))))

(defun ros-generate-prototype (type name topic)
  "Generate prototypes for ros messages or service (decided by TYPE) of name NAME to be published on TOPIC."
  (let ((prototype-text (ros-shell-command-to-string (format "rosmsg-proto %s %s" type name)))
        (buffer-name topic))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    (pop-to-buffer buffer-name)
    (erase-buffer)
    (insert prototype-text)
    (if (string= type "srv") (ros-service-call-mode) (ros-topic-pub-mode))))
  
(define-derived-mode ros-topic-pub-mode text-mode "ros-topic-pub-mode"
  "major mode for publishing ros msg")

(define-key ros-topic-pub-mode-map (kbd "C-c C-c") 'ros-topic-pub-buffer)
(define-key ros-topic-pub-mode-map (kbd "C-c C-k") 'kill-this-buffer)

(define-derived-mode ros-service-call-mode text-mode "ros-service-call-mode"
  "major mode for calling ros services")

(define-key ros-service-call-mode-map (kbd "C-c C-c") 'ros-service-call-buffer)
(define-key ros-service-call-mode-map (kbd "C-c C-k") 'kill-this-buffer)

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
(defun ros-topic-pub-buffer (arg)
  "Publish the mesage defined in the buffer, if ARG is nil publish the message one, otherwise ARG is the rate of the publishing."
  (interactive (list current-prefix-arg))
  (let* ((topic (buffer-name))
         (type (ros-get-topic-type topic))
         (message-text (string-trim (s-trim (buffer-string)) "\"" "\""))
         (buffer-name (concat "*rostopic pub " topic "*"))
         (old-buffer (current-buffer))
         (rate-argument (if arg (format "-r %d" (prefix-numeric-value arg)) "--once")))
    (ros-process-start-process buffer-name (format "rostopic pub %s %s %s %s" topic type message-text rate-argument))
    (kill-buffer old-buffer)))

(defun ros-get-topic-type (topic)
  "Get message type of TOPIC."
  (ros-get-topic-service-type "topic" topic))

(defun ros-get-service-type (topic)
  "Get service type of TOPIC."
  (ros-get-topic-service-type "service" topic))

(defun ros-get-topic-service-type (type topic)
  "Get message or service (decided by TYPE) type of TOPIC."
  (ros-shell-command-to-string (format "ros%s type %s" type topic)))

(defun ros-topic-list-by-type ()
  "Create a list of topic-type pairs."
  (let* ((output (ros-shell-command-to-string (format "rostopic list -v")))
         (matches (s-match-strings-all "/\\(.*\\) \\[\\(.*\\)]*\]" output)))
    (mapcar 'cdr matches)))

(defun ros-topic-completing-read-type (topic-type-pairs)
  "Completing read function for types in TOPIC-TYPE-PAIRS."
  (completing-read "Type:" (delq nil(delete-dups (mapcar 'cadr topic-type-pairs))) nil t))

(defun ros-topic-filter-by-type (type topic-type-pairs)
  "Return all topics in TOPIC-TYPE-PAIRS which have type TYPE."
  (delq nil (delete-dups (mapcar (lambda (x) (when (string= (cadr x) type) (car x))) topic-type-pairs))))

(defun ros-topic-completing-read-topic-filtered-by-type(type topic-type-pairs)
  "Completing read function for all topics in TOPIC-TYPE-PAIRS which have type TYPE."
  (completing-read "Topic: "  (ros-topic-filter-by-type type topic-type-pairs) nil t))

(defun ros-topic-completing-read-topic-filtered ()
  "Completing read function for topics filtered by type."
  (let ((topic-type-pairs (ros-topic-list-by-type)))
    (ros-topic-completing-read-topic-filtered-by-type (ros-topic-completing-read-type topic-type-pairs) topic-type-pairs)))

;;;###autoload
(defun ros-topic-pub (topic)
  "Draft ros message to be published on TOPIC."
  (interactive (list (ros-generic-completing-read "topic")))
  (ros-generate-prototype "msg" (ros-get-topic-type (s-trim-right topic)) topic))

;;;###autoload
(defun ros-topic-pub-filtered (topic)
  "Draft ros message to be published on TOPIC which is filtered by type."
  (interactive (list (ros-topic-completing-read-topic-filtered)))
  (ros-topic-pub topic))

;;;###autoload
(defun ros-service-call (topic)
  "Draft a service call to be called on TOPIC."
  (interactive (list (ros-generic-completing-read "service")))
  (ros-generate-prototype "srv" (ros-get-service-type topic) topic))

;;;###autoload
(defun ros-service-call-buffer ()
  "Call the service specified in the buffer."
  (interactive)
  (let* ((service (buffer-name))
         (arguments (string-trim (s-trim (buffer-string)) "\"" "\""))
         (buffer-name (concat "*rosservice call " service "*"))
         (old-buffer (current-buffer)))
    (ros-process-start-process buffer-name (concat "rosservice call " service " " arguments))
    (kill-buffer old-buffer)))


;;;###autoload
(defun ros-insert-import-msg (message)
  "Prompt for MESSAGE and include it in file."
  (interactive (list (ros-generic-completing-read "msg")))
  (ros-insert-import "msg" message))

;;;###autoload
(defun ros-insert-import-srv (service)
  "Prompt for SERVICE and include it in file."
  (interactive (list (ros-generic-completing-read "srv")))
  (ros-insert-import "srv" service))

(defun ros-insert-import (type name)
  "Insert TYPE (either msg or srv) definition for NAME in the current buffer."
  (let ((package (car (split-string name "/")))
        (item-name (car (cdr(split-string name "/")))))
    (cond ((string= major-mode "python-mode") (ros-insert-import-python type package item-name))
          ((string= major-mode "c++-mode") (ros-insert-import-cpp type package item-name))
          (t (message "Only works in Python and C++ mode")))))

;;;###autoload
(defun ros-insert-msg (name)
  "Insert  definition for msg NAME in the current buffer."
  (interactive (list (ros-generic-completing-read "msg")))
  (ros-insert-msg-srv name))

;;;###autoload
(defun ros-insert-srv (name)
  "Insert  definition for srv NAME in the current buffer."
  (interactive (list (ros-generic-completing-read "srv")))
  (ros-insert-msg-srv name))

(defun ros-insert-msg-srv (name)
  "Insert (either msg or srv) definition for NAME in the current buffer."
  (let ((package (car (split-string name "/")))
        (item-name (car (cdr (split-string name "/")))))
    (insert (format " %s::%s" package item-name))))

;;;###autoload
(defun ros-insert-topic (topic)
  "Prompt for TOPIC and insert it at point."
  (interactive (list (ros-generic-completing-read "topic")))
  (insert topic))

;;;###autoload
(defun ros-insert-topic-filtered (topic)
  "Prompt for TOPIC filtered by type and insert it at point."
  (interactive (list (ros-topic-completing-read-topic-filtered)))
  (insert topic))

(defun ros-insert-import-python (type package name)
  "Insert TYPE (either msg or srv) definition for NAME which is part of PACKAGE in the current python buffer."
  (let ((start-import-statement (format "from %s.%s import" package type)))
    (when (not (ros-import-is-included-python-p type package name))
      (if (ros-import-search-same-package-import-python type package)
          (progn
            (goto-char (ros-import-search-same-package-import-python type package))
            (move-end-of-line nil)
            (insert (format ", %s" name)))
        (goto-char (ros-insert-import-python-best-import-location type))
        (end-of-line)
        (newline-and-indent)
        (insert (format "%s %s" start-import-statement name))))))

(defun ros-insert-import-python-best-import-location (type)
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

(defun ros-import-is-included-python-p (type package name)
  "Return t if NAME in PACKAGE of TYPE is already included in the current python buffer."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (format "from %s.%s import .*%s[, \n]" package type name) nil t)))

(defun ros-import-search-same-package-import-python (type package)
  "Search for import of TYPE  of PACKAGE in the current buffer.
TYPE can be either msg or srv.
Return nil if there is None and the point of the first import if there is one."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (format "from %s.%s import" package type) nil t)))


(defun ros-insert-import-cpp (type package name)
  "Insert TYPE (either msg or srv) definition for NAME which is part of PACKAGE in the current cpp buffer."
  (when (not (ros-import-is-included-cpp-p package name))
    (progn
      (goto-char  (ros-insert-import-cpp-best-import-location type package))
      (end-of-line)
      (newline-and-indent)
      (insert (format "#include <%s/%s.h>" package name)))))

(defun ros-import-is-included-cpp-p (package name)
  "Return t if NAME in PACKAGE of TYPE is already included in the current cpp buffer."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (format "#include <%s\/%s.h>" package name) nil t)))

(defun ros-insert-import-cpp-best-import-location (type package)
  "Return the best location for an cpp include of TYPE.
TYPE can be either msg or srv.
The best location would be another import of the same PACKAGE,
the second best another import of this TYPE
the third best another include
and lastly the beginning of the buffer."
  (or (ros-string-in-buffer (format "#include <%s/.*>" package)) (ros-string-in-buffer (format "#include <.*%ss/.*>" type)) (ros-string-in-buffer "#include") (point-min)))

;;;###autoload
(defun ros-dired-package (package)
  "Open the root of PACKAGE in dired."
  (interactive (list (ros-completing-read-packages)))
  (dired (ros-get-package-path package)))

(defun ros-get-package-path (package)
  "Return the path PACKAGE lies in."
  (s-trim(ros-shell-command-to-string (concat "rospack find " package))))

(defun ros-loggers (node)
  "List of all current loggers of NODE."
  (ros-shell-output-as-list (concat "rosconsole list " node)))

(defun ros-completing-read-logger(node)
  "Completing read function for loggers in NODE."
    (completing-read "Logger: " (ros-loggers node) nil t))

;;;###autoload
(defun ros-logger-set-level ()
  "Prompt for NODE and LOGGER and set the logger level."
  (interactive)
  (let* ((node (ros-generic-completing-read "node"))
         (logger (ros-completing-read-logger node))
         (current-value (s-trim (ros-shell-command-to-string (concat "rosconsole get " node " " logger))))
         (new-value (completing-read "Level: " '("debug" "info" "warn" "error" "fatal") nil t nil nil current-value)))
    (ros-shell-command-to-string (concat "rosconsole set " node " " logger " " new-value))))

(defun ros-param-list()
  "List of ROS parameters."
  (ros-shell-output-as-list "rosparam list"))

(defun ros-param-completing-read()
  "Completing read function for ROS parameters."
  (completing-read "Parameter: " (ros-param-list) nil t))

;;;###autoload
(defun ros-param-set(parameter)
  "Prompt for PARAMETER and set it to a new value."
  (interactive (list (ros-param-completing-read)))
  (let* ((current-value (string-trim (ros-shell-command-to-string (concat "rosparam get " parameter)) "'" "'"))
         (new-value (ros-param-read-value parameter current-value)))
    (ros-shell-command-to-string (concat "rosparam set " parameter " " new-value))))

(defun ros-param-read-value (parameter old-value)
  "Prompt for a new value for PARAMETER, the collection is generated based on the OLD-VALUE of PARAMETER."
  (let* ((bool-collection '("true" "false"))
         (collection (when (member old-value bool-collection) bool-collection)))
    (completing-read (format "%s: " parameter) collection nil collection (unless collection old-value) nil (when collection old-value))))


(defun ros-dynamic-reconfigure-cmd(cmd)
  "Prepend CMD with the call to dynamic reconfigure and return the command as string."
  (concat "rosrun dynamic_reconfigure dynparam " cmd))

(defun ros-dynamic-reconfigure-list-nodes()
  "List of ROS dynamic reconfigure nodes."
  (ros-shell-output-as-list (ros-dynamic-reconfigure-cmd "list")))

(defun ros-dynamic-reconfigure-completing-read-node()
  "Completing read function for dynamic reconfigure nodes."
  (completing-read "Node:" (ros-dynamic-reconfigure-list-nodes) nil t))

(defun ros-dynamic-reconfigure-list-params(node)
  "List of dynamicly reconfigurable parameters in NODE."
  (ros-dynamic-reconfigure-parse-parameter-value-pairs (ros-shell-command-to-string (ros-dynamic-reconfigure-cmd (concat "get " node)))))

(defun ros-dynamic-reconfigure-clean-dictionary-and-remove-subgroups(dict)
    "Remove the surrounding braces of the DICT and remove all subgroups."
    (replace-regexp-in-string "[^,]*{[^}]*}" "" (s-replace "}" ""(s-replace  "’" ""  (substring dict 1 -1)) )))

(defun ros-dynamic-reconfigure-parse-parameter-value-pairs(dict)
  "Parse DICT string to get a hashtable of parameter names and values."
  (let* ((parameter_pairs (split-string-and-unquote (ros-dynamic-reconfigure-clean-dictionary-and-remove-subgroups dict) ","))
         (params #s(hash-table size 100 test equal data ())))
    (dolist (elem parameter_pairs params)
      (let ((pair (split-string-and-unquote elem ":")))
        (puthash (string-trim(car pair) "[ \t\n\r\']+"  "[ \t\n\r\']+") (string-trim(car (cdr pair))) params)))))

(defun ros-dynamic-reconfigure-completing-read-parameter(params)
  "Completing read function for parameters in hashtable PARAMS."
  (completing-read "Parameter:" (hash-table-keys params) nil t))

(defun ros-dynamic-reconfigure-read-new-value (parameter-name parameter-table)
  "Read new value for PARAMETER-NAME in hashtable PARAMETER-TABLE."
  (let* ((bool-collection '("True" "False"))
         (current-value (gethash parameter-name parameter-table))
         (collection (when (member current-value bool-collection) bool-collection)))
    (completing-read (format "%s: " parameter-name) collection nil collection (unless collection current-value) nil (when collection current-value))))

;;;###autoload
(defun ros-dynamic-reconfigure-set-param(node)
  "Dynamically reconfigure a parametern in NODE."
  (interactive (list (ros-dynamic-reconfigure-completing-read-node)))
  (let* ((parameter-table (ros-dynamic-reconfigure-list-params node))
         (parameter-name (ros-dynamic-reconfigure-completing-read-parameter  parameter-table))
         (new-value (ros-dynamic-reconfigure-read-new-value parameter-name parameter-table)))
    (ros-shell-command-to-string (ros-dynamic-reconfigure-cmd (format "set %s %s %s" node parameter-name new-value)))))

(defvar ros-env-ros-master nil)
(defvar ros-env-saved-ros-masters '(("default" . nil)))
(defvar ros-env-network-interface nil)

(defun ros-env-ros-master-uri ()
  "Return the current ROS-MASTER—URI."
  (when ros-env-ros-master
    (s-trim(format "http://%s:11311/" ros-env-ros-master))))

(defun ros-env-completing-read-ros-master ()
  "Completing Read function for saved ROS Masters.
This returns the master in the form \"(Name, Value)\""
  (completing-read "ROS—Master: " (mapcar 'car ros-env-saved-ros-masters) nil t nil nil (when ros-env-ros-master (car (rassoc ros-env-ros-master ros-env-saved-ros-masters)))))

;;;###autoload
(defun ros-env-set-ros-master (new-master)
  "Prompt for NEW-MASTER to set the variable `ros-env-ros-master'."
  (interactive  (list (ros-env-completing-read-ros-master)))
  (setq ros-env-ros-master (cdr(assoc (s-trim(car(split-string new-master "("))) ros-env-saved-ros-masters)))
  (message (concat "ROS Master is set to " ros-env-ros-master)))

(defun ros-env-get-ip-address (dev)
  "Return the IP-address for network device DEV."
  (format-network-address (car (network-interface-info dev)) t))

(defun ros-env-ros-ip ()
  "Return the IP of the current `ros-env-network-interface'.
If this is not set return nil"
  (when ros-env-network-interface
    (car (split-string (ros-env-get-ip-address ros-env-network-interface) ":"))))

(defun ros-env-network-interfaces ()
  "List the network interfaces of the machine."
  (mapcar (lambda (name-ip-pair) (ros-env-network-interface-description-string (car name-ip-pair) (car(split-string(format-network-address(cdr name-ip-pair) ":"))))) (network-interface-list)))

(defun ros-env-network-interface-description-string (name ip)
  "Format NAME and IP as \"NAME (IP)\"."
  (format "%s (%s)" name ip))

(defun ros-env-completing-read-network-interface()
  "Completing read function for network interfaces."
  (completing-read "Network interface:" (ros-env-network-interfaces) nil t nil nil (when ros-env-network-interface (ros-env-network-interface-description-string ros-env-network-interface (ros-env-ros-ip)))))

;;;###autoload
(defun ros-env-select-network-interface (interface)
  "Prompt for INTERFACE to set the variable `ros-env-network-interface'."
  (interactive (list (ros-env-completing-read-network-interface)))
  (setq ros-env-network-interface (s-trim(car (split-string interface "(")))))

(defun ros-process-roscore-running-p ()
  "Return t if there is a roscore running on the system, nil otherwise."
  (not(string= (ros-shell-command-to-string "rosnode list") "ERROR: Unable to communicate with master!")))

(defun ros-env-completing-read-host-directory()
  "Completing read function for host directory."
  (completing-read "Host Directory: " ros-env-saved-host-directory nil nil ros-env-host-directory))

;;;###autoload
(defun ros-env-select-host-directory(directory)
  "Prompt for DIRECTORY and set it to host directory."
  (interactive (list (ros-env-completing-read-host-directory)))
  (setq ros-env-host-directory directory))


(defhydra hydra-ros-main (:color blue :hint nil :foreign-keys warn)
  "
_c_: Compile          _m_: Messages      _p_: Packages          _T_: Topics
_d_: Debug            _n_: Nodes         _S_: Active Services   _t_: Tests
_e_: Environment      _P_: Parameters     _s_: Services         _w_: Workspaces
"
  ("c" hydra-ros-compile/body)
  ("e" hydra-ros-environment/body)
  ("s" hydra-ros-services/body)
  ("m" hydra-ros-messages/body)
  ("n" hydra-ros-nodes/body)
  ("d" hydra-ros-debug/body)
  ("p" hydra-ros-packages/body)
  ("P" hydra-ros-parameters/body)
  ("S" hydra-ros-active-services/body)
  ("T" hydra-ros-topics/body)
  ("t" hydra-ros-tests/body)
  ("w" hydra-ros-workspaces/body)
  ("q" nil "quit" :color blue))

(defhydra hydra-ros-compile (:color blue :hint nil :foreign-keys warn)
 "
_r_: Compile from history  _p_: Build current package  _w_: Build current workspace
^ ^                        _P_: Build a package        _W_: Build a workspace
"
  ("r" ros-catkin-compile-from-history)
  ("p" ros-catkin-build-current-package)
  ("P" ros-catkin-build-package)
  ("w" ros-catkin-build-current-workspace)
  ("W" ros-catkin-build-workspace)
  ("q" nil "quit hydra")
  ("^" hydra-ros-main/body "Go back"))
  
(defhydra hydra-ros-environment (:color blue :hint nil :foreign-keys warn)
  "
_m_: Set ROS Master _n_: Set Network Interface _h_: Set Host Directory
"
  ("m" ros-env-set-ros-master)
  ("n" ros-env-select-network-interface)
  ("h" ros-env-select-host-directory)
  ("q" nil "quit hydra")
  ("^" hydra-ros-main/body "Go back"))

(defhydra hydra-ros-services (:color blue :hint nil :foreign-keys warn)
  "
_i_: Insert srv type at point               _s_: Show srv 
_I_: Insert import statement for srv type
"
  ("s" ros-srv-show)
  ("i" ros-insert-srv)
  ("I" ros-insert-import-srv)
  ("q" nil "quit hydra")
  ("^" hydra-ros-main/body "Go back"))


(defhydra hydra-ros-active-services (:color blue :hint nil :foreign-keys warn)
  "
_s_: Show active Service    _c_: Call active service
"
  ("s" ros-service-show)
  ("c" ros-service-call)
  ("q" nil "quit hydra")
  ("^" hydra-ros-main/body "Go back"))


(defhydra hydra-ros-workspaces (:color blue :hint nil :foreign-keys warn)
  "
_s_: Select Workspace _c_: Build current workspace _x_: Clean current workspace   _t_: Test current workspace
_p_: Select Profile   _C_: Build a workspace       _X_: Clean a workspace         _T_: Test a workspace     
"
  ("s" ros-select-workspace)
  ("p" ros-select-profile)
  ("c" ros-catkin-build-current-workspace)
  ("C" ros-catkin-build-workspace)
  ("x" ros-catkin-clean-current-workspace)
  ("X" ros-catkin-clean-workspace)
  ("t" ros-catkin-test-current-workspace)
  ("T" ros-catkin-test-workspace)
  ("q" nil "quit hydra")
  ("^" hydra-ros-main/body "Go back"))


(defhydra hydra-ros-topics (:color blue :hint nil :foreign-keys warn)
  "
_e_: Echo Topic                   _s_: Show Topic                   _p_: Publish message on topic                   _i_: Insert name of topic at point
_E_: Echo Topic filtered by type  _S_: Show Topic filtered by type  _P_: Publlish message on topic filtered by type _I_: Insert name of topic at point filtered by type
"
  ("e" ros-topic-echo) 
  ("E" ros-topic-echo-filtered) 
  ("s" ros-topic-show)
  ("S" ros-topic-show-filtered)
  ("p" ros-topic-pub)
  ("P" ros-topic-pub-filtered)
  ("i" ros-insert-topic)
  ("I" ros-insert-topic-filtered)
  ("q" nil "quit hydra")
  ("^" hydra-ros-main/body "Go back"))

(defhydra hydra-ros-messages (:color blue :hint nil :foreign-keys warn)
  "
_i_: Insert message type at point               _s_: Show message 
_I_: Insert import statement for message type
"
  ("s" ros-msg-show)
  ("i" ros-insert-msg)
  ("I" ros-insert-import-msg)
  ("q" nil "quit hydra")
  ("^" hydra-ros-main/body "Go back"))

(defhydra hydra-ros-nodes (:color blue :hint nil :foreign-keys warn)
  "
_s_: Show node
"
  ("s" ros-node-show)
  ("q" nil "quit hydra")
  ("^" hydra-ros-main/body "Go back"))

(defhydra hydra-ros-debug (:color blue :hint nil :foreign-keys warn)
  "
_l_: Set ROS logger level 
"
  ("l" ros-logger-set-level)
  ("q" nil "quit hydra")
  ("^" hydra-ros-main/body "Go back"))

(defhydra hydra-ros-packages (:color blue :hint nil :foreign-keys warn)
  "
_c_: Build current package _t_: Test current package _x_: Clean current package   _f_: Find file in current package _g_: Grep in current package
_C_: Build a package       _T_: Test a package       _X_: Clean a package         _F_: Open a package in dired
"
  ("c" ros-catkin-build-current-package)
  ("C" ros-catkin-build-package)
  ("t" ros-catkin-test-current-package)
  ("T" ros-catkin-test-package)
  ("x" ros-catkin-clean-current-package)
  ("X" ros-catkin-clean-package)
  ("f" spaceros-find-in-current-package)
  ("F" ros-dired-package)
  ("g" spaceros-grep-in-current-package)
  ("q" nil "quit hydra")
  ("^" hydra-ros-main/body "Go back"))


(defhydra hydra-ros-tests (:color blue :hint nil :foreign-keys warn)
  "
_t_: Run test at point   _f_: Run current test file  _w_: Test current workspace  _p_: Run tests in current package
^ ^                      ^ ^                         _W_: Test a workspace        _P_: Run tests in a package
"
  ("t" ros-catkin-test-at-point)
  ("f" ros-catkin-test-current-test-file)
  ("w" ros-catkin-test-current-workspace)
  ("W" ros-catkin-test-workspace)
  ("p" hydra-ros-test-current-package/body)
  ("P" hydra-ros-test-a-package/body)
  ("q" nil "quit hydra")
  ("^" hydra-ros-main/body "Go back"))

(defhydra hydra-ros-test-current-package (:color blue :hint nil :foreign-keys warn)
  "
_f_: Run a test file   _a_: Run all tests
"
  ("f" ros-catkin-test-file-in-current-package)
  ("a" ros-catkin-test-current-package)
  ("q" nil "quit hydra")
  ("^" hydra-ros-tests/body "Go back"))


(defhydra hydra-ros-test-a-package (:color blue :hint nil :foreign-keys warn)
  "
_f_: Run a test file in a package   _a_: Run all tests in a package
"
  ("f" ros-catkin-test-file-in-package)
  ("a" ros-catkin-test-package)
  ("q" nil "quit hydra")
  ("^" hydra-ros-tests/body "Go back"))

(defhydra hydra-ros-parameters (:color blue :hint nil :foreign-keys warn)
  "
_s_: Set ROS parameter   _d_: Set dynamic reconfigure parameter
"
  ("s" ros-param-set)
  ("d" ros-dynamic-reconfigure-set-param)
  ("q" nil "quit hydra")
  ("^" hydra-ros-main/body "Go back"))

(provide 'ros)

;;; ros.el ends here

