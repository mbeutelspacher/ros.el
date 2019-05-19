;;; ros.el --- package to interact with and write code for ROS systems

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

;; package to interact with and write code for ROS systems

;;; Code:
(require 'dired)
(require 's)
(require 'subr-x)

(defgroup ros nil "Related to the Robot Operating System." :group 'external)

(defcustom ros-distro (getenv "ROS_DISTRO") "Name of ROS Distribution." :type 'string :group 'ros)

(defcustom ros-default-workspace nil
  "Path to binary/devel directory of default catkin workspace."
  :group 'ros-workspace
  :type 'directory)

(defvar ros-current-workspace nil "Path to binary/devel directory of current catkin workspace.")

(defun ros-current-workspace ()
  "Return path to binary/devel directory of current catkin workspace or to default workspace if not set."
  (if ros-current-workspace ros-current-workspace ros-default-workspace))

(defvar ros-current-profile "default" "Profile in current workspace.")

(defcustom ros-workspaces (list ros-default-workspace)
  "List of paths to binary/devel directories of catkin workspaces."
  :group 'ros-workspace
  :type 'sexp)

(defvar ros-setup-file-extension (let ((shell (getenv "SHELL")))
                                      (cond
                                       ((s-suffix-p "zsh" shell) ".zsh")
                                       ((s-suffix-p "bash" shell) ".bash")
                                       (t ".sh"))))

(defun ros-catkin-source-workspace-command (workspace &optional profile)
  "Return the right sourcing command for WORKSPACE. If PROFILE is not nil, this profile is used, otherwise the default profile is used."
  (if (not workspace)
      (format "source /opt/ros/%s/setup%s" ros-distro ros-setup-file-extension)
    (let ((source-file (concat (ros-catkin-locate-command workspace "d" profile) "/setup" ros-setup-file-extension)))
      (unless (file-exists-p source-file)
        (let* ((extended-devel-space (ros-catkin-extended-devel-space workspace profile))
              (extended-source-file (concat extended-devel-space "/setup" ros-setup-file-extension)))
          (message extended-source-file)
          (if (and (file-exists-p extended-source-file) (y-or-n-p (concat source-file " does not exist, do you want to source " extended-source-file" instead?")))
              (setq source-file extended-source-file)
              (error "%s could not be sourced" source-file))))
      (concat "source " source-file))))

(defun ros-catkin-extended-devel-space (workspace &optional profile)
  "Return the path to the devel space that WORKSPACE with optinal PROFILE or default profile extends."
  (let ((profile-flag (if profile (concat "--profile " profile) "")))
    (s-trim (car (split-string (car (cdr (split-string (shell-command-to-string (format "cd %s && catkin --no-color config %s | awk '{if ($1 == \"Extending:\"){print $3}}'" workspace profile-flag)) "\n"))) ":"))))
  )

(defun ros-completing-read-workspace ()
  "Read a workspace from the minibuffer."
  (completing-read "Workspace: " ros-workspaces nil t nil nil (ros-current-workspace)))


(defun ros-select-workspace (workspace &optional profile)
  "Select current ROS workspace.
Set `ros-current-workspace' to WORKSPACE and `ros-current-profile' to PROFILE.
If called interactively prompt for WORKSPACE and PROFILE."
  (interactive (list (ros-completing-read-workspace)))
  (setq ros-current-workspace workspace)
  (if profile
      (setq ros-current-profile profile)
      (setq ros-current-profile (ros-completing-read-catkin-profiles workspace))
    ))

(defun ros-select-profile(profile)
  "Set `ros-current-profile' to PROFILE.  If called interactively prompt for PROFILE."
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
  (let ((profile-str (if profile (format "--profile %s" profile) "")))
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
in `ros-current-workspace' and `ros-current-profile'."
  (let ((wspace (if workspace workspace (ros-current-workspace)))
         (prof (if profile profile ros-current-profile))
        (export-master-uri (if (ros-env-ros-master-uri) (format "export ROS_MASTER_URI=%s" (ros-env-ros-master-uri)) "true"))
        (export-ros-ip (if (ros-env-ros-ip) (format "export ROS_IP=%s" (ros-env-ros-ip)) "true"))
        )
    (format "%s && %s && %s && %s" export-master-uri export-ros-ip (ros-catkin-source-workspace-command wspace prof) cmd)))

(defun ros-shell-command-to-string (cmd &optional workspace profile)
  "Run CMD after sourcing workspace and return output as a string.
If workspace or profile are nil the ones specified in `ros-current-workspace'
 and `ros-current-profile' are used."
(s-trim (shell-command-to-string (ros-shell-prepend-ros-environment-commands cmd workspace profile))))

(defun ros-shell-output-as-list (cmd &optional workspace)
  "Run CMD after sourcing workspace and return output lines as a list.
The WORKSPACE or if nil the one returned by `ros-current-workspace' is sourced."
    (split-string (ros-shell-command-to-string cmd workspace) "\n"))

(defun ros-packages ()
  "List all available ROS packages in the current workspace."
  (ros-shell-output-as-list "rospack list-names"))

(defun ros-completing-read-packages ()
  "Completing read function for ROS packages."
  (completing-read "Package: " (ros-packages) nil t ))

(defun ros-current-package ()
  "Return the name of the ROS package the current buffer lies in.
If the current buffer does not lie in a ROS package return nil."
  (interactive)
  (let* ((package-path (locate-dominating-file (ros-current-directory) "package.xml")))
    (when package-path (file-name-nondirectory(directory-file-name(file-name-directory package-path))))))

(defun ros-current-directory ()
  "Return the directory of the current buffer or the current dired directory."
  (if (buffer-file-name) (file-name-directory (buffer-file-name)) (dired-current-directory)))

(defun ros-catkin-compile-command (cmd workspace &optional profile additional_cmd)
  "Run catkin CMD after sourcing WORKSPACE with optional PROFILE.
If ADDITIONAL_CMD is not nil, run it after the command."
  (let* ((default-directory workspace)
         (compilation-buffer-name-function (lambda (major-mode-name) "*catkin*"))
         (profile-flag (if profile (format "--profile %s" profile) ""))
         (add-cmd (if additional_cmd (format "&& %s" additional_cmd) "")))
    (compile (ros-shell-prepend-ros-environment-commands (format "catkin %s %s && %s" cmd profile-flag add-cmd) workspace profile))))

(defun ros-catkin-build-workspace(workspace &optional profile)
  "Build the WORKSPACE with PROFILE or default if not provided.
If called interactively prompt for WORKSPACE and PROFILE."
  (interactive (list (ros-completing-read-workspace)))
  (let ((prof (if profile profile (ros-completing-read-catkin-profiles workspace))))
    (ros-catkin-compile-command "build" workspace prof)))

(defun ros-catkin-build-current-workspace()
  "Build the workspace with profile specified in `ros-current-workspace' and `ros-current-profile'."
  (interactive)
  (ros-catkin-build-workspace (ros-current-workspace) ros-current-profile))

(defun ros-catkin-build-package(package)
  "Build the ROS package PACKAGE in the workspace specified in `ros-current-workspace' and with the profile specified in `ros-current-profile'."
  (interactive (list (ros-completing-read-packages)))
  (ros-catkin-compile-command (format "build %s" package) (ros-current-workspace) ros-current-profile))

(defun ros-catkin-build-current-package ()
  "If the current buffer is part of a ROS package in the workspace specified by  `ros-current-workspace', build it."
  (interactive)
  (ros-catkin-build-package (ros-current-package)))

(defun ros-catkin-clean-workspace (workspace &optional profile)
  "Run `catkin clean' in WORKSPACE with optional PROFILE."
  (interactive (list (ros-completing-read-workspace)))
  (let ((prof (if profile profile (ros-completing-read-catkin-profiles workspace))))
    (when (y-or-n-p (concat "Do you really want to clean " workspace " %s with profile " prof " ?"))
        (ros-catkin-compile-command "clean -y" workspace profile))))

(defun ros-catkin-clean-current-workspace()
  "Run `catkin clean' in workspace with profile specified in `ros-current-workspace' and `ros-current-profile'."
  (interactive)
  (ros-catkin-clean-workspace (ros-current-workspace) ros-current-profile))

(defun ros-catkin-clean-package (package)
  "Clean the ROS PACKAGE."
  (interactive (list (ros-completing-read-packages)))
    (when (y-or-n-p (concat "Do you really want to clean " package "?"))
      (ros-catkin-compile-command "clean -y" package)))

(defun ros-catkin-clean-current-package()
  "If the current buffer is part of a ROS package in the workspace specified by  `ros-current-workspace', clean it."
  (interactive)
  (ros-catkin-clean-package (ros-current-package)))
 
(defun ros-catkin-test-package(package)
  "Build and run all unittests in PACKAGE."
  (interactive (list (ros-completing-read-packages)))
  (ros-catkin-compile-command (concat "run_tests --no-deps " package) (ros-current-workspace) ros-current-profile (concat "catkin_test_results build/" package) )
  )

(defun ros-catkin-test-current-package()
  "Build and run all unittests in the package the current buffer lies in."
  (interactive)
  (ros-catkin-test-package (ros-current-package)))

(defun ros-catkin-test-workspace(workspace &optional profile)
  "Build and run all unittests in WORKSPACE with profile PROFILE."
  (interactive (list (ros-completing-read-workspace)))
  (let ((prof (if profile profile (ros-completing-read-catkin-profiles workspace))))
    (ros-catkin-compile-command "build --catkin-make-args run_tests" workspace prof)))

(defun ros-test-current-workspace()
  "Build and run all unittests in the workspace with profile specified in `ros-current-workspace' and `ros-current-profile'."
  (interactive)
  (ros-catkin-test-workspace (ros-current-workspace) ros-current-profile))

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
  (let ((command))
    (setq command (cond ((string= type "msg") "show")
                         (t "info")))
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
  "major mode for displaying ros info messages"
  )

(define-key ros-info-mode-map (kbd "S") 'ros-show-thing-at-point)
(define-key ros-info-mode-map (kbd "E") 'ros-echo-topic-at-point)
(define-key ros-info-mode-map (kbd "C") 'ros-call-service-at-point)
(define-key ros-info-mode-map (kbd "K") 'ros-kill-node-at-point)

(defun ros-msg-show (msg)
  "Prompt for MSG and show structure."
  (interactive (list (ros-generic-completing-read "msg")))
  (ros-generic-show-info "msg" msg))

(defun ros-topic-show (topic)
  "Prompt for TOPIC and show subscribers and publishers."
  (interactive (list (ros-generic-completing-read "topic")))
  (ros-generic-show-info "topic" topic))
  
(defun ros-service-show (service)
  "Prompt for active SERVICE and show structure."
  (interactive (list (ros-generic-completing-read "service")))
  (ros-generic-show-info "service" service))

(defun ros-srv-show (service)
"Prompt for (not necessarily active) SERVICE and show structure."
(interactive (list (ros-generic-completing-read "srv")))
(ros-generic-show-info "srv" service))

(defun ros-node-show (node)
  "Prompt for NODE and show published and subscribed topics and provided services."
  (interactive (list (ros-generic-completing-read "node")))
  (ros-generic-show-info "node" node))

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

(defun ros-echo-topic-at-point ()
  "Get thing at point and if it is a topic echo it."
  (interactive)
  (let ((thing (thing-at-point 'symbol)))
    (if (member thing (ros-generic-list "topic"))
        (ros-topic-echo thing)
        (message (format "%s is not an active topic" thing)))))

(defun ros-call-service-at-point ()
  "Get thing at point and if it is a service call it."
  (interactive)
  (let ((thing (thing-at-point 'symbol)))
    (if (member thing (ros-generic-list "service"))
        (ros-service-call thing)
      (message (format "%s is not an active service" thing)))))


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
                         (progn
                           (ros-shell-command-to-string (format "rosnode kill %s" node))
                           (if (member node (ros-generic-list "node"))
                               (message (format "Failed to kill node %s" node))
                             (message (format "Killed node %s successfully" node)))))
    (message (format "There is no node %s to kill" node))))


(defun ros-topic-echo (topic)
  "Prompt for TOPIC and echo it."
  (interactive (list (ros-generic-completing-read "topic")))
  (let* ((topic-full-name (if (string-match "^/" topic) topic (concat "/" topic)))
         (buffer-name (concat "*rostopic:" topic-full-name "*")))
    (ros-process-start-process buffer-name (concat "rostopic echo " topic-full-name))))

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
These sections help to identitfy the type of the symbol at point e.g. Topic, Node etc."
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
    (if (string= type "srv") (ros-service-call-mode) (ros-topic-pub-mode))
    ))
  
(define-derived-mode ros-topic-pub-mode text-mode "ros-topic-pub-mode"
  "major mode for publishing ros msg"
  )
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
  (let* ((info (ros-generic-get-info type topic))
         (test (string-match "Type: \\(.*\\)\n" info)))
    (match-string 1 info)))

(defun ros-topic-pub (topic)
  "Draft ros message to be published on TOPIC."
  (interactive (list (ros-generic-completing-read "topic")))
  (ros-generate-prototype "msg" (ros-get-topic-type (s-trim-right topic)) topic))

(defun ros-service-call (topic)
  "Draft a service call to be called on TOPIC."
  (interactive (list (ros-generic-completing-read "service")))
  (ros-generate-prototype "srv" (ros-get-service-type topic) topic))

(defun ros-service-call-buffer ()
  "Call the service specified in the buffer."
  (interactive)
  (let* ((service (buffer-name))
         (type (ros-get-service-type service))
         (arguments (string-trim (s-trim (buffer-string)) "\"" "\""))
         (buffer-name (concat "*rosservice call " service "*"))
         (old-buffer (current-buffer)))
    (ros-process-start-process buffer-name (concat "rosservice call " service " " arguments))
    (kill-buffer old-buffer)))

(defun ros-insert-import-msg (message)
  "Prompt for MESSAGE and include it in file."
  (interactive (list (ros-generic-completing-read "msg")))
  (ros-insert-import "msg" message))


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
          (t (message "Only works in Python and C++ mode")))
  )
)
(defun ros-insert-import-python (type package name)
  "Insert TYPE (either msg or srv) definition for NAME which is part of PACKAGE in the current python buffer."
  (let ((start-import-statement (format "from %s.%s import" package type)))
      (progn
        (when (not (ros-import-is-included-python-p type package name))
            (if (ros-import-search-same-package-import-python type package)
                (progn
                  (goto-char (ros-import-search-same-package-import-python type package))
                  (move-end-of-line nil)
                  (insert (format ", %s" name)))
              (progn
                (goto-char (ros-insert-import-python-best-import-location type))
                (end-of-line)
                (newline-and-indent)
                (insert (format "%s %s" start-import-statement name))))))))

(defun ros-insert-import-python-best-import-location (type)
    "Return the best location for an python import of TYPE.
TYPE can be either msg or srv.
The best location would be another import of this TYPE,
the second best another import and lastly the beginning of the buffer."
    (or (ros-string-in-buffer-p (format "from .*\.%s import .*" type)) (ros-string-in-buffer-p "import") (point-min)))

(defun ros-string-in-buffer-p (string)
  "Return t if STRING is in the current buffer, nil otherwise."
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
the third best another incleude
and lastly the beginning of the buffer."
  (or (ros-string-in-buffer-p (format "#include <%s/.*>" package)) (ros-string-in-buffer-p (format "#include <.*%ss/.*>" type)) (ros-string-in-buffer-p "#include") (point-min))
  )

(defun ros-dired-package (package)
  "Open the root of PACKAGE in dired."
  (interactive (list (ros-completing-read-packages)))
  (dired (s-trim(ros-shell-command-to-string (concat "rospack find " package)))))

(defun ros-loggers (node)
  "List of all current loggers of NODE."
  (ros-shell-output-as-list (concat "rosconsole list " node)))

(defun ros-completing-read-logger(node)
  "Completing read function for loggers in NODE."
    (completing-read "Logger: " (ros-loggers node) nil t))

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

(defun ros-param-set(parameter)
  "Prompt for PARAMETER and set it to a new value."
  (interactive (list (ros-param-completing-read)))
  (let* ((current-value (string-trim (ros-shell-command-to-string (concat "rosparam get " parameter)) "'" "'"))
         (new-value (ros-param-read-value parameter current-value)))
    (ros-shell-command-to-string (concat "rosparam set " parameter " " new-value))))

(defun ros-param-read-value (parameter old-value)
  "Prompt for a new value for PARAMETER, the collection is generted based on the OLD-VALUE of PARAMETER."
  (let ((collection)
        (bool-collection '("true" "false")))
    (when (member old-value bool-collection) (setq collection bool-collection))
    (completing-read (format "%s: " parameter) collection nil collection (unless collection old-value) nil (when collection old-value))))


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

(defun ros-env-set-ros-master (new-master)
  "Prompt for NEW-MASTER to set it to `ros-env-ros-master'."
  (interactive  (list (ros-env-completing-read-ros-master)))
  (setq ros-env-ros-master (cdr(assoc (s-trim(car(split-string new-master "("))) ros-env-saved-ros-masters)))
  (message (concat "ROS Master is set to " ros-env-ros-master))
  )

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

(defun ros-env-select-network-interface (interface)
  "Prompt for INTERFACE to set it to `ros-env-network-interface'."
  (interactive (list (ros-env-completing-read-network-interface)))
  (setq ros-env-network-interface (s-trim(car (split-string interface "(")))))

(defun ros-process-roscore-running-p ()
  "Return t if there is a roscore running on the system, nil otherwise."
  (not(string= (ros-shell-command-to-string "rosnode list") "ERROR: Unable to communicate with master!")))

(provide 'ros)

;;; ros.el ends here

