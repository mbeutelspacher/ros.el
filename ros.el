;;; ros.el --- package to interact with and write code for ROS systems

;; Copyright (C) 2019 Max Beutelspacher

;; Author: Max Beutelspacher <max.beutelspacher@mailbox.org>
;; Version: 0.1

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

(defcustom ros-distro (getenv "ROS_DISTRO") "Name of ROS Distribution.")

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

(defun ros-source-workspace-command (workspace &optional profile)
  "Return the right sourcing command for this workspace at PATH."
  (if (not workspace)
      (format "source /opt/ros/%s/setup%s" ros-distro ros-setup-file-extension)
    (let ((source-file (concat (ros-catkin-locate-command workspace "d" profile) "/setup" ros-setup-file-extension)))
      (unless (file-exists-p source-file)
        (let* ((extended-devel-space (ros-catkin-get-extended-devel-space workspace profile))
              (extended-source-file (concat extended-devel-space "/setup" ros-setup-file-extension)))
          (message extended-source-file)
          (if (and (file-exists-p extended-source-file) (y-or-n-p (format "%s does not exist, do you want to source %s instead?" source-file extended-source-file)))
              (setq source-file extended-source-file)
              (error "%s could not be sourced" source-file))))
      (concat "source " source-file))))

(defun ros-catkin-get-extended-devel-space (workspace &optional profile)
  (let ((profile-flag (if profile (concat "--profile " profile) "")))
    (s-trim (car (split-string (car (cdr (split-string (shell-command-to-string (format "cd %s && catkin --no-color config %s | awk '{if ($1 == \"Extending:\"){print $3}}'" workspace profile-flag)) "\n"))) ":"))))
  )

(defun ros-completing-read-workspace ()
  "Read a workspace from the minibuffer."
  (completing-read "Workspace: " ros-workspaces nil t nil nil (ros-current-workspace)))


(defun ros-select-workspace (workspace &optional profile)
  "Set `ros-current-workspace' to WORKSPACE."
  (interactive (list (ros-completing-read-workspace)))
  (setq ros-current-workspace workspace)
  (if profile 
      (setq ros-current-profile profile)
      (setq ros-current-profile (ros-completing-read-catkin-profiles workspace))
    ))

(defun ros-select-profile()
  (interactive)
  (ros-select-workspace (ros-current-workspace)))

(defun ros-catkin-locate-command (workspace flag &optional profile) 
  (let ((profile-str (if profile (format "--profile %s" profile) "")))
    (s-trim(shell-command-to-string (format "cd %s && catkin locate -%s %s" workspace flag profile-str)))))

(defun ros-list-catkin-profiles (workspace)
  (ros-shell-output-as-list (format "cd %s && catkin profile list -u" workspace)))

(defun ros-completing-read-catkin-profiles (workspace)
  (let ((profiles (ros-list-catkin-profiles workspace)))
    (completing-read (format "Profile: " ) profiles nil t nil nil (if (member ros-current-profile profiles) ros-current-profile "default"))))

(defun ros-shell-command-to-string (cmd &optional workspace)
  "Source the current workspace and run CMD and return the output as string."
  (let ((wspace (if workspace workspace (ros-current-workspace)))
        (export-master-uri (if (ros-env-ros-master-uri) (format "export ROS_MASTER_URI=%s" (ros-env-ros-master-uri)) "true"))
        (export-ros-ip (if (ros-env-ros-ip) (format "export ROS_IP=%s" (ros-env-ros-ip)) "true"))
        )
    (s-trim (shell-command-to-string (format "%s && %s && %s && %s" export-master-uri export-ros-ip (ros-source-workspace-command wspace) cmd)))))

(defun ros-shell-output-as-list (cmd &optional workspace)
  "Run CMD and return a list of each line of the output."
  (let ((wspace (if workspace workspace (ros-current-workspace))))
    (split-string (ros-shell-command-to-string cmd workspace)
                  "\n")))

(defun ros-packages ()
  "List all available ros packages in the current workspace."
  (ros-shell-output-as-list "rospack list-names"))

(defun ros-completing-read-packages ()
  (completing-read "Package: " (ros-packages) nil t ))

(defun ros-current-package ()
  "returns the name of the current ros package"
  (interactive)
  (let* ((package-path (locate-dominating-file (ros-current-directory) "package.xml")))
    (file-name-nondirectory(directory-file-name(file-name-directory package-path)))))

(defun ros-current-directory ()
  (if (buffer-file-name) (file-name-directory (buffer-file-name)) (dired-current-directory)))

(defun ros-catkin-compile-command (cmd workspace &optional profile additional_cmd)
  "Run catkin CMD after sourcing WORKSPACE."
  (let* ((default-directory workspace)
         (compilation-buffer-name-function (lambda (major-mode-name) "*catkin*"))
         (profile-flag (if profile (format "--profile %s" profile) ""))
         (add-cmd (if additional_cmd (format "&& %s" additional_cmd) "")))
    (compile (format "%s && catkin %s %s %s" (ros-source-workspace-command workspace profile) cmd profile-flag add-cmd))))

(defun ros-catkin-build-workspace(workspace &optional profile)
  (interactive (list (ros-completing-read-workspace)))
  (let ((prof (if profile profile (ros-completing-read-catkin-profiles workspace))))
    (ros-catkin-compile-command "build" workspace prof)))

(defun ros-catkin-build-current-workspace()
  (interactive)
  (ros-catkin-build-workspace (ros-current-workspace) ros-current-profile))

(defun ros-catkin-build-package(package)
  (interactive (list (ros-completing-read-packages)))
  (ros-catkin-compile-command (format "build %s" package) (ros-current-workspace) ros-current-profile))

(defun ros-catkin-build-current-package ()
  (interactive)
  (ros-catkin-build-package (ros-current-package)))

(defun ros-catkin-clean-workspace (workspace &optional profile)
  (interactive (list (ros-completing-read-workspace)))
  (let ((prof (if profile profile (ros-completing-read-catkin-profiles workspace))))
    (when (y-or-n-p (format "Do you really want to clean %s with profile %s" workspace prof))
        (ros-catkin-compile-command "clean -y" workspace profile))))
(defun ros-catkin-clean-current-workspace()
  (interactive)
  (ros-catkin-clean-workspace (ros-current-workspace) ros-current-profile))

(defun ros-catkin-clean-package (package)
  (interactive (list (ros-completing-read-packages)))
    (when (y-or-n-p (format "Do you really want to clean %s" package))
      (ros-catkin-compile-command "clean -y" package)))

(defun ros-catkin-clean-current-package()
  (interactive)
  (ros-catkin-clean-package (ros-current-package)))
 
  
(defun ros-catkin-test-package(package)
  (interactive (list (ros-completing-read-packages)))
  (ros-catkin-compile-command (concat "run_tests --no-deps " package) (ros-current-workspace) ros-current-profile (concat "catkin_test_results build/" package) )
  )

(defun ros-catkin-test-current-package()
  (interactive)
  (ros-catkin-test-package (ros-current-package)))

(defun ros-catkin-test-workspace(workspace &optional profile)
  (interactive (list (ros-completing-read-workspace)))
  (let ((prof (if profile profile (ros-completing-read-catkin-profiles workspace))))
    ((ros-catkin-compile-command "build --catkin-make-args run_tests" workspace prof))))

(defun ros-test-current-workspace()
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
    (ros-process-start-process buffer-name "rostopic" (list "echo" topic-full-name))))

(defun ros-process-start-process (buffer-name cmd args)
  (let* ((master-uri (ros-env-ros-master-uri))
         (ros-ip (ros-env-ros-ip))
         (process-environment (cons (when master-uri (concat "ROS_MASTER_URI=" master-uri))(cons (when ros-ip (concat "ROS_IP=" ros-ip)) process-environment)))
         (process (apply 'start-process buffer-name buffer-name cmd (mapcar #'identity args))))
    (view-buffer-other-window (process-buffer process))
    (ros-process-mode)))

(defun ros-info-get-section ()
  "Get the section of thing at point."
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
         (buffer-name (concat "*rostopic pub " topic "*"))
         (old-buffer (current-buffer))
         (rate-argument (if arg (format "-r %d" (prefix-numeric-value arg)) "--once")))
    (ros-process-start-process buffer-name "rostopic" (list "pub" topic type message-text rate-argument))
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
    (ros-process-start-process buffer-name "rosservice" (list "call" service arguments))
    (kill-buffer old-buffer)))

(defun ros-insert-import-msg (name)
  "Ask for message and include it in file."
  (interactive (list (ros-generic-completing-read "msg")))
  (ros-insert-import "msg" name))


(defun ros-insert-import-srv (name)
  "Ask for service and include it in file."
  (interactive (list (ros-generic-completing-read "srv")))
  (ros-insert-import "srv" name))

(defun ros-insert-import (type name)
  (let ((package (car (split-string name "/")))
        (item-name (car (cdr(split-string name "/")))))
    (cond ((string= major-mode "python-mode") (ros-insert-import-python type package item-name))
          ((string= major-mode "c++-mode") (ros-insert-import-cpp type package item-name))
          (t (message "Only works in Python and C++ mode")))
  )
)
(defun ros-insert-import-python (type package name)
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
    (or (ros-string-in-buffer (format "from .*\.%s import .*" type)) (ros-string-in-buffer "import") (point-min))
)

(defun ros-string-in-buffer (string)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward string nil t)
    )
  )
(defun ros-import-is-included-python-p (type package name)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (format "from %s.%s import .*%s[, \n]" package type name) nil t)))

(defun ros-import-search-same-package-import-python (type package)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (format "from %s.%s import" package type) nil t)))


(defun ros-insert-import-cpp (type package name)
  (when (not (ros-import-is-included-cpp-p package name))
    (progn
      (goto-char  (ros-insert-import-cpp-best-import-location type package))
      (end-of-line)
      (newline-and-indent)
      (insert (format "#include <%s/%s.h>" package name)))))

(defun ros-import-is-included-cpp-p (package name)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (format "#include <%s\/%s.h>" package name) nil t)))

(defun ros-insert-import-cpp-best-import-location (type package)
  (or (ros-string-in-buffer (format "#include <%s/.*>" package)) (ros-string-in-buffer (format "#include <.*%ss/.*>" type)) (ros-string-in-buffer "#include") (point-min))
  )

(defun ros-dired-package (package)
  (interactive (list (ros-completing-read-packages)))
  (dired (s-trim(ros-shell-command-to-string (concat "rospack find " package)))))

(defun ros-loggers (node)
  (ros-shell-output-as-list (concat "rosconsole list " node)))

(defun ros-completing-read-logger(node)
    (completing-read "Logger: " (ros-loggers node) nil t))

(defun ros-logger-set-level ()
  (interactive)
  (let* ((node (ros-generic-completing-read "node"))
         (logger (ros-completing-read-logger node))
         (current-value (s-trim (ros-shell-command-to-string (concat "rosconsole get " node " " logger))))
         (new-value (completing-read "Level: " '("debug" "info" "warn" "error" "fatal") nil t nil nil current-value)))
    (ros-shell-command-to-string (concat "rosconsole set " node " " logger " " new-value))))

(defun ros-param-list()
  (ros-shell-output-as-list "rosparam list"))

(defun ros-param-completing-read()
  (completing-read "Parameter: " (ros-param-list) nil t))

(defun ros-param-set(parameter)
  (interactive (list (ros-param-completing-read)))
  (let* ((current-value (string-trim (ros-shell-command-to-string (concat "rosparam get " parameter)) "'" "'"))
         (new-value (ros-param-read-value parameter current-value)))
    (ros-shell-command-to-string (concat "rosparam set " parameter " " new-value))))

(defun ros-param-read-value (parameter old-value)
  (let ((collection)
        (bool-collection '("true" "false")))
    (when (member old-value bool-collection) (setq collection bool-collection))
    (completing-read (format "%s: " parameter) collection nil collection (unless collection old-value) nil (when collection old-value))))


(defvar ros-env-ros-master nil)
(defvar ros-env-saved-ros-masters '(("default" . nil)))
(defvar ros-env-network-interface nil)

(defun ros-env-ros-master-uri ()
  (when ros-env-ros-master
    (s-trim(format "http://%s:11311/" ros-env-ros-master))))

(defun ros-env-completing-read-ros-master ()
  (completing-read "ROS—Master: " (mapcar 'car ros-env-saved-ros-masters) nil t nil nil (when ros-env-ros-master (car (rassoc ros-env-ros-master ros-env-saved-ros-masters)))))


(defun ros-env-set-ros-master (new-master)
  (interactive  (list (ros-env-completing-read-ros-master)))
  (setq ros-env-ros-master (cdr(assoc (s-trim(car(split-string new-master "("))) ros-env-saved-ros-masters)))
  )

(defun ros-env-get-ip-address (dev)
  "get the IP-address for device DEV"
  (format-network-address (car (network-interface-info dev)) t))

(defun ros-env-ros-ip ()
  (when ros-env-network-interface
    (car (split-string (ros-env-get-ip-address ros-env-network-interface) ":"))))

(defun ros-env-network-interfaces ()
  ""
  (mapcar (lambda (name-ip-pair) (ros-env-network-interface-description-string (car name-ip-pair) (car(split-string(format-network-address(cdr name-ip-pair) ":"))))) (network-interface-list)))

(defun ros-env-network-interface-description-string (name ip)
  ""
  (format "%s (%s)" name ip))

(defun ros-env-completing-read-network-interface()
  (completing-read "Network interface:" (ros-env-network-interfaces) nil t nil nil (when ros-env-network-interface (ros-env-network-interface-description-string ros-env-network-interface (ros-env-ros-ip)))))

(defun ros-env-select-network-interface (interface)
  (interactive (list (ros-env-completing-read-network-interface)))
  (setq ros-env-network-interface (s-trim(car (split-string interface "(")))))


(provide 'ros)

;;; ros.el ends here

