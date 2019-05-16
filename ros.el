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

(defcustom ros-default-workspace (format "/opt/ros/%s"
                                         ros-distro)
  "Path to binary/devel directory of default catkin workspace."
  :group 'ros-workspace
  :type 'directory)

(defvar ros-current-workspace nil "Path to binary/devel directory of current catkin workspace.")

(defun ros-current-workspace ()
  "Return path to binary/devel directory of current catkin workspace or to default workspace if not set."
  (if ros-current-workspace ros-current-workspace ros-default-workspace))

(defcustom ros-workspaces (list ros-default-workspace)
  "List of paths to binary/devel directories of catkin workspaces."
  :group 'ros-workspace
  :type 'sexp)


(defvar ros-setup-file-extension (let ((shell (getenv "SHELL")))
                                      (cond
                                       ((s-suffix-p "zsh" shell) ".zsh")
                                       ((s-suffix-p "bash" shell) ".bash")
                                       (t ".sh"))))
(defun ros-setup-file-path (path)
  "Return the path to the right setup file in PATH."
  (concat (file-name-as-directory path) "setup" ros-setup-file-extension)
  )

(defun ros-source-workspace-command (workspace)
  "Return the right sourcing command for this workspace at PATH."
  (format "source %s" (ros-setup-file-path workspace)))

(defun ros-completing-read-workspace ()
  "Read a workspace from the minibuffer."
  (completing-read "Workspace: " ros-workspaces nil t nil nil (ros-current-workspace)))

(defun ros-select-workspace (path)
  "Set `ros-current-workspace' to PATH."
  (interactive (list (ros-completing-read-workspace)))
  (setq ros-current-workspace path))


(defun ros-shell-command-to-string (cmd &optional workspace)
  "Source the current workspace and run CMD and return the output as string."
  (let ((wspace (if workspace workspace (ros-current-workspace))))
    (shell-command-to-string (format "%s && %s" (ros-source-workspace-command wspace) cmd))))

(defun ros-shell-output-as-list (cmd &optional workspace)
  "Run CMD and return a list of each line of the output."
  (let ((wspace (if workspace workspace (ros-current-workspace))))
    (split-string (ros-shell-command-to-string cmd workspace)
                  "\n")))

(defun ros-run-process(cmd buffer-name workspace)
  "Source workspace, run CMD, print output in BUFFER-NAME."
  (let* ((wspace (if workspace workspace (ros-current-workspace)))
         (process (start-process buffer-name buffer-name (format "%s && %s" (ros-source-workspace-command wspace) cmd))))
    (pop-to-buffer buffer-name)
    (ros-info-mode)))

(defun ros-packages ()
  "List all available ros packages in the current workspace."
  (ros-shell-output-as-list "rospack list-names"))

(defun ros-catkin-command (cmd workspace)
  "Run catkin CMD after sourcing WORKSPACE."
  (let* ((default-directory workspace)
         (compilation-buffer-name-function (lambda (major-mode-name) "*catkin build*")))
    (compile (format "%s && catkin %s" (ros-source-workspace-command workspace) cmd))))

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
         (buffer-name (concat "*rostopic:" topic-full-name "*"))
         (process (start-process buffer-name buffer-name "rostopic" "echo" topic-full-name)))
    (view-buffer-other-window (process-buffer process))
    (ros-info-mode)))

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

(define-derived-mode ros-service-call-mode text-mode "ros-service-call-mode"
  "major mode for calling ros services")
(define-key ros-service-call-mode-map (kbd "C-c C-c") 'ros-service-call-buffer)


(defun ros-topic-pub-buffer (arg)
  "Publish the mesage defined in the buffer, if ARG is nil publish the message one, otherwise ARG is the rate of the publishing."
  (interactive (list current-prefix-arg))
  (let* ((topic (buffer-name))
         (type (ros-get-topic-type topic))
         (message-text (buffer-string))
         (buffer-name (concat "*rostopic pub " topic "*"))
         (old-buffer (current-buffer))
         (rate-argument (if arg (format "-r %d" (prefix-numeric-value arg)) "--once"))
         (process (start-process buffer-name buffer-name "rostopic" "pub" topic type (concat "" message-text) rate-argument)))
    
    (switch-to-buffer (process-buffer process))
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
         (message-lines (split-string(buffer-string) "\n"))
         (buffer-name (concat "*rosservice call " service "*"))
         (old-buffer (current-buffer))
         (process (start-process buffer-name buffer-name "rosservice" "call" service (string-join message-lines "\n"))))
    
    (switch-to-buffer (process-buffer process))
    (kill-buffer old-buffer)))



(defun ros-insert-import-msg (name)
  "Ask for message and include it in file."
  (interactive (list (ros-generic-completing-read "msg")))
  (ros-insert-import "msg" name)
  )


(defun ros-insert-import-srv (name)
  "Ask for service and include it in file."
  (interactive (list (ros-generic-completing-read "srv")))
  (ros-insert-import "srv" name)
  )

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

(provide 'ros)

;;; ros.el ends here

