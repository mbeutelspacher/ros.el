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

(require 'with-shell-interpreter)
(require 's)

(defgroup ros nil "Related to the Robot Operating System."
  :group 'external)

(defvar ros-current-workspace nil "Path to binary/devel directory of current catkin workspace.")

(defvar ros-current-profile "default" "Name of the current profile in `ros-current-workspace'.")

(defvar ros-current-tramp-prefix nil "Prefix to all paths such that tramp can be used to run commands on remote systems.")

(defun ros-shell-command-to-string (cmd)
  "Run CMD in `ros-current-workspace' on `ros-current-tramp-prefix' or the host system if `ros-current-tramp-prefix' is nil and return the output as string."
  (s-trim (with-shell-interpreter :path (concat ros-current-tramp-prefix ros-current-workspace):form
            (shell-command-to-string (format "/bin/bash -c \"%s\"" cmd)))))

(defun ros-catkin-locate-devel ()
  "Return the path to the devel folder in `ros-current-workspace' with profile `ros-current-profile'."
  (s-trim (ros-shell-command-to-string (concat "catkin locate -d --profile " ros-current-profile))))



(provide 'ros)

;;; ros.el ends here
