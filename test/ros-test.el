;;; -*- lexical-binding: t; -*-
;;; ros-test.el --- Tests for ros.el


;; Copyright (C) 2013 Max Beutelspacher

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

;; Tests for ros.el

;;; Code:
(require 'ros)
(require 'buttercup)

(describe "ROS 1 Tests"
  :var (docker-name)
  (before-all
    (progn (shell-command "docker build . -t ros-el-noetic -f docker/Dockerfile_noetic")
           (setq docker-name (substring (s-trim(shell-command-to-string "docker run -it -d --name ros-el-noetic ros-el-noetic"))0 12))
           (async-shell-command "docker exec -it ros-el-noetic bash -c 'sleep 1 && source /opt/ros/noetic/setup.bash && roscore'")))
  (before-each  (progn (setq ros-workspaces (list (ros-dump-workspace :tramp-prefix (format "/docker:root@%s:" docker-name) :workspace "/ws" :extends '("/opt/ros/noetic/")) (ros-dump-workspace :tramp-prefix "" :workspace "/ws" :extends '("/opt/ros/noetic/")))) (setq ros-cache nil)) (setq ros-current-workspace (car ros-workspaces)))
  (after-all
    (shell-command "docker stop ros-el-noetic && docker rm ros-el-noetic"))
  (describe "Correct source command"
    (it "returns the correct source command"
      (expect (ros-current-source-command) :to-equal "source /opt/ros/noetic/setup.bash && test -f /ws/install/setup.bash && source /ws/install/setup.bash 2> /dev/null|| true")))
  (describe "Run Shell commands"
    (it "on local machine"
      (let ((ros-current-workspace (cl-second ros-workspaces)))
        (expect (ros-shell-command-to-string "hostname") :to-equal (s-trim(shell-command-to-string "hostname")))))
    (it "on docker machine"
      (expect (ros-shell-command-to-string "hostname") :to-equal docker-name)))
  (describe "Detect ROS Version"
    (it "detect ROS1"
      (expect (ros-current-version) :to-equal 1)))
  (describe "Packages"
    (it "List packages in workspace"
      (expect (ros-list-packages) :to-contain "navigation_tutorials"))
    (it "Get location of package"
      (let ((package-location-list (ros-list-package-locations)))
        (expect (kvalist->keys package-location-list) :to-contain "navigation_tutorials")
        (expect (cdr (assoc "navigation_tutorials" package-location-list)) :to-equal "/ws/src/navigation_tutorials/navigation_tutorials")))

    (it "List files of package"
      (let ((files (ros-package-files "navigation_tutorials")))
        (expect files :to-contain "package.xml")
        (expect files :to-contain "CMakeLists.txt")))
    (it  "Detect current package"
      (let ((default-directory (concat (ros-current-tramp-prefix) (ros-current-workspace) "/src/navigation_tutorials/roomba_stage/maps")))
        (expect (ros-current-package) :to-equal "roomba_stage")))
    (it  "current package returns nil if no package"
      (let ((default-directory (concat (ros-current-tramp-prefix) "~")))
        (expect (ros-current-package) :to-equal nil))))
  (describe "Colcon actions"
    (it "Can Load dumped colcon action"
      (expect (ros-load-colcon-action
               (ros-dump-colcon-action
                :workspace ros-current-workspace
                :verb "build"
                :flags '("--continue-on-error" "--packages-up-to navigation_tutorials") :post-cmd "echo foobar"))
              :to-equal (concat "source /opt/ros/noetic/setup.bash && colcon build --continue-on-error --packages-up-to navigation_tutorials && echo foobar"))))

  (describe "Work with messages and Services and Actions"
    (it "Can List messages"
      (let ((msgs (ros-list-messages)))
        (expect msgs :to-contain "std_msgs/String")
        (expect msgs :to-contain "nav_msgs/Path")))
    (it "Can insert Message Name in python"
      (with-temp-buffer
        (python-mode)
        (ros-insert-message "std_msgs/String")
        (expect (s-trim (buffer-string)) :to-equal "String")))
    (it "Can insert  Message Import in python"
      (with-temp-buffer
        (python-mode)
        (ros-insert-message-import "std_msgs/String" (point))
        (expect (s-trim (buffer-string)) :to-equal "from std_msgs.msg import String")))
    (it "Can insert Message Name in c++"
      (with-temp-buffer
        (c++-mode)
        (ros-insert-message "std_msgs/String")
        (expect (s-trim (buffer-string)) :to-equal "std_msgs::String")))
    (it "Can insert  Message Import in c++"
      (with-temp-buffer
        (c++-mode)
        (ros-insert-message-import "std_msgs/String" (point))
        (expect (s-trim (buffer-string)) :to-equal "#include <std_msgs/String.h>")))
    (it "Can List srvs"
      (let ((srvs (ros-list-srvs)))
        (expect srvs :to-contain "nav_msgs/LoadMap")
        (expect srvs :to-contain "std_srvs/SetBool")))
    (it "Can insert srv Name in python"
      (with-temp-buffer
        (python-mode)
        (ros-insert-srv "std_srvs/Trigger")
        (expect (s-trim (buffer-string)) :to-equal "Trigger")))
    (it "Can insert srv import in python"
      (with-temp-buffer
        (python-mode)
        (ros-insert-srv-import "std_srvs/Trigger" (point))
        (expect (s-trim (buffer-string)) :to-equal "from std_srvs.srv import Trigger")))
    (it "Can insert srv Name in c++"
      (with-temp-buffer
        (c++-mode)
        (ros-insert-srv "std_srvs/Trigger")
        (expect (s-trim (buffer-string)) :to-equal "std_srvs::Trigger")))
    (it "Can insert srv import in c++"
      (with-temp-buffer
        (c++-mode)
        (ros-insert-srv-import "std_srvs/Trigger" (point))
        (expect (s-trim (buffer-string)) :to-equal "#include <std_srvs/Trigger.h>"))))
  (describe "Work with rostopic, rosnode and rosservice"
    (it "Can list topics"
      (expect (ros-topic-list) :to-equal '("/rosout" "/rosout_agg")))
    (it "Can get type of topic"
      (expect (ros-topic-type "/rosout") :to-equal "rosgraph_msgs/Log"))
    (it "Can list nodes"
      (expect (ros-node-list) :to-equal '("/rosout")))
    (it "Can list services"
      (expect (ros-service-list) :to-equal '("/rosout/get_loggers" "/rosout/set_logger_level")))
    (it "Can get type of service"
      (expect (ros-service-type "/rosout/get_loggers") :to-equal "roscpp/GetLoggers"))
    )
  (describe "Can get Mixins from colcon"
    (it "build"
      (expect (ros-get-colcon-mixins "build") :to-contain "ccache"))
    (it "test"
      (expect (ros-get-colcon-mixins "test") :to-contain "linters-only")
      ))

  (describe "Cache"
    (it "Can Store And Retrieve in the cache"
      (ros-cache-store  "messages"  (list "std_msgs/String"))
      (expect (ros-cache-load "messages") :to-equal (list "std_msgs/String")))
    (it "Always takes the newest Value"
      (ros-cache-store "messages" (list  "std_msgs/String"))
      (ros-cache-store "messages" (list "nav_msgs/Path"))
      (expect (ros-cache-load "messages") :to-equal (list "nav_msgs/Path")))
    (it "If key does not exist run function and store the result in cache"
      (expect (ros-cache-load "messages" 'ros-list-messages) :to-equal (ros-list-messages))
      (expect (ros-cache-load "messages") :to-equal (ros-list-messages)))))

(describe "ROS 2 Tests"
  :var (docker-name)
  (before-all
    (progn (shell-command "docker build . -t ros-el-foxy -f docker/Dockerfile_foxy")
           (setq docker-name (substring (s-trim(shell-command-to-string "docker run -it -d --name ros-el-foxy ros-el-foxy"))0 12))))
  (before-each  (progn  (setq ros-workspaces (list (ros-dump-workspace :tramp-prefix (format "/docker:root@%s:" docker-name) :workspace "/ws" :extends '("/opt/ros/foxy/")) (ros-dump-workspace :tramp-prefix "" :workspace "/ws" :extends '("/opt/ros/foxy/")))) (setq ros-cache nil)) (setq ros-current-workspace (car ros-workspaces)))
  (after-all
    (shell-command "docker stop ros-el-foxy && docker rm ros-el-foxy"))
  (describe "Detect ROS Version"
    (it "detect ROS2"
      (expect (ros-current-version) :to-equal 2)))
  (describe "Packages"
    (it "List packages in workspace"
      (expect (ros-list-packages) :to-contain "sam_bot_description"))
    (it "Get location of package"
      (let ((package-location-list (ros-list-package-locations)))
        (expect (kvalist->keys package-location-list) :to-contain "sam_bot_description")
        (expect (cdr (assoc "sam_bot_description" package-location-list)) :to-equal "/ws/src/navigation2_tutorials/sam_bot_description")))
    (it "List files of package"
      (let ((files (ros-package-files "sam_bot_description")))
        (expect files :to-contain "package.xml")
        (expect files :to-contain "CMakeLists.txt")
        (expect files :to-contain "launch/display.launch.py")))
    (it  "Detect current package"
      (let ((default-directory (concat (ros-current-tramp-prefix) (ros-current-workspace) "/src/navigation2_tutorials/sam_bot_description")))
        (expect (ros-current-package) :to-equal "sam_bot_description"))))
  (describe "Work with rostopic, rosnode and rosservice"
    (it "Can list topics"
      (expect (ros-topic-list) :to-equal '("/parameter_events" "/rosout")))
    (it "Can get type of topic"
      (expect (ros-topic-type "/rosout") :to-equal "rcl_interfaces/msg/Log"))
    )
   (describe "Can get Mixins from colcon"
    (it "build"
      (expect (ros-get-colcon-mixins "build") :to-contain "ccache"))
    (it "test"
      (expect (ros-get-colcon-mixins "test") :to-contain "linters-only")
      ))

  (describe "Work with messages and srvs"
    (it "Can List messages"
      (let ((msgs (ros-list-messages)))
        (expect msgs :to-contain "std_msgs/msg/String")
        (expect msgs :to-contain "nav_msgs/msg/Path")))
    (it "Can List services"
      (let ((srvs (ros-list-srvs)))
        (expect srvs :to-contain "nav_msgs/srv/GetMap")
        (expect srvs :to-contain "std_srvs/srv/SetBool")))
    (it "Can List actions"
      (let ((actions (ros-list-actions)))
        (expect actions :to-contain "nav2_msgs/action/Wait")
        (expect actions :to-contain "test_msgs/action/Fibonacci")))
    (it "Can insert Message Name in python"
      (with-temp-buffer
        (python-mode)
        (ros-insert-message "std_msgs/msg/String")
        (expect (s-trim (buffer-string)) :to-equal "String")))
    (it "Can insert  Message Import in python"
      (with-temp-buffer
        (python-mode)
        (ros-insert-message-import "std_msgs/msg/String" (point))
        (expect (s-trim (buffer-string)) :to-equal "from std_msgs.msg import String")))
    (it "Can insert Message Name in c++"
      (with-temp-buffer
        (c++-mode)
        (ros-insert-message "std_msgs/msg/String")
        (expect (s-trim (buffer-string)) :to-equal "std_msgs::msg::String")))
    (it "Can insert  Message Import in c++"
      (with-temp-buffer
        (c++-mode)
        (ros-insert-message-import "std_msgs/msg/String" (point))
        (expect (s-trim (buffer-string)) :to-equal "#include <std_msgs/msg/string.hpp>")))
    (it "Can insert  Message Import in c++ with camel case to snake case"
      (with-temp-buffer
        (c++-mode)
        (ros-insert-message-import "ackermann_msgs/msg/AckermannDrive" (point))
        (expect (s-trim (buffer-string)) :to-equal "#include <ackermann_msgs/msg/ackermann_drive.hpp>")))
    (it "Can insert Srv Name in python"
      (with-temp-buffer
        (python-mode)
        (ros-insert-srv "std_srvs/srv/Trigger")
        (expect (s-trim (buffer-string)) :to-equal "Trigger")))
    (it "Can insert Srv import in python"
      (with-temp-buffer
        (python-mode)
        (ros-insert-srv-import "std_srvs/srv/Trigger" (point))
        (expect (s-trim (buffer-string)) :to-equal "from std_srvs.srv import Trigger")))
    (it "Can insert Srv Name in c++"
      (with-temp-buffer
        (c++-mode)
        (ros-insert-srv "std_srvs/srv/Trigger")
        (expect (s-trim (buffer-string)) :to-equal "std_srvs::srv::Trigger")))
    (it "Can insert Srv Import in c++"
      (with-temp-buffer
        (c++-mode)
        (ros-insert-srv-import "std_srvs/srv/Trigger" (point))
        (expect (s-trim (buffer-string)) :to-equal "#include <std_srvs/srv/trigger.hpp>")))
    (it "Can insert Action Name in python"
      (with-temp-buffer
        (python-mode)
        (ros-insert-action  "nav2_msgs/action/Wait")
        (expect (s-trim (buffer-string)) :to-equal "Wait")))
    (it "Can insert Action import in python"
      (with-temp-buffer
        (python-mode)
        (ros-insert-action-import  "nav2_msgs/action/Wait" (point))
        (expect (s-trim (buffer-string)) :to-equal "from nav2_msgs.action import Wait")))
    (it "Can insert Action Name in c++"
      (with-temp-buffer
        (c++-mode)
        (ros-insert-action  "nav2_msgs/action/Wait")
        (expect (s-trim (buffer-string)) :to-equal "nav2_msgs::action::Wait")))
    (it "Can insert Action Import in c++"
      (with-temp-buffer
        (c++-mode)
        (ros-insert-action-import  "nav2_msgs/action/Wait" (point))
        (expect (s-trim (buffer-string)) :to-equal "#include <nav2_msgs/action/wait.hpp>"))))
  )











;;; ros-test.el ends here
